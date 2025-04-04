module Generation (generateCode) where

import AbsLatte
import Env
import BackEnv
import Asm
import Common

import Data.Map as M
import Control.Monad.RWS

generateCode :: Program -> TcOutput -> String
generateCode program tcresult =
    let tcResultWithInternals = addInternals internalFunctions tcresult
        locals = localsProgram program
        initEnv = (tcResultWithInternals, locals)
        initState = ([M.empty], 8, "", 0, [], [])
        (_, _, output) = runRWS (emitProgram program) initEnv initState in
    printAsm output

addInternals :: (M.Map Ident Type) -> (M.Map Ident Type) -> (M.Map Ident Type)
addInternals ints tcresult =
    M.union tcresult ints

emitProgram :: Program -> CmpMonad ()
emitProgram (Program _ topdefs) = do
    tell [Global "main"]
    tell $ Prelude.map Extern builtInFunctions
    tell [SectionText]
    forM_ topdefs emitTopDef
    strings <- getStrings
    unless (Prelude.null strings) $ do
        tell [SectionData]
        emitStrings (reverse strings) 0

emitTopDef :: TopDef -> CmpMonad ()
emitTopDef (FnDef _ _ name args b) = do
    emptyVars
    putFunctionName name
    emitFunctionHeader
    emitArgs args
    emitBlock b
    emitFunctionFooter

emitFunctionHeader :: CmpMonad ()
emitFunctionHeader = do
    name <- getFunctionName
    locSize <- localsSize
    tell [
        Label name,
        Push "rbp",
        Mov "rbp" "rsp",
        Sub "rsp" locSize]

emitFunctionFooter :: CmpMonad ()
emitFunctionFooter = do
    name <- getFunctionName
    locSize <- localsSize
    tell [Label $ name ++ "#end"]
    -- when (name == "main") $
    --     tell [Call "_gcClean"]
    tell [
        Add "rsp" locSize,
        Pop "rbp",
        Return]

emitArgs :: [Arg] -> CmpMonad ()
emitArgs args = do
    let argsDecl = Prelude.map (\(Arg pos t id) -> Decl pos t [NoInit pos id]) args
        argsRegNum = min 6 $ length args
        argsRegs = Prelude.take argsRegNum registersArgs
        argsInRegs = Prelude.take argsRegNum args
        argsWithRegs = zip argsRegs argsInRegs
        restArgs = Prelude.drop argsRegNum args
        argsIndexes = Prelude.map (*8) [2..] :: [Int]
    forM_ argsRegs reserveReg
    forM_ argsDecl emitStmt
    forM_ argsWithRegs $ \(reg, Arg _ _ ident) -> do
        localReserve 1 $ \[addr] -> do
            void $ emitVarLocation ident
            tell [
                Pop addr,
                Mov ("[" ++ addr ++ "]") reg]
            freeRegs [reg]
    forM_ (zip restArgs argsIndexes) $ \(Arg _ _ ident, index) ->
        localReserve 2 $ \[addr, val] -> do
            void $ emitVarLocation $  ident
            tell [
                Pop addr,
                Mov val $ "[rbp" ++ " + " ++ show index ++ "]",
                Mov ("[" ++ addr ++ "]") val]

emitVarLocation :: Ident -> CmpMonad Type
emitVarLocation id = do
    (vars, _) <- getVarState
    let varsScope = Prelude.filter (M.member id) vars
    let (addr, t) = head varsScope ! id
    localReserve 1 $ \[r] ->
        tell [
            Mov r "rbp",
            Sub r $ show addr,
            Push r]
    return t

emitStmt :: Stmt -> CmpMonad ()
emitStmt stmt = case stmt of
    Empty _ -> return ()
    BStmt _ block -> emitBlock block
    Decl _ t items -> forM_ items $ emitDecl t
    Ass _ left expr -> do
        _ <- emitExpr expr
        assign left
    Incr pos id ->
        emitStmt $
            Ass pos id (EAdd BNFC'NoPosition (EVar BNFC'NoPosition id) (Plus BNFC'NoPosition) (ELitInt BNFC'NoPosition 1))
    Decr pos id ->
        emitStmt $
            Ass pos id (EAdd BNFC'NoPosition (EVar BNFC'NoPosition id) (Minus BNFC'NoPosition) (ELitInt BNFC'NoPosition 1))
    Ret _ expr -> do
        _ <- emitExpr expr
        tell [Pop "rax"]
        jumpEndLabel
    VRet _ -> jumpEndLabel
    Cond _ expr stmt -> do
        _ <- emitExpr expr
        afterLabel <- ifLabel
        localReserve 1 $ \[r] ->
            tell [
                Pop r,
                Cmp r "0",
                Je afterLabel]
        emitStmt stmt
        tell [Label afterLabel]
    CondElse _ expr stmt1 stmt2 -> do
        _ <- emitExpr expr
        (elseLabel, afterLabel) <- ifElseLabels
        localReserve 1 $ \[r] ->
            tell [
                Pop r,
                Cmp r "0",
                Je elseLabel]
        emitStmt stmt1
        tell [
            Jmp afterLabel,
            Label elseLabel]
        emitStmt stmt2
        tell [Label afterLabel]
    While _ expr stmt -> do
        (beginLabel, afterLabel) <- whileLabels
        tell [Label beginLabel]
        _ <- emitExpr expr
        localReserve 1 $ \[r] ->
            tell [
                Pop r,
                Cmp r "0",
                Je afterLabel]
        emitStmt stmt
        tell [
            Jmp beginLabel,
            Label afterLabel]
    SExp _ expr -> do
        t <- emitExpr expr
        unless (typeEq t (Void BNFC'NoPosition)) $
            tell [Add "rsp" "8"]

emitBlock :: Block -> CmpMonad ()
emitBlock (Block _ stmts) = do
    (vars, nextVar) <- getVarState
    putVarState (M.empty : vars, nextVar)
    forM_ stmts emitStmt
    (vars', nextVar') <- getVarState
    putVarState (tail vars', nextVar')

emitDecl :: Type -> Item -> CmpMonad ()
emitDecl t (Init pos ident expr) = do
    emitDecl t $ NoInit pos ident
    _ <- emitExpr expr
    assign ident
emitDecl t (NoInit _ id) = do
    (vars, addr) <- getVarState
    let (scopeVars:rest) = vars
    let scopeVarsAfter = M.insert id (addr, t) scopeVars
    putVarState (scopeVarsAfter: rest, addr + 8)
    tell [Mov (showLocalVarAddress addr) "0"]

showLocalVarAddress :: Int -> String
showLocalVarAddress n = "qword[rbp - " ++ (show n) ++ "]"

assign :: Ident -> CmpMonad ()
assign id = do
  _ <- emitVarLocation id
  localReserve 2 $ \[l, e] ->
    tell [
      Pop l,
      Pop e,
      Mov ("qword[" ++ l ++ "]") e]

emitExpr :: Expr -> CmpMonad Type
emitExpr expr = case expr of
    EVar _ id -> dereference id
    ELitInt _ n -> do
        tell [Push $ show n]
        return $ Int BNFC'NoPosition
    ELitTrue _ -> do
        tell [Push "1"]
        return $ Bool BNFC'NoPosition
    ELitFalse _ -> do
        tell [Push "0"]
        return $ Bool BNFC'NoPosition
    EApp _ id exprs -> emitApp id exprs
    EString _ s -> do
        stringLabel <- stringLiteralLabel s
        localReserveReg "rax" $
            tell [
                Mov (head registersArgs) stringLabel,
                Call "_copyString",
                Push "rax"]
        return $ Str BNFC'NoPosition
    AbsLatte.Neg _ expr -> do
        _ <- emitExpr expr
        localReserve 1 $ \[r] ->
            tell [Pop r, Asm.Neg r, Push r]
        return $ Int BNFC'NoPosition
    Not _ expr -> do
        _ <- emitExpr expr
        localReserve 1 $ \[r] ->
            tell [Pop r, Xor r "1", Push r]
        return $ Bool BNFC'NoPosition
    EMul _ expr1 op expr2 -> emitMulOp expr1 expr2 op
    EAdd _ expr1 op expr2 -> emitAddOp expr1 expr2 op
    ERel _ expr1 op expr2 -> do
        emitRelOp expr1 expr2 op
        return $ Bool BNFC'NoPosition
    EAnd _ expr1 expr2 -> do
        emitAnd expr1 expr2
        return $ Bool BNFC'NoPosition
    EOr _ expr1 expr2 -> do
        emitOr expr1 expr2
        return $ Bool BNFC'NoPosition

emitApp :: Ident -> [Expr] -> CmpMonad Type
emitApp id exprs = do
    let argsLen = length exprs
        argsRegs = Prelude.take (min 6 argsLen) registersArgs
        argsToRemove = argsLen - 6
    forM_ (reverse exprs) emitExpr
    let callReg = last registers64
    tell $ Prelude.map Pop argsRegs
    let (Ident name) = id
    tell [Call name]
    when (argsToRemove > 0) $
        tell [Add "rsp" $ show $ argsToRemove * 8]
    funs <- askFunctions
    let (Fun _ t _) = funs ! id
    unless (typeEq t (Void BNFC'NoPosition)) $
        localReserveReg "rax" $
            tell [Push "rax"]
    return t
    

emitMulOp :: Expr -> Expr -> MulOp -> CmpMonad Type
emitMulOp expr1 expr2 op = do
    _ <- emitExpr expr1
    _ <- emitExpr expr2
    localReserveReg "rax" $
        localReserve 1 $ \[r] ->
            tell [
                Pop r,
                Pop "rax",
                Cqo,
                mulOpStmt op r,
                Push $ mulOpResult op]
    return $ Int BNFC'NoPosition

mulOpStmt :: MulOp -> String -> AsmInstr
mulOpStmt (Times _) = Mul
mulOpStmt (AbsLatte.Div _) = Asm.Div
mulOpStmt (Mod _) = Asm.Div

mulOpResult :: MulOp -> String
mulOpResult (Times _) = "rax"
mulOpResult (AbsLatte.Div _) = "rax"
mulOpResult (Mod _) = "rdx"

emitAddOp :: Expr -> Expr -> AddOp -> CmpMonad Type
emitAddOp expr1 expr2 (Plus _) = do
    t <- localGen $ emitExpr expr1
    case t of
        Str _ -> void $ emitApp (Ident "_concatStrings") [expr1, expr2]
        _ -> binaryOp expr1 expr2 Add
    return t
emitAddOp expr1 expr2 (Minus _) = do
    binaryOp expr1 expr2 Sub
    return $ Int BNFC'NoPosition

binaryOp :: Expr -> Expr -> (String -> String -> AsmInstr) -> CmpMonad ()
binaryOp expr1 expr2 op = do
    _ <- emitExpr expr1
    _ <- emitExpr expr2
    localReserve 2 $ \[r1, r2] ->
        tell [
        Pop r2,
        Pop r1,
        op r1 r2,
        Push r1]

emitRelOp :: Expr -> Expr -> RelOp -> CmpMonad ()
emitRelOp expr1 expr2 op = do
    _ <- emitExpr expr2
    _ <- emitExpr expr1
    localReserve 2 $ \[r1, r2] -> do
        let r1_32 = to32bit r1
            r1_8 = to8bit r1
        tell [
            Pop r1,
            Pop r2,
            Cmp r1 r2,
            Set op r1_8,
            Movzx r1_32 r1_8,
            Push r1]

emitAnd :: Expr -> Expr -> CmpMonad ()
emitAnd expr1 expr2 = do
    _ <- emitExpr expr1
    (push0Label, afterAndLabel) <- andLabels
    localReserve 1 $ \[r] ->
        tell [
            Pop r,
            Cmp r "0",
            Je push0Label]
    _ <- emitExpr expr2
    tell [
        Jmp afterAndLabel,
        Label push0Label,
        Push "0",
        Label afterAndLabel]

emitOr :: Expr -> Expr -> CmpMonad ()
emitOr expr1 expr2 = do
    _ <- emitExpr expr1
    (push1Label, afterOrLabel) <- orLabels
    localReserve 1 $ \[r] ->
        tell [
            Pop r,
            Cmp r "0",
            Jne push1Label]
    _ <- emitExpr expr2
    tell [
        Jmp afterOrLabel,
        Label push1Label,
        Push "1",
        Label afterOrLabel]

dereference :: Ident -> CmpMonad Type
dereference id = do
    t <- emitVarLocation id
    localReserve 1 $ \[r] ->
        tell [
        Pop r,
        Mov r $ "qword[" ++ r ++ "]",
        Push r]
    return t

emitStrings :: [String] -> Int -> CmpMonad ()
emitStrings [] _ = return ()
emitStrings (s:rest) i = do
  let label = "_string" ++ show i
  tell [DeclData label Byte [parseStringLiteral s]]
  emitStrings rest (i+1)

parseStringLiteral :: String -> String
parseStringLiteral s =
  let parsedString = show s
      content = init (tail parsedString) in
  "`" ++ content ++ "\\0`"