module Typechecker (typecheck) where

import Data.Map as M
import Control.Monad.Except
import Control.Monad.State

import AbsLatte
import Env
import Common

typecheck :: Program -> Either TcError TcOutput
typecheck (Program pos topdefs) =
    case getFunctionTypes topdefs of
        Left err -> Left err
        Right functions ->
            let (result, finalState) =
                    runState (runExceptT $ checkProgram (Program pos topdefs)) (functions, M.empty, M.empty, (Ident "", Void BNFC'NoPosition)) in
            case result of
                Left err -> Left err
                Right _ ->
                    let (functions, _, _, _) = finalState in
                    Right functions

getFunctionTypes :: [TopDef] -> Either TcError (M.Map Ident Type)
getFunctionTypes topdefs =
    case getFunctionTypesImpl topdefs M.empty of
        Left err -> Left err
        Right m ->
            let userFunctions = (M.map (\(_, a) -> a) m) in
            if M.member (Ident "main") userFunctions then
                Right $ addSpecialFunctions userFunctions
            else Left NoMain

addSpecialFunctions :: (M.Map Ident Type) -> (M.Map Ident Type)
addSpecialFunctions m =
    let specialFuncs = [("printInt", Void BNFC'NoPosition, [Int BNFC'NoPosition]),
                        ("printString", Void BNFC'NoPosition, [Str BNFC'NoPosition]),
                        ("error", Void BNFC'NoPosition, []),
                        ("readInt", Int BNFC'NoPosition, []),
                        ("readString", Str BNFC'NoPosition, [])] in
    addSpecialFuncsImpl m specialFuncs

addSpecialFuncsImpl :: (M.Map Ident Type) -> [(String, Type, [Type])] -> (M.Map Ident Type)
addSpecialFuncsImpl m [] = m
addSpecialFuncsImpl m ((fname, ret, args):fs) =
    M.insert (Ident fname) (Fun BNFC'NoPosition ret args) (addSpecialFuncsImpl m fs)

getFunctionTypesImpl :: [TopDef] -> (M.Map Ident (BNFC'Position, Type)) ->
                        Either TcError (M.Map Ident (BNFC'Position, Type))
getFunctionTypesImpl [] m = Right m
getFunctionTypesImpl ((FnDef pos t id args _):topdefs) m =
    let (Ident fname) = id in
    if elem fname ["printInt", "printString", "error", "readInt", "readString"] then
        Left (FunctionNameReserved pos id)
    else case M.lookup id m of
        Just (prevPos, _) ->
            Left (FunctionRedeclaration id pos prevPos)
        Nothing -> 
            let types = getArgTypes args in
                getFunctionTypesImpl
                    topdefs
                    (M.insert id (pos, (Fun BNFC'NoPosition t types)) m)

getArgTypes :: [Arg] -> [Type]
getArgTypes [] = []
getArgTypes ((Arg _ t _):args) = t:(getArgTypes args)

localCheck :: TcMonad a -> TcMonad a
localCheck comp = do
    state <- get
    ret <- comp
    put state
    return ret

checkProgram :: Program -> TcMonad ()
checkProgram (Program _ topdefs) =
    forM_ topdefs checkTopDef

checkTopDef :: TopDef -> TcMonad ()
checkTopDef (FnDef pos t id args b) = do
    localCheck $ do
        checkArgs pos id args
        (funcs, vars, bvars, _) <- get
        put (funcs, M.union bvars vars, M.empty, (id, t))
        checkBlock b

checkArgs :: BNFC'Position -> Ident -> [Arg] -> TcMonad ()
checkArgs pos id args = do
    correctArgs pos id M.empty args
    forM_ args checkArg

correctArgs :: BNFC'Position -> Ident -> (M.Map Ident BNFC'Position) -> [Arg] -> TcMonad ()
correctArgs _ _ _ [] = return ()
correctArgs pos fname m ((Arg argPos _ id):args) = do
    case M.lookup id m of
        Nothing ->
            correctArgs pos fname (M.insert id argPos m) args
        Just prev_pos ->
            throwError (ArgumentRepeated argPos fname id prev_pos)

checkArg :: Arg -> TcMonad ()
checkArg (Arg pos t id) = do
    (funcs, vars, bvars, retInfo) <- get
    put (funcs, M.insert id t vars, bvars, retInfo)

checkBlock :: Block -> TcMonad ()
checkBlock (Block pos stmts) = do
    localCheck $ do
        (funcs, vars, bvars, ret) <- get
        put (funcs, M.union bvars vars, M.empty, ret)
        forM_ stmts checkStmt

checkStmt :: Stmt -> TcMonad ()
checkStmt stmt = do
    (fname, retType) <- askReturn
    case stmt of
        Empty _ ->
            return ()
        BStmt pos b ->
            checkBlock b
        Decl pos t items ->
            forM_ items (checkDecl t)
        Ass pos left right ->
            checkAssignment (Ass pos left right)
        Incr pos id -> do
            t <- typeOf (EVar BNFC'NoPosition id)
            when (not $ typeEq t (Int ())) $ throwError (MismatchedType pos (Int BNFC'NoPosition) t)
        Decr pos id -> do
            t <- typeOf (EVar BNFC'NoPosition id)
            when (not $ typeEq t (Int ())) $ throwError (MismatchedType pos (Int BNFC'NoPosition) t)
        Ret pos expr -> do
            t <- typeOf expr
            when (typeEq retType (Void ())) $ throwError (ReturnWhenVoid pos fname)
            when (not $ typeEq retType t) $ throwError (ReturnWrongType pos fname retType t)
        VRet pos ->
            when (not $ typeEq retType (Void ())) $ throwError (ReturnVoidIncorrect pos fname retType)
        Cond pos expr stmt -> do
            t <- typeOf expr
            when (not $ typeEq t (Bool ())) $ throwError (MismatchedType pos (Bool BNFC'NoPosition) t)
            checkStmt stmt
        CondElse pos expr stmt1 stmt2 -> do
            t <- typeOf expr
            when (not $ typeEq t (Bool ())) $ throwError (MismatchedType pos (Bool BNFC'NoPosition) t)
            checkStmt stmt1
            checkStmt stmt2
        While pos expr stmt -> do
            t <- typeOf expr
            when (not $ typeEq t (Bool ())) $ throwError (MismatchedType pos (Bool BNFC'NoPosition) t)
            checkStmt stmt
        SExp pos expr ->
            void $ typeOf expr

checkDecl :: Type -> Item -> TcMonad ()
checkDecl t item = do
    (funcs, vars, bvars, ret) <- get
    case item of
        NoInit pos id -> do
            when (M.member id bvars) $ throwError (VariableRedeclaredInBlock pos id)
            put (funcs, vars, M.insert id t bvars, ret)
        Init pos id expr -> do
            when (M.member id bvars) $ throwError (VariableRedeclaredInBlock pos id)
            exprType <- typeOf expr
            when (not $ typeEq t exprType) $ throwError (MismatchedType pos t exprType)
            put (funcs, vars, M.insert id t bvars, ret)

checkAssignment :: Stmt -> TcMonad ()
checkAssignment stmt = do
    case stmt of
        Ass pos id right -> do
            ltype <- typeOf (EVar BNFC'NoPosition id)
            rtype <- typeOf right
            when (not $ typeEq ltype rtype) $ throwError (MismatchedType pos ltype rtype)
        otherwise -> throwError InternalError

typeOf :: Expr -> TcMonad Type
typeOf expr = do
    case expr of
        EVar pos id -> do
            (_, vars, bvars, _) <- get
            case M.lookup id bvars of
                Just t -> return t
                Nothing ->
                    case M.lookup id vars of
                        Just t -> return t
                        Nothing -> throwError $ (NameNotInScope pos id)
        ELitInt _ _ ->
            return $ Int BNFC'NoPosition
        ELitTrue _ ->
            return $ Bool BNFC'NoPosition
        ELitFalse _ ->
            return $ Bool BNFC'NoPosition
        EApp pos id exprs ->
            typeOfApp pos id exprs
        EString _ _ ->
            return $ Str BNFC'NoPosition
        Neg pos expr -> do
            t <- typeOf expr
            when (not $ typeEq t (Int ())) $ throwError (MismatchedType pos (Int BNFC'NoPosition) t)
            return $ Int BNFC'NoPosition
        Not pos expr -> do
            t <- typeOf expr
            when (not $ typeEq t (Bool ())) $ throwError (MismatchedType pos (Bool BNFC'NoPosition) t)
            return $ Bool BNFC'NoPosition
        EMul _ expr1 _ expr2 -> do
            assertType (Int BNFC'NoPosition) expr1 expr2
            return $ Int BNFC'NoPosition
        EAdd pos expr1 _ expr2 -> do
            t1 <- typeOf expr1
            case t1 of
                Int _ -> do
                    assertType (Int BNFC'NoPosition) expr1 expr2
                    return $ Int BNFC'NoPosition
                Str _ -> do
                    assertType (Str BNFC'NoPosition) expr1 expr2
                    return $ Str BNFC'NoPosition
                t1 -> do
                    t2 <- typeOf expr2
                    throwError (WrongAddition pos t1 t2)
        ERel pos expr1 op expr2 -> do
            t1 <- typeOf expr1
            case op of
                EQU _ ->
                    case t1 of
                        Bool _ -> assertType (Bool BNFC'NoPosition) expr1 expr2
                        Int _ -> assertType (Int BNFC'NoPosition) expr1 expr2
                        otherwise -> do
                            t2 <- typeOf expr2
                            throwError (WrongRelOpTypes pos op t1 t2)
                NE _ ->
                    case t1 of
                        Bool _ -> assertType (Bool BNFC'NoPosition) expr1 expr2
                        Int _ -> assertType (Int BNFC'NoPosition) expr1 expr2
                        otherwise -> do
                            t2 <- typeOf expr2
                            throwError (WrongRelOpTypes pos op t1 t2)
                otherwise ->
                    assertType (Int BNFC'NoPosition) expr1 expr2
            return $ Bool BNFC'NoPosition
        EAnd _ expr1 expr2 -> do
            assertType (Bool BNFC'NoPosition) expr1 expr2
            return $ Bool BNFC'NoPosition
        EOr _ expr1 expr2 -> do
            assertType (Bool BNFC'NoPosition) expr1 expr2
            return $ Bool BNFC'NoPosition

typeOfApp :: BNFC'Position -> Ident -> [Expr] -> TcMonad Type
typeOfApp pos id exprs = do
    (funcs, _, _, _) <- get
    case M.lookup id funcs of
        Nothing -> throwError $ (NameNotInScope pos id)
        Just t -> case t of
            Fun _ t args -> do
                correctAppArgs pos id exprs args
                return t
            otherwise -> throwError InternalError

correctAppArgs :: BNFC'Position -> Ident -> [Expr] -> [Type] -> TcMonad ()
correctAppArgs _ _ [] [] = return ()
correctAppArgs pos fname [] _ =
    throwError (TooFewArguments pos fname)
correctAppArgs pos fname _ [] =
    throwError (TooManyArguments pos fname)
correctAppArgs pos fname (expr:exprs) (typ:types) = do
    let exprPos = getExprPos expr
    exprType <- typeOf expr
    when (not $ typeEq exprType typ) $ throwError (MismatchedType exprPos typ exprType)
    correctAppArgs pos fname exprs types

assertType :: Type -> Expr -> Expr -> TcMonad ()
assertType t expr1 expr2 = do
    let pos1 = getExprPos expr1
        pos2 = getExprPos expr2
    t1 <- typeOf expr1
    t2 <- typeOf expr2
    when (not $ typeEq t1 t) $ throwError (MismatchedType pos1 t t1)
    when (not $ typeEq t2 t) $ throwError (MismatchedType pos2 t t2)
    return ()

askReturn :: TcMonad (Ident, Type)
askReturn = do
    (_, _, _, (fname, t)) <- get
    return (fname, t)
