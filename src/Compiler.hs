module Compiler (compile) where

import AbsLatte
import Env
import Typechecker
import Common
import Generation

import Data.Map as M

compile :: Program -> Either String String
compile program =
    case typecheck program of
        Left err -> Left (show err)
        Right tcresult ->
            case simplify program of
                Left err -> Left (show err)
                Right newProgram ->
                    let programNamesChanged = renameVariables newProgram in
                    Right $ generateCode programNamesChanged tcresult



-- Simplify some constructions in code:
-- if(true), if(false), code after 'return' etc.
simplify :: Program -> Either AnError Program
simplify (Program pos topdefs) =
    let simpleTopdefs = Prelude.map simplifyTopDef topdefs in
    case sequence $ Prelude.map checkReturn simpleTopdefs of
        Left err -> Left err
        Right newTopdefs -> Right $ Program pos newTopdefs

simplifyTopDef :: TopDef -> TopDef
simplifyTopDef (FnDef pos t id args b)  =
    FnDef pos t id args (simplifyBlock b)

simplifyBlock :: Block -> Block
simplifyBlock (Block pos stmts) =
    let newStmts = Prelude.map simplifyStmt stmts in
    Block pos (cutFromReturn newStmts)

simplifyStmt :: Stmt -> Stmt
simplifyStmt stmt =
    case stmt of
        BStmt pos b -> BStmt pos (simplifyBlock b)
        Cond pos expr stmtIn ->
            case expr of
                ELitTrue _ -> stmtIn
                ELitFalse _ -> Empty pos
                otherwise -> stmt
        CondElse pos expr stmtIn1 stmtIn2 ->
            case expr of
                ELitTrue _ -> stmtIn1
                ELitFalse _ -> stmtIn2
                otherwise -> stmt
        While pos expr stmtIn ->
            case expr of
                ELitFalse _ -> Empty pos
                otherwise -> stmt
        otherwise -> stmt

cutFromReturn :: [Stmt] -> [Stmt]
cutFromReturn [] = []
cutFromReturn (stmt:stmts) =
    case stmt of
        Ret pos expr -> [Ret pos expr]
        VRet pos -> [VRet pos]
        otherwise -> stmt:(cutFromReturn stmts)

checkReturn :: TopDef -> Either AnError TopDef
checkReturn topdef =
    let (FnDef _ t id _ b) = topdef in
    if typeEq t (Void ()) then
        Right topdef
    else case checkReturnBlock id b of
        Left err -> Left err
        Right b -> Right topdef
        
checkReturnBlock :: Ident -> Block -> Either AnError ()
checkReturnBlock fname (Block pos stmts) =
    case stmts of
        [] -> Left $ NoReturn pos fname
        otherwise ->
            let lastStmt = last stmts in
            checkReturnStmt fname lastStmt

checkReturnStmt :: Ident -> Stmt -> Either AnError ()
checkReturnStmt fname stmt =
    case stmt of
        BStmt _ b -> checkReturnBlock fname b
        Ret _ _ -> Right ()
        VRet _ -> Right ()
        CondElse _ _ stmt1 stmt2 ->
            case checkReturnStmt fname stmt1 of
                Left err -> Left err
                Right () ->
                    case checkReturnStmt fname stmt2 of
                        Left err -> Left err
                        Right () -> Right ()
        While pos expr _ ->
            case expr of
                ELitTrue _ -> Right ()
                otherwise -> Left $ NoReturn pos fname
        otherwise -> Left $ NoReturn (getStmtPos stmt) fname



-- Changle all variable names to 'variable@row_col' to get rid of all nuances
-- related to overshadowing
renameVariables :: Program -> Program
renameVariables (Program pos topdefs) =
    Program pos (Prelude.map renameVarsTopdef topdefs)

renameVarsTopdef :: TopDef -> TopDef
renameVarsTopdef (FnDef pos t id args b) =
    FnDef pos t id args (renameVarsBlock b M.empty)

renameVarsBlock :: Block -> (M.Map Ident Ident) -> Block
renameVarsBlock (Block pos stmts) m =
    Block pos (renameVarsStmts stmts m)

renameVarsStmts :: [Stmt] -> (M.Map Ident Ident) -> [Stmt]
renameVarsStmts [] _ = []
renameVarsStmts (stmt:stmts) m = case stmt of
    Empty pos -> (Empty pos):(renameVarsStmts stmts m)
    BStmt pos b -> (BStmt pos (renameVarsBlock b m)):(renameVarsStmts stmts m)
    Decl pos t items ->
        let (newMap, newStmt) = renameVarsDecl stmt m in
        newStmt:(renameVarsStmts stmts newMap)
    Ass pos left right ->
        let newLeft = renameVar left m
            newRight = renameVarsExpr right m in
        (Ass pos newLeft newRight):(renameVarsStmts stmts m)
    Incr pos id ->
        (Incr pos (renameVar id m)):(renameVarsStmts stmts m)
    Decr pos id ->
        (Decr pos (renameVar id m)):(renameVarsStmts stmts m)
    Ret pos expr ->
        (Ret pos (renameVarsExpr expr m)):(renameVarsStmts stmts m)
    VRet pos ->
        stmt:(renameVarsStmts stmts m)
    Cond pos expr stmt ->
        case renameVarsStmts [stmt] m of
            [newStmt] -> (Cond pos (renameVarsExpr expr m) newStmt):(renameVarsStmts stmts m)
            _ -> []
    CondElse pos expr stmt1 stmt2 -> do
        case (renameVarsStmts [stmt1] m, renameVarsStmts [stmt2] m) of
            ([newStmt1], [newStmt2]) ->
                (CondElse pos (renameVarsExpr expr m) newStmt1 newStmt2):(renameVarsStmts stmts m)
            _ -> []
    While pos expr stmt -> do
        case renameVarsStmts [stmt] m of
            [newStmt] -> (While pos (renameVarsExpr expr m) newStmt):(renameVarsStmts stmts m)
            _ -> []
    SExp pos expr -> (SExp pos (renameVarsExpr expr m)):(renameVarsStmts stmts m)

renameVar :: Ident -> (M.Map Ident Ident) -> Ident
renameVar id m = case M.lookup id m of
    Nothing -> id
    Just newId -> newId

renameVarsDecl :: Stmt -> (M.Map Ident Ident) -> (M.Map Ident Ident, Stmt)
renameVarsDecl (Decl pos t items) m =
    let (newMap, newItems) = renameVarsItems items m in
    (newMap, Decl pos t newItems)

renameVarsItems :: [Item] -> (M.Map Ident Ident) -> (M.Map Ident Ident, [Item])
renameVarsItems [] m = (m, [])
renameVarsItems (item:items) m =
    case item of
        NoInit pos id ->
            let newId = transformIdent id pos
                newMap = M.insert id newId m
                (retMap, retItems) = renameVarsItems items newMap in
            (retMap, (NoInit pos newId):retItems)
        Init pos id expr ->
            let newId = transformIdent id pos
                newExpr = renameVarsExpr expr m
                newMap = M.insert id newId m
                (retMap, retItems) = renameVarsItems items newMap in
            (retMap, (Init pos newId newExpr):retItems)

transformIdent :: Ident -> BNFC'Position -> Ident
transformIdent (Ident id) pos =
    case pos of
        BNFC'NoPosition -> Ident id
        Just (row, col) -> Ident (id ++ "@" ++ (show row) ++ "_" ++ (show col))

renameVarsExpr :: Expr -> (M.Map Ident Ident) -> Expr
renameVarsExpr expr m = case expr of
    EVar pos id -> EVar pos (renameVar id m)
    EApp pos id exprs -> EApp pos id (Prelude.map (\e -> renameVarsExpr e m) exprs)
    Neg pos expr -> Neg pos (renameVarsExpr expr m)
    Not pos expr -> Not pos (renameVarsExpr expr m)
    EMul pos expr1 op expr2 -> EMul pos (renameVarsExpr expr1 m) op (renameVarsExpr expr2 m)
    EAdd pos expr1 op expr2 -> EAdd pos (renameVarsExpr expr1 m) op (renameVarsExpr expr2 m)
    ERel pos expr1 op expr2 -> ERel pos (renameVarsExpr expr1 m) op (renameVarsExpr expr2 m)
    EAnd pos expr1 expr2 -> EAnd pos (renameVarsExpr expr1 m) (renameVarsExpr expr2 m)
    EOr pos expr1 expr2 -> EOr pos (renameVarsExpr expr1 m) (renameVarsExpr expr2 m)
    _ -> expr