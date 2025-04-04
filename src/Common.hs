module Common where

import AbsLatte

showIdent :: Ident -> String
showIdent (Ident id) = id

showPosition :: BNFC'Position -> String
showPosition BNFC'NoPosition = "(unknown)"
showPosition (Just (line, col)) = "(" ++ (show line) ++ "," ++ (show col) ++ ")"

typeEq :: Type' a -> Type' b -> Bool
typeEq (Int _) (Int _) = True
typeEq (Str _) (Str _) = True
typeEq (Bool _) (Bool _) = True
typeEq (Void _) (Void _) = True
typeEq _ _ = False

showType :: Type' a -> String
showType (Int _) = "int"
showType (Str _) = "string"
showType (Bool _) = "boolean"
showType (Void _) = "void"
showType _ = "(internal)"

showRelOp :: RelOp -> String
showRelOp (LTH _) = "<"
showRelOp (LE _) = "<="
showRelOp (GTH _) = ">"
showRelOp (GE _) = ">="
showRelOp (EQU _) = "=="
showRelOp (NE _) = "!="

getStmtPos :: Stmt -> BNFC'Position
getStmtPos stmt =
    case stmt of
        Empty pos -> pos
        BStmt pos _ -> pos
        Decl pos _ _ -> pos
        Ass pos _ _ -> pos
        Incr pos _ -> pos
        Decr pos _ -> pos
        Ret pos _ -> pos
        VRet pos -> pos
        Cond pos _ _ -> pos
        CondElse pos _ _ _ -> pos
        While pos _ _ -> pos
        SExp pos _ -> pos

getExprPos :: Expr -> BNFC'Position
getExprPos expr =
    case expr of
        EVar pos _ -> pos
        ELitInt pos _ -> pos
        ELitTrue pos -> pos
        ELitFalse pos -> pos
        EApp pos _ _ -> pos
        EString pos _ -> pos
        Neg pos _ -> pos
        Not pos _ -> pos
        EMul pos _ _ _ -> pos
        EAdd pos _ _ _ -> pos
        ERel pos _ _ _ -> pos
        EAnd pos _ _ -> pos
        EOr pos _ _ -> pos
