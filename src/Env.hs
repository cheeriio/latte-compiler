module Env where

import Data.Map as M
import Control.Monad.Except
import Control.Monad.State

import AbsLatte
import Common

data TcError = FunctionRedeclaration Ident BNFC'Position BNFC'Position |
               ArgumentRepeated BNFC'Position Ident Ident BNFC'Position  |
               ReturnWhenVoid BNFC'Position Ident |
               ReturnWrongType BNFC'Position Ident Type Type |
               ReturnVoidIncorrect BNFC'Position Ident Type |
               MismatchedType BNFC'Position Type Type |
               NameNotInScope BNFC'Position Ident |
               TooFewArguments BNFC'Position Ident |
               TooManyArguments BNFC'Position Ident |
               FunctionNameReserved BNFC'Position Ident |
               WrongAddition BNFC'Position Type Type |
               WrongRelOpTypes BNFC'Position RelOp Type Type |
               VariableRedeclaredInBlock BNFC'Position Ident |
               NoMain |
               InternalError

instance Show TcError where
    show (FunctionRedeclaration id pos prevPos) =
        (showPosition pos) ++ ": Redeclaration of function " ++
        (identToString id) ++ ". Function was originally declared at " ++
        (show prevPos)
    show (ArgumentRepeated pos fname arg prevPos) =
        (showPosition pos) ++ ": Redeclaration of argument " ++
        (identToString arg) ++ " inside the function " ++ (identToString fname) ++
        ". Argument was previously declared at " ++ (showPosition prevPos)
    show (ReturnWhenVoid pos fname) =
        (showPosition pos) ++ ": Returning a value from inside function " ++
        (identToString fname) ++ " which has return type 'void'"
    show (ReturnWrongType pos fname retType t) =
        (showPosition pos) ++ ": Returning type " ++ (showType t) ++
        " from function " ++ (identToString fname) ++ " which has return type" ++
        (showType retType)
    show (ReturnVoidIncorrect pos fname t) =
        (showPosition pos) ++ ": Returning nothing from inside function " ++
        (identToString fname) ++ " which has return type " ++ (showType t)
    show (MismatchedType pos expectedType foundType) =
        (showPosition pos) ++ ": expected type " ++ (showType expectedType) ++
        " but got " ++ (showType foundType)
    show (NameNotInScope pos id) =
        (showPosition pos) ++ ": Name " ++ (identToString id) ++ " not in scope"
    show (TooFewArguments pos fname) =
        (showPosition pos) ++ ": Too few arguments passed when invoking function " ++
        (identToString fname)
    show (TooManyArguments pos fname) =
        (showPosition pos) ++ ": Too many arguments passed when invoking function " ++
        (identToString fname)
    show (FunctionNameReserved pos fname) =
        (showPosition pos) ++ ": Function name " ++ (identToString fname) ++ " is reserved"
    show (WrongAddition pos t1 t2) =
        (showPosition pos) ++ ": Addition for types " ++ (showType t1) ++
        " and " ++ (showType t2) ++ " not supported"
    show (WrongRelOpTypes pos op t1 t2) =
        (showPosition pos) ++ ": " ++ (showRelOp op) ++ " for types " ++ (showType t1) ++
        " and " ++ (showType t2) ++ " not supported"
    show (VariableRedeclaredInBlock pos id) =
        (showPosition pos) ++ ": Variable " ++ (identToString id) ++
        " was already declared inside the block"
    show NoMain = "No function main found"
    show InternalError = "Unexcpected internal error"

identToString :: Ident -> String
identToString (Ident id) = id

-- Currently we only return info on function types
type TcOutput = M.Map Ident Type

-- Functions X Variables X Current block variables X Return Info
type TcState = (M.Map Ident Type, M.Map Ident Type, M.Map Ident Type, (Ident, Type))

type TcMonad a = ExceptT TcError (State TcState) a

data AnError = NoReturn BNFC'Position Ident

instance Show AnError where
    show (NoReturn pos fname) =
        (showPosition pos) ++ ": Control reaches end of function " ++
        (identToString fname) ++ " without return"
