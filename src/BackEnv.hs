module BackEnv where

import Data.Map as M
import Data.List
import Env
import Control.Monad.RWS
import Asm
import AbsLatte



type CmpEnv = (TcOutput, M.Map Ident Int)

-- Map var_name -> (address, type) X
-- nextVar X
-- Current function name X
-- Next label id X
-- String literals X 
-- Registers
type CmpState = ([M.Map Ident (Int, Type)], Int, String, Int, [String], [String])

type CmpMonad = RWS CmpEnv [AsmInstr] CmpState

putFunctionName :: Ident -> CmpMonad ()
putFunctionName (Ident name) = do
    (varAddr, nextVar, _, label, strings, regs) <- get
    put (varAddr, nextVar, name, label, strings, regs)

getFunctionName :: CmpMonad String
getFunctionName = do
  (_, _, name, _, _, _) <- get
  return name

nextLabelId :: CmpMonad Int
nextLabelId = do
    (varAddr, nextVar, name, label, strings, regs) <- get
    put (varAddr, nextVar, name, label + 1, strings, regs)
    return label

getReservedRegs :: CmpMonad [String]
getReservedRegs = do
    (_, _, _, _, _, regs) <- get
    return regs

putReservedRegs :: [String] -> CmpMonad ()
putReservedRegs regs = do
    (varAddr, nextVar, name, label, strings, _) <- get
    put (varAddr, nextVar, name, label, strings, regs)

stringLiteralLabel :: String -> CmpMonad String
stringLiteralLabel s = do
    (varAddr, nextVar, name, label, strings, regs) <- get
    let len = length strings
        index = case elemIndex s strings of
            Nothing -> len
            Just x -> len - x - 1
        newStrings = if index == len then s:strings else strings
    put (varAddr, nextVar, name, label, newStrings, regs)
    return ("_string" ++ show index)

getStrings :: CmpMonad [String]
getStrings = do
    (_, _, _, _, strings, _) <- get
    return strings

getVarState :: CmpMonad ([M.Map Ident (Int, Type)], Int)
getVarState = do
    (varAddr, nextVar, _, _, _, _) <- get
    return (varAddr, nextVar)

putVarState :: ([M.Map Ident (Int, Type)], Int) -> CmpMonad ()
putVarState (varAddr, nextVar) = do
    (_, _, name, label, strings, regs) <- get
    put (varAddr, nextVar, name, label, strings, regs)

initVarState :: ([M.Map Ident (Int, Type)], Int)
initVarState = ([M.empty], 8)

emptyVars :: CmpMonad ()
emptyVars = putVarState initVarState

askFunctions :: CmpMonad TcOutput
askFunctions = do
    (fns, _) <- ask
    return fns

askLocals :: CmpMonad (M.Map Ident Int)
askLocals = do
  (_, locals) <- ask
  return locals

localsSize :: CmpMonad String
localsSize = do
    name <- getFunctionName
    locals <- askLocals
    return $ show $ 8 * locals ! (Ident name)

localGen :: CmpMonad a -> CmpMonad a
localGen action = do
    state <- get
    r <- pass $ do
        (r, _) <- listen action
        return (r, const [])
    put state
    return r

reserveReg :: String -> CmpMonad ()
reserveReg reg = do
    reservedRegs <- getReservedRegs
    putReservedRegs (reg:reservedRegs)

reserveRegs :: Int -> CmpMonad [String]
reserveRegs n = do
    reservedRegs <- getReservedRegs
    let available = Prelude.take n $ registers64 Data.List.\\ reservedRegs
    mapM_ reserveReg available
    return available

freeRegs :: [String] -> CmpMonad ()
freeRegs regs = do
    reservedRegs <- getReservedRegs
    let newReservedRegs = reservedRegs Data.List.\\ regs
    putReservedRegs newReservedRegs

localReserve :: Int -> ([String] -> CmpMonad a) -> CmpMonad a
localReserve n action = do
    regs <- reserveRegs n
    res <- action regs
    freeRegs regs
    return res

localReserveReg :: String -> CmpMonad a -> CmpMonad a
localReserveReg reg = localReserveRegs [reg]

localReserveRegs :: [String] -> CmpMonad a -> CmpMonad a
localReserveRegs regs action = do
    forM_ regs reserveReg
    res <- action
    freeRegs regs
    return res

-- Calculating locals in program
localsProgram :: Program -> (M.Map Ident Int)
localsProgram (Program _ topDefs) = Prelude.foldl localsTopDef M.empty topDefs

localsTopDef :: (M.Map Ident Int) -> TopDef -> (M.Map Ident Int)
localsTopDef locals (FnDef _ _ name args block) =
    M.insert name (length args + localsBlock block) locals

localsBlock :: Block -> Int
localsBlock (Block _ stmts) = sum $ Prelude.map localsStmt stmts

localsStmt :: Stmt -> Int
localsStmt stmt = case stmt of
    BStmt _ b -> localsBlock b
    Decl _ _ items -> length items
    Cond _ _ stmt -> localsStmt stmt
    CondElse _ _ stmt1 stmt2 -> max (localsStmt stmt1) (localsStmt stmt2)
    While _ _ stmt -> localsStmt stmt
    _ -> 0

-- Labels & jumps
endLabel :: CmpMonad String
endLabel = do
  name <- getFunctionName
  return $ name ++ "#end"

jumpEndLabel :: CmpMonad ()
jumpEndLabel = do
  label <- endLabel
  tell [Jmp label]

twoLabels :: CmpMonad (String, Int, Int)
twoLabels = do
  name <- getFunctionName
  labelId1 <- nextLabelId
  labelId2 <- nextLabelId
  return (name, labelId1, labelId2)

orLabels :: CmpMonad (String, String)
orLabels = do
  (name, labelId1, labelId2) <- twoLabels
  return (name ++ "#orPush1#" ++ show labelId1,
    name ++ "#orAfter#" ++ show labelId2)

andLabels :: CmpMonad (String, String)
andLabels = do
  (name, labelId1, labelId2) <- twoLabels
  return (name ++ "#andPush0#" ++ show labelId1,
    name ++ "#andAfter#" ++ show labelId2)

ifLabel :: CmpMonad String
ifLabel = do
  name <- getFunctionName
  labelId <- nextLabelId
  return (name ++ "#ifAfter#" ++ show labelId)

ifElseLabels :: CmpMonad (String, String)
ifElseLabels = do
  (name, labelId1, labelId2) <- twoLabels
  return (name ++ "#ifElse#" ++ show labelId1,
    name ++ "#ifElseAfter#" ++ show labelId2)

whileLabels :: CmpMonad (String, String)
whileLabels = do
  (name, labelId1, labelId2) <- twoLabels
  return (name ++ "#whileBegin#" ++ show labelId1,
    name ++ "#whileAfter#" ++ show labelId2)
