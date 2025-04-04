module Asm where

import AbsLatte
import Data.List
import Data.Map as M

data Size = Byte | Word | DWord | QWord deriving (Eq)

instance Show Size where
    show Byte = "db"
    show Word = "dw"
    show DWord = "dd"
    show QWord = "dq"

data AsmInstr =
    Add String String |
    Sub String String |
    Div String |
    Mul String |
    Neg String |
    Xor String String |
    Test String String |
    Cmp String String |
    Jmp String |
    Je String |
    Jne String |
    Label String |
    Call String |
    Return |
    Mov String String |
    Movzx String String |
    Set RelOp String |
    Pop String |
    Push String |
    Cqo |
    DeclData String Size [String] |
    Extern String |
    Global String |
    SectionData |
    SectionText
    deriving (Eq)

instance Show AsmInstr where
    show (Add op1 op2) = showOperation "add" op1 op2
    show (Sub op1 op2) = showOperation "sub" op1 op2
    show (Asm.Div op) = "idiv " ++ op
    show (Mul op) = "imul " ++ op
    show (Asm.Neg op) = "neg " ++ op
    show (Xor op1 op2) = showOperation "xor" op1 op2
    show (Test op1 op2) = showOperation "test" op1 op2
    show (Cmp op1 op2) = showOperation "cmp" op1 op2
    show (Jmp label) = "jmp " ++ label
    show (Je label) = "je " ++ label
    show (Jne label) = "jne " ++ label
    show (Label label) = label ++ ":"
    show (Call f) =
        if elem f builtInFunctions then
            "push rbx" ++
            "\n\tmov rbx, rsp" ++
            "\n\tand rsp, -16" ++
            "\n\tcall " ++ f ++
            "\n\tmov rsp, rbx" ++
            "\n\tpop rbx"
        else
            "call " ++ f
    show Return = "ret"
    show (Mov op1 op2) = showOperation "mov" op1 op2
    show (Movzx op1 op2) = showOperation "movzx" op1 op2
    show (Set relop op) = (showRelOp relop) ++ " " ++ op
    show (Pop op) = "pop " ++ op
    show (Push op) = "push " ++ op
    show Cqo = "cqo"
    show (DeclData name size content) =
        name ++ " " ++ (show size) ++ " " ++ (showDataContent content)
    show (Extern name) = "extern " ++ name
    show (Global name) = "global " ++ name
    show SectionData = "\nsection .data"
    show SectionText = "\nsection .text"

showOperation :: String -> String -> String -> String
showOperation operation op1 op2 = operation ++ " " ++ op1 ++ ", " ++ op2

showRelOp :: RelOp -> String
showRelOp op = case op of
    LTH _ -> "setl"
    LE _ -> "setle"
    GTH _ -> "setg"
    GE _ -> "setge"
    EQU _ -> "sete"
    NE _ -> "setne"

showDataContent :: [String] -> String
showDataContent [] = ""
showDataContent [last] = last
showDataContent (h:t) = h ++ ", " ++ (showDataContent t)

registers64 :: [String]
registers64 = ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11"]

registers32 :: [String]
registers32 = ["eax", "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"]

registers8 :: [String]
registers8 = ["al", "dil", "sil", "dl", "cl", "r8b", "r9b", "r10b", "r11b"]

registersArgs :: [String]
registersArgs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

to32bit :: String -> String
to32bit reg =
    let (Just index) = elemIndex reg registers64 in
    registers32 !! index

to8bit :: String -> String
to8bit reg =
    let (Just index) = elemIndex reg registers64 in
    registers8 !! index

builtInFunctions :: [String]
builtInFunctions =
    ["printInt", "printString", "error", "readInt", "readString",
     "_copyString", "_concatStrings"]

internalFunctions :: M.Map Ident Type
internalFunctions =
    M.fromList [(Ident "_copyString", Fun BNFC'NoPosition (Str BNFC'NoPosition) [Str BNFC'NoPosition]),
                (Ident "_concatStrings", Fun BNFC'NoPosition (Str BNFC'NoPosition) [(Str BNFC'NoPosition), (Str BNFC'NoPosition)])]

printAsm :: [AsmInstr] -> String
printAsm [] = ""
printAsm (instr:instrs) =
    (addTab instr) ++ (show instr) ++ "\n" ++ (printAsm instrs)

addTab :: AsmInstr -> String
addTab instr = case instr of
    Extern _ -> ""
    Global _ -> ""
    Label _ -> ""
    SectionData -> ""
    SectionText -> ""
    _ -> "\t"
