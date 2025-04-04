module Main (main) where

import System.Exit (exitFailure)
import Control.Exception
import System.IO
import System.IO.Error
import System.Environment
import System.Directory
import System.FilePath.Posix (takeBaseName, takeDirectory)
import System.Process

import AbsLatte
import ParLatte
import ErrM

import Compiler

main :: IO ()
main = do
    path <- parseArgs
    src <- readSourceCode path
    case pProgram $ myLexer src of
        Bad err -> exit err
        Ok tree ->
            case compile tree of
                Left err -> exit err
                Right code -> do
                    hPutStrLn stderr "OK"
                    let directory = takeDirectory path
                        name = takeBaseName path
                        output_file = directory ++ "/" ++ name ++ ".s"
                        temp_file = directory ++ "/" ++ name ++ ".o"
                        program_file = directory ++ "/" ++ name
                    writeFile output_file code
                    callCommand ("nasm -f elf64 " ++ output_file ++ " -o " ++ temp_file ++ " > /dev/null 2>&1")
                    callCommand ("gcc -w " ++ temp_file ++ " lib/runtime.o -o " ++ program_file ++ " > /dev/null 2>&1")
                    removeFile temp_file
                    return ()

parseArgs :: IO String
parseArgs = do
    args <- getArgs
    case args of
        [path] -> return path
        _ -> do
            progName <- getProgName
            exit ("Usage: " ++ progName ++ " <source file>")


readSourceCode :: String -> IO String
readSourceCode path = do
    result <- try (readFile path)
    case result of
        Left e
            | isDoesNotExistError e -> exit "File does not exist."
            | isPermissionError e -> exit "Cannot read file. Permission denied."
            | otherwise -> exit "Cannot read file. Unknown error."
        Right src -> return src

exit :: String -> IO a
exit message = do
    hPutStrLn stderr ("ERROR\n" ++ message)
    exitFailure
