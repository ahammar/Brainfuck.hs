
-- brainfuck.hs -- A simple brainfuck interpreter.
-- Written by Andreas Hammar <ahammar@gmail.com>
--
-- This is free and unencumbered software released into the public domain.
-- See the UNLICENSE file for details.

module Main where

import Control.Monad.Error
import Data.ByteString.Internal
import Data.Char
import Data.Word
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec

data Instruction = MoveRight           --  >
                 | MoveLeft            --  <
                 | Increment           --  +
                 | Decrement           --  -
                 | ReadByte            --  ,
                 | WriteByte           --  .
                 | Loop [Instruction]  -- []

-- Parsing
simple ch ins = do
    char ch
    return ins

moveRight = simple '>' MoveRight
moveLeft  = simple '<' MoveLeft
increment = simple '+' Increment
decrement = simple '-' Decrement
readByte  = simple ',' ReadByte
writeByte = simple '.' WriteByte

loop = do
    char '['
    instructions <- many instruction
    char ']'
    return $ Loop instructions

instruction = (moveRight <|> moveLeft <|> increment
          <|> decrement <|> readByte <|> writeByte <|> loop)

program = many instruction

parseProgram input = case parse program "brainfuck" source of
    Left  err  -> throwError $ Parser err
    Right prog -> return prog
    where source = filter (`elem` "<>+-,.[]") input

-- Execution
type Byte = Word8
type Environment = ([Byte], Byte, [Byte])

cleanEnvironment = (zeroes, 0, zeroes)
    where zeroes = 0 : zeroes

exec (x:xs, y, zs) MoveLeft  = return $ (xs, x, y:zs)
exec (xs, y, z:zs) MoveRight = return $ (y:xs, z, zs)
exec (xs, y, zs)   Increment = return $ (xs, y+1, zs)
exec (xs, y, zs)   Decrement = return $ (xs, y-1, zs)

exec env@(_, y, _) WriteByte = putChar (w2c y) >> return env
exec (xs, _, zs)   ReadByte  = do
    ch <- getChar
    return $ (xs, c2w ch, zs)

exec env@(_, 0, _)   (Loop is) = return env
exec env@(_, _, _) l@(Loop is) = do
    newEnv <- foldM exec env is
    exec newEnv l

runProgram = do foldM_ exec cleanEnvironment

-- Command line nterface
data Flag = Version | Help
    deriving (Eq, Ord, Enum, Show, Bounded)

flags = [
    Option [] ["version"] (NoArg Version)
        "Print version information and exit.",
    Option [] ["help"] (NoArg Help)
        "Print this help message and exit."]

parseArgs argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        if null fs || Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else
                if Version `elem` args
                    then do hPutStrLn stderr version
                            exitWith ExitSuccess
                    else return fs
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header  = "Usage: brainfuck [OPTION] [FILE] ..."
          version = "brainfuck.hs v1.0"

runFile file = do
    contents <- readFile file
    case parseProgram contents of
        Left err -> print err
        Right val -> runProgram val

main = do files <- getArgs >>= parseArgs
          mapM_ runFile files

-- Error handling
data BrainfuckError = Parser ParseError
                    | Default String

showError (Parser err) = "Parse error at " ++ show err
showError (Default msg) = msg

instance Show BrainfuckError where show = showError

instance Error BrainfuckError where
    noMsg = Default "An unknown error occurred."
    strMsg = Default

type ThrowsError = Either BrainfuckError

