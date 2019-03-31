module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = getArgs >>= parseArgs >>= runStory

-- For now just assume arg is just the one file name we care about.
parseArgs [fname] = readFile fname
