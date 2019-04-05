module Lib
    ( runStory, story, tellStory
    ) where

import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)

data StoryOption = StoryOption {
  optionLabel :: String,
  optionText :: String
} deriving (Show)

data StoryBlock = StoryBlock {
  blockLabel :: String,
  blockText :: String,
  blockOptions :: [StoryOption]
} deriving (Show)

newtype Story = Story (Map.Map String StoryBlock)
  deriving (Show)

storyLabel :: GenParser Char st String
storyLabel = do
  lbl <- many lower
  string ":\n"
  return lbl

storyOption :: GenParser Char st StoryOption
storyOption = do
  char '*'
  lbl <- many lower
  char '*'
  many space
  text <- many (noneOf "\n")
  char '\n'
  return $ StoryOption lbl text

storyText :: GenParser Char st String
storyText = unlines <$> many storyLine

storyLine :: GenParser Char st String
storyLine = do
  first <- noneOf "*"
  rest <- many (noneOf "\n")
  char '\n'
  return $ first : rest

storyBlock :: GenParser Char st StoryBlock
storyBlock = do
  lbl <- storyLabel
  text <- storyText
  options <- many storyOption
  return $ StoryBlock lbl text options

story :: GenParser Char st Story
story = do
  blocks <- many (do b <- storyBlock
                     many space
                     return b)
  return $ Story $ Map.fromList (map indexBlock blocks)
  where
    indexBlock b = (blockLabel b, b)

tellStoryOptions :: [StoryOption] -> IO ()
tellStoryOptions [option] = putStrLn (optionText option)
tellStoryOptions options = mapM_ putStrLn optionStrings
  where
    optionStrings = zipWith prefixNumber [1..] (map optionText options)
    prefixNumber n str = show n ++ ". " ++ str

tellStoryBlock :: StoryBlock -> IO ()
tellStoryBlock blk = do
  putStr $ blockText blk
  getLine
  tellStoryOptions (blockOptions blk)
  
tellStory :: String -> Story -> IO ()
tellStory "end" _ = putStrLn "The End."
tellStory lbl stry = do
  let (Story s) = stry
  case (s Map.!? lbl) of
    Nothing -> putStrLn "The story doesn't say what comes next..."
    Just blk -> do tellStoryBlock blk
                   let opts = blockOptions blk
                   optN <- readStoryOption $ length opts
                   putStrLn ""
                   tellStory (optionLabel $ opts !! optN) stry

readStoryOption :: Int -> IO (Int)
-- No use asking if there's only one choice. Still read input to pause.
readStoryOption 1 = getLine >> return 0
readStoryOption maxChoice = do
  putStr $ "Enter an option (1-" ++ (show maxChoice) ++ "): "
  hFlush stdout
  line <- getLine
  case (readMaybe line :: Maybe Int) of
    Nothing -> tryAgain
    (Just n) ->  if n < 1 || n > maxChoice
                 then tryAgain
                 else return $ n - 1
  where
    tryAgain = putStrLn "That's not a choice." >> readStoryOption maxChoice

runStory :: String -> IO ()
runStory s = do
  case parse story "(unknown)" s of
    (Left err) -> print err
    (Right stry) -> tellStory "start" stry
