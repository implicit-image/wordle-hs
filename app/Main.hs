module Main where

import Data.Vector as V
import Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Console.ANSI

import Words (wordList)
import UI (drawFrame, Frame(..))

data Status = NotFound | WrongPosition Int | CorrectPosition Int deriving (Eq, Show)

data WordleResult = WR {
        lastResult :: V.Vector (Status, Char),
        possibleWords :: V.Vector T.Text
        }

main :: IO ()
main = do
  -- args  <- getArgs
  -- args' <- parseArgs
  setSGR [
    SetConsoleIntensity NormalIntensity,
    SetItalicized False,
    SetUnderlining NoUnderline,
    SetBlinkSpeed NoBlink,
    SetVisible False,
    SetSwapForegroundBackground False,
    SetColor Foreground Dull Red,
    SetColor Background Dull Black
    ]
  clearScreen
  drawFrame $ Frame (0, 0) (30, 0) $ T.pack "Hewwo"
  a <- getLine
  finals <- solvingLoop wordLength wordList
  pprintV finals
  where
    wordLength = 5


prompt :: IO ()
prompt = do
  st <- getLine
  return ()


solvingLoop :: Int -> V.Vector T.Text -> IO (V.Vector T.Text)
solvingLoop n v
  | n <= 0    = return v
  | otherwise = do
      clearScreen
      setCursorPosition 0 0
      putStrLn "Enter last guess:"
      word  <- TIO.getLine  -- ex. crane
      putStrLn "Enter resulting tile pattern:"
      stats <- TIO.getLine  -- ex. 00102
      let
        status  = parseStatus stats word
        filters = V.foldl (.) id $ fmap getFilter status
        v'      = filters v
      pprintV $ sortWords 0 v'
      solvingLoop (n - 1) $ v'


pprintV :: Show a => V.Vector a -> IO ()
pprintV v = do
  print $ V.length v
  forM_ v $ \e -> do
    putStrLn $ show e


sortWords _ v = v

parseStatus :: T.Text -> T.Text -> V.Vector (Status, Char)
parseStatus stats word = V.zipWith (,) v1 v2
  where
    v1 = parseStatusNums stats
    v2 = parseStatusChars word

mkStatus :: (Int, Int) -> Status
mkStatus (0, _) = NotFound
mkStatus (1, i) = WrongPosition i
mkStatus (2, i) = CorrectPosition i
mkStatus _      = NotFound

parseStatusNums :: T.Text -> V.Vector Status
parseStatusNums = fmap mkStatus .
  (\v -> V.zip v (indecesVector v))
  . toIntVector . toCharVector
  where
    toCharVector :: T.Text -> V.Vector Char
    toCharVector = V.fromList . T.unpack
    toIntVector :: V.Vector Char -> V.Vector Int
    toIntVector = fmap (read . return)
    indecesVector ::V.Vector a -> V.Vector Int
    indecesVector v = V.generate (V.length v) id


parseStatusChars :: T.Text -> V.Vector Char
parseStatusChars = V.fromList . T.unpack

parseArgs :: [String] -> V.Vector (Status, Char)
parseArgs = undefined

getFilter :: (Status, Char) -> (V.Vector T.Text -> V.Vector T.Text)
getFilter (NotFound, ch) = id
getFilter (WrongPosition i, ch) = V.filter (\t -> ch /= T.index t i)
getFilter (CorrectPosition i, ch) = V.filter (\t -> ch == T.index t i)
