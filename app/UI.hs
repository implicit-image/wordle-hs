-- |

module UI where

import qualified Data.Text as T
import System.Console.ANSI
import Control.Monad (replicateM_)


data Frame = Frame {
  frameWH :: (Int, Int),
  frameXY :: (Int, Int),
  frameLabel   :: T.Text
  }


drawFrame :: Frame -> IO ()
drawFrame f@(Frame (w, h) (x, y) l) = do
  setCursorPosition x y
  replicateM_ w $ putStr "="
