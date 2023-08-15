-- app/Main.hs
{-# LANGUAGE LambdaCase #-}

-- | Entry point for the hs-blog-gen program
module Main where

import Control.Exception
import qualified HsBlog
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

data MyException
  = ErrZero
  | ErrOdd Int
  deriving (Show)

instance Exception MyException

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output
    ConvertSingle input output ->
      let -- Here, action is the next steps we want to do.
          -- It takes as input the values we produce,
          -- uses it, and then returns control for us to clean-up
          -- afterwards.
          withInputHandle :: (String -> Handle -> IO a) -> IO a
          withInputHandle action =
            case input of
              Stdin ->
                action "" stdin
              InputFile file ->
                withFile file ReadMode (action file)

          -- Note that in both functions our action can return any `a`
          -- it wants.
          withOutputHandle :: (Handle -> IO a) -> IO a
          withOutputHandle action =
            case output of
              Stdout ->
                action stdout
              OutputFile file -> do
                exists <- doesFileExist file
                shouldOpenFile <-
                  if exists
                    then confirm
                    else pure True
                if shouldOpenFile
                  then withFile file WriteMode action
                  else exitFailure
       in withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

sayDiv2 :: Int -> IO ()
sayDiv2 n
  | n == 0 = throwIO ErrZero
  | odd n = throwIO (ErrOdd n)
  | otherwise = print (n `div` 2)

------------------------------------------------

-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirm