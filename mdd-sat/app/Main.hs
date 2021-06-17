module Main where

import System.Exit (exitFailure)
import System.Environment
import System.IO
import System.IO.Error (tryIOError)

import Lib

main :: IO ()
main = do
  -- args <- getArgs
  template <- loadFileMay "samples/grid/grid_8x8_a2.cpf" -- "samples/map.cpf" --
  case template of
    Nothing -> print "err" >> exitFailure
    Just v -> print (mdd_sat v)

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents