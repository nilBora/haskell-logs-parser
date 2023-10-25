module Main where

import Data.List
import Data.String
import Control.Monad (forM_)

main :: IO ()
main = do
    log <- readFile "log.txt"
    let logLines = lines log
    let logLinesWithIndex = zip [0..] logLines
    let logLinesWithIndexFiltered = filter (\(_, x) -> any (`isInfixOf` x) getDefaultTypes) logLinesWithIndex
    
    forM_ logLinesWithIndexFiltered $ \(i, line) -> do
        putStrLn $ show i ++ " - " ++ line


getDefaultTypes :: [String]
getDefaultTypes = ["WARNING", "ERROR", "NOTICE"]

colorRed :: String -> String
colorRed = colorize "31"

colorize :: String -> String -> String
colorize color text = "\x1b[" ++ color ++ "m" ++ text ++ "\x1b[0m"
