import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import System.IO
import System.Environment
import System.Exit


main :: IO ()
main = do
    args <- getArgs

    if length args /= 1
        then do
            putStrLn "Usage: todo <directory>"
            exitFailure
        else do
            let dir = head args
            files <- getRecursiveContents dir
            mapM_ checkFile files
            --setCurrentDirectory dir

    --files <- getRecursiveContents getCurrentDidirrectory
    

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
    

checkFile :: String -> IO ()
checkFile file = do
    content <- readFile $ file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(i, line) -> "TODO" `isInfixOf` line) fileLinesWithIndex 
    --let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, dropWhile (/= 'T') line)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, removeBeforeTodo line)) fileLinesWithIndexFiltered
    if length fileLinesWithIndexFilteredMapped == 0
        then return ()
        else do
            putStrLn $ "Checking file: " ++ file
            mapM_ print fileLinesWithIndexFilteredMapped


removeBeforeTodo :: String -> String
removeBeforeTodo input = case dropWhile (not . isPrefixOf "//TODO") (tails input) of
    (rest:_) -> rest
    _ -> input