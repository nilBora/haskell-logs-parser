import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import System.IO
import System.Environment
import System.Exit
import Data.Char


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
            --mapM_ checkFile files
            let totalTODOs = sum <$> mapM checkFile files
            total <- totalTODOs
            putStrLn $ "Total TODOs: " ++ show total
            exitSuccess

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path

        if isDirectory
            then getRecursiveContents path
            else if isSuffixOf ".php" path || isSuffixOf ".xml" path || isSuffixOf ".yaml" path || isSuffixOf ".yml" path
                then return [path]
                else return []
    return (concat paths)
    

checkFile :: String -> IO Int
checkFile file = do
    content <- readFile $ file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(i, line) -> "TODO" `isInfixOf` line) fileLinesWithIndex 
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, removeBeforeTodo line)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMappedTrim = map (\(i, line) -> (i, dropWhile (/= 'T') line)) fileLinesWithIndexFilteredMapped
    
    if length fileLinesWithIndexFilteredMappedTrim == 0
        then return 0
        else do
            putStrLn "------ --------------------------------------------------"    
            let msg = "Line: " ++ file
            putStrLn $ colorGreen msg
            mapM_ putStrLn $ map (\(i, line) -> show i ++ "     " ++ line) fileLinesWithIndexFilteredMappedTrim
            putStrLn "------ --------------------------------------------------"
            let count = length fileLinesWithIndexFilteredMappedTrim
            return count
            

colorGreen :: String -> String
colorGreen input = "\x1b[32m" ++ input ++ "\x1b[0m"

removeBeforeTodo :: String -> String
removeBeforeTodo input = case dropWhile (not . isPrefixOf "TODO") (tails input) of
    (rest:_) -> rest
    _ -> input
    