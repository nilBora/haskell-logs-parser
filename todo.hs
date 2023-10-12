import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import System.IO

main :: IO ()
main = do
    files <- getRecursiveContents "./scan"
    mapM_ checkFile files

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
    putStrLn $ "Checking file: " ++ file
    content <- readFile $ file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(i, line) -> "TODO" `isInfixOf` line) fileLinesWithIndex 
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, dropWhile (/= 'T') line)) fileLinesWithIndexFiltered
   
    mapM_ print fileLinesWithIndexFilteredMapped