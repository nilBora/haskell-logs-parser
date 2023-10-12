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
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, (words line) !! 1)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMappedSorted = sortOn snd fileLinesWithIndexFilteredMapped
    let fileLinesWithIndexFilteredMappedSortedMapped = map (\(i, line) -> (i, "TODO: " ++ line)) fileLinesWithIndexFilteredMappedSorted
    let fileLinesWithIndexFilteredMappedSortedMappedWithIndex = map (\(i, line) -> (show i) ++ " - " ++ line) fileLinesWithIndexFilteredMappedSortedMapped   
   
    mapM_ print fileLinesWithIndexFiltered
    mapM_ print fileLinesWithIndexFilteredMapped
    mapM_ print fileLinesWithIndexFilteredMappedSorted
    mapM_ print fileLinesWithIndexFilteredMappedSortedMapped
    mapM_ print fileLinesWithIndexFilteredMappedSortedMappedWithIndex