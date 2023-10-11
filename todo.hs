import Data.List
import System.Directory
--import Data.String

main :: IO ()
main = do
    -- scan folder with files
    directory <- getDirectoryContents "./scan"
    let files = filter (\file -> ".log" `isSuffixOf` file) directory
    let filesWithIndex = zip [1..] files
    let filesWithIndexMapped = map (\(i, file) -> (show i) ++ " - " ++ file) filesWithIndex
    mapM_ print filesWithIndexMapped

    mapM_ checkFile files
    





checkFile :: String -> IO ()
checkFile file = do
    putStrLn $ "Checking file: " ++ file
    content <- readFile $ "./scan/" ++ file
    let linesOfFile = lines content
    let fileLinesWithIndex = zip [1..] linesOfFile
    let fileLinesWithIndexFiltered = filter (\(i, line) -> "TODO" `isInfixOf` line) fileLinesWithIndex 
    let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, (words line) !! 1)) fileLinesWithIndexFiltered
    let fileLinesWithIndexFilteredMappedSorted = sortOn snd fileLinesWithIndexFilteredMapped
    let fileLinesWithIndexFilteredMappedSortedMapped = map (\(i, line) -> (i, "TODO: " ++ line)) fileLinesWithIndexFilteredMappedSorted
    let fileLinesWithIndexFilteredMappedSortedMappedWithIndex = map (\(i, line) -> (show i) ++ " - " ++ line) fileLinesWithIndexFilteredMappedSortedMapped   


    -- let linesOfFileWithIndexMapped = map (\(i, line) -> (show i) ++ " - " ++ line) linesOfFileWithIndex
    -- mapM_ print linesOfFileWithIndexMapped
    -- putStrLn "-------------------"
    -- putStrLn "-------------------"
    -- putStrLn "---------------"
    -- checkFile fileInput = do
    -- let fileInput = "./scan/" ++ fileInput
    -- file <- readFile fileInput
    -- let fileLines = lines file
    -- let fileLinesWithIndex = zip [1..] fileLines
    -- let fileLinesWithIndexFiltered = filter (\(i, line) -> "TODO" `isInfixOf` line) fileLinesWithIndex
    -- let fileLinesWithIndexFilteredMapped = map (\(i, line) -> (i, (words line) !! 1)) fileLinesWithIndexFiltered
    -- let fileLinesWithIndexFilteredMappedSorted = sortOn snd fileLinesWithIndexFilteredMapped
    -- let fileLinesWithIndexFilteredMappedSortedMapped = map (\(i, line) -> (i, "TODO: " ++ line)) fileLinesWithIndexFilteredMappedSorted
    -- let fileLinesWithIndexFilteredMappedSortedMappedWithIndex = map (\(i, line) -> (show i) ++ " - " ++ line) fileLinesWithIndexFilteredMappedSortedMapped
   
    mapM_ print fileLinesWithIndexFiltered
    mapM_ print fileLinesWithIndexFilteredMapped
    mapM_ print fileLinesWithIndexFilteredMappedSorted
    mapM_ print fileLinesWithIndexFilteredMappedSortedMapped
    mapM_ print fileLinesWithIndexFilteredMappedSortedMappedWithIndex