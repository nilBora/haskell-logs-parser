import Data.List
import Data.String

main :: IO ()
main = do
    log <- readFile "log.txt"
    let logLines = lines log
    let logLinesWithIndex = zip [0..] logLines
    let logLinesWithIndexFiltered = filter (\(i, line) -> "WARNING" `isInfixOf` line) logLinesWithIndex
    let logLinesWithIndexFilteredMapped = map (\(i, line) -> (i, (words line) !! 1)) logLinesWithIndexFiltered
    let logLinesWithIndexFilteredMappedSorted = sortOn snd logLinesWithIndexFilteredMapped
    let logLinesWithIndexFilteredMappedSortedMapped = map (\(i, date) -> (logLines !! i)) logLinesWithIndexFilteredMappedSorted
    let logLinesWithIndexFilteredMappedSortedMappedFiltered = filter (\line -> "ERROR" `isInfixOf` line) logLinesWithIndexFilteredMappedSortedMapped
    let logLinesWithIndexFilteredMappedSortedMappedFilteredMapped = map (\line -> (words line) !! 1) logLinesWithIndexFilteredMappedSortedMappedFiltered
    let logLinesWithIndexFilteredMappedSortedMappedFilteredMappedFiltered = filter (\date -> "2018-06-17" `isInfixOf` date) logLinesWithIndexFilteredMappedSortedMappedFilteredMapped
    let logLinesWithIndexFilteredMappedSortedMappedFilteredMappedFilteredMapped = map (\date -> (words date) !! 1) logLinesWithIndexFilteredMappedSortedMappedFilteredMappedFiltered

    -- Println each element logLinesWithIndexFiltered
    mapM_ print logLinesWithIndexFiltered
    --print logLinesWithIndexFiltered
    print logLinesWithIndexFilteredMapped

    putStrLn(doSomething "Hello" "World")

doSomething :: String -> String -> String
doSomething a b = a ++ " " ++ b