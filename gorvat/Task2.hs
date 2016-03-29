import Data.Char
import Data.List
import Data.Map (toList, fromListWith)

letters = '\'':(['A'..'Z']++['a'..'z'])
sentenceEnds = ['.', '!', '?']

getWords :: [Char] -> [[Char]]
getWords [] = []
getWords string = 
    let (word, rest) = span (`elem` letters) string
    in word:(getWords (dropWhile (`notElem` letters) rest))

filterCapsedOnlyWords :: [[Char]] -> [[Char]]
filterCapsedOnlyWords list = 
    filter (\word -> toUpper (head word) == (head word) && (map toUpper word) /= word
                     && find (==(map toLower word)) list == Nothing) list

getNamesFromText :: [Char] -> [[Char]]
getNamesFromText = filterCapsedOnlyWords . nub . filter (not . null) . getWords

getAllNamesInSentence :: [Char] -> [[Char]] -> [[Char]]
getAllNamesInSentence sentence names = intersect (getWords sentence) names

getSentences :: [Char] -> [[Char]]
getSentences [] = []
getSentences text = 
    let (sentence, rest) = span (`notElem` sentenceEnds) text
    in sentence:(getSentences (dropWhile (`notElem` letters) rest))

getAllNamePairs :: [Char] -> [([Char], [Char])]
getAllNamePairs text =
    let names = getNamesFromText text
        sentences = getSentences text
    in concat $ map (\sentence -> 
                        let lst = getAllNamesInSentence sentence names
                        in [(word1, word2) | word1 <- lst, 
                                             word2 <- lst, 
                                             word1 < word2]) sentences

getAllNameFrequencies :: [Char] -> [(([Char], [Char]), Int)]
getAllNameFrequencies text =
    let allPairs = getAllNamePairs text
    in toList $ fromListWith (+) [(pair, 1) | pair <- allPairs]

printElement ((first, second), number) = do
    putStrLn $ first ++ " " ++ second ++ " " ++ (show number)

printResult [] = return()
printResult (x:xs) = do
    printElement x
    printResult xs

main = do
    contents <- getContents
    printResult $ sortBy (\a b -> compare (snd b) (snd a)) 
                $ getAllNameFrequencies contents