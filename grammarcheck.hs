import Data.List

correctGrammar :: String -> String
correctGrammar sentence = intercalate " " $ map correctWord (words sentence)

correctWord :: String -> String
correctWord word
    | last word == 's' && not (isPluralNoun word) = init word -- remove 's' from verb
    | last word == 'y' && not (isVowel (last (init word))) = init (init word) ++ "ies" -- change 'y' to 'ies' in plural form of noun
    | otherwise = word -- no correction needed

isVowel :: Char -> Bool
isVowel c = elem (toLower c) "aeiou"

isPluralNoun :: String -> Bool
isPluralNoun word
    | last word == 's' = True
    | last word == 'e' && last (init word) == 's' = True -- ex: churches
    | otherwise = False
