module Dictionary
  ( randomWord
  )
where

import           System.Random                  ( randomRIO )


newtype WordList =
  WordList [String]

allWords :: IO WordList
allWords = WordList . lines <$> readFile "data/dict.txt"

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
 where
  gameLength :: String -> Bool
  gameLength w = let l = length w in l > minWordLength && l < maxWordLength

randomWord' :: WordList -> IO String
randomWord' (WordList wl) = do
  randomIdx <- randomRIO (0, length wl - 1)
  return $ wl !! randomIdx

randomWord :: IO String
randomWord = gameWords >>= randomWord'
