module Puzzle
  ( Puzzle(Puzzle)
  , freshPuzzle
  , handleGuess
  )
where

import           Data.List                      ( intersperse
                                                , sort
                                                )


data Puzzle =
  Puzzle String
         [Maybe Char]
         String
         Int
         Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed numGuesses limit) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " Guesssed so far: " ++ guessed ++ "\nGuesses left: " ++ show guessesLeft
      where
        guessesLeft = limit + 1 - numGuesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s blanks [] 0 7 where blanks = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _ _) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s n n') c m =
  let zipper guessed wordChar guessedChar =
        if wordChar == guessed then Just wordChar else guessedChar
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar
  in  Puzzle word newFilledInSoFar (sort (c : s)) (n + m) n'

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "you already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "this character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess 0)
    (False, _) -> do
      putStrLn "this character wasn't in the word, try again"
      return (fillInCharacter puzzle guess 1)



