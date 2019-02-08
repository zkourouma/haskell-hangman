module Game
  ( runGame
  , freshPuzzle
  )
where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Maybe                     ( isJust )
import           System.Exit                    ( exitSuccess )

import           Data.List                      ( intersperse )

data Puzzle =
  Puzzle String
         [Maybe Char]
         String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " Guesssed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s blanks [] where blanks = map (const Nothing) s
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word
                                                         newFilledInSoFar
                                                         (c : s)
 where
  zipper guessed wordChar guessedChar =
    if wordChar == guessed then Just wordChar else guessedChar
  newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "you already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "this character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "this character wasnt in the word, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = when (length guessed > 7) $ do
  putStrLn "you lose"
  putStrLn $ "the word was: " ++ wordToGuess
  exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = when (all isJust filledInSoFar) $ do
  putStrLn "you win"
  exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "current puzzle is: " ++ show puzzle
  putStr "guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "your guess must be a single character"
