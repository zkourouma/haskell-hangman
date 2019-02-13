module Game
  ( runGame
  , freshPuzzle
  )
where

import           Data.Char                      ( toLower )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )
import           Data.List                      ( intersperse
                                                , sort
                                                )
import           System.Exit                    ( exitSuccess )
import           Dictionary                     ( randomWord )

guessLimit :: Int
guessLimit = 7

data Puzzle =
  Puzzle String
         [Maybe Char]
         String
         Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed guesses) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " Guesssed so far: " ++ guessed ++ "\nGuesses left: " ++ show guessesLeft
      where
        guessesLeft = guessLimit + 1 - guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s blanks [] 0 where blanks = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s n) c m =
  let zipper guessed wordChar guessedChar =
        if wordChar == guessed then Just wordChar else guessedChar
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar
  in  Puzzle word newFilledInSoFar (sort (c : s)) (n + m)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "you already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "this character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess 0)
    (False, _) -> do
      putStrLn "this character wasnt in the word, try again"
      return (fillInCharacter puzzle guess 1)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guesses) = when (guesses > guessLimit) $ do
  putStrLn "you lose"
  putStrLn $ "the word was: " ++ wordToGuess
  playAgain

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = when (all isJust filledInSoFar) $ do
  putStrLn ("you win! the word was: " ++ catMaybes filledInSoFar)
  playAgain

playAgain :: IO ()
playAgain = do
  putStrLn "would you like to play again? [y/N]"
  word <- randomWord
  let newPuzzle = freshPuzzle (fmap toLower word)
  playAgain <- getLine
  case map toLower playAgain of
    "y" -> runGame newPuzzle
    _   -> exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "current puzzle is: " ++ show puzzle
  putStr "guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "your guess must be a single character"
