module Game
  ( play
  )
where

import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )
import           Data.Char                      ( toLower )
import           Dictionary                     ( randomWord )
import           Puzzle                         ( Puzzle(Puzzle)
                                                , freshPuzzle
                                                , handleGuess
                                                )

play :: Int -> IO ()
play _ = do
  word <- randomWord
  runGame False (freshPuzzle (fmap toLower word))

runGame :: Bool -> Puzzle -> IO ()
runGame False puzzle = do

  putStrLn $ "current puzzle is: " ++ show puzzle
  putStr "guess a letter\n"
  guess <- getLine
  case guess of
    [c] -> applyGuess c puzzle
    _   -> do
      putStrLn "your guess must be a single character"
      runGame False puzzle
runGame True puzzle = endGame puzzle (gameWin puzzle)

applyGuess :: Char -> Puzzle -> IO ()
applyGuess c puzzle = do
  newPuzzle <- handleGuess puzzle c
  let won  = gameWin newPuzzle
  let lost = gameLoss newPuzzle
  runGame (won || lost) newPuzzle

gameLoss :: Puzzle -> Bool
gameLoss (Puzzle _ _ _ numGuesses limit) = numGuesses > limit

gameWin :: Puzzle -> Bool
gameWin (Puzzle _ filledInSoFar _ _ _) = all isJust filledInSoFar

endGame :: Puzzle -> Bool -> IO ()
endGame (Puzzle _ filledInSoFar _ _ _) True =
  putStrLn ("you win! the word was: " ++ catMaybes filledInSoFar)
endGame (Puzzle wordToGuess _ _ _ _) False =
  putStrLn $ "you lose! the word was: " ++ wordToGuess
