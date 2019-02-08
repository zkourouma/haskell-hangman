module Main where

import           Data.Char                      ( toLower )
import           Dictionary                     ( randomWord )
import           Game                           ( freshPuzzle
                                                , runGame
                                                )

main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
