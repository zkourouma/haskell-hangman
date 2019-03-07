module Main
  ( main
  )
where

import           Data.Char                      ( toLower )
import           System.Exit                    ( exitSuccess )
import           Game                           ( play )

main :: IO ()
main = do
  play 0
  playAgain


playAgain :: IO ()
playAgain = do
  putStrLn "would you like to play again? [y/N]"
  replay <- getLine
  case map toLower replay of
    "y" -> play 0
    _   -> exitSuccess
