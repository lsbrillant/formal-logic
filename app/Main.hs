module Main where

import qualified Logic.Sentential.Parse as SP
import           Logic.Sentential


main :: IO ()
main = putStrLn $ case SP.parseWff $ "((A0 /\\ A1) <-> (A2 /\\ A3))" of
        Right wff -> showLatex wff
        Left err  -> show err


