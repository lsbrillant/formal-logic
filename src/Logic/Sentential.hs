module Logic.Sentential where

import Text.ParserCombinators.Parsec

data Wff = A Int 
         | E_not  Wff
         | E_and  Wff Wff
         | E_or   Wff Wff
         | E_impl Wff Wff
         | E_eq   Wff Wff
         deriving(Eq)

instance Show Wff where
    show (A n)          = "A_" ++ (show n)
    show (E_not expr)   = "(~" ++ (show expr) ++ ")"
    show (E_and e1 e2)  = "(" ++ (show e1) ++ " /\\ " ++ (show e2) ++ ")"
    show (E_or e1 e2)   = "(" ++ (show e1) ++ " \\/ " ++ (show e2) ++ ")"
    show (E_impl e1 e2) = "(" ++ (show e1) ++ " -> " ++ (show e2) ++ ")"
    show (E_eq e1 e2)   = "(" ++ (show e1) ++ " <-> " ++ (show e2) ++ ")"


evaluate :: (Int -> Bool) -> Wff -> Bool
evaluate v (A n)          = v n
evaluate v (E_not expr)   = not $ evaluate v expr
evaluate v (E_and e1 e2)  = (evaluate v e1) && (evaluate v e2)
evaluate v (E_or e1 e2)   = (evaluate v e1) || (evaluate v e2)
evaluate v (E_impl e1 e2) = let b_e1 = evaluate v e1 in (not b_e1) || (b_e1 && (evaluate v e2))
evaluate v (E_eq e1 e2)   = (evaluate v e1) == (evaluate v e2)

-- Shows a Wff for use in a LaTeX formula
showLatex :: Wff -> String
showLatex (A n)         = "A_{"++ (show n) ++"}"
showLatex (E_not expr)  = "(\\neg " ++ (showLatex expr) ++ ")"
showLatex (wff) = e_box wff
    where e_box (E_and e1 e2)  = show_e_box e1 "\\land" e2
          e_box (E_or e1 e2)   = show_e_box e1 "\\lor" e2
          e_box (E_impl e1 e2) = show_e_box e1 "\\rightarrow" e2
          e_box (E_eq e1 e2)   = show_e_box e1 "\\leftrightarrow" e2
          show_e_box e1 box e2 = "(" ++ (showLatex e1) ++ 
                                 " " ++ box ++ " " ++ 
                                 (showLatex e2) ++ ")"
{-
entanceSymbols :: Wff -> [Int]
sentanceSymbols e = [] 
    where
        ss_rec wff xs 


--truthTable :: Wff -> String
-}
