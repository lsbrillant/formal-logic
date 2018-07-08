module Logic.Sentential.Parse where

import Text.ParserCombinators.Parsec

import Logic.Sentential

parseWff :: String -> Either ParseError Wff
parseWff s = parse wff "" s 

wff = (sentence_symbol <|> expr)
    where
        sentence_symbol = 
            do (char 'A') <|> (char 'a')
               n <- (many digit)
               return $ A $ read n
        expr = 
            do (char '(')
               spaces
               e <- (e_neg <|> e_box)
               spaces
               (char ')')
               return e
        e_neg = 
            do (char '~')
               e <- wff
               return $ E_not e
        e_box = 
            do e1 <- wff
               spaces
               (do { string "\\/"
                   ; spaces
                   ; e2 <- wff
                   ; return $ E_or e1 e2 }
                <|>
                do { string "/\\"
                   ; spaces
                   ; e2 <- wff
                   ; return $ E_and e1 e2 }
                <|> 
                do { string "->"
                   ; spaces
                   ; e2 <- wff
                   ; return $ E_impl e1 e2 }
                <|> 
                do { string "<->"
                   ; spaces
                   ; e2 <- wff
                   ; return $ E_eq e1 e2 })
