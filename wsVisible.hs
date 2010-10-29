module Main where


main :: IO ()
main = do source <- getContents
          putStr $ map visible source


visible :: Char -> Char
visible ' '  = 'S'
visible '\t' = 'T'
visible '\n' = 'L'

