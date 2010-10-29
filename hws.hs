module Main where


main :: IO ()
main = do program <- getContents
          insns <- compile program
          run $ initVM insns
          


compile = undefined



initVM = undefined



run = undefined


