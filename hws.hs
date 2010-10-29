module Main where


main :: IO ()
main = do program <- getContents
          let insns = compile program
          run $ initVM insns

------------------------------------------------------------------------

compile :: String -> [Instruction]
compile = undefined


initVM :: [Instruction] -> VM
initVM = undefined


run :: VM -> IO ()
run = undefined


------------------------------------------------------------------------

type Instruction = String   -- temporary


data VM = VM { stack :: [Int]
             , heap :: [Int]
             , inst :: [Instruction]
             }

