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

data Instruction = Push Int
                 | Dup
                 | Copy Int
                 | Swap
                 | Discard
                 | Slide Int
                 | Add
                 | Sub
                 | Mul
                 | Div
                 | Mod
                 | HeapWrite
                 | HeapRead
                 | Label WSLabel
                 | Call WSLabel
                 | Jump WSLabel
                 | JumpZero WSLabel
                 | JumpNega WSLabel
                 | Return
                 | Exit
                 | CharOut
                 | NumOut
                 | CharIn
                 | NumIn


type WSLabel = String


data VM = VM { stack :: [Int]
             , heap :: [Int]
             , inst :: [Instruction]
             }

