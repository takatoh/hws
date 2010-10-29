module Main where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
-- import qualified Text.ParserCombinators.Parsec.Token as P
-- import Text.ParserCombinators.Parsec.Language( haskellStyle, haskellDef )
import Char


------------------------------------------------------------------------

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

------------------------------------------------------------------------

--
-- Compiler
--


data Token = Space
           | Tab
           | LF


-- Space
wsSpace :: Parser Token
wsSpace = do { char ' '
           ; return Space
           }

-- Tab
wsTab :: Parser Token
wsTab = do { char '\t'
         ; return Tab
         }

-- LF
wsLf :: Parser Token
wsLf = do { char '\n'
        ; return LF
        }

-- token
vToken :: Parser Token
vToken = do { wsSpace
            ; return Space
            }
     <|> do { wsTab
            ; return Tab
            }

-- value
value :: Parser [Token]
value = do { v <- many1 vToken
           ; wsLf
           ; return v
           }



