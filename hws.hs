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
--          run $ initVM insns
          mapM_ (putStrLn.show) insns

------------------------------------------------------------------------


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
                 | ParseErr String
                 deriving (Show, Eq)


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
wsSpace = do { char 'S'
           ; return Space
           }

-- Tab
wsTab :: Parser Token
wsTab = do { char 'T'
         ; return Tab
         }

-- LF
wsLf :: Parser Token
wsLf = do { char 'L'
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

----

-- push
push :: Parser Instruction
push = do { wsSpace
          ; wsSpace
          ; v <- value
          ; return (Push (parseInt v))
          }

-- dup
dup :: Parser Instruction
dup = do { wsSpace
         ; wsLf
         ; wsSpace
         ; return Dup
         }

-- copy
copy :: Parser Instruction
copy = do { wsSpace
          ; wsTab
          ; wsSpace
          ; v <- value
          ; return (Copy (parseInt v))
          }

-- swap
swap :: Parser Instruction
swap = do { wsSpace
          ; wsLf
          ; wsTab
          ; return Swap
          }

-- discard
discard :: Parser Instruction
discard = do { wsSpace
          ; wsLf
          ; wsLf
          ; return Discard
          }

-- slide
slide :: Parser Instruction
slide = do { wsSpace
           ; wsTab
           ; wsLf
           ; v <- value
           ; return (Slide (parseInt v))
           }

----

-- add
add :: Parser Instruction
add = do { wsTab
         ; wsSpace
         ; wsSpace
         ; wsSpace
         ; return Add
         }

-- sub
sub :: Parser Instruction
sub = do { wsTab
         ; wsSpace
         ; wsSpace
         ; wsTab
         ; return Sub
         }

-- mul
mul :: Parser Instruction
mul = do { wsTab
         ; wsSpace
         ; wsSpace
         ; wsLf
         ; return Mul
         }

-- div
wsDiv :: Parser Instruction
wsDiv = do { wsTab
         ; wsSpace
         ; wsTab
         ; wsSpace
         ; return Div
         }

-- mod
wsMod :: Parser Instruction
wsMod = do { wsTab
         ; wsSpace
         ; wsTab
         ; wsLf
         ; return Mod
         }

----

-- heap_write
heapWrite :: Parser Instruction
heapWrite = do { wsTab
               ; wsTab
               ; wsSpace
               ; return HeapWrite
               }

-- heap_read
heapRead :: Parser Instruction
heapRead = do { wsTab
              ; wsTab
              ; wsTab
              ; return HeapRead
              }

----

-- label
wsLabel :: Parser Instruction
wsLabel = do { wsLf
           ; wsSpace
           ; wsSpace
           ; v <- value
           ; return (Label (parseLabel v))
           }

-- call
call :: Parser Instruction
call = do { wsLf
          ; wsSpace
          ; wsTab
          ; v <- value
          ; return (Call (parseLabel v))
          }

-- jump
jump :: Parser Instruction
jump = do { wsLf
          ; wsSpace
          ; wsLf
          ; v <- value
          ; return (Jump (parseLabel v))
          }

-- jump_zero
jumpZero :: Parser Instruction
jumpZero = do { wsLf
              ; wsTab
              ; wsSpace
              ; v <- value
              ; return (JumpZero (parseLabel v))
              }

-- jump_nega
jumpNega :: Parser Instruction
jumpNega = do { wsLf
              ; wsTab
              ; wsTab
              ; v <- value
              ; return (JumpNega (parseLabel v))
              }

-- return
wsReturn :: Parser Instruction
wsReturn = do { wsLf
              ; wsTab
              ; wsLf
              ; return Return
              }

-- exit
wsExit :: Parser Instruction
wsExit = do { wsLf
            ; wsLf
            ; wsLf
            ; return Exit
            }

-- char_out
charOut :: Parser Instruction
charOut = do { wsTab
             ; wsLf
             ; wsSpace
             ; wsSpace
             ; return CharOut
             }

-- num_out
numOut :: Parser Instruction
numOut = do { wsTab
            ; wsLf
            ; wsSpace
            ; wsTab
            ; return NumOut
            }

-- char_in
charIn :: Parser Instruction
charIn = do { wsTab
            ; wsLf
            ; wsTab
            ; wsSpace
            ; return CharIn
            }

-- num_in
numIn :: Parser Instruction
numIn = do { wsTab
           ; wsLf
           ; wsTab
           ; wsTab
           ; return NumIn
           }

----

-- instructions
instruction :: Parser Instruction
instruction = do { i <- push
                 ; return i
                 }
          <|> do { i <- dup
                 ; return i
                 }
          <|> do { i <- copy
                 ; return i
                 }
          <|> do { i <- swap
                 ; return i
                 }
          <|> do { i <- discard
                 ; return i
                 }
          <|> do { i <- slide
                 ; return i
                 }
          <|> do { i <- add
                 ; return i
                 }
          <|> do { i <- sub
                 ; return i
                 }
          <|> do { i <- mul
                 ; return i
                 }
          <|> do { i <- wsDiv
                 ; return i
                 }
          <|> do { i <- wsMod
                 ; return i
                 }
          <|> do { i <- heapWrite
                 ; return i
                 }
          <|> do { i <- heapRead
                 ; return i
                 }
          <|> do { i <- wsLabel
                 ; return i
                 }
          <|> do { i <- call
                 ; return i
                 }
          <|> do { i <- jump
                 ; return i
                 }
          <|> do { i <- jumpZero
                 ; return i
                 }
          <|> do { i <- jumpNega
                 ; return i
                 }
          <|> do { i <- wsReturn
                 ; return i
                 }
          <|> do { i <- wsExit
                 ; return i
                 }
          <|> do { i <- charOut
                 ; return i
                 }
          <|> do { i <- numOut
                 ; return i
                 }
          <|> do { i <- charIn
                 ; return i
                 }
          <|> do { i <- numIn
                 ; return i
                 }

-- instruction list
instructionList :: Parser [Instruction]
instructionList = do { is <- many1 instruction
                     ; return is
                     }

----

runCompile :: Parser [Instruction] -> String -> [Instruction]
runCompile p input = case (parse p [] input) of
                     Left err -> [ParseErr (show err)]
                     Right x  -> x

runLex :: Parser [Instruction] -> String -> [Instruction]
runLex p input = runCompile (do { x <- p
                                ; eof
                                ; return x
                                }) input

compile :: String -> [Instruction]
compile = runLex instructionList

----

parseInt :: [Token] -> Int
parseInt = undefined

parseLabel :: [Token] -> WSLabel
parseLabel = undefined

------------------------------------------------------------------------

