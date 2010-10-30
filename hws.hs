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


--initVM :: [Instruction] -> VM
--initVM = undefined
--
--
--run :: VM -> IO ()
--run = undefined


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

-- instructions
instruction :: Parser Instruction
instruction = do { wsSpace
                 ; i <- do { wsSpace
                           ; v <- value
                           ; return (Push (parseInt v))             -- SSn  push
                           }
                    <|> do { wsTab
                           ; j <- do { wsSpace
                                     ; v <- value
                                     ; return (Copy (parseInt v))   -- STSn copy
                                     }
                              <|> do { wsLf
                                     ; v <- value
                                     ; return (Slide (parseInt v))  -- STL  slide
                                     }
                           ; return j
                           }
                    <|> do { wsLf
                           ; j <- do { wsSpace
                                     ; return Dup                   -- SLS  dup
                                     }
                              <|> do { wsTab
                                     ; return Swap                  -- SLT  swap
                                     }
                              <|> do { wsLf
                                     ; return Discard               -- SLL  discard
                                     }
                           ; return j
                           }
                 ; return i
                 }
          <|> do { wsTab
                 ; i <- do { wsSpace
                           ; j <- do { wsSpace
                                     ; k <- do { wsSpace
                                               ; return Add         -- TSSS add
                                               }
                                        <|> do { wsTab
                                               ; return Sub         -- TSST sub
                                               }
                                        <|> do { wsLf
                                               ; return Mul         -- TSSL sub
                                               }
                                     ; return k
                                     }
                              <|> do { wsTab
                                     ; k <- do { wsSpace
                                               ; return Div         -- TSTS div
                                               }
                                        <|> do { wsLf
                                               ; return Mod         -- TSTL mod
                                               }
                                     ; return k
                                     }
                           ; return j
                           }
                    <|> do { wsTab
                           ; j <- do { wsSpace
                                     ; return HeapWrite             -- TTS  heap_write
                                     }
                              <|> do { wsTab
                                     ; return HeapRead              -- TTT  heap_read
                                     }
                           ; return j
                           }
                    <|> do { wsLf
                           ; j <- do { wsSpace
                                     ; k <- do { wsSpace
                                               ; return CharOut     -- TLSS char_out
                                               }
                                        <|> do { wsTab
                                               ; return NumOut      -- TLST num_out
                                               }
                                     ; return k
                                     }
                              <|> do { wsTab
                                     ; k <- do { wsSpace
                                               ; return CharIn      -- TLTS char_in
                                               }
                                        <|> do { wsTab
                                               ; return NumIn       -- TLTT num_in
                                               }
                                     ; return k
                                     }
                           ; return j
                           }
                 ; return i
                 }
          <|> do { wsLf
                 ; i <- do { wsSpace
                           ; j <- do { wsSpace
                                     ; v <- value
                                     ; return (Label (parseLabel v))  -- LSSl label
                                     }
                              <|> do { wsTab
                                     ; v <- value
                                     ; return (Call (parseLabel v))   -- LSTl call
                                     }
                              <|> do { wsLf
                                     ; v <- value
                                     ; return (Jump (parseLabel v))   -- LSLl jump
                                     }
                           ; return j
                           }
                    <|> do { wsTab
                           ; j <- do { wsSpace
                                     ; v <- value
                                     ; return (JumpZero (parseLabel v))  -- LTSl jump_zero
                                     }
                              <|> do { wsTab
                                     ; v <- value
                                     ; return (JumpNega (parseLabel v))  -- LTTl jump_nega
                                     }
                              <|> do { wsLf
                                     ; return Return                     -- LTL  return
                                     }
                           ; return j
                           }
                    <|> do { wsLf
                           ; wsLf
                           ; return Exit                                 -- LLL  exit
                           }
                 ; return i
                 }
          <?> "Err: instruction"


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
parseInt t = 1

parseLabel :: [Token] -> WSLabel
parseLabel t = "label1"

------------------------------------------------------------------------

