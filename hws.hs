module Main where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Char

import System.Environment ( getArgs )
import System.Console.GetOpt


------------------------------------------------------------------------


progName = "hws"
version  = "v0.1.0"


main :: IO ()
main = do argv <- getArgs
          (o, n) <- parseArgs argv
          if optShowVersion o then
              putStrLn version
            else if optShowHelp o then
              putStrLn $ usageInfo header options
            else do program <- readFile (head n)
                    let insns = compile program
                    if optInstruction o then
                        showInst insns
                      else
                        run $ initVM insns


------------------------------------------------------------------------

-- Command-line options

data Options = Options { optShowVersion :: Bool
                       , optShowHelp    :: Bool
                       , optInstruction :: Bool
                       } deriving (Show, Eq)

defaultOptions = Options  { optShowVersion = False
                          , optShowHelp    = False
                          , optInstruction = False
                          }

options :: [OptDescr (Options -> Options)]
options = [ Option ['i']     ["instruction"]
            (NoArg (\ opts -> opts { optInstruction = True }))
            "show instructions"
          , Option ['v']     ["version"]
            (NoArg (\ opts -> opts { optShowVersion = True }))
            "show version"
          , Option ['h','?'] ["help"]
            (NoArg (\ opts -> opts { optShowHelp = True }))
            "show this message"
          ]

header :: String
header = "Usage: " ++ progName ++ " [OPTIONS...] PROG_FILE"

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv = case getOpt Permute options argv of
                   (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

showInst :: [Instruction] -> IO ()
showInst = mapM_ (putStrLn . show)

------------------------------------------------------------------------

--
-- Data difinition
--


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
             , heap :: [(Int, Int)]
             , inst :: ([Instruction], [Instruction])
             , labelList :: [(WSLabel, Int)]
             , callStack :: [Int]
             }

------------------------------------------------------------------------

--
-- Compiler
--


data Token = Space
           | Tab
           | LF
           deriving (Show, Eq)


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
parseInt (x:xs) = (f x) * (g xs)
  where
    f Space =  1
    f Tab   = (-1)
    g = g' . map (\x -> if x == Space then 0 else 1)
    g' = sum . zipWith (*) (iterate (*2) 1) . reverse

parseLabel :: [Token] -> WSLabel
parseLabel = concatMap (\t -> "[" ++ show t ++ "]")

------------------------------------------------------------------------

--
-- Virtual Machine
--


initVM :: [Instruction] -> VM
initVM insns = VM { stack = []
                  , heap = []
                  , inst = ([], insns)
                  , labelList = findLabels insns
                  , callStack = []
                  }

findLabels :: [Instruction] -> [(WSLabel, Int)]
findLabels i = f $ zip i [0..]
  where
    f []                    = []
    f (((Label x), n):xs) = (x, n) : f xs
    f (( _,        _):xs) = f xs

----

run :: VM -> IO ()
run vm = case fetch vm of
         Exit -> return ()
         i    -> do vmNext <- step i vm
                    run vmNext

fetch :: VM -> Instruction
fetch = f . snd . inst
  where
    f []     = Exit
    f (x:xs) = x

step :: Instruction -> VM -> IO VM
step (Push n)     vm = return $ vm { stack = n:stack vm, inst = shiftInst vm }
step Dup          vm = let (n, v) = pop vm in
                       return $ v { stack = n:n:stack v, inst = shiftInst v }
step (Copy n)     vm = let m = head $ drop n $ stack vm in
                       return $ vm { stack = m:stack vm, inst = shiftInst vm }
step Swap         vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = x2:x1:xs, inst = shiftInst vm }
step Discard      vm = return $ vm { stack = (tail.stack) vm, inst = shiftInst vm }
step (Slide n)    vm = let (x:xs) = stack vm in
                       return $ vm { stack = x:drop n xs, inst = shiftInst vm }
step Add          vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = (x2+x1):xs, inst = shiftInst vm }
step Sub          vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = (x2-x1):xs, inst = shiftInst vm }
step Mul          vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = (x2*x1):xs, inst = shiftInst vm }
step Div          vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = (x2 `div` x1):xs, inst = shiftInst vm }
step Mod          vm = let (x1:x2:xs) = stack vm in
                       return $ vm { stack = (x2 `mod` x1):xs, inst = shiftInst vm }
step HeapWrite    vm = let (val:addr:xs) = stack vm in
                       return $ vm { stack = xs, heap = (addr, val):heap vm, inst = shiftInst vm }
step HeapRead     vm = let (addr, v) = pop vm in
                       let x = lookup addr $ heap vm in
                       case x of
                       Just y -> return $ v { stack = y:stack v, inst = shiftInst v }
step (Label l)    vm = return $ vm { inst = shiftInst vm }
step (Call l)     vm = let addr = 1 + (length.fst.inst) vm in
                       return $ jump l $ vm { callStack = addr : callStack vm }
step (Jump l)     vm = return $ jump l vm
step (JumpZero l) vm = let (p, v) = pop vm in
                       if p == 0 then return $ jump l v
                                 else return $ v { inst = shiftInst v }
step (JumpNega l) vm = let (p, v) = pop vm in
                       if p < 0 then return $ jump l v
                                else return $ v { inst = shiftInst v }
step Return       vm = let (addr, v) = popAddr vm in
                       return $ jumpAddr addr v
step CharOut      vm = do let (n, v) = pop vm
                          putStr $ [chr n]
                          return $ v { inst = shiftInst v }
step NumOut       vm = do let (n, v) = pop vm
                          putStr $ show n
                          return $ v { inst = shiftInst v }
step CharIn       vm = do c <- getChar
                          return $ vm { stack = ord c:stack vm, inst = shiftInst vm }
step NumIn        vm = do s <- getLine
                          return $ vm { stack = read s:stack vm, inst = shiftInst vm }

----

pop :: VM -> (Int, VM)
pop vm = let (x:xs) = stack vm in
         (x, vm { stack = xs })

popAddr :: VM -> (Int, VM)
popAddr vm = let (x:xs) = callStack vm in
             (x, vm { callStack = xs })

shiftInst :: VM -> ([Instruction], [Instruction])
shiftInst vm = let (xs, (y:ys)) = inst vm in
               (y:xs, ys)

unshiftInst :: VM -> ([Instruction], [Instruction])
unshiftInst vm = let ((x:xs), ys) = inst vm in
                 (xs, x:ys)

jump :: WSLabel -> VM -> VM
jump l vm = let i = lookupLable l vm in
            let c = (length.fst.inst) vm in
            if i < c then let is = times unshift (c-i) $ inst vm in
                          vm { inst = is }
                     else let is = times shift (i-c) $ inst vm in
                          vm { inst = is }

jumpAddr :: Int -> VM -> VM
jumpAddr addr vm = let c = (length.fst.inst) vm in
                   if addr < c then let is = times unshift (c-addr) $ inst vm in
                                    vm { inst = is }
                               else let is = times shift (addr-c) $ inst vm in
                                    vm { inst = is }

lookupLable :: WSLabel -> VM -> Int
lookupLable l vm = case (lookup l $ labelList vm) of
                   Just x -> x

times :: (a -> a) -> Int -> a -> a
times f 1 x = f x
times f n x = f (times f (n-1) x)

shift :: ([a], [a]) -> ([a], [a])
shift (xs, [])     = (xs, [])
shift (xs, (y:ys)) = (y:xs, ys)

unshift :: ([a], [a]) -> ([a], [a])
unshift ([], ys)     = ([], ys)
unshift ((x:xs), ys) = (xs, x:ys)

------------------------------------------------------------------------

