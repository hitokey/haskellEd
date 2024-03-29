module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.ANSI
import System.Environment
import System.IO
import System.Directory
import Doc
import Data.List

type Repl a= InputT IO a

ckModule [] = putStr "-- missing module id"
ckModule [x] = putStr (x++" -- missing where keyword")
ckModule (x:"where":xs) = putStr x
ckModule xs = putStrLn (unwords xs)

ckWhere [] = return ()
ckWhere [x]= return ()
ckWhere (x:"where":xs) = do
   putStr " where "
   setSGR [Reset]
   putChars (unwords xs)
ckWhere xs= putStrLn (unwords xs)

getCondition [] = ""
getCondition (' ':'=':' ':xs) = ""
getCondition (x:xs) = (x: getCondition xs)

dropCondition []= ""
dropCondition (' ':'=':' ':xs)= (' ':'=':' ':xs)
dropCondition (x:xs) = dropCondition xs

putChars []= putStrLn ""
putChars ('w':'h':'e':'r':'e':xs)= do
     setSGR [SetColor Foreground Vivid Green]
     putStr "where"
     setSGR [Reset]
     putChars xs
putChars ('-':'-':xs)= do
   setSGR [SetColor Foreground Vivid Blue]
   putStrLn  ('-':'-':xs)
   setSGR [Reset]
putChars (' ':'d':'o':xs) = do
    setSGR [SetColor Foreground Vivid Green]
    putStr " do"
    setSGR [Reset]
    putChars xs
putChars (' ':'|':' ':xs) = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStr (getCondition (' ':'|':' ':xs))
    setSGR [Reset]
    putChars (dropCondition (xs))
putChars (x:xs)= do
   putChar x
   putChars xs

cntLines bff= numLines (getBefore c n bf ('>':cur) post) 14
  where
     (_, (i, c, e), n, bf, cur, post)= theBuffer bff

withoutCursorLines bff= nonumLines n (nonumBefore c n (cur:bf) ">"  post) 14
 where
   (kr, (i,c,e), n, bf, cur, post) = theBuffer bff

numLines (n, []) _ = putStrLn ""
numLines (n, xs) 0= putStrLn ""
numLines (n, (('>':'i':'m':'p':'o':'r':'t':x):xs)) m= do
  putStr (mkNumCursor "> " n)
  setSGR [SetColor Foreground Vivid Red]
  putStr "import"
  setSGR [Reset]
  putStrLn x
  numLines (n+1, xs) (m-1)
numLines (n, (('>':'-':'-':x):xs)) m= do
  putStr (mkNumCursor "> " n)
  setSGR [SetColor Foreground Vivid Blue]
  putStr "--"
  putStrLn x
  setSGR [Reset]
  numLines (n+1, xs) (m-1)
numLines (n, (('>':'m':'o':'d':'u':'l':'e':' ':x):xs)) m= do
  putStr (mkNumCursor "> " n)
  let ws = words x
  setSGR [SetColor Foreground Vivid Red]
  putStr "module "
  setSGR [Reset]
  ckModule ws
  setSGR [SetColor Foreground Vivid Green]
  ckWhere ws
  setSGR [Reset]
  numLines (n+1, xs) (m-1)
numLines (n, (('>':x):xs)) m= do 
 putStr (mkNumCursor "> " n)
 putChars x
 numLines (n+1, xs) (m-1)
numLines (n, (('i':'m':'p':'o':'r':'t':x):xs)) m= do
  putStr (mkNumCursor "  " n)
  setSGR [SetColor Foreground Vivid Red]
  putStr "import"
  setSGR [Reset]
  putStrLn x
  numLines (n+1, xs) (m-1)
numLines (n, (('m':'o':'d':'u':'l':'e':' ':x):xs)) m= do
  putStr (mkNumCursor "  " n)
  let ws = words x
  setSGR [SetColor Foreground Vivid Red]
  putStr "module "
  setSGR [Reset]
  ckModule ws
  setSGR [SetColor Foreground Vivid Green]
  ckWhere ws
  setSGR [Reset]
  numLines (n+1, xs) (m-1)
numLines (n, (('-':'-':x):xs)) m= do
  putStr (mkNumCursor "  " n)
  setSGR [SetColor Foreground Vivid Blue]
  putStr "--"
  putStrLn x
  setSGR [Reset]
  numLines (n+1, xs) (m-1)
numLines (n, x:xs) m | isInfixOf "::" x = do
  putStr (mkNumCursor "  " n)
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn x
  setSGR [Reset]
  numLines (n+1, xs) (m-1)
numLines (n, x:xs) m = do
  putStr (mkNumCursor "  " n)
  putChars x
  numLines (n+1, xs) (m-1)

nonumLines i (n, []) _ = putStrLn ""
nonumLines i (n, xs) 0= putStrLn "" 
nonumLines i (n, (('>':x):xs)) m= do
 putStr (indent 6)
 putChars x
 nonumLines i (n+1, xs) (m-1)
nonumLines i (n, (('i':'m':'p':'o':'r':'t':x):xs)) m= do
  putStr (indent 6)
  setSGR [SetColor Foreground Vivid Red]
  putStr "import"
  setSGR [Reset]
  putStrLn x
  nonumLines i (n+1, xs) (m-1)
nonumLines i (n, (('m':'o':'d':'u':'l':'e':' ':x):xs)) m= do
  putStr (indent 6)
  let ws = words x
  setSGR [SetColor Foreground Vivid Red]
  putStr "module "
  setSGR [Reset]
  ckModule ws
  setSGR [SetColor Foreground Vivid Green]
  ckWhere ws
  setSGR [Reset]
  nonumLines i (n+1, xs) (m-1)
nonumLines i (n, (('-':'-':x):xs)) m= do
  putStr (indent 6)
  setSGR [SetColor Foreground Vivid Blue]
  putStr "--"
  putStrLn x
  setSGR [Reset]
  nonumLines i (n+1, xs) (m-1)
nonumLines i (n, x:xs) m = do
  putStr (indent 6)
  putChars x
  nonumLines i (n+1, xs) (m-1)

process :: Document -> IO () 
process xs= do
  setCursorPosition 15 0
  clearFromCursorToScreenBeginning
  setCursorPosition 0 0   
  cntLines xs

processWithoutCursor :: Document -> IO () 
processWithoutCursor xs= do
  setCursorPosition 15 0
  clearFromCursorToScreenBeginning
  setCursorPosition 0 0  
  withoutCursorLines xs

sv :: String -> Document -> IO ()
sv fName buffer = do
     writeFile "draft.dat" (allToString buffer)
     s <- readFile "draft.dat"
     writeFile fName s

edtarLinha bff 
   | n < 1= 0
   | n <= c= n-1
   | otherwise= c
 where
   (kr, (i,c,e), n, _, _, _)= theBuffer bff

appRepl ::  Document -> Repl ()
appRepl xs= do
  liftIO $ processWithoutCursor xs
  liftIO $ setCursorPosition (getCursorLine xs) 0
  liftIO $ clearLine  
  minput <- getInputLine (appendCursor xs)
  case minput of
    Nothing -> outputStrLn "Goodbye"
    Just ['.'] -> do liftIO $ process xs
                     repl xs
    Just input -> do let nuBf= appendLine xs input
                     appRepl (stackDelete nuBf)

redtar :: Document -> Repl ()
redtar xs= do
   liftIO $ setCursorPosition (edtarLinha xs) 0
   liftIO $ clearLine  
   minput <- getInputLineWithInitial (cursorLine xs) (gLin xs)
   case minput of
     Nothing -> outputStrLn "Goodbye"
     Just input -> do let nuBf=  executeCommand ("r "++input) xs
                      liftIO $ process nuBf
                      appRepl nuBf

repl ::  Document -> Repl ()
repl xs= do
  liftIO $ setCursorPosition 16 0
  liftIO $ clearLine
  minput <- getInputLine (cursorLine xs)
  case minput of
           Nothing -> outputStrLn "Goodbye"
           Just ('q':' ':fname) -> do let (file:_)= words fname 
                                      liftIO $ sv file xs
                                      liftIO $ clearFromCursorToScreenEnd
                                      -- liftIO $ clearScreen
           Just ('s':' ':fname) -> do liftIO $ sv fname xs
                                      liftIO $ process xs
                                      repl xs
           Just ['j'] -> do let nuBf=executeCommand (linJ xs) xs
                            liftIO $ process nuBf
                            repl nuBf
           Just ['k'] -> do let nuBf= executeCommand (linK xs) xs
                            liftIO $ process nuBf
                            repl nuBf
           Just ['a'] -> appRepl xs
           Just ['i'] -> do 
                          let nuBf= insertLine xs ""
                          liftIO $ process nuBf
                          liftIO $ setCursorPosition (edtarLinha nuBf) 0
                          liftIO $ clearLine
                          minput <- getInputLineWithInitial (cursorLine nuBf) (gLin nuBf)
                          case minput of
                             Nothing -> outputStrLn "Goodbye"
                             Just input -> do let nuBuf= ureplaceLine nuBf input
                                              liftIO $ process nuBuf
                                              appRepl (stackDelete nuBuf)
           Just ['e'] -> redtar xs
           Just input -> do let nuBf= executeCommand input xs
                            liftIO $ process nuBf
                            repl nuBf

check p f= do
   result <- p f
   if result
      then return ()
      else writeFile f "-- "

sz [] acc= show acc
sz (x:xs) acc= sz xs (acc+1)

edt [f]= do
  -- clearScreen
  setTitle ("hed: "++f)
  setCursorPosition 16 0
  let (file:_)= words f
  check doesFileExist file
  s <- readFile  file
  setCursorPosition 16 0
  clearFromCursorToScreenBeginning
  setCursorPosition 15 2
  setSGR [SetUnderlining SingleUnderline]
  putStr file
  setSGR [Reset]
  let buff= fillDocument s
  process buff
  runInputT defaultSettings (repl buff)
edt _ = do
  setCursorPosition 16 0
  clearFromCursorToScreenBeginning
  let buff= fillDocument "-- "
  process buff
  runInputT defaultSettings (repl buff)

main= do
  f <- getArgs
  edt f



{--
Para utilizar o ansi-terminal, faça download:

https://hackage.haskell.org/package/ansi-terminal

Compile com:

sudo cabal install

Documentação: 

hackage.haskell.org/
package/ansi-terminal-0.6.2.3/
docs/System-Console-ANSI.html

cursorForward :: Int -> IO () -- Number of chars to move
cursorBackward :: Int -> IO () -- Number of chars to move
cursorUpLine::Int->IO () -- Number of lines to move
cursorDownLine::Int-> IO () -- Number of lines to move

setCursorColumn::Int-> IO () -- 0 based column to move
setCursorPosition::Int->Int -> IO () -- 0 based line/column to move to

clearFromCursorToScreenEnd :: IO ()
clearFromCursorToScreenBeginning::IO ()
clearScreen :: IO ()
clearFromCursorToLineEnd :: IO ()
clearFromCursorToLineBeginning :: IO ()
clearLine :: IO ()

setSGR :: [SGR] -> IO ()
setSGR [SetColor Foreground Vivid Red]
setSGR [SetColor Background Vivid Blue]
setSGR [Reset]

Cores: Black, Red, Green, Yellow, Blue
       Magenta, Cyan, White

Intensidade: Dull, Vivid

setSGR [SetUnderline SingleUnderline]

--}

