module Doc where

import Data.List (isInfixOf)

data Clipboard = C [[Char]] [[Char]]
    deriving (Show)
data Document = Buffer Clipboard (Int, Int, Int)
                    Int [[Char]] [Char] [[Char]]
    deriving (Show)

fillDocument :: String -> Document
fillDocument input = Buffer (C [] []) (1, 6, 15) 1 [] (head readin) (tail readin)
    where readin = lines input

appendLine :: Document -> [Char] -> Document
appendLine (Buffer kr  ice 0 [] [] []) newline = Buffer kr ice 1 [] newline [] 
appendLine (Buffer kr ice lineNum prev cur post) newline = 
                Buffer kr ice (lineNum + 1) (cur : prev) newline post

trimnl = reverse . dropWhile (== '\n') . reverse

apLine :: Document -> [Char] -> Document
apLine (Buffer kr ice 0 [] [] []) newline = Buffer kr ice 1 [] newline [] 
apLine (Buffer kr ice lineNum prev cur post) newline = 
          Buffer kr ice lineNum  prev (cur ++ newline) post

mergeLine :: Document -> [Char] -> Document
mergeLine (Buffer kr ice 0 [] [] []) newline = Buffer kr ice 1 [] newline [] 
mergeLine (Buffer kr ice lineNum prev cur []) newline =
          Buffer kr ice lineNum prev cur []
mergeLine (Buffer kr ice lineNum prev cur post) newline = 
          Buffer kr ice lineNum  prev (cur ++ (head post)) (tail post)

searchString m str (Buffer kr ice n prev cur post)
    | isInfixOf str cur = n
searchString m str (Buffer kr ice n prev cur (x:post))=
    searchString m str (Buffer kr ice (n+1) (cur:prev) x post)
searchString m str (Buffer kr ice n prev cur [])= m  

insertLine :: Document -> [Char] -> Document
insertLine (Buffer kr ice 0 [] [] []) newline = Buffer kr ice 1 [] newline []
insertLine (Buffer kr ice lineNum prev cur post) newline = 
                Buffer kr ice lineNum prev newline (cur : post)

deleteLine :: Document -> Document
deleteLine (Buffer kr ice 0 _  _ _ )   = Buffer kr ice 0 [] [] []
deleteLine (Buffer kr ice _ [] _ post) = Buffer kr ice 1 [] (head post) (tail post)
deleteLine (Buffer kr ice lineNum prev _ post) = 
                   Buffer kr ice (lineNum - 1) (tail prev)  (head prev) post

deleteSaveLine :: Document -> Document
deleteSaveLine (Buffer kr ice 0 _  _ _ )   = Buffer kr ice 0 [] [] []
deleteSaveLine (Buffer (C u kr) ice _ [] cur post) = 
          Buffer (C (("a "++cur):u) (cur:kr)) ice 1 [] (head post) (tail post)
deleteSaveLine (Buffer (C u kr) ice lineNum prev cur post) = 
                   Buffer (C (("a " ++ cur):u) (cur:kr)) ice (lineNum - 1)
                             (tail prev)  (head prev) post

replaceLine :: Document -> String -> Document
replaceLine (Buffer (C u kr) ice lineNum prev cur post) newline =
                  Buffer (C (("r "++cur):u) kr) ice lineNum prev newline post

ureplaceLine :: Document -> String ->  Document
ureplaceLine (Buffer kr ice lineNum prev cur post) newline =
                  Buffer kr ice lineNum prev newline post

changeLine :: Document -> Document
changeLine (Buffer (C u []) ice lineNum pref cur post) =
       Buffer (C u []) ice lineNum pref cur post
changeLine (Buffer (C u (x:kr)) ice lineNum prev cur post) =
              Buffer (C u (cur:kr)) ice lineNum prev x post

downLine :: Document -> Int -> Document
downLine org@(Buffer (C u kr) ice lineNum prev cur []) _ = org
downLine (Buffer (C u kr) ice lineNum prev cur post) 1 = 
    gotoLine (Buffer (C ("k 1":u) kr) ice lineNum prev cur post) (lineNum+1)
downLine (Buffer (C u kr) ice lineNum prev cur post) iline
  | iline > (length post) = gotoLine (Buffer (C (("k "++(show (length post))):u) kr)
                                       ice lineNum prev cur post) (lineNum+iline)
  | otherwise             = gotoLine (Buffer (C (("k "++(show iline)):u) kr)
                                        ice lineNum prev cur post) (lineNum+iline)

upLines :: Document -> Int -> Document
upLines (Buffer kr ice 1 [] cur post) _ = (Buffer kr ice 1 [] cur post)
upLines (Buffer (C u kr) ice lineNum prev cur post) 1 =
    gotoLine (Buffer (C ("j 1":u) kr) ice lineNum prev cur post) (lineNum - 1)
upLines (Buffer (C u kr) ice lineNum prev cur post) iline
    | iline > (length prev) = gotoLine (Buffer (C (("j "++(show (0+(length prev)))):u) kr)
                                        ice lineNum prev cur post) (lineNum-iline)
    | otherwise             = gotoLine (Buffer (C (("j "++(show (0+iline))):u) kr)
                                        ice lineNum prev cur post)  (lineNum - iline)

gotoLine :: Document -> Int -> Document
gotoLine org@(Buffer kr ice lineNum prev cur post) newline
    | newline < 1       = gotoLine org 1
    | lineNum > newline = gotoLine (Buffer kr ice (lineNum-1) 
                                           (tail prev) 
                                           (head prev) 
                                           (cur:post)) newline  
    | post == []         = org
    | lineNum < newline = gotoLine (Buffer kr ice (lineNum+1) 
                                           (cur:prev) 
                                           (head post) 
                                           (tail post)) newline  
    | otherwise         = org

numString :: Document -> String
numString (Buffer kr ice n _ cur post)= unlines 
                                  (zipWith (++)  
                                    [(show i) ++ "- " | i <- [n..1000]]
                                    (cur:post))

from1NumString :: Document -> String
from1NumString (Buffer kr ice 1 _ cur post)= unlines 
                                  (zipWith (++)  
                                    [(show i) ++ "- " | i <- [1..1000]]
                                    (cur:post))
from1NumString doc= from1NumString $ gotoLine doc 1

mkNumCursor csr n | n < 10  = (show n)++"   "++csr
mkNumCursor csr n | n < 100 = (show n)++"  "++csr
mkNumCursor csr n | n < 1000= (show n)++" "++csr
mkNumCursor csr n = (show n)++csr

indent n= [' ' | x <- [1..n]]

getBefore i n [] cur post = (n, cur:post)
getBefore 0 n (x1:xs) cur post= (n,  cur:post)
getBefore i n (x1:xs) cur post= getBefore (i-1) (n-1) xs x1 (cur:post)

nonumBefore i n [] cur post = (n, cur:post)
nonumBefore 0 n (x1:xs) cur post= (n,  cur:post)
nonumBefore i n (x1:xs) cur post= nonumBefore (i-1) (n-1) xs x1 (cur:post)

allToString :: Document -> String
allToString (Buffer kr ice 1 _ cur post) = unlines (cur:post) 
allToString doc = allToString $ gotoLine doc 1

gLin (Buffer kr ice n _ x _)= (x, "")

cursorLine (Buffer kr ice n _ _ aftr) =  (mkNumCursor "$ " n)

appendCursor (Buffer kr ice n _ _  aftr)=  (mkNumCursor "@ " n)

insCursor (Buffer kr ice n _ _  aftr)=  (mkNumCursor "i " n)

searchit str (Buffer kr ice m prev cur aftr) =
   searchString m str (Buffer kr ice m prev cur aftr)

tl (x:xs)= xs

toKillRing :: Document -> Document
toKillRing (Buffer (C u kr) ice n prev cur aftr) =
     Buffer (C u (cur : kr)) ice n prev cur aftr 

topKillRing :: Document -> [Char]
topKillRing (Buffer (C _ []) ice n prev cur aftr) = []
topKillRing (Buffer (C _ (x:xs)) ice n prev cur aftr)= x

gyrateKillRing :: Document -> Document
gyrateKillRing (Buffer (C u []) ice n prev cur aftr) =
      Buffer (C u []) ice n prev cur aftr
gyrateKillRing (Buffer (C u (x:xs)) ice n prev cur aftr)= 
    Buffer (C u (xs ++ [x])) ice n prev cur aftr

xKillRing :: Document -> Document
xKillRing (Buffer (C u []) ice n prev cur aftr) =
      Buffer (C u []) ice n prev cur aftr
xKillRing (Buffer (C u (x:xs)) ice n prev cur aftr)= 
    Buffer (C u xs) ice n prev cur aftr

xxxKillRing :: Document -> Document
xxxKillRing (Buffer (C u xs) ice n prev cur aftr)= 
    Buffer (C u []) ice n prev cur aftr

undoCmd org@(Buffer (C [] xs) ice n prev cur post) = org
undoCmd (Buffer (C (cmd:u) xs) ice n prev cur post) =
         executeUndo cmd (Buffer (C u xs) ice n prev cur post)

addUndoGo (Buffer (C u xs) ice n prev cur post) =
    Buffer (C (("g "++(show n)):u) xs) ice n prev cur post

udownLine :: Document -> Int -> Document
udownLine org@(Buffer (C u kr) ice lineNum prev cur []) _ = org
udownLine org@(Buffer kr ice lineNum prev cur post) 1 =
    gotoLine org (lineNum+1)
udownLine (Buffer (C u kr) ice lineNum prev cur post) iline
  | iline > (length post) = gotoLine (Buffer (C u kr)
                                       ice lineNum prev cur post) (lineNum+iline)
  | otherwise             = gotoLine (Buffer (C u kr)
                                        ice lineNum prev cur post) (lineNum+iline)

uupLines :: Document -> Int -> Document
uupLines (Buffer kr ice 1 [] cur post) _ = (Buffer kr ice 1 [] cur post)
uupLines org@(Buffer kr ice lineNum prev cur post) 1 = 
                gotoLine org (lineNum-1) 
uupLines (Buffer (C u kr) ice lineNum prev cur post) iline
    | iline > (length prev) = gotoLine (Buffer (C u kr)
                                        ice lineNum prev cur post) (lineNum-iline)
    | otherwise             = gotoLine (Buffer (C u kr)
                                        ice lineNum prev cur post)  (lineNum - iline)

getTheNum cmd =   ((read.head.(drop 1).words) cmd)::Int

executeUndo :: [Char] -> Document -> Document
executeUndo [] doc = doc 
executeUndo command doc 
    | abriv == 'd' && args == 0 = deleteSaveLine doc
    | abriv == 'D' && args == 0 = deleteLine doc
    | abriv == 'x' && args == 0 = xKillRing doc
    | abriv == 'X' && args == 0 = xxxKillRing doc
    | abriv == 'g' && args == 1 = gotoLine doc (getTheNum command)
    | abriv == 'j' && args == 1 = udownLine doc (getTheNum command)
    | abriv == 'k' && args == 1 = uupLines doc (getTheNum command)
    | abriv == 'r' && args >= 1 = ureplaceLine doc stament
    | abriv == 'r' = ureplaceLine doc []
    | abriv == 'R' && args == 0 = ureplaceLine doc (topKillRing doc)
    | abriv == 'i' && args >= 1 = insertLine doc stament
    | abriv == 'I' && args == 0 = insertLine doc (topKillRing doc)
    | abriv == 'a' && args >= 1 = appendLine doc stament
    | abriv == 'a' && args == 0 = appendLine doc (tail command)
    | abriv == 'A' && args == 0 = appendLine doc (topKillRing doc)
    | abriv == 'b' && args >= 1 = apLine doc stament
    | abriv == 'C' && args >= 0 = changeLine doc
    | abriv == 'o' && args == 0 = appendLine doc []
    | abriv == 'm' && args == 0 = mergeLine doc [] 
    | otherwise    = doc
        where abriv   = (head.head.words) command
              stament = ((tail.dropWhile (/=' ')).tail) command
              args    = length.tail.words $ command

executeCommand :: [Char] -> Document -> Document
executeCommand [] doc = doc 
executeCommand command doc 
    | abriv == 'd' && args == 0 = deleteSaveLine doc
    | abriv == 'D' && args == 0 = deleteLine doc
    | abriv == 'x' && args == 0 = xKillRing doc
    | abriv == 'X' && args == 0 = xxxKillRing doc
    | abriv == 'g' && args == 1 = gotoLine (addUndoGo doc) (getTheNum command) 
    | abriv == 'j' && args == 1 = downLine doc (getTheNum command)
    | abriv == 'k' && args == 1 = upLines doc (getTheNum command)
    | abriv == '/'  = gotoLine (addUndoGo doc) (searchit (tl command)  doc)
    | abriv == 'r' && args >= 1 = replaceLine doc stament
    | abriv == 'r' = replaceLine doc []
    | abriv == 'R' && args == 0 = replaceLine doc (topKillRing doc)
    | abriv == 'i' && args >= 1 = insertLine (stackDelete doc) stament
    | abriv == 'I' && args == 0 = insertLine doc (topKillRing doc)
    | abriv == 'a' && args >= 1 = appendLine (stackDelete doc) stament
    | abriv == 'A' && args == 0 = appendLine doc (topKillRing doc)
    | abriv == 'b' && args >= 1 = apLine doc stament
    | abriv == 'C' && args >= 0 = changeLine doc
    | abriv == 'o' && args == 0 = appendLine doc []
    | abriv == 'm' && args == 0 = mergeLine doc [] 
    | abriv == 'y' && args == 0 = toKillRing doc 
    | abriv == 'g' && args == 0 = gyrateKillRing doc
    | abriv == 'u' && args == 0 = undoCmd doc
    | otherwise    = doc
        where abriv   = (head.head.words) command
              stament = ((tail.dropWhile (/=' ')).tail) command
              args    = length.tail.words $ command

linJ (Buffer kr ice n _ _ _)= "j 1"
linK (Buffer kr ice n _ _ _)= "k 1"

getCursorLine (Buffer kr (i,c,e) n _ _ _)
     | c >= 13= 13
     | n <= c = n
     | otherwise= c

theBuffer (Buffer (C u kr) ice n b cur p)= (kr, ice, n, b, cur, p)
stackDelete (Buffer (C u kr) ice n b cur p)= Buffer (C ("D":u) kr) ice n b cur p

