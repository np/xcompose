{-# LANGUAGE PatternGuards #-}
import NP.Unicode
import Data.Char
import Data.List

genLine :: String -> String -> String
genLine name s = "<Multi_key> " ++ keysFromString name ++ " : \"" ++ s ++ "\""

genLines :: [(String,String)] -> String
genLines = unlines . map (uncurry genLine)

keysFromString :: String -> String
keysFromString = intercalate " " . map keyFromChar

-- NOTE: /usr/include/X11/keysymdef.h
keyFromCharData :: [(Char,String)]
keyFromCharData =
  [('\'', "apostrophe")
  ,('`',  "grave")
  ,('<',  "less")
  ,('>',  "greater")
  ,('|',  "bar")
  ,('(',  "parenleft")
  ,(')',  "parenright")
  ,('[',  "bracketleft")
  ,(']',  "bracketright")
  ,('{',  "braceleft")
  ,('}',  "braceright")
  ,('+',  "plus")
  ,('-',  "minus")
  ,('^',  "asciicircum")
  ,('.',  "period")
  ,('=',  "equal")
  ,('~',  "asciitilde")
  ,('/',  "slash")
  ,('\\', "backslash")
  ,('?',  "question")
  ,('!',  "exclam")
  ,('_',  "underscore")
  ,(':',  "colon")
  ,(';',  "semicolon")
  ,('*',  "asterisk")
  ,('"',  "quotedbl")
  ,('#',  "numbersign")
  ,(',',  "comma")
  ,(' ',  "space")
  ,('\t', "tab")
  ,('@',  "at")
  ,('\n', "Return")
  ,('→',  "Right")
  ,('←',  "Left")
  ,('↑',  "Up")
  ,('↓',  "Down")
  ,('&',  "ampersand")
  ]

keyFromChar :: Char -> String
keyFromChar c | isAscii c && isAlphaNum c          = ['<',c,'>']
              | Just s <- lookup c keyFromCharData = '<' : s ++ ">"
              | otherwise                          = error $ "genXCompose.keyFromChar: incomplete table, unexpected `" ++ c : "' (" ++ show (ord c) ++ ")"

disambMore = filter ((/= "fake") . snd) . disamb . ((".>","fake"):)

main :: IO ()
main = do
  let static = ""
  writeFile ".XCompose" (txt ++ static)
  where txt = genLines . disambMore $ greek ++ futhark ++ symbols ++ subscripts ++ superscripts
