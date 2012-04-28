import System.IO
import Data.Char --for the toLower function
import Control.Applicative

type Rune  = String
type Runes = String
type Codes = String
type Code  = String

longest_code = 5

means :: Code -> Maybe Rune
means ""        = Just ""
means "f"       = Just "<img src='anglo1.bmp'>"
means "u"       = Just "<img src='anglo2.bmp'>"
means "v"       = Just "<img src='anglo2.bmp'>"
means "th"      = Just "<img src='anglo3.bmp'>"
means "o"       = Just "<img src='anglo4.bmp'>"
means "r"       = Just "<img src='tolkien2.bmp'>"
means "c"       = Just "<img src='tolkien13.bmp'>"
means "g"       = Just "<img src='anglo7.bmp'>"
means "w"       = Just "<img src='tolkien1.bmp'>"
means "h"       = Just "<img src='anglo9.bmp'>"
means "n"       = Just "<img src='anglo10.bmp'>"
means "i"       = Just "<img src='anglo11.bmp'>"
means "j"       = Just "<img src='anglo12.bmp'>"
means "ii"      = Just "<img src='anglo13.bmp'>"
means "p"       = Just "<img src='anglo14.bmp'>"
means "x"       = Just "<img src='anglo15.bmp'>"
means "z"       = Just "<img src='anglo15.bmp'>"
means "s"       = Just "<img src='anglo16.bmp'>"
means "t"       = Just "<img src='tolkien12.bmp'>"
means "b"       = Just "<img src='tolkien6.bmp'>"
means "e"       = Just "<img src='anglo17.bmp'>"
means "m"       = Just "<img src='anglo18.bmp'>"
means "l"       = Just "<img src='anglo19.bmp'>"
means "ng"      = Just "<img src='anglo20.bmp'>"
means "ce"      = Just "<img src='anglo21.bmp'>"
means "d"       = Just "<img src='anglo22.bmp'>"
means "a"       = Just "<img src='anglo23.bmp'>"
means "ae"      = Just "<img src='anglo24.bmp'>"
means "y"       = Just "<img src='anglo25.bmp'>"
means "ea"      = Just "<img src='anglo26.bmp'>"
means "gh"      = Just "<img src='anglo27.bmp'>"
means "k"       = Just "<img src='tolkien17.bmp'>"
means "kh"      = Just "<img src='anglo29.bmp'>"
means "."       = Just "<img src='dot.png'>"
means ","       = Just "<img src='comma.png'>"
means "!"       = Just "<img src='!.png'>"
means ")"       = Just "<img src=').png'>"
means (' ':[])  = Just "<img src='blank.png'>"
means ":)"      = Just "<img src='smiley.png'>" 
means [x]       = Just ("???" ++ [x] ++ "???")
means  _        = Nothing


-- Each element in the list is an attempt to find the correct code length. 
-- eg. ":)" is a 2-length code. " " and ":" are a 1-length codes.
-- if its the wrong code length, it won't find the rune so there will be Nothing.
-- in that case, keep checking the other attempts
-- if it has found the correct rune length, it will have Just the Rune. 
-- in that case, use the correct Rune and add it to the current list of correct Runes
-- ignore all the other attempts, because we have found one that works
-- if we have run out of attempts, run screaming for the hills
stick :: [(Maybe Rune, Runes)] -> Runes
stick []                 = "???out of guesses???"
stick ((Just x , y ):_ ) = x ++ y
stick ((Nothing, _ ):as) = stick as


-- Make a list of attempts at how big the code is (longest first). Then "stick" the correct
-- list of attempts together.
meaning :: Codes -> Runes
meaning ""     = ""
meaning xs     = stick [(means $ take n xs, meaning $ drop n xs) | n <- reverse [1..longest_code]]               


main :: IO()
main = do putStrLn "This program reads a file called codes.txt and creates its representation in anglo-saxon runes in runes.html"
          putStrLn "You can open runes.html in your webbrowser"
          putStrLn "If you see an error message, its probably because you haven't got a codes.txt file in this current directory"
          putStrLn "----------------------"  
          cs <- fmap (fmap toLower) $ readFile "codes.txt"
          putStrLn "codes.txt"
          putStrLn "----------------------"
          putStrLn cs
          putStrLn "----------------------"
          putStrLn "runes.html"
          putStrLn "----------------------"
          putStrLn (meaning cs)
          putStrLn "----------------------" 
          writeFile "runes.html" (meaning cs) 

--------------------


--------------------

tolkienmeans :: Code -> Maybe Rune
tolkienmeans ""        = Just ""
tolkienmeans "p"       = Just "<img src='tolkien1.bmp'>"
tolkienmeans "b"       = Just "<img src='tolkien2.bmp'>"
tolkienmeans "f"       = Just "<img src='tolkien3.bmp'>"
tolkienmeans "v"       = Just "<img src='tolkien4.bmp'>"
tolkienmeans "hw"      = Just "<img src='tolkien5.bmp'>"
tolkienmeans "m"       = Just "<img src='tolkien6.bmp'>"
tolkienmeans "mb"      = Just "<img src='tolkien7.bmp'>"
tolkienmeans "mh"      = Just "<img src='tolkien7.bmp'>"
tolkienmeans "t"       = Just "<img src='tolkien8.bmp'>"
tolkienmeans "d"       = Just "<img src='tolkien9.bmp'>"
tolkienmeans "th"      = Just "<img src='tolkien10.bmp'>"
tolkienmeans "dh"      = Just "<img src='tolkien11.bmp'>"
tolkienmeans "n"       = Just "<img src='tolkien12.bmp'>"
tolkienmeans "r"       = Just "<img src='tolkien12.bmp'>"
tolkienmeans "ch"      = Just "<img src='tolkien13.bmp'>"
tolkienmeans "j"       = Just "<img src='tolkien14.bmp'>"
tolkienmeans "sh"      = Just "<img src='tolkien15.bmp'>"
tolkienmeans "zh"      = Just "<img src='tolkien16.bmp'>"
tolkienmeans "nj"      = Just "<img src='tolkien17.bmp'>"
tolkienmeans "z"       = Just "<img src='tolkien17.bmp'>"
tolkienmeans "k"       = Just "<img src='tolkien18.bmp'>"
tolkienmeans "g"       = Just "<img src='tolkien19.bmp'>"
tolkienmeans "kh"      = Just "<img src='tolkien20.bmp'>"
tolkienmeans "gh"      = Just "<img src='tolkien21.bmp'>"
tolkienmeans "ng"      = Just "<img src='tolkien22.bmp'>"
tolkienmeans (' ':[])  = Just "<img src='blank.png'>"
tolkienmeans [x]       = Just ("???" ++ [x] ++ "???")
tolkienmeans  _        = Nothing