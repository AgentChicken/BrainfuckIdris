module Brainfuck
import Data.Vect
import Debug.Error

--------------------------------------------------
---------------------COMPILE----------------------
--------------------------------------------------

{-
  Declare an abstract data type for all possible Brainfuck commands
    INCP means INCrement Pointer
    DECP means DECrement Pointer
    INCD means INCrement Data
    DECD means DECrement Data
    OUTB means OUTput one Byte
    ACCB means ACCept one Byte
    JUMP means JUMP to matching right bracket if data is zero
    BACK means go BACK to matching left bracket if data is nonzero
-}
data BrainfuckCommand = INCP
                      | DECP
                      | INCD
                      | DECD
                      | OUTB
                      | ACCB
                      | JUMP
                      | BACK
{-
  A list of acceptable keywords in Brainfuck
  > means increment pointer
  < means decrement pointer
  + means increment data
  - means decrement data
  . means output one byte
  , means accept one byte
  [ means jump to right bracket if data is zero
  ] means jump to left bracket if data is nonzero
-}
keywords : List Char
keywords = ['>','<','+','-','.',',','[',']']

{-
  Given a String, this function determines
  whether or not all symbols in the String
  are Brainfuck commands
-}
correctChars : String -> Bool
correctChars s = correctChars' $ unpack s where
  correctChars' : List Char -> Bool
  correctChars' []        = True
  correctChars' (x :: xs) = if Prelude.List.elem x keywords
                              then correctChars' xs
                              else False

{-
  Given a Char and a List of Chars, this
  function determines how many times the
  Char appears in the List
-}
occurences : Char -> List Char -> Int
occurences x []        = 0
occurences x (y :: xs) = if x == y
                           then 1 + occurences x xs
                           else occurences x xs

{-
  This function determines whether or not
  a String contains the same number of left
  brackets as right brackets. Having a different
  number of left and right brackets doesn't
  necessarily mean the program is malformed.
-}
correctBrackets : String -> Bool
correctBrackets s = (occurences '[' s') == (occurences ']' s') where
  s' = unpack s

{-
  This function determines whether there are
  at least as many '>'s as '<'s to decrease
  the likelihood that the pointer goes off the
  tape. Note that a program's having more
  decrements than increments doesn't necessarily
  imply that the pointer goes off the tape.
-}
correctPointer : String -> Bool
correctPointer s = (occurences '>' s') >= (occurences '<' s') where
  s' = unpack s

{-
  Given a String, this function will perform
  correctChars, correctBrackets, and correctPointer
  and print the results of the test to the user.
-}
check : String -> IO ()
check s = do
  if correctChars s
    then putStrLn "All input recognized."
    else putStrLn "Unrecognized input."
  if correctBrackets s
    then putStrLn "All brackets matched."
    else putStrLn "Warning: mismatched brackets. This may be intentional, but is likely a mistake."
  if correctPointer s
    then putStrLn "There are at least as many pointer increment symbols as decrement symbols. Note that this does not guarantee that the pointer won't go past the tape."
    else putStrLn "Warning: more pointer decrement symbols than increment symbols. Depending on how you use pointer increments and decrements in loops, this may be inconsequential."

{-
  Given a Char, this function will output the
  appropriate Brainfuck command.
-}
partial
translate : Char -> BrainfuckCommand
translate '>' = INCP
translate '<' = DECP
translate '+' = INCD
translate '-' = DECD
translate '.' = OUTB
translate ',' = ACCB
translate '[' = JUMP
translate ']' = BACK

{-
  Given a String, this function will output
  a List of the corresponding Brainfuck commands.
-}
compile : String -> List BrainfuckCommand
compile s = compile' $ unpack s where
  compile' : List Char -> List BrainfuckCommand
  compile' [] = []
  compile' (x :: xs) = translate x :: compile' xs


--------------------------------------------------
-----------------------RUN------------------------
--------------------------------------------------

{-
  Create a list of 1000 zeroes.
-}
tape : List Int
tape = replicate 1000 0

run : List BrainfuckCommand -> Vect 1000 Int -> Int -> IO ()
run []           ys x = putStrLn "\nProcess completed successfully."
run (INCP :: xs) ys x = if x == 999
                          then putStrLn "Error: pointer is out of bounds."
                          else run xs ys (x + 1)
run (DECP :: xs) ys x = if x == 0
                          then putStrLn "Error: pointer is out of bounds."
                          else run xs ys (x - 1)
run (INCD :: xs) ys x = run xs (updateAt (restrict 999 (cast x)) (\x => x + 1) ys) x
run (DECD :: xs) ys x = run xs (updateAt (restrict 999 (cast x)) (\x => x - 1) ys) x
run (OUTB :: xs) ys x = do
                          print $ chr $ Data.Vect.index (restrict 999 (cast x)) ys
                          run xs ys x
run (ACCB :: xs) ys x = do
                          input <- getChar
                          run xs (updateAt (restrict 999 (cast x)) (\x => input) ys) x
run (JUMP :: xs) ys x = ?run_rhs_9
run (BACK :: xs) ys x = ?run_rhs_10
