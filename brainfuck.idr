module Brainfuck
import Data.Vect

data BrainfuckCommand = INCP
                      | DECP
                      | INCD
                      | DECD
                      | OUTB
                      | ACCB
                      | JUMP
                      | BACK
                      | NULL

correctChars : String -> Bool
correctChars s = correctChars' $ unpack s where
  correctChars' : List Char -> Bool
  correctChars' []        = True
  correctChars' (x :: xs) = if Prelude.List.elem x ['>','<','+','-','.',',','[',']']
                              then correctChars' xs
                              else False

occurences : Char -> List Char -> Int
occurences x []        = 0
occurences x (y :: xs) = if x == y
                           then 1 + occurences x xs
                           else occurences x xs

correctBrackets : String -> Bool
correctBrackets s = (occurences '[' s') == (occurences ']' s') where
  s' = unpack s

correctPointer : String -> Bool
correctPointer s = (occurences '>' s') >= (occurences '<' s') where
  s' = unpack s

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

translate : Char -> BrainfuckCommand

modifyIndex : Vect m n -> Int -> Vect m n

findNext : Vect m n -> Char -> Int

findPrevious : Vect m n -> Char -> Int
