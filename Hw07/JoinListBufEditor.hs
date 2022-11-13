module JlMain where

import Buffer
import Editor
import JoinList
import Scrabble
import Sized

initialBuffer :: JoinList (Score, Size) String
initialBuffer =
  fromString
    "This buffer is for notes you don't want to save, and for/nevaluation of steam valve coefficients./nTo load a different file, type the character L followed/nby the name of the file./n"

main = runEditor editor initialBuffer
