--module PegExample where
module Peg.PegExample where

import Peg.Peg

-- Examples:

data MyST = MyTree [MyST] | MyStr String | MyAlt MyST | MyAster [MyST] | MyPlus [MyST]
  deriving (Show, Eq)

-- "a":
--myGrammar = PegEqToken id "a"

-- ( "j" | "a" + ) "b" "c":
myGrammar = PegCat MyTree [
              PegAlt MyAlt [
                PegEqToken MyStr "j",
                PegPlus MyPlus (PegEqToken MyStr "a")],
              PegEqToken MyStr "b",
              PegEqToken MyStr "c"]

-- "a":
--myGrammar = PegCat concat [PegEqToken id "a"]

-- ():
--myGrammar = PegCat concat ([]::[PegGrammar String])

main = print $ pegMatch myGrammar $ map (:[]) "abcd"
