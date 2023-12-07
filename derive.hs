module Derive
    ( Regex(..), match
    ) where

data Regex = EmptySet
  | EmptyString
  | Character Char
  | Concat Regex Regex
  | ZeroOrMore Regex
  | Or Regex Regex
  | OneOrMore Regex
  | Optional Regex deriving (Show)

data RoseTree a = Node a [RoseTree a] deriving (Show)
-- Text expression "ab@b|"
-- Should be (a@b)|b
--        |
--      /   \
--     @     b
--    / \ 
--   a   b

-- This structure works with how derive.hs already works
-- let pattern = Or ()
-- let myTree= Node '|' [Node '@' [Node 'a' [], Node 'b' []], Node 'b'[]]


roseTreeToRegex :: RoseTree Char -> Regex
roseTreeToRegex (Node '|' [left, right]) = Or (roseTreeToRegex left) (roseTreeToRegex right)
roseTreeToRegex (Node '@' [left, right]) = Concat (roseTreeToRegex left) (roseTreeToRegex right)
roseTreeToRegex (Node '*' [child]) = ZeroOrMore (roseTreeToRegex child)
roseTreeToRegex (Node '+' [child]) = OneOrMore (roseTreeToRegex child)
roseTreeToRegex (Node '?' [child]) = Optional (roseTreeToRegex child)
roseTreeToRegex (Node 'ε' []) = EmptyString
roseTreeToRegex (Node '∅' []) = EmptySet
roseTreeToRegex (Node c []) = Character c
roseTreeToRegex _ = EmptySet 


nullable :: Regex -> Bool
nullable EmptySet = False
nullable EmptyString = True
nullable Character{} = False
nullable (Concat a b) = nullable a && nullable b
nullable ZeroOrMore{} = True
nullable (Or a b) = nullable a || nullable b
-- Need revising
nullable (OneOrMore _) = False
nullable (Optional _) = True

deriv :: Regex -> Char -> Regex
deriv EmptyString _ = EmptySet
deriv EmptySet _ = EmptySet
deriv (Character a) c = if a == c 
  then EmptyString else EmptySet
deriv (Concat r s) c = if nullable r
  then (deriv r c `Concat` s) `Or` deriv s c
  else deriv r c `Concat` s
deriv (ZeroOrMore r) c =
  deriv r c `Concat` ZeroOrMore r
deriv (Or r s) c =
  deriv r c `Or` deriv s c 
-- Need revising
deriv (OneOrMore r) c =
  deriv r c `Concat` ZeroOrMore r
deriv (Optional r) c = 
    (deriv r c `Concat` ZeroOrMore r) `Or` deriv r c

match :: Regex -> String -> Bool
match expr string = nullable (foldl deriv expr string)