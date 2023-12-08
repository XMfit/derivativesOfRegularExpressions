{-
 - Author: Thomas Johnson, thomasjohnso2020@my.fit.edu
 - Author: Angel Star, e-mail address
 - Course: CSE 4250, Fall 2023
 - Project: Proj4, Derivatives of Regular Expressions
 - Language implementation: Glasgow Haskell Compiler, Version 8.8.4
 - Resources: 
 - Wikipedia for rose trees: https://en.wikipedia.org/wiki/Rose_tree
 - Article from Walter Schulze, and his derivation algorithm
 -}


module Main where


data Regex = EmptySet
  | EmptyString
  | Character Char
  | Concat Regex Regex
  | ZeroOrMore Regex
  | Or Regex Regex
  | OneOrMore Regex
  | Optional Regex deriving (Show)


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
deriv (ZeroOrMore r) c = deriv r c `Concat` ZeroOrMore r
deriv (Or r s) c = deriv r c `Or` deriv s c 
-- Need revising
deriv (OneOrMore r) c = deriv r c `Concat` ZeroOrMore r
deriv (Optional r) c = EmptyString `Or` deriv r c


match :: Regex -> String -> Bool
match expr string = nullable (foldl deriv expr string)


isOperator :: Char -> Bool
isOperator c = c `elem` ['|', '+', '*', '?', '@']  -- Add more operators as needed

isUnaryOperator :: Char -> Bool
isUnaryOperator c = c `elem` ['+', '*', '?']

data RoseTree a = Node a [RoseTree a] deriving (Show)

-- Parse a string into a RoseTree
parseRoseTree :: String -> RoseTree Char
parseRoseTree = head . foldl processChar [] 
  where
    processChar :: [RoseTree Char] -> Char -> [RoseTree Char]
    processChar stack c
      | isOperator c =
          if isUnaryOperator c then
              let (child:rest) = stack
              in Node c [child] : rest
          else
              let (right:left:rest) = stack
              in Node c [left, right] : rest
      | otherwise = Node c [] : stack


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


evaluate :: String -> String
evaluate x
  | match (roseTreeToRegex (parseRoseTree expr)) str = "yes"
  | otherwise = "no"
  where tokens = words x
        expr = head tokens
        str = if 'ε' `elem` (tokens !! 1) then "" else tokens !! 1




main :: IO()
main = interact (unlines . map evaluate . lines)