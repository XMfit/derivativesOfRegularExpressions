{-
 - Author: Thomas Johnson, thomasjohnso2020@my.fit.edu
 - Author: Angel Star, e-mail address
 - Course: CSE 4250, Fall 2023
 - Project: Proj4, deriveatives of Regular Expressions
 - Language implementation: Glasgow Haskell Compiler, Version 8.8.4
 - Resources: 
 - Wikipedia for rose trees: https://en.wikipedia.org/wiki/Rose_tree
 - Article from Walter Schulze, and his deriveation algorithm
 - Learn you a haskell book http://learnyouahaskell.com/chapters
 -}


module Main where


data Regex = EmptySet
  | EmptyString
  | Character Char
  | Or Regex Regex
  | Concat Regex Regex
  | ZeroOrMore Regex
  | OneOrMore Regex
  | Optional Regex deriving (Show)


nullable :: Regex -> Bool
nullable EmptySet = False
nullable EmptyString = True
nullable (Character _) = False
nullable (Or a b) = nullable a || nullable b
nullable (Concat a b) = nullable a && nullable b
nullable (ZeroOrMore _) = True
nullable (OneOrMore r) = nullable r
nullable (Optional _) = True


derive :: Regex -> Char -> Regex
derive EmptyString _ = EmptySet
derive EmptySet _ = EmptySet
derive (Character a) b = if a == b then EmptyString else EmptySet
derive (Or r s) c = derive r c `Or` derive s c
derive (Concat r s) c = if not (nullable r) then derive r c `Concat` s else (derive r c `Concat` s) `Or` derive s c
derive (ZeroOrMore r) c = derive r c `Concat` ZeroOrMore r
derive (OneOrMore r) c = derive r c `Concat` ZeroOrMore r
derive (Optional r) c = EmptyString `Or` derive r c


match :: Regex -> String -> Bool
match expr string = nullable (foldl derive expr string)

-- Helper function for finding if its an operator
isOperator :: Char -> Bool
isOperator c = c `elem` ['|', '+', '*', '?', '@']

isUnaryOperator :: Char -> Bool
isUnaryOperator c = c `elem` ['+', '*', '?']

-- Rose tree structure containing type value of Node a and another tree
data RoseTree a = Node a [RoseTree a] deriving (Show)

-- Parse a string into a RoseTree
parseRoseTree :: String -> RoseTree Char
parseRoseTree = head . foldl processChar [] 
  where
    -- Function to process each character and update the stack
    processChar :: [RoseTree Char] -> Char -> [RoseTree Char]
    processChar stack c
      | isOperator c =  -- If char is operator
          if isUnaryOperator c then  -- Check if its unary
              let (child:rest) = stack  -- Pop one node
              in Node c [child] : rest  -- Create new node and push back to stack
          else  -- Else for binary
              let (right:left:rest) = stack  -- Pop two nodes
              in Node c [left, right] : rest  -- Create new node and push back to stack
      | otherwise = Node c [] : stack  -- For operands create a leaf node and push to stack

roseTreeToRegex :: RoseTree Char -> Regex
-- binary ops
roseTreeToRegex (Node '|' [left, right]) = Or (roseTreeToRegex left) (roseTreeToRegex right)
roseTreeToRegex (Node '@' [left, right]) = Concat (roseTreeToRegex left) (roseTreeToRegex right)
-- unary ops
roseTreeToRegex (Node '*' [child]) = ZeroOrMore (roseTreeToRegex child)
roseTreeToRegex (Node '+' [child]) = OneOrMore (roseTreeToRegex child)
roseTreeToRegex (Node '?' [child]) = Optional (roseTreeToRegex child)
-- special chars
roseTreeToRegex (Node 'ε' []) = EmptyString
roseTreeToRegex (Node '∅' []) = EmptySet
-- default char
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