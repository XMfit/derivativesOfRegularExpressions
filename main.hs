module Main where

transform :: String -> String
transform x
      | True = "yes"
      | otherwise = "no"
    where 
        tokens = words x
        expr = head tokens
        str = last tokens 



main :: IO()
main = interact (unlines . map transform . lines)