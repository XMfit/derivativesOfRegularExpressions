import Data.List

splitString :: String -> [String]
splitString = map (:[]) 

toInfix :: String -> String
toInfix = head . foldl foldingFunction [] . splitString
    where   foldingFunction (x:y:ys) "|" = ("(" ++ y ++ "|" ++ x ++ ")"):ys
            foldingFunction (x:y:ys) "+" = ("(" ++ y ++ "+" ++ x ++")"):ys
            foldingFunction (x:y:ys) "@" = ("(" ++ y ++ "@" ++ x ++")"):ys
            -- These are urany ops so other method doesn't work
            foldingFunction (x:xs) "*" = (x ++ "*"):xs
            foldingFunction (x:xs) "?" = (x ++ "?"):xs
            foldingFunction xs numberString = numberString:xs
            -- Idk how to handle empty or epsilon

-- splitString toInfix ("expr")