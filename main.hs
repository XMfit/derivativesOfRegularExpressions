

nullable :: String -> Bool
nullable expr
    | expr == "ε" = True
    | '*' `elem` expr = True
    | '|' `elem` expr = let (left, _:right) = break (== '|') expr
                        in nullable left || nullable right
    | '@' `elem` expr = let (left, _:right) = break (== '@') expr
                        in nullable left && nullable right
    | otherwise = False

-- From the article, i assume Regex is just a string alias 
-- match :: Regex -> String -> Bool
-- match r str = nullable (foldl deriv r str)


transform :: String -> String
transform x
        | match expr str = "yes"
        | otherwise = "no"
    where
        tokens = words x
        expr = head tokens
        str = last tokens

match :: String -> String -> Bool
match expr str = True


main :: IO()
main = interact transform