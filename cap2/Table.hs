module Table where

emptyTable :: [(String, a)]
emptyTable = []

insertTable :: String -> a -> [(String, a)] -> [(String, a)]
insertTable s a t 
                | elemTable s t = t
                | otherwise = (s, a) : t


elemTable :: String -> [(String, a)] -> Bool
elemTable s t = not $ null [p | p <- t, fst p /= s]


updateTable :: String -> a -> [(String, a)] -> [(String, a)]
updateTable s a [] = error "Item not found!"
updateTable s a (x:xs) 
                     | s == (fst $ x) = (s, a) : xs
                     | otherwise = updateTable s a xs

removeTable :: String -> [(String, a)] -> (a, [(String, a)])
removeTable s [] = error "Item not found!"
removeTable s (x:xs)
                   | s == (fst $ x) = (snd x, xs)
                   | otherwise = removeTable s xs