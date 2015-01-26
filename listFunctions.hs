import Data.List (sortBy)
import Data.Ord (comparing)


split :: Int -> [a] -> ([a], [a])
split 0 xs = ([], xs)
split n [] = ([], [])
split n (x:xs) = (x:(fst $ split (n - 1) xs), snd $ split (n - 1) xs)


take :: Int -> [a] -> [a]
take n xs = fst $ split n xs


drop :: Int -> [a] -> [a]
drop n xs = snd $ split n xs


lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = error "list too short"


length :: [a] -> Int
length xs = foldr (\y ys -> ys + 1) 0 xs


oddIntersperce :: [a] -> [[a]] -> [a]
oddIntersperce sep xs = foldr step [] xs
    where step y [] = y
          step y ys = y ++ sep ++ ys


meanOfList :: Fractional a => [a] -> a
meanOfList = uncurry (/) . foldr (\y (s, l) -> (s + y, l + 1)) (0, 0)


palindromize :: [a] -> [a]
palindromize xs = foldr (:) (reverse xs) xs


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy (comparing Main.length) xs


data Tree a = Node a (Tree a) (Tree a)
            | Empty


height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = max (height l) (height r) + 1
