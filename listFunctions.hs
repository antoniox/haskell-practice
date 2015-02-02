import Data.List (sortBy, splitAt)
import Data.Ord (comparing)

take' :: Int -> [a] -> [a]
take' n xs = fst $ splitAt n xs


drop' :: Int -> [a] -> [a]
drop' n xs = snd $ splitAt n xs


lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = error "list too short"


length' :: [a] -> Int
length' xs = foldr (\y ys -> ys + 1) 0 xs


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
sortByLength xs = sortBy (comparing length) xs


break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p [] = ([], [])
break' p (x:xs)
    | p x = ([], (x:xs))
    | otherwise = (x:(fst $ break' p xs), (snd $ break' p xs))


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (x:xs) (y:ys)
    | x == y = isPrefixOf' xs ys
    | otherwise = False


isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' xs ys = isPrefixOf' (reverse xs) (reverse ys)


null' :: [a] -> Bool
null' [] = True
null' _ = False


head' :: [a] -> a
head' (x:xs) = x


tail' :: [a] -> [a]
tail' (x:xs) = xs


last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs


init' :: [a] -> [a]
init' (x:[]) = []
init' (x:xs) = x:init' xs


concat' :: [[a]] -> [a]
concat' ((x:xs):ys) = x:(concat' (xs:ys))
concat' ([]:ys) = concat' ys
concat' [] = []

reverse' :: [a] -> [a]
reverse' (x:xs) = reverse' xs ++ [x]
reverse' [] = []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs


or' :: [Bool] -> Bool
or' xs = foldr (||) False xs


all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (\y ys -> p y && ys) True xs


any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (\y ys -> p y || ys) False xs
