import Data.List (sortBy, sort, splitAt)
import Data.Ord (comparing)


data Direction = LeftTurn
                | Straight
                | RightTurn
                deriving (Eq, Show)


data Point2D = Point2D {
    px :: Double,
    py :: Double
} deriving (Eq, Ord, Show)


points :: [(Double, Double)] -> [Point2D]
points ((a, b):xs) = Point2D a b:points xs
points _ = []


data Vector2D = Vector2D {
    vx :: Double,
    vy :: Double
} deriving (Eq, Show)


vec :: Point2D -> Point2D -> Vector2D
vec a b = Vector2D (px a - px b) (py a - py b)

crossProduct :: Vector2D -> Vector2D -> Double
crossProduct a b = (vx a) * (vy b) - (vy a) * (vx b)


getDirection :: Point2D -> Point2D -> Point2D -> Direction
getDirection a b c
    | crossProduct' > 0 = LeftTurn
    | crossProduct' < 0 = RightTurn
    | otherwise = Straight
    where crossProduct' = crossProduct (vec b a) (vec c b)


getDirections :: [Point2D] -> [Direction]
getDirections (x:y:z:zs) = (getDirection x y z):(getDirections (y:z:zs))
getDirections _ = []


norm :: Vector2D -> Double
norm a = sqrt $ (vx a) * (vx a) + (vy a) * (vy a)


angle :: Point2D -> Point2D -> Double
angle a b = acos $ vx (vec b a) / norm (vec b a)


tplfunc f g x = (f x, g x)


grahamsScan :: [Point2D] -> [Point2D]
grahamsScan xs = grahamsScan' processed
    where processed = lowest:(sortBy (comparing $ tplfunc (angle lowest) px) rest) 
          lowest:rest = sortBy (\a b -> compare (py a, px a) (py b, px b)) xs

          grahamsScan' ys = case other of
               [] -> ys
               z:zs -> grahamsScan' $ (fst $ unzip left) ++ (fst $ unzip zs)
            where (left, other) = span (\(_, t) -> t == LeftTurn) pairs
                  pairs = zip ys (LeftTurn:(getDirections ys) ++ [LeftTurn])
