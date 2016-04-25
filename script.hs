import Data.List.Split
import Data.List
import Data.Ord

data Location = Location
                { lat :: Float
                , lon :: Float
                , name :: Integer
                }

instance Eq Location where
  l1 == l2 = name l1 == name l2

instance Show Location where
  show (Location _ _ n) = show n

newtype Path = Path { unPath :: [Location] }
               deriving (Show, Eq)

parse :: (Integer, String) -> Location
parse (i, s) = Location (read lat) (read lon) i
          where [lat, lon] = splitOn " " s

distance :: Location -> Location -> Float
distance (Location x1 y1 _) (Location x2 y2 _) = sqrt (x * x + y * y)
                                             where x = x2 - x1
                                                   y = y2 - y1

paths :: [a] -> [(a,a)]
paths yss@(y:ys) = (last ys, y):(zip yss (tail yss))

distanceTraveled :: Path -> Float
distanceTraveled (Path xs) = foldr (\curr acc -> acc + (uncurry distance curr)) 0 (paths xs )

startFrom1 :: Path -> Path
startFrom1 (Path xs@((Location _ _ 1):_)) = Path xs
startFrom1 (Path (x:xs)) = (startFrom1 (Path (xs++[x])))

gotoLower :: Path -> Path
gotoLower (Path xss@(x:y:xs))
  | i1 < i2 = Path xss
  | otherwise = Path (x:(reverse (y:xs)))
  where
    i1 = name y
    i2 = name . last $ xs

swap :: Eq a => a -> a -> [a] -> [a]
swap n m = map (\x -> if n == x then m else if m == x then n else x)

allPossibleSwaps :: Eq a => [a] -> [[a]]
allPossibleSwaps xs = [swap i j xs | i <- xs, j <- xs]

computeAllPossibleDistances :: Path -> [(Path, Float)]
computeAllPossibleDistances (Path fs) = zip swaps ds 
  where
    swaps = map Path . allPossibleSwaps $ fs
    ds = map distanceTraveled swaps

format :: Path -> Path
format = gotoLower . startFrom1

loopUntil :: (Path, Float) -> Float -> (Path, Float)
loopUntil (argPath, argDistance) threshold -- 10330 is a good threshold
  | argDistance < threshold = (argPath, argDistance)
  | otherwise = (loopUntil currMin threshold)
  where currMin = (minimumBy (comparing snd) (computeAllPossibleDistances argPath))

main = do
  x <- readFile "input.txt"
  let fs = Path . map parse . zip [1..] . tail . lines $ x
  --print (distanceTraveled fs)
  print (loopUntil (fs, (distanceTraveled fs)) 10330)
  --let answer = minimumBy (comparing snd) (computeAllPossibleDistances fs)
  --print (format (fst answer))
