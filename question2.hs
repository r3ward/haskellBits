import Data.List
import Control.Monad
import Data.Map (fromListWith, toList)

{- *** Question 2.1 *** -}
pretty :: [[String]] -> String
pretty
     = unlines.concat

{- *** Question 2.2 *** -}
type Point
 = ( Int, Int )

glider :: [ Point ]
glider
 = [(0,2),(1,3),(2,1),(2,2),(2,3)]

-- Take the list of points and row to find, returns list of all points on that row
feeder :: Int -> [Point] -> [Int]
feeder y [] 
 = []
feeder y ((a,b):zs)
 | y == b = a : feeder y zs
 | otherwise = feeder y zs

-- Takes list from above function and draws the rows
rowGenerate :: Int -> [Int] -> [String]
rowGenerate 0 [] = []
rowGenerate x [] = "." : rowGenerate (x-1) []
rowGenerate x (a:as) = rowGenerate' x x (a:as)
 where
 rowGenerate' c 0 [] = []
 rowGenerate' c x [] = "." : rowGenerate' c (x-1) []
 rowGenerate' c x (a:as)
  | c - x == a = "#" : rowGenerate' c (x-1) (as)
  | otherwise = "." : rowGenerate' c (x-1) (a:as)

-- Combines all functions to print out the input
visualisation :: Int -> Int -> [[ Point ]] -> [String]
visualisation x y z = visualisation' y x y z
 where
 visualisation' a x 0 z = []
 visualisation' a x y z = concat (rowGenerate x (sort(feeder (a-y) (concat z))) ) : visualisation' a x (y-1) z

{- *** Question 2.3 *** -}
-- returns all 9 neighbours for a point
generateNeighbourList :: Point -> [ Point ]
generateNeighbourList (x, y)
 = [(x-1, y-1), (x, y-1),
    (x+1, y-1), (x-1, y),
    (x+1, y), (x-1, y+1),
    (x, y+1), (x+1, y+1)]

--generates a list of every possible point for each point
generatePossible :: [ Point ] -> [[ Point ]]
generatePossible []
 = []
generatePossible (x:xs)
 =  generateNeighbourList x : generatePossible xs

--counts up the occurances of each point
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs
 = toList (fromListWith (+) [(x, 1) | x <- xs])

-- point selector for all dead cells
pointSelector :: [ Point ] ->  [(Point, Int)] -> [ Point ]
pointSelector y []
 = []
pointSelector y ((a,b):xs)
    | b == 3 || (( elem a y ) && b == 2 ) = a : pointSelector y xs
    | otherwise = pointSelector y xs

evolution :: [ Point ] -> [[ Point ]]
evolution x
  = iterate (\y -> pointSelector y (frequency (concat (generatePossible y)))) x