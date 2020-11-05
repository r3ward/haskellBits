import Data.List
import Control.Monad

convert :: (Integer, Integer, Integer, Integer, Integer, Integer) -> [Integer]
convert (a, b, c, d, e, f) = [a, b, c, d, e, f]

{- *** Question 1.1 *** -}
rule1 :: (Integer, Integer, Integer, Integer, Integer, Integer) -> Bool
rule1 x
 | (convert x) < nub (convert x) = False
 | otherwise = True

{- *** Question 1.2 *** -}
rule2method :: [Integer] -> Bool
rule2method (x:[]) 
  = True
rule2method (x:y:zs) 
  | even x && odd y = rule2method (y:zs)
  | odd x && even y  = rule2method (y:zs)
  | otherwise = False

rule2 :: (Integer, Integer, Integer, Integer, Integer, Integer) -> Bool
rule2 (xs)
 = rule2method(convert xs)

{- *** Question 1.3 *** -}
rule3method :: [Integer] -> Bool
rule3method[] = True
rule3method (x:[]) = True
rule3method (x:y:zs)
  | abs(x-y) > 2 = rule3method (y:zs)
  | otherwise = False

rule3 :: (Integer, Integer, Integer, Integer, Integer, Integer) -> Bool
rule3 (xs)
 = rule3method(convert xs)

{- *** Question 1.4 *** -}
rule4 :: (Integer, Integer, Integer, Integer, Integer, Integer) -> Bool
rule4 (a, b, c, d, e, f)
    | mod (a*10+b) (e*10+f) == 0 && mod (c*10+d) (e*10+f) == 0 = True
    | otherwise = False
 
{- *** Question 1.5 *** -}
possibles = fmap (\[a,b,c,d,e,f] -> (a,b,c,d,e,f)) . replicateM 6 $ [0..5]

{- *** Question 1.6 *** -}
solution:: (Integer, Integer, Integer, Integer, Integer, Integer) -> Bool
solution xs
  | rule1 xs && rule2 xs && rule3 xs && rule4 xs = True
  | otherwise = False

main :: IO()
main  
 = putStrLn(show(rule4 (4,9,6,3,0,7)))