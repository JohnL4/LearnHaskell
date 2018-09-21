import Data.Char
import Data.Map.Strict as Map (Map,empty,insertWith)

main :: IO ()
main = do
  putStrLn $ show $ map sndDigSqrt [2..9]
  putStrLn $ show $ map (sndDigFunc (**0.5)) [2..9]
  putStrLn $ show $ map (sndDigFunc log) [2..9]
  
  putStrLn $ ("log: " ++) $ show $
    freqDist $ map (sndDigFunc log) [2..1000000]
  putStrLn $ ("sqrt: " ++) $ show $
    freqDist $ map sndDigSqrt [2..1000000]

sndDigSqrt :: (Floating a, Show a) => a -> Int
sndDigSqrt x =
  digitToInt $ (filter (/= '.') $ dropWhile (\c -> c == '0' || c == '.' ) $ show (x ** 0.5)) !! 1

sndDigFunc :: (Floating a, Show a) => (a -> a) -> a -> Int
sndDigFunc f x =
  digitToInt $ (filter (/= '.') $ dropWhile (\c -> c == '0' || c == '.' ) $ show $ f x) !! 1

freqDist :: (Integral a, Integral b) => [a] -> Map a b
freqDist ints =
  foldr (\i m -> insertWith (+) i 1 m) empty ints







{-
  floor (10 * (x ** 0.5 - firstDig))
  where
    firstDig = fromIntegral (floor (x ** 0.5))
-}

  
--  (x**0.5 - fromIntegral (floor (x**0.5))) * 10 - (x**0.5 - fromIntegral (floor (x**0.5)))

