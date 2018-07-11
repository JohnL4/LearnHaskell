-- import Data.List

main :: IO ()
main = interact evalRPNlines

evalRPNlines :: String -> String
evalRPNlines s = unlines $ map (("=\t" ++) . evalRPN) $ lines s

evalRPN :: String -> String
evalRPN = show . head . foldl evalRPNelement [] . words

evalRPNelement :: (Floating a, Read a) => [a] -> String -> [a]
evalRPNelement (x1:x2:xs) "**" = (x2 ** x1):xs
evalRPNelement (x1:x2:xs) "*" = (x2 * x1):xs
evalRPNelement (x1:x2:xs) "/" = (x2 / x1):xs
evalRPNelement (x1:x2:xs) "+" = (x2 + x1):xs
evalRPNelement (x1:x2:xs) "-" = (x2 - x1):xs
evalRPNelement xs "sum" = [foldl (+) 0 xs]
evalRPNelement xs "prod" = [foldl (*) 1 xs]
evalRPNelement (x1:xs) "dup" = x1:x1:xs
evalRPNelement (x1:xs) "log" = log x1 : xs
evalRPNelement (x1:xs) "exp" = exp x1 : xs
evalRPNelement (x1:xs) "sin" = sin x1 : xs
evalRPNelement (x1:xs) "cos" = cos x1 : xs
evalRPNelement xs "pi" = pi : xs
evalRPNelement xs numberString = read numberString : xs
