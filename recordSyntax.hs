data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , phoneNumber :: String
                     } deriving (Show)

main :: IO ()
main = do
  putStrLn $ show $ Person "John" "Lusk" 57 "919-951-9632"
  
