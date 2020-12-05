import Data.List.Split 

main = do 
        input <- readFile "input"
        putStrLn $ show $ mm $ head $ lines input
        putStrLn $ show (True && False)

mm :: String -> ([[Char]], Char, String)
mm a = 
  let w = words a
      limits = splitOn "-" $ head w 
      character = head (w !! 1)
      password = w !! 2 
  in (limits , character, password)


