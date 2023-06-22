import Data.Char
con :: [Int] -> Int
con [] = 0
con (x:xs) = (x * (10  ^length xs) ) + con xs

nov :: Int -> String
nov ax = show ax

rev :: String -> [Int]
rev [] = []
rev (x:ax) = (ord x - 48) : rev(ax)


somarListas :: [Int] -> [Int] -> [Int]
somarListas [] [] = []
somarListas x y  = let ax = con(x) 
                       by = con(y)
                    in  rev(nov(ax + by))


main :: IO ()
main = do
input1 <- getLine
input2 <- getLine
let result = somarListas (read input1 :: [Int])  (read input2 :: [Int])
print result