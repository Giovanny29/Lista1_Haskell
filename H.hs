main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result
mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (x:ax) = (0) : mul2 [] ax 
mul2 (x:ax) [] = (0) : mul2 (ax) []
mul2  (a:ax) (b:bx) = (a * b) : mul2 ax bx 
 