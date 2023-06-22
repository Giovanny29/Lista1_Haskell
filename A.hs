acumulador :: String -> Int -> Int
acumulador [] a = a
acumulador(x:xs) a |isDigit x = acumulador xs (a+ (ord x - ord '0'))  
                   | otherwise = acumulador xs a 

sumNumbers :: String -> Int
sumNumbers [] = 0
sumNumbers a = acumulador a 0



main = do
  a <- getLine
  let result = sumNumbers a
  print result