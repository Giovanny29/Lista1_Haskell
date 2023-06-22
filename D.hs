somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos [] _ = [] -- Caso base: lista vazia retorna lista vazia
somarMultiplos (x : xs) m
  | m == 0 = replicate (length (x : xs)) 0 -- Se M for 0, retorna lista de zeros
  | m > x = 0 : somarMultiplos xs m -- Se M for maior que o elemento, retorna 0
  | otherwise = calcularSomaMultiplos x m : somarMultiplos xs m -- Caso contrário, calcula a soma dos múltiplos

calcularSomaMultiplos :: Int -> Int -> Int
calcularSomaMultiplos x m = sum ([i | i <- [1,2.. x], i `mod` m == 0] )-- Soma os múltiplos de M até X
main :: IO ()
main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result