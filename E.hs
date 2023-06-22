import Data.String

troca :: String -> String
troca [] = []
troca (a:ax) | a == ';' = " "  ++ troca ax
             | otherwise =  (a) : troca ax

vetorializa :: String -> [String]
vetorializa [] = []
vetorializa a = words(troca a)

junta :: [String] -> Int -> [Double]
junta [] i = []
junta (a:ax) 0 = []
junta (a:ax) i |  i `mod` 4 == 0 = read a : junta ax (i+1)
               | otherwise = junta ax (i + 1) 

limites :: [Double] -> (Double,Double)
limites [] = (0 ,0) 
limites n =  (minimum n , maximum n)


minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0,0)
minMaxCartao v =  limites(junta (vetorializa v) 1)


main = do
    a <- getLine
    let result = minMaxCartao a
    print result
