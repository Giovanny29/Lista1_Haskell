import Data.String

troca :: String -> String
troca [] = []
troca (a:ax) | a == ';' = " "  ++ troca ax
             | otherwise =  (a) : troca ax

vetorializa :: String -> [String]
vetorializa [] = []
vetorializa a = words(troca a)

separaMes ::[String] -> Int ->[String]
separaMes [] i = []
separaMes n 0 = []
separaMes (a:ax) i | i `mod` 4 /= 0 && even i = (a) : separaMes 
                    ax(i+1)
                   |otherwise = separaMes ax (i+1)
                   
separaDiv ::[String] -> Int ->[Double]
separaDiv [] i = []
separaDiv n 0 = []
separaDiv (a:ax) i | i `mod` 4 ==0 =  read a : 
                     separaDiv ax (i+1)
                   |otherwise = separaDiv ax (i+1)

criaDuplas :: [String] -> [Double] -> [(String , Double)]
criaDuplas [] b = []
criaDuplas a [] = []
criaDuplas (a:ax) (b:bx) = (a,b) : criaDuplas ax bx 

juntaTudo:: String -> [(String , Double)]
juntaTudo a =  criaDuplas(separaMes (vetorializa a) 1 )  (separaDiv(vetorializa a ) 1)

soma :: [(String , Double)]-> String->[Double]
soma [] a = []
soma a [] = []
soma (a:ax) b | fst a == b = snd a : soma ax b
              | otherwise = soma ax b

logMes :: String -> String -> Double
logMes a b = foldl (+) 0  (soma (juntaTudo b) a)


main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result



