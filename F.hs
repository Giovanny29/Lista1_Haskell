remDiv:: Int -> [a] -> ([a],[a])
remDiv x vet = let a = take (x-1) vet
                   b = drop (x) vet
                   in (a,b)






main = do
  n <- readLn
  x <- getLine
  print $ remDiv (n :: Int) (words x)