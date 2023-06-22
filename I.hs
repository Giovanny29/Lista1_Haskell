trans :: Char ->[(Char,Char)]->Char
trans ' ' [] = ' '
trans b [] =  b
trans b (a:ax) | b == fst (a) = snd a
              | otherwise  = trans b ax


decEnigma  ::String ->[(Char,Char)] ->String
decEnigma  a n = [trans x n | x <- a ]


main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result