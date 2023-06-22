type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa xs = supFucntion xs 0
  where
    supFucntion [] b = b --parada
    supFucntion (x:xs) b --recursividade
      | fst x == "Multiplica" = supFucntion xs (b * snd x)
      | fst x == "Soma" = supFucntion xs (b + snd x) --acumuladores salvos
      | fst x == "Subtrai" = supFucntion xs (b - snd x)
      | fst x == "Divide" && snd x == 0 = -666
      | fst x == "Divide" = supFucntion xs (b `div` snd x) --divisao inteira se tiver erro é aqui 
      | otherwise = supFucntion xs b --recursivo

      
main = do
    a <- getLine
    let result = executa (read a)
    print result