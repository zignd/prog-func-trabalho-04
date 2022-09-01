-- Nome: Igor Hipólito Vieira

{-
1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr 
devolva o fatorial de n.
-}
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

{-
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de 
números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos 
reais listados.
-}
quadradoReal :: [Float] -> [Float]
quadradoReal lista = map (^2) lista

{-
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de 
palavras e devolve uma lista com o comprimento de cada uma destas palavras.
-}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras lista = map length lista

{-
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior 
número entre 0 e 100000 que seja divisivel por 29.
-}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum $ filter (\n -> (mod n 29) == 0) [0..99999]

{-
5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um 
inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
-}
maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum $ filter (\v -> (mod v n) == 0) [0..99999]

{-
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva 
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De 
tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2 ... + 𝑛^2. 
-}
somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^2) [1..n])

{-
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o 
comprimento (cardinalidade) de uma lista dada. 
-}
comprimento :: [Int] -> Int
comprimento lista = foldl (+) 0 (map (\n -> 1) lista)

{-
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso 
das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada 
uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. 
-}
{-
  Esses exemplos daqui estão diretamente na função main
-}
multiplicar :: (Int, Int) -> Int
multiplicar (a, b) = a * b

concatenar :: (String, String) -> String
concatenar (a, b) = a ++ b

dividir :: Float -> Float -> Float
dividir a b = a / b

resto :: Int -> Int -> Int
resto a b = a `mod` b

main = do
  putStrLn $ "Func. 1: entrada 7; resultado: " ++ show (fatorialn 7)
  putStrLn $ "Func. 2: entrada [1.5, -2.3, 3.5]; resultado: " ++ show (quadradoReal [1.5, -2.3, 3.5])
  putStrLn $ "Func. 3: entrada [\"olá\", \"mundo\"]; resultado: " ++ show (comprimentoPalavras ["olá", "mundo"])
  putStrLn $ "Func. 4: resultado: " ++ show (maiorMultiploDe29)
  putStrLn $ "Func. 5: entrada 29; resultado: " ++ show (maiorMultiploDe 29)
  putStrLn $ "Func. 6: entrada 4; resultado: " ++ show (somaQuadrados 4)
  putStrLn $ "Func. 7: entrada [2, 3, 4]; resultado: " ++ show (comprimento [2, 3, 4])
  putStrLn $ "Func. 8: flip; entrada (>) 10 20; resultado: " ++ show (flip (>) 10 20)
  putStrLn $ "Func. 8: flip; entrada (++) \" Hipolito\" \"Igor\"; resultado: " ++ show (flip (++) " Hipolito" "Igor")
  putStrLn $ "Func. 8: odd; entrada 25; resultado: " ++ show (odd 25)
  putStrLn $ "Func. 8: odd; entrada 26; resultado: " ++ show (odd 26)
  putStrLn $ "Func. 8: max; entrada 11 22; resultado: " ++ show (max 11 22)
  putStrLn $ "Func. 8: max; entrada 33 22; resultado: " ++ show (max 33 22)
  putStrLn $ "Func. 8: min; entrada 11 22; resultado: " ++ show (min 11 22)
  putStrLn $ "Func. 8: min; entrada 33 22; resultado: " ++ show (min 33 22)

  putStrLn $ "Func. 8: curry; entrada multiplicar 2 3; resultado: " ++ show (curry multiplicar 2 3)
  putStrLn $ "Func. 8: curry; entrada multiplicar 2 3; resultado: " ++ show (curry concatenar "Igor" " Hipolito")
  putStrLn $ "Func. 8: uncurry; entrada dividir (30, 6); resultado: " ++ show (uncurry dividir (30, 6))
  putStrLn $ "Func. 8: uncurry; entrada mod (45, 7); resultado: " ++ show (uncurry mod (45, 7))
  
