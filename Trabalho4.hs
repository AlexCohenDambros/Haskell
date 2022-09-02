-- Alex Cohen Dambrós Lopes
import Text.Printf

{-1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função foldr devolva o fatorial de n. -}
fatorialn :: Int -> Int
fatorialn valor = foldr (*) 1 [1 .. valor]

{-2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos inteiros listados. -}
quadradoReal :: [Double] -> [Double]
quadradoReal lista = map (^ 2) lista

{-3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. -}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras lista = map (length) lista

{-4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29. -}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = last (filter (\x -> (x `mod` 29) == 0) [0 .. 100000])

{-5. Usando  a  função  filter  escreva  uma  função,  chamada maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. -}
maiorMultiploDe :: Int -> Int
maiorMultiploDe valor = last (filter (\x -> (x `mod` valor) == 0) [0 .. 100000])

{-6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 +2^2 +3^2 +4^2...+𝑛^2.-}
somaQuadrados :: Int -> Int
somaQuadrados x = foldr (\x y -> x ^ 2 + y) 0 [0 .. x]

{-7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.  -}
comprimento :: [lista] -> Int
comprimento lista = foldl (\x y -> x + 1) 0 lista

{-8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. -}

preludeFlip1 :: Double -> Double -> Double 
preludeFlip1 x y = flip (/) x y

preludeFlip2 :: Double -> Double -> Bool 
preludeFlip2 x y = flip (>) x y

preludeMax :: Int -> Int -> Int
preludeMax x y = max x y

preludeMin :: Int -> Int -> Int
preludeMin x y = min x y

preludeCurry :: Int -> Int -> Int
preludeCurry x y = curry fst 2 3

preludeCurry2 :: Int -> Int -> Int
preludeCurry2 = curry (\ (x,y) -> 2*x+y)

preludeUncurry :: Int -> Int -> Int
preludeUncurry x y = uncurry mod (5,4)

preludeUncurry2 :: (Int, Int) -> Int
preludeUncurry2 = uncurry (*) 

main = do
  printf "\nFuncao fatorialn: entrada: %d; resultado: %d \n" (6 :: Int) (fatorialn 6)
  printf "\nFuncao quadradoReal: entrada: %s; resultado: %s \n" ("[1, 2, 3, 4, 5]" :: String) (show (quadradoReal [-2 .. 5]))
  printf "\nFuncao comprimentoPalavras: entrada: %s; resultado: %s \n" ("[teste, oi]" :: String) (show (comprimentoPalavras ["teste", "oi"]))
  printf "\nFuncao maiorMultiploDe29: entrada: ; resultado: %d \n" (maiorMultiploDe29)
  printf "\nFuncao maiorMultiploDe: entrada: %d; resultado: %d \n" (29 :: Int) (maiorMultiploDe 29)
  printf "\nFuncao somaQuadrados: entrada: %d; resultado: %d \n" (3 :: Int) (somaQuadrados 3)
  printf "\nFuncao comprimento: entrada: %s; resultado: %d \n" ("[1,2,3]" :: String) (comprimento [2, 2, 3])

  printf "\nFuncao preludeFlip1: entrada: %f %f; resultado: %f \n" (1 :: Double) (2 :: Double) (preludeFlip1 1 2)
  printf "\nFuncao preludeFlip2: entrada: %f %f; resultado: %s \n" (3 :: Double) (5 :: Double) (show(preludeFlip2 3 5))
  printf "\nFuncao preludeMax1: entrada: %d %d; resultado: %d \n" (10 :: Int) (50 :: Int) (preludeMax 10 50)
  printf "\nFuncao preludeMax2: entrada: %d %d; resultado: %d \n" (10 :: Int) (500 :: Int) (preludeMax 10 500)
  printf "\nFuncao preludeMin1: entrada: %d %d; resultado: %d \n" ((-10) :: Int) (2 :: Int) (preludeMin (-10) 2)
  printf "\nFuncao preludeMin2: entrada: %d %d; resultado: %d \n" (0 :: Int) (4 :: Int) (preludeMin 0 4)
  printf "\nFuncao preludeCurry: entrada: %d %d; resultado: %d \n" (2 :: Int) (3 :: Int) (preludeCurry 2 3)
  printf "\nFuncao preludeCurry2: entrada: %d %d; resultado: %d \n" (2 :: Int) (3 :: Int) (preludeCurry2 2 3)
  printf "\nFuncao preludeUncurry: entrada: %d %d; resultado: %d \n" (5 :: Int) (4 :: Int) (preludeUncurry 5 4)
  printf "\nFuncao preludeUncurry2: entrada: %d %d; resultado: %d \n" (3 :: Int) (2 :: Int) (preludeUncurry2 (3,2))