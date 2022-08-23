-- Alex Cohen Dambrós Lopes
import Text.Printf

{- 1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell. -}
fibonacci :: Int -> [Int]
fibonacci x = if x <= 1 then [x] else (fibonacci (x-1)) ++ [(last(fibonacci (x-1))) + (last(fibonacci(x-2)))]

{-2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. -}
mdc :: Int -> Int -> Int
mdc a b = if b == 0 then abs a else mdc b (a`mod`b)

{-3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade. -}
somaD :: Int -> Int
somaD valor = if valor == 0 then 0 else (valor`mod`10) + somaD (div valor 10)

{-4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5. -}
multiplos :: Int -> Int
multiplos valor 
  | valor == 0 = 0
  | (valor `mod` 3 == 0) = valor + multiplos (valor-1)
  | (valor `mod` 5 == 0) = valor + multiplos (valor-1)
  | otherwise = multiplos (valor-1)

{-5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. -}
somaQuadrados :: [Int] -> Int
somaQuadrados x = if x == [] then 0 else (head x)^2 + somaQuadrados(tail x)

quadradosSoma :: [Int] -> Int
quadradosSoma x = if x == [] then 0 else sum(x)^2

diferenca :: [Int] -> Int
diferenca x = somaQuadrados(x) - quadradosSoma(x)

{-6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. -}
removeElements :: Int -> [Int] -> [Int]
removeElements valor [] = []
removeElements valor lista = if (head lista) `mod` valor == 0 then removeElements valor (tail lista) else [head lista] ++ removeElements valor (tail lista) 

crivoEuler :: [Int] -> [Int]
crivoEuler [] = []
crivoEuler lista = [head lista] ++ crivoEuler(removeElements (head lista) (tail lista))

{-7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. -}
lucas :: Int -> [Int]
lucas x
  | x == 0 = [2]
  | x == 1 = [2] ++ [1]
  | otherwise = (lucas (x-1)) ++ [((last(lucas (x-1))) + (last(lucas (x-2))))]
  
{-8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].-}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:xb) = aoContrario (xb) ++ [x]

{-9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.  -}
somaRecursiva :: Int -> Int -> Int
somaRecursiva valor1 valor2 
  | valor2 == 0 = 0
  | valor2 > 0 = valor1 + somaRecursiva valor1 (valor2-1) 

{-10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista. -}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs


main = do
  printf "\nFuncao fibonacci: entrada: %d; resultado: %s \n" (7 :: Int) (show(fibonacci 7))
  printf "\nFuncao MDC: entrada: %d %d; resultado: %d \n" (10 :: Int) (5 :: Int) (mdc 10 5)
  printf "\nFuncao somaD: entrada: %d; resultado: %d \n" (1234 :: Int) (somaD 1234)
  printf "\nFuncao multiplos: entrada: %d; resultado: %d \n" (9999 :: Int) (multiplos 9999)
  printf "\nFuncao diferenca: entrada: %s; resultado: %d \n" ("[1,2,3]" :: String) (diferenca [1,2,3])
  printf "\nFuncao crivoEuler: entrada: %s; resultado: %s \n" ("[2..30]" :: String) (show(crivoEuler [2..30]))
  printf "\nFuncao Lucas: entrada: %d; resultado: %s \n" (7 :: Int) (show(lucas 7))
  printf "\nFuncao aoContrario: entrada: %s; resultado: %s \n" ("[1,2,3,4]" :: String) (show(aoContrario [1,2,3,4]))
  printf "\nFuncao somaRecursiva: entrada: %d %d; resultado: %d \n" (3 :: Int) (3 :: Int) (somaRecursiva 3 3)
  printf "\nFuncao comprimento: entrada: %s; resultado: %d \n" ("[1,2,3,4]" :: String) (comprimento [1,2,3,4])