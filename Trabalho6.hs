-- Alex Cohen Dambrós Lopes
import Text.Printf
import Data.Char

{- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. -}
divisoresden :: Int -> [Int]
divisoresden valor = [n | n <- [1..valor], (valor `mod` n) == 0]

{-2. Usando  List Comprehension  escreva  uma  função, chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: String -> Char -> Int
contaCaractere string character = length [n | n <- string, (toLower n) == (toLower character)]

{-3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [n*2 | n <- lista, n>=0]

{-4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras valor = [(a,b,c) | a <-[1..valor], b <-[1..valor], c <- [1..valor], ((a^2) + (b^2)) == (c^2), c > a, c > b, b > a]


{-5. Números  perfeitos  são  aqueles  cuja  soma  dos seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos valor = [n | n <- [1..valor], (sum (init (divisoresden n))) == n]

{-6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = (sum [n*m | (n, m) <- zip lista1 lista2])

{-7. Usando  List Comprehension  escreva  uma  função, chamada  primeirosPrimos,  que  devolva uma lista contendo, os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos valor = takeWhile (\x -> length [n | n <- [2..(x-1)], length((divisoresden n)) == 2] < valor ) [n | n <- [2..], length((divisoresden n)) == 2]

{-8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par ordenados  contendo  uma  potência  de  2  e  uma potência de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados valor = [(2^n, 3^n) | n <- [1..valor]]

main = do
  printf "\nFuncao divisoresden: entrada: %d; resultado: %s \n" (10 :: Int) (show(divisoresden 10))
  printf "\nFuncao contaCaractere: entrada: %s, %s; resultado: %s \n" ("Palavra" :: String) ("a" :: String) (show(contaCaractere "Palavra" 'a'))
  printf "\nFuncao dobroNaoNegativo: entrada: %s; resultado: %s \n" ("[1..10]" :: String) (show(dobroNaoNegativo [1..10]))
  printf "\nFuncao pitagoras: entrada: %d; resultado: %s \n" (10 :: Int) (show(pitagoras 10))
  printf "\nFuncao numerosPerfeitos: entrada: %d; resultado: %s \n" (500 :: Int) (show(numerosPerfeitos 500))
  printf "\nFuncao produtoEscalar: entrada: %s, %s; resultado: %s \n" ("[1..3]" :: String) ("[1..3]" :: String) (show(produtoEscalar [1..3] [1..3]))
  printf "\nFuncao primeirosPrimos: entrada: %d; resultado: %s \n" (10 :: Int) (show(primeirosPrimos 10))
  printf "\nFuncao paresOrdenados: entrada: %d; resultado: %s \n" (10 :: Int) (show(paresOrdenados 10))
