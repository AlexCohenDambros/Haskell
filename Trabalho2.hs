-- Alex Cohen Dambrós Lopes
import Text.Printf

soma1 :: Int -> Int
soma1 valor = valor + 1

sempre :: entrada -> Int
sempre entrada = 0

treco :: Double -> Double -> Double -> Double
treco v1 v2 v3 = (v1 + v2) * v3

resto :: Int -> Int -> Int
resto div1 div2 = div1 `mod` div2
  
precoMaior :: Double -> Double -> Double -> Double -> Double
aux :: Double -> Double -> Double
aux x y = if x >= y then x else y 
precoMaior val1 val2 val3 val4 = (aux (aux val1 val2) (aux val3 val4))

impar :: Int -> Int -> Bool
impar num1 num2 = if (num1 * num2) `mod` 2 == 0 then False else True

par :: (Int,Int) -> Int
par x = (fst x) + (snd x)

qt7 :: Double -> Double -> Double -> Double
qt7 x y z = (x ^ 2) + (y / 2) + z 

diagnostico :: Double -> String
diagnostico peso 
  | imc < 17 = "Muito abaixo do peso"
  | imc <= 18.49 = "Abaixo do peso"
  | imc <= 24.99 = "Peso normal"
  | imc <= 29.99 = "SobrePeso"
  | imc <= 34.99 = "Obesidade leve"
  | imc <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade morbida"
  where 
    imc = peso / (1.72 ^ 2)

bissexto :: Int -> Bool
bissexto ano = if ((ano `mod` 4 == 0) && (ano `mod` 100 /= 0)) || (ano `mod` 400 == 0) then True else False

main = do
  {- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada. -}
  printf "\nFuncao soma1: entrada: %d; resultado: %d \n" (2 :: Int) (soma1 2)

  {-2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. -}
  printf "Funcao sempre: entrada: %d; resultado: %d \n" (1234 :: Int) (sempre 1234)

  {- 3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. -}
  printf "Funcao treco: entrada: %f %f %f; resultado: %f \n" (1.2 :: Float) (2.5 :: Float) (3.5 :: Float) (treco 1.2 2.5 3.5)

  {- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros. -}
  printf "Funcao resto: entrada: %d %d; resultado: %d \n" (9 :: Int) (5 :: Int) (resto 9 5)

  {- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários. -}
  printf "Funcao precoMaior: entrada: %f %f %f %f; resultado: %f \n" (10 :: Float) (2 :: Float) (3 :: Float) (4 :: Float) (precoMaior 10 2 3 4)


  {- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar. -}
  printf "Funcao impar: entrada: %d %d; resultado: %s \n" (9 :: Int) (3 :: Int) (show(impar 9 3))

{- Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros. -}
  printf "Funcao par: entrada: %d %d; resultado: %d \n" (2 :: Int) (6 :: Int) (par (2, 6))

  {- 7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 + 𝑦 / 2 + 𝑧. -}
  printf "Funcao qt7: entrada: %f %f %f; resultado: %f \n" (2 :: Float) (8 :: Float) (5 :: Float) (qt7 2 8 5)

  {- 8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br). Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.-}
  printf "Funcao diagnostico: entrada: %d; resultado: %s \n" (70 :: Int) (show(diagnostico 70))

  {- 9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 / 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 / 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.   -}
  printf "Funcao bissexto: entrada: %d; resultado: %s \n" (2000 :: Int) (show(bissexto 2000))