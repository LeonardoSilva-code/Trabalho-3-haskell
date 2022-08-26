--Leonardo Silva de Abreu

{-
1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência de  Fibonacci,  utilizando Haskell.  
-}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

{-
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell. 
-}
euclidesMDC :: Int -> Int -> Int
euclidesMDC a 0 = abs a
euclidesMDC a b = euclidesMDC b (mod a b)

{-
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função deverá  devolver  10.  Utilizando  Haskell  e recursividade. 
-}
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos n = mod n 10 + somaDigitos (div n 10)

{-
4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
-}
somaMultiplos :: [Int] -> Int
somaMultiplos [] = 0
somaMultiplos (x:xs)
                | mod x 3 == 0 = x + somaMultiplos xs
                | mod x 5 == 0 = x + somaMultiplos xs
                | otherwise = somaMultiplos xs

{-
5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
-}
elevaQuadrado :: Int -> Int
elevaQuadrado n = n * n

somaDiff :: [Int] -> Int
somaDiff xs
        | (elevaQuadrado (sum xs)) > sum (map elevaQuadrado xs) = (elevaQuadrado (sum xs)) -( sum (map elevaQuadrado xs))
        | otherwise = (sum (map elevaQuadrado xs)) - (elevaQuadrado (sum xs))




{-
6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. 
-}

{-
7.Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado
-}

lucas :: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n = (lucas(n-1)) + (lucas (n-2))

{-
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1]. 
-}
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

{-
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
-}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x y = x + somaRecursiva x (y-1) 

{-
10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
-}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs



main = do
putStrLn "TRABALHO 3. listas e recursão"

--Fibonacci teste
let fibonacciInput = 7
let fibonacciResultado = fibonacci fibonacciInput
let fibonacciTesteStr = "Func. fibonacci: entrada:" ++ show fibonacciInput ++ "; resultado:" ++ show fibonacciResultado
putStrLn fibonacciTesteStr


--somaDigitos teste
let somaDigitosInput = 1234
let somaDigitosResultado = somaDigitos somaDigitosInput
let somaDigitosTesteStr = "Func. somaDigitos: entrada:" ++ show somaDigitosInput ++ "; resultado:" ++ show somaDigitosResultado
putStrLn somaDigitosTesteStr

--somaDigitos teste
let aoContrarioInput = [1,2,3]
let aoContrarioResultado = aoContrario aoContrarioInput
let aoContrarioTesteStr = "Func. aoContrario: entrada:" ++ show aoContrarioInput ++ "; resultado:" ++ show aoContrarioResultado
putStrLn aoContrarioTesteStr

--somaRecursiva teste
let somaRecursivaInput1 = 6
let somaRecursivaInput2 = 3
let somaRecursivaResultado = somaRecursiva somaRecursivaInput1 somaRecursivaInput2
let somaRecursivaTesteStr = "Func. somaRecursiva: entrada:" ++ show somaRecursivaInput1 ++ ", " ++ show somaRecursivaInput2 ++ "; resultado:" ++ show somaRecursivaResultado
putStrLn somaRecursivaTesteStr

--comprimento teste
let comprimentoInput = [1,2,3,4,8,6,9]
let comprimentoResultado = comprimento comprimentoInput
let comprimentoTesteStr = "Func. comprimento: entrada:" ++ show comprimentoInput ++ "; resultado:" ++ show comprimentoResultado
putStrLn comprimentoTesteStr

--euclidesMDC teste
let euclidesMDCInput1 = 44
let euclidesMDCInput2 = 22
let euclidesMDCResultado = euclidesMDC euclidesMDCInput1 euclidesMDCInput2
let euclidesMDCTesteStr = "Func. euclidesMDC: entrada:" ++ show euclidesMDCInput1 ++ ", " ++ show euclidesMDCInput2 ++ "; resultado:" ++ show euclidesMDCResultado
putStrLn euclidesMDCTesteStr

--somaMultiplos teste
let somaMultiplosInput = [0..999]
let somaMultiplosResultado = somaMultiplos somaMultiplosInput
let somaMultiplosTesteStr = "Func. somaMultiplos: entrada:[0..999]; resultado:" ++ show somaMultiplosResultado
putStrLn somaMultiplosTesteStr

--lucas teste
let lucasInput = 6
let lucasResultado = lucas lucasInput
let lucasTesteStr = "Func. lucas: entrada:[0..999]; resultado:" ++ show lucasResultado
putStrLn lucasTesteStr
