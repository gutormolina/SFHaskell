-- 1) Implementar a função
-- multLista :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e multiplica todos os 
-- elementos dessa lista pelo inteiro.

multLista :: Int -> [Int] -> [Int]
multLista a [] = []
multLista a (x:xs) = a * x : multLista a xs

-- 2) Implemente a função
-- elemento :: Int -> [Int] -> Bool
-- que recebe um inteiro e uma lista, e devolve um booleano
-- dizendo se o inteiro se encontra na lista

elemento :: Int -> [Int] -> Bool
elemento a [] = False
elemento a (x:xs)
    | a == x = True
    | otherwise = elemento a xs

-- 3) Implemente a função
-- conta :: Int -> [Int] -> Int
-- que recebe um inteiro e uma lista, e diz quantas vezes o 
-- inteiro ocorre dentro da lista

conta :: Int -> [Int] -> Int
conta a [] = 0
conta a (x:xs)
    | a == x = 1 + conta a xs
    | otherwise = conta a xs

-- 4) Implemente a função:
-- contaMaiores :: Int -> [Int]-> Int
-- que recebe um inteiro e uma lista e conta quantos elementos 
-- da lista são maiores que o inteiro passado como argumento

contaMaiores :: Int -> [Int] -> Int
contaMaiores a [] = 0
contaMaiores a (x:xs)
    | a < x = 1 + contaMaiores a xs
    | otherwise = contaMaiores a xs

-- 5) Implemente a função
-- maiores :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e devolve uma lista contendo 
-- somente os valores que estavam na lista inicial e que são 
-- maiores do que o inteiro passado como argumento

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (x:xs)
    | a < x = x : maiores a xs
    | otherwise = maiores a xs

-- 6) Implementar a função
-- geraLista :: Int -> Int -> [Int]
-- que recebe um inteiro m e um inteiro n e devolve uma lista 
-- contendo m vezes n
-- > geraLista 3 7
-- [7, 7, 7]

geraLista :: Int -> Int -> [Int]
geraLista m n
    | m == 0 = []
    | otherwise = n : geraLista (m - 1) n

-- 7) Implementar a função
-- addFim :: Int -> [Int] -> [Int]
-- que recebe um inteiro, uma lista e adiciona o elemento no fim
-- da lista (sem usar o ++):
-- > addFim 10 [1,2]
-- [1,2,10]

addFim :: Int -> [Int] -> [Int]
addFim a [] = [a]
addFim a 