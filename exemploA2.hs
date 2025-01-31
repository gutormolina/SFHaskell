-- Listas em Haskell

-- Listas são descritas usando colchetes

-- ['a', 'b', 'c']
-- [True, False, True]
-- [1, 2, 3, 4]

-- permite inserir um elemento na frente de uma lista
-- 3 : [4, 5]

-- 1 : [] == [1]
-- (1, 2) : (3, 4) : [] == [(1, 2), (3, 4)]

-- [1, 2, 3, 4] é a mesma coisa que
-- 1 : 2 : 3 : 4 : []

-- cons assume o tipo

-- ++ para concatenação de listas
-- [1, 2, 3] ++ [4, 5, 6]
-- [1, 2, 3, 4, 5, 6]

-- somaLista
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

-- multiplicaLista
multDois :: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs

-- Insertion Sort

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x: ins a xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)