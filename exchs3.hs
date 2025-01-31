-- Para os próximos exercícios, usea seguinte definição de Árvore:
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 :: Arvore
arv1 =
    Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

-- 1) Implementar a função
-- multArvore:: Int -> Arvore -> Arvore
-- que recebe um inteiro e uma árvore, e multiplica todos os valores 
-- contidos na árvore pelo inteiro

multArvore:: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (x * n)
multArvore x (Nodo n a1 a2) = Nodo (x * n)  (multArvore x a1)
                                            (multArvore x a2)

-- 2) Implemente a função
-- contaFolhas :: Arvore -> Int
-- que recebe uma árvore e conta quantas folhas existem nessa árvore

contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) =    contaFolhas a1 +
                                contaFolhas a2

-- 3) Implemente a função
-- contaNodos :: Arvore -> Int
-- que conta quantos Nodos uma árvore possui

contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + contaNodos a1 +
                                contaNodos a2

-- 4) Implemente a função:
-- quantasVezes :: Int -> Arvore -> Int
-- que recebe um inteiro e uma árvore, e conta quantas vezes
-- esse inteiro aparece na árvore

quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n)
    | n == x    = 1
    | otherwise = 0
quantasVezes x (Nodo n a1 a2)
    | n == x    = 1 +   quantasVezes x a1 +
                        quantasVezes x a2
    | otherwise =       quantasVezes x a1 +
                        quantasVezes x a2