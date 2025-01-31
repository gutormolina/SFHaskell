data Temperatura = Frio | Calor
    deriving(Eq, Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving(Eq, Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _     = Frio


data Forma = Circulo Float | Retangulo Float Float
    deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a


-- Arvore == superclasse ; Folha e Nó == subclasses
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 :: Arvore
arv1 =
    Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

arv2 :: Arvore
arv2 = 
    Nodo 10 (Folha 9) (Nodo 14 (Folha 6) (Nodo 1 (Folha 2) (Folha 4)))

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 +
                                somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2 * n)
multDoisArvore (Nodo n a1 a2) = Nodo (2 * n) (multDoisArvore a1)
                                             (multDoisArvore a2)