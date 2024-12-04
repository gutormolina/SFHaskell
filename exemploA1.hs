--      exemplo.hs
--      comentario
--
idade :: Int        -- Um valor inteiro constante
idade = 17

testeIdade :: Bool  -- Usa a definição de idade
testeIdade = idade >= 18

quadrado :: Int -> Int  -- função que eleva num
quadrado x = x * x      -- quadrado

mini :: Int -> Int -> Int   -- funcao que mostra
mini a b                    -- o menor entre
    | a <= b    = a         -- dois valores
    | otherwise = b


-- Função maiorDeIdade

maiorDeIdade :: Int -> Bool
maiorDeIdade x = x >= 18


-- Função tresIguais

tresIguais :: Int -> Int -> Int -> Bool -- nem precisaria dos parenteses
tresIguais a b c = (a == b) && (a == c) -- por causa da ordem de precedência