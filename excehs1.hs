--      Exercícios Extra de Programação Haskell 1

-- 1) Escreva a função osQuatroSaoIguais que possui tipo
-- Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos são iguais

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (a == c) && (a == d)

-- 2) Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que recebe 3 
-- valores e diz quantos desses valores são iguais

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
    | (a == b) && (a == c) = 3
    | (a == b) || (a == c) || (b == c) = 2
    | otherwise = 0

-- 3) Defina a função todosDiferentes :: Int -> Int -> Int -> Bool que retorna
-- True se todos os seus argumentos são diferentes. Obs: m /= n retorna True se
-- m e n são diferentes

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (a /= c) && (b /= c)

-- 4) O que está errado com a seguinte definição de todosDiferentes:
-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )

-- R: nesta definição, n pode ser igual a p e mesmo assim a função validará
-- como false

-- 5) Escreva uma definição de quantosSaoIguais que use a função todosDiferentes
-- e a função todosIguais

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a == b) && (a == c)

quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 a b c
    | todosIguais a b c = 3
    | todosDiferentes a b c = 0
    | otherwise = 2

-- 6) Defina a função elevadoDois :: Int -> Int que recebe um argumento n e
-- devolve como resposta n²

elevadoDois :: Int -> Int
elevadoDois n = n * n

-- 7) Defina a função elevadoQuatro :: Int -> Int que recebe um argumento n e
-- devolve como resposta n⁴. Use elevadoDois para definir elevadoQuatro

elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois n * elevadoDois n

-- Supondo que exista uma função vendas:
-- vendas :: Int -> Int
-- que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
-- na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente
-- uma função chamada vendaTotal, que recebe um argumento n e calcula
-- todas as vendas da semana 0 at´e a semana n. Observe que essa função deve
-- ser recursiva. Exemplo de calculo: As vendas da semana 0 até a semana 2,
-- podem ser calculados usando a seguinte formula:
-- vendas 0 + vendas 1 + vendas 2

vendas :: Int -> Int
vendas s = 1            -- valor controle

vendaTotal :: Int -> Int
vendaTotal (-1) = 0
vendaTotal n = vendas n + vendaTotal (n - 1)