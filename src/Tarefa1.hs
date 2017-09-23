{-|
Module: Main

Description : Módulo Haskell com funções geradoras de um mapa de Bomberman

Copyright: Henrique Manuel Palmeira Pereira <a80261@alunos.uminho.pt>
           Ricardo Filipe Sousa Caçador <a81064@alunos.uminho.pt>

Um módulo contendo definições Haskell para o cálculo de funções que permitam concretizar um mapa do jogo Bomberman sob a forma de lista de strings,
partindo de uma dimensão e de uma semente geradora de números aleatórios.

-}

module Main where

import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import System.Random
import Data.Char

{- |
A função 'grelha' recebe dois números inteiros, isto é, o número da linha do mapa e a sua dimensão,
e devolve a linha @y@ do mapa em forma de string. É de notar que a primeira linha do mapa é a linha 0
e a última é a linha @d-1@.

=Propriedade:

prop> Para a função ter o resultado desejado, tem que respeitar o seguinte: y <= d

==Por exemplo:

>>> grelha 1 9
"#  ???  #"
>>> grelha 8 9
"#########"
-}

grelha :: Int -> Int -> String
grelha y d | y==1 && d==5 = '#':replicate (d-2) ' ' ++ "#"
           | y==3 && d==5 = '#':replicate (d-2) ' ' ++ "#"
           | y==0 || y==(d-1) = replicate d '#'
           | y==1 || y==(d-2) = "#  "++replicate (d-6) '?'++"  #"
           | y==2 || y==(d-3) = "# "++aux1 (d-4) '#'++" #"
           | odd y && y<=d = aux d '?'
           | even y && y<=d = aux1 d '#'

{- |
A função 'aux' é uma função auxiliar, utilizada na 'grelha', que nos dá uma string que representa as linhas em posições ímpares 
(exceto as linhas 1 para qualquer dimensão superior a 5 e linhas 1 e 3 nos mapas de dimensão 5) de um mapa de dimensão d.
Recebe a dimensão do mapa e um Char, que tem que ser ' ' para o propósito desejado.

=Propriedade:

prop> aux 0 x = " "

==Por exemplo:
>>> aux 9 ' '
"#       #"
>>> aux 13 ' '
"#           #"

-}

aux :: Int -> Char -> String
aux 0 x = " "
aux d x = '#':replicate (d-2) x ++ "#" -- x tem q ser ' '

{- |
A função 'aux1' é uma função auxiliar, utilizada na 'grelha', que nos dá uma string que representa as linhas em posições pares 
(exceto as linhas 0, (d-1), 2 e (d-3)) de um mapa de dimensão d.
Recebe a dimensão do mapa e um Char, que tem que ser '#' para o propósito desejado.

==Por exemplo:
>>> aux1 9 '#'
"#?#?#?#?#"
>>> aux1 13 '#'
"#?#?#?#?#?#?#"

-}

aux1 :: Int -> Char -> String
aux1 d x = unwords1 (replicate (d-div d 2) x) -- x tem q ser '#'

{- |
A função 'unwords1' é uma função auxiliar, utilizada na 'aux1', que dada uma string, devolve uma lista dos elementos que compõem a string 
intercalados com '?', sob a forma de string.

=Propriedades:

prop> unwords1 [] = "?"
prop> unwords1 [x] = [x]

==Por exemplo:
>>> unwords1 "####"
"#?#?#?#"
>>> unwords1 '##'
"#?#"

-}

unwords1 :: String -> String
unwords1 [] = "?"
unwords1 [x] = [x]
unwords1 (h:t) = [h]++"?"++unwords1 t

{- |
A função 'mapaSemN' é uma função que, dados dois números inteiros (posição da linha que encabeça o mapa e a dimensão), devolve uma lista de strings
que representam o mapa.
Para a lista devolvida não ser vazia, o primeiro inteiro recebido tem que ser inferior ou igual a (d-1).

==Por exemplo:
>>> mapaSemN 0 9
["#########","#  ???  #","# #?#?# #","#???????#","#?#?#?#?#","#???????#","# #?#?# #","#  ???  #","#########"]
>>> mapaSemN 2 9
["# #?#?# #","#???????#","#?#?#?#?#","#???????#","# #?#?# #","#  ???  #","#########"]

-}

mapaSemN :: Int -> Int -> [String]
mapaSemN x d | x<=(d-1) = grelha x d:mapaSemN (x+1) d -- x tem q ser 0
             | otherwise = []

{- |
A função 'numeros' é uma função que nos dá uma lista de números gerados aleatóriamente, cujo tamanho varia consoante a dimensão do mapa. O seu contéudo
depende da semende inserida na função.

==Por exemplo:
>>> numeros 9 0
[83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]
>>> numeros 7 1
[35,26,65,67,58,27,14,35,60]

-}
{- | A função 'nPontos' é uma função auxiliar, definida dentro da 'numeros', que conta o número de pontos que são necessário de preencher
dentro do mapa com a lista de números aleatórios, dada a dimensão do mapa, ou seja, defino o tamanho da lista devolvida pela 'numeros'. 
Esta função parte da 'nPedras'.

==Por exemplo:
>>> nPontos 9
28
>>> nPontos 7
9
-}
{- | A função 'nPedras' é uma função auxiliar, definida dentro da 'numeros', que conta o número de pedras de cada mapa, dada a sua dimensão.

==Por exemplo:
>>> nPedras 9
41
>>> nPedras 7
28
-}

numeros :: Int -> Int -> [Int]  -- número de randoms q precisa cada mapa dado a dimensao e a semente
numeros d s = take (nPontos d) $ randomRs (0,99) (mkStdGen s)
    where nPontos d = if d>5 then d^2-nPedras d-12  
                               else d^2-nPedras d
          nPedras d = 4*d-4 + (div (d-3) 2)^2 

{- |
A função 'alterarLinha' é uma função que, dada uma linha do mapa e a lista de inteiros gerada pela 'numeros', vai substituir os caracteres '?' pelo
valor associado a cada número da lista de inteiros, ordenadamente, isto é, o primeiro espaço é substituído pelo valor do primeiro número, o segundo 
espaço pelo valor do segundo número e assim sucessivamente, devolvendo a lista alterada.

Sendo y um número dessa lista, os valores associados são os seguintes:
1. y = 0 ou y = 1 - '+' (representa o power-up Bombs)
2. y = 2 ou y = 3 - '!' (representa o power-up Flames)
3. 4 <= y <= 39 - '?' (representa um tijolo)
4. 40 <= y <= 99 - ' ' (representa um espaço vazio no mapa)

=Propriedades:

prop> alterarLinha [] _ = []
prop> alterarLinha l1 [] = l1

==Por exemplo:
>>> alterarLinha "#?????#" [1,3,12,67,31]
"#+!? ?#"
>>> alterarLinha "#?#?#?#" [1,3,12,67,31]
"#+#!#?#"

-}

alterarLinha :: String -> [Int] -> String
alterarLinha [] _ = []
alterarLinha l1 [] = l1
alterarLinha (x:xs) (y:ys) | x=='?' && y>=40 && y<=99 = ' ':alterarLinha xs ys
                           | x=='?' && (y==0 || y==1) = '+':alterarLinha xs ys
                           | x=='?' && (y==2 || y==3) = '!':alterarLinha xs ys
                           | x=='?' && y>=4 && y<=39 = '?':alterarLinha xs ys
                           | otherwise = x:alterarLinha xs (y:ys)

{- |
A função 'linha', dado o mapa (lista de strings) e um número inteiro, dá-nos uma string, ou seja, a linha do mapa que está na posição indicada pelo
número inserido. É de notar que a posição da primeira linha corresponde a 0.

=Propriedades:

prop> linha [] _ = []
prop> linha (x:xs) 0 = x
prop> linha (x:y:xs) 1 = y

==Por exemplo:
>>> linha ["#########","#  ???  #","# #?#?# #","#???????#","#?#?#?#?#","#???????#","# #?#?# #","#  ???  #","#########"] 3
"#???????#"
>>> linha ["#########","#  ???  #","# #?#?# #","#???????#","#?#?#?#?#","#???????#","# #?#?# #","#  ???  #","#########"] 6
"# #?#?# #"

-}

linha :: [String] -> Int -> String -- dada um nº devolve a linha nessa posicao
linha [] _ = []
linha (x:xs) 0 = x
linha (x:y:xs) 1 = y
linha (h:t) k = linha t (k-1)

{- |
A função 'numerotirar' recebe a posição de uma linha e a dimensão do mapa e devolve o número de inteiros gerados aleatóriamente utilizados nessa linha
e nas anteriores.

==Por exemplo:
>>> numerotirar 3 9
12
>>> numerotirar 3 11
17

-}

numerotirar :: Int -> Int -> Int -- numero de numeros utilizados numa linha e nas anteriores
numerotirar k d | k==0 = 0
                | k==0 && d==5 = 0
                | k==1 || k==(d-2) = (d-6) + numerotirar (k-1) d
                | k==2 || k==(d-3) = (d-2-(div d 2)-1) + numerotirar (k-1) d
                | even k = (d-(div d 2)-1) + numerotirar (k-1) d
                | odd k = (d-2) + numerotirar (k-1) d

{- |
A função 'dropar' tem como objetivo dar-nos a lista de inteiros que serão utilizados para preencher uma linha, não permitindo que sejam utilizados 
os números das linhas anteriores. A função devolve-nos essa lista recebendo a dimensão do mapa, a semente da função 'numeros' e a posição da linha.
Na base desta função estão portanto as funções 'numeros' e 'numerotirar'.
Para a lista devolvida não ser vazia, a posição da lista terá que ser menor ou igual a (d-1).

==Por exemplo:
>>> dropar 9 0 3
[87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]
>>> dropar 9 0 4
[32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]


-}

dropar :: Int -> Int -> Int -> [Int] -- lista de int sem os numeros utilizados nas anteriores
dropar d s k | k==0 = numeros d s
             | k<=(d-1) = drop (numerotirar (k-1) d) (numeros d s)
             | otherwise = []

{- |
A função 'posicao' recebe uma linha do mapa e um Char e devolve a posição desse Char na string, ou seja, dá-nos um inteiro que representa
a posição da primeira ocorrência desse caracter. É importante ter em atenção que o primeiro caracter de uma string ocupa a posição 0.

==Por exemplo:
>>> posicao "# #?#?# #" '?'
3
>>> posicao "#?#?#?#?#" '?'
1

-}

posicao :: String -> Char -> Int -- posicao de um k numa string
posicao (x:xs) k | k==x = 0
                 | otherwise = 1 + posicao xs k

{- |
A função 'tirarPrimeiro1' recebe uma linha do mapa e devolve essa mesma linha, mas substituindo a primeira ocorrência do caracter '+' pelo caracter
'?', de maneira a evitar repetir as coordenadas dos power-ups.

=Propriedade:
prop> tirarPrimeiro1 [] = []

==Por exemplo:
>>> tirarPrimeiro1 "#?# #+#+#"
"#?# #?#+#"
>>> tirarPrimeiro1 "#?# #+#!#"
"#?# #?#!#"

-}

tirarPrimeiro1 :: String -> String -- altera a primeira ocorrencia de '+'
tirarPrimeiro1 [] = []
tirarPrimeiro1 (x:xs) | x=='+' = '?':xs
                      | otherwise = x:tirarPrimeiro1 xs

{- |
A função 'tirarPrimeiro2' tem o mesmo objetivo da 'tirarPrimeiro1', mas funciona para a primeira ocorrência do caracter '!'.

=Propriedade:
prop> tirarPrimeiro2 [] = []

==Por exemplo:
>>> tirarPrimeiro2 "#?#+#!#!#"
"#?#+#?#!#"
>>> tirarPrimeiro1 "#?# #!# #"
"#?# #?# #"

-}

tirarPrimeiro2 :: String -> String -- altera a primeira ocorrencia de '!'
tirarPrimeiro2 [] = []
tirarPrimeiro2 (x:xs) | x=='!' = '?':xs
                      | otherwise = x:tirarPrimeiro2 xs

{- |
A função 'darCoordLinha1', que recebe uma linha do mapa (duas vezes) e a sua posição no mapa, tem como objetivo imprimir uma lista de strings
com as coordenadas dos power-ups Bombs ('+') na linha do mapa inserida. 
As strings resultantes terão o formato "+ (posicao na linha) (posicao da linha)".
Tem por base as funções 'posicao' e 'tirarPrimeiro1'.

=Propriedade:
prop> darCoordLinha1 [] _ _ = []

==Por exemplo:
>>> darCoordLinha1 "# #+# #" "# #+# #" 0
["+ 3 0"]
>>> darCoordLinha1 "#+#+#+#" "#+#+#+#" 0
["+ 1 0","+ 3 0","+ 5 0"]

-}

darCoordLinha1 :: String -> String -> Int -> [String] -- l1 tem q ser igual a (x:xs)  || dá as coordenadas do '+' numa linha
darCoordLinha1 [] _ _ = []
darCoordLinha1 (x:xs) l1 k | x=='+' = ["+ "++show (posicao l1 x)++" "++show k]++darCoordLinha1 xs (tirarPrimeiro1 l1) k
                           | otherwise = darCoordLinha1 xs l1 k

{- |
A função 'darCoordLinha2' irá funcionar da mesma maneira que a 'darCoordLinha1', mas para os power-ups Flames ('!').
As strings devolvidas terão o formato "! (posição na linha) (posição da linha)".

=Propriedade:
prop> darCoordLinha2 [] _ _ = []

==Por exemplo:
>>> darCoordLinha2 "# #!# #" "# #!# #" 0
["! 3 0"]
>>> darCoordLinha2 "#!#!#!#" "#!#!#!#" 0
["! 1 0","! 3 0","! 5 0"]

-}

darCoordLinha2 :: String -> String -> Int -> [String] -- l1 tem q ser igual a (x:xs)  || dá as coordenadas do '!' numa linha
darCoordLinha2 [] _ _ = []
darCoordLinha2 (x:xs) l1 k | x=='!' = ["! "++show (posicao l1 x)++" "++show k]++darCoordLinha2 xs (tirarPrimeiro2 l1) k
                           | otherwise = darCoordLinha2 xs l1 k

{- |
A função 'mapa1' recebe a dimensão do mapa e a semente utilizada para gerar a lista de números aleatórios e devolve uma lista de strings que representam
as linhas do mapa. Nestas linhas estão inseridos os power-ups, representados pelos seus caracteres ('+' - Bombs,'!' - Flames), bem como os tijolos ou 
espaços vazios definidos pela lista de números aleatórios.
Na sua base, estão as funções 'alterarLinha', 'linha','mapaSemN' e 'dropar'.

==Por exemplo:
>>> mapa1 9 0
["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"]
>>> mapa1 9 2
["#########","#    ?  #","# # #?# #","#   ??  #","# # #?#+#","#?    + #","# #?# # #","#  ? ?  #","#########"]

-}

mapa1 :: Int -> Int -> [String] -- dá o mapa c os '!' e '+'
mapa1 d s  = aux50 0 d s 
        where aux50 k d s | k<=(d-1) = alterarLinha (linha (mapaSemN 0 d) k) (dropar d s k): aux50 (k+1) d s
                          | otherwise = []

{- |
A função 'darCoordMapa1' tem por base a 'darCoordLinha1', uma vez que, recebendo a dimensão do mapa, a semente recebida e a posição da primeira linha
do mapa (0), devolve uma lista de strings contendo as coordenadas do power-up Bombs ('+') existentes em todo o mapa.
Utiliza ainda as funções 'mapa1' e 'linha'.

==Por exemplo:
>>> darCoordMapa1 9 0 0
["+ 5 2","+ 3 3"]
>>> darCoordMapa1 9 2 0
["+ 7 4","+ 6 5"]

-}

darCoordMapa1 :: Int -> Int -> Int -> [String]  -- k==0  || dá as coordenadas dos '+' num mapa
darCoordMapa1 d s k | k<=(d-1) = darCoordLinha1 (linha (mapa1 d s) k) (linha (mapa1 d s) k) k++darCoordMapa1 d s (k+1)
                    | otherwise = []

{- |
A função 'darCoordMapa2' funciona de maneira idêntica à 'darCoordMapa1', mas devolve a lista das coordenadas dos power-ups Flames ('!').

==Por exemplo:
>>> darCoordMapa2 9 0 0
["! 5 5"]
>>> darCoordMapa2 9 7 0
["! 5 1","! 5 4"]

-}

darCoordMapa2 :: Int -> Int -> Int -> [String]  -- k==0  || dá as coordenadas dos '!' num mapa
darCoordMapa2 d s k | k<=(d-1) = darCoordLinha2 (linha (mapa1 d s) k) (linha (mapa1 d s) k) k++darCoordMapa2 d s (k+1)
                    | otherwise = []

{- |
A função 'substituiLinha', recebendo uma linha do mapa, devolve essa mesma linha alterando os caracteres dos power-ups ('+' e '!') para o caracter '?'.

=Propriedade:
prop> substituiLinha [] = []

==Por exemplo:
>>> substituiLinha "#! + !#"
"#? ? ?#"
>>> substituiLinha "#!+!+!#"
"#?????#"

-}

substituiLinha :: String -> String -- retira os '!' e '+' duma linha
substituiLinha [] = []
substituiLinha (x:xs) | x=='+' = '?':substituiLinha xs
                      | x=='!' = '?':substituiLinha xs
                      | otherwise = x:substituiLinha xs

{- |
A função 'substituiMapa' irá realizar o mesmo processo que a 'substituiLinha', mas desta vez para todo o mapa, ou seja, para toda a lista de strings que
o compõem.

=Propriedade:
prop> substituiMapa [] = []

==Por exemplo:
>>> substituiMapa ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"]
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
>>> substituiMapa ["###########","#  ?  ??  #","# #?#?# # #","#? +  ? ?+#","# # # #?# #","#   !   ??#","# #?# # #?#","#  ? ?? ??#","# #?# # # #","#    !    #","###########"]
["###########","#  ?  ??  #","# #?#?# # #","#? ?  ? ??#","# # # #?# #","#   ?   ??#","# #?# # #?#","#  ? ?? ??#","# #?# # # #","#    ?    #","###########"]

-}

substituiMapa :: [String] -> [String] -- retira os '!' e '+' do mapa
substituiMapa [] = []
substituiMapa (x:xs) = substituiLinha x: substituiMapa xs

{- |
A função 'mapa' é a função principal, isto é, com todas as funções definidas anteriormente, contrói um mapa (lista de strings) a partir da 
sua dimensão e da semente utilizada para gerar a lista de números aleatórios referida anteriormente.
Funciona concatenando as listas modificas do mapa (através de 'substituiMapa' e 'mapa1') e as listas das coordenadas dos power-ups (derivadas das 
funções 'darCoordMapa1' e 'darCoordMapa2').

==Por exemplo:
>>> mapa 9 0
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
>>> mapa 11 8
["###########","#  ?  ??  #","# #?#?# # #","#? ?  ? ??#","# # # #?# #","#   ?   ??#","# #?# # #?#","#  ? ?? ??#","# #?# # # #","#    ?    #","###########","+ 3 3","+ 9 3","! 4 5","! 5 9"]

-}

mapa :: Int -> Int -> [String]
mapa d s = substituiMapa (mapa1 d s)++darCoordMapa1 d s 0++darCoordMapa2 d s 0

{- |
O programa 'main' foi-nos facultado para a realização deste projeto e pode ser usado para testar a função 'mapa', por nós definida, ou seja,
após compilado, o programa aceita como parâmetros a dimensão e a semente e depois invoca
a função ​ mapa ​ imprimindo o resultado no stdout.

-}

main :: IO ()
main = do a <- getArgs

          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"
