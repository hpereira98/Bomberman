{-|
Module: Main

Description : Módulo Haskell com funções geradoras de um mapa e de suas informações através de um comando dado a um jogador

Copyright: Henrique Manuel Palmeira Pereira <a80261@alunos.uminho.pt>
           Ricardo Filipe Sousa Caçador <a81064@alunos.uminho.pt>

Um módulo contendo definições Haskell para o cálculo de funções que permitam concretizar  um certo comando (Up, Down, Left, Right ou Bomb) num mapa 
do jogo Bomberman sob a forma de lista de strings,
partindo de um mapa (lista de String), um jogador e um comando.

-}
module Main where

import Data.Char
import System.Environment
import Data.List


{- |
A função 'tirarCoord' recebe uma lista de Strings ( neste caso em específico, o mapa do Bomberman) e o um inteiro ( o número do jogador)
e devolve a coordenada do jogador pedido.

=Propriedade:

prop> tirarCoord [] _ = []

==Por exemplo:
>>> tirarCoord ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 2 1", "1 7 7"] 0
"0 2 1"

>>> tirarCoord ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 2 1", "1 7 7"] 1
"1 7 7"

-}
tirarCoord :: [String] -> Int -> String 
tirarCoord [] _ = []
tirarCoord ((a:b):t) j | [a]==show j = (a:b)
                       | otherwise = tirarCoord t j
{- |
A função 'verificaChar' recebe uma String e dá um Bool consoante encontrar ou não os Char @+@ ou @!@.

=Propriedade:

prop> verificaChar [] = False


==Por exemplo:
>>> verificaChar "0 2 2 +"
True
>>> verificaChar "0 2 2"
False

-}
               
verificaChar :: String -> Bool
verificaChar [] = False
verificaChar (h:t) | h== '+' || h== '!' = True
                   | otherwise = verificaChar t

{-|
A função 'cChar' recebe uma string e conta o número de powerups (@+@ e @!@) nessa string.

=Propriedade:

prop> cChar [] = 0

==Por exemplo:
>>> cChar "0 2 2"
0
>>> cChar "0 2 2 +++!"
4

-}
cChar :: String -> Int
cChar [] = 0
cChar (h:t) | h== '!' || h== '+' = 1 + cChar t
            | otherwise = cChar t

{-|
A função 'getCoord' recebe uma string e devolve um tuplo com as coordenadas dessa string.

==Por exemplo:
>>> getCoord "0 2 2 ++!!"
["0", "2", "2"]

>>> getCoord "1 10 10 ++!!"
["1","10","10"]

-}

getCoord :: String -> [String]
getCoord []= []
getCoord (h:t) | (h <='9' && h>='0') && (t == []) = [show (digitToInt h)]
               | (h <='9' && h>='0') && (head t <='9' && head t>='0') = (show (digitToInt h) ++ show (digitToInt (head t) )) : getCoord (drop 1 t)
               | (h <='9' && h>='0') && (head t == ' ' || head t == '+' || head t == '!') = (show (digitToInt h)) : getCoord (drop 1 t)
               | (h == ' ' || h == '+' || h == '!') = getCoord t
               | otherwise = getCoord t 
{-|
A função 'tuplo' recebe uma lista de String e devolve um tuplo com coordenadas.

==Por exemplo:
>>> tuplo ["0",10"."10"]
(10,10)
-}

tuplo:: [String] -> (Int,Int)
tuplo [x,y,z] = (read y, read z)

{-|
A função 'tuploPowers' recebe uma lista de inteiros com @2 inteiros@ e dá um tuplo. Tem como objetivo transformar esses números em @coordenadas@.

==Por exemplo:
>>> tuploPowers [2,2]
(2,2)

-}

tuploPowers :: [Int] -> (Int,Int)
tuploPowers [x,y] = (x,y)

{-|
A função 'cMais' recebe uma string e conta o número de power-ups @bombas@ nessa string.

prop> cMais [] = 0

==Por exemplo:
>>> cMais "0 1 1 +++!"
3

>>> cMais "0 1 1"
0

-}
cMais :: String -> Int
cMais [] = 0
cMais (h:t) | h == '+' = 1 + cMais t
            | otherwise = cMais t

{-|
A função 'cFlames' recebe uma string e conta o número de power-ups @flames@ nessa string.

=Propriedade:

prop> cFlames [] = 0

==Por exemplo:
>>> cFlames "0 1 1 +++!"
1

>>> cFlames "0 1 1"
0

-}
cFlames :: String -> Int
cFlames [] = 0
cFlames (h:t) | h == '!' = 1 + cFlames t
            | otherwise = cFlames t

{-|
A função 'tuploString' recebe um inteiro, o jogador, um par de coordenadas e transforma tudo na respetiva string.

==Por exemplo:
>>> tuploString 0 (1,1)
"0 1 1"

-}

tuploString :: Int -> (Int,Int) -> String
tuploString j (x,y) = show j ++ " " ++  show x ++ " " ++ show y 

{-|
A função 'tuploStringMaisRight' recebe um inteiro, o jogador, um par de coordenadas e transforma tudo na respetiva string adicionando @+1@ ao @x@.

==Por exemplo:
>>>tuploStringMaisRight 0 (1,1)
"0 2 1"

-}

tuploStringMaisRight :: Int -> (Int,Int) -> String
tuploStringMaisRight j (x,y) =  show j ++ " " ++ show (x+1) ++ " " ++ show y


{-|
A função 'andarR' recebe uma String, um Int (o jogador), um Char (comando do movimento) e dá a nova String depois de efetuar o movimento.

=Propriedade:

prop> andarR [] j m = []

==Por exemplo:
>>> andarR "0 2 2" 0 'R' 
"0 3 2"
-} 

andarR :: String -> Int -> Char -> String
andarR [] j m = []
andarR x j m | m == 'R' = tuploStringMaisRight j (tuplo (getCoord x ))
             | otherwise = []
{-|
A função 'tuploStringMaisLeft' recebe um inteiro, o jogador, um par de coordenadas e transforma tudo na respetiva string subtraindo @-1@ ao @x@ caso
o x > 1.)

==Por exemplo:
>>> tuploStringMaisLeft 0 (1,1)
"0 1 1"
>>> tuploStringMaisLeft 0 (2,2)
"0 1 2" 
-}
tuploStringMaisLeft :: Int -> (Int,Int) -> String
tuploStringMaisLeft j (x,y) | x >1 = show j ++ " " ++ show (x-1) ++ " " ++ show y
                            | otherwise = show j ++ " " ++ show x ++ " " ++ show y

{-|
A função 'andarL' recebe uma String, um Int (o jogador), um Char (comando do movimento) e dá a nova String depois de efetuar o movimento.

=Propriedade:

prop> andarL [] j m = []

==Por exemplo:
>>> andarL "0 2 2" 0 'L' 
"0 1 2"
-} 

andarL :: String -> Int -> Char -> String
andarL [] j m = []
andarL x j m | m == 'L' = tuploStringMaisLeft j (tuplo (getCoord x ))
             | otherwise = []

{-|
A função 'tuploStringMaisDown' recebe um inteiro, o jogador, um par de coordenadas e transforma tudo na respetiva string adicionando @+1@ ao @y@.

==Por exemplo:

>>> tuploStringMaisDown 0 (1,1)
"0 1 2"

-}
tuploStringMaisDown :: Int -> (Int,Int) -> String
tuploStringMaisDown j (x,y) = show j ++ " " ++ show x ++ " " ++ show (y+1)


{-|
A função 'andarD' recebe uma String, um Int (o jogador), um Char (comando do movimento) e dá a nova String depois de efetuar o movimento.

=Propriedade:

prop> andarD [] j m = []

==Por exemplo:
>>> andarD "0 2 2" 0 'D' 
"0 2 3"
-} 


andarD :: String -> Int -> Char -> String
andarD [] j m = []
andarD x j m | m == 'D' = tuploStringMaisDown j (tuplo (getCoord x ))
             | otherwise = []


{-|
A função 'tuploStringMaisUp' recebe um inteiro, o jogador, um par de coordenadas e transforma tudo na respetiva string subtraindo @-1@ ao @y@.

==Por exemplo:

>>> tuploStringMaisUp 0 (1,1)
"0 1 2"

-}
tuploStringMaisUp :: Int -> (Int,Int) -> String
tuploStringMaisUp j (x,y) | y>1 = show j ++ " " ++ show x ++ " " ++ show (y-1)
                          | otherwise = show j ++ " " ++ show x ++ " " ++ show y 

{-|
A função 'andarU' recebe uma String, um Int (o jogador), um Char (comando do movimento) e dá a nova String depois de efetuar o movimento.

=Propriedade:

prop> andarU [] j m = []

==Por exemplo:
>>> andarR "0 2 2" 0 'U' 
"0 2 1"
-} 


andarU :: String -> Int -> Char -> String
andarU [] j m = []
andarU x j m | m == 'U' = tuploStringMaisUp j (tuplo (getCoord x ))
             | otherwise = []



{-|
A função 'andar' recebe uma lista de String (o mapa), um Int (o jogador), um Char (comando do movimento) e dá um par de coordenadas para
onde o jogador se moveu

=Propriedade:

prop> andar [] j m = (0,0)

==Por exemplo:
>>> andar ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 2 1", "1 7 7"] 0 'R'
(3,1)

-}
andar :: [String] -> Int-> Char -> (Int,Int)
andar [] j m = (0,0)
andar mapa j m | m == 'U' = tuplo (getCoord (andarU (tirarCoord mapa j) j 'U') ) 
               | m == 'D' = tuplo (getCoord (andarD (tirarCoord mapa j) j 'D') )
               | m == 'L' = tuplo (getCoord (andarL (tirarCoord mapa j) j 'L') )
               | m == 'R' = tuplo (getCoord (andarR (tirarCoord mapa j) j 'R') )           
{-| 
A função 'tP' recebe uma lista de String (mapa) e dá uma lista com os pares ordenados de todos os cardinais e pontos de interrogação.

==Por exemplo:
>>> tP ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 2 1", "1 7 7"]
[(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(0,1),(8,1),(0,2),(2,2),(3,2),(4,2),(5,2),(6,2),(8,2),(0,3),(3,3),(6,3),(8,3),(0,4),(1,4),(2,4),(4,4),
(6,4),(7,4),(8,4),(0,5),(2,5),(5,5),(8,5),(0,6),(2,6),(3,6),(4,6),(5,6),(6,6),(8,6),(0,7),(3,7),(4,7),(8,7),(0,8),(1,8),(2,8),(3,8),(4,8),(5,8),(6,8),(7,8),(8,8)]

-}
tP :: [String] -> [(Int,Int)]
tP  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '#' || h== '?'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)


-- converte a string da coordenada de um power up no respetivo tuplo


{-|
A função 'coordB' recebe uma lista de String (mapa) e dá uma lista com todos os pares ordenados dos power-ups flames.

=Propriedade:

prop> coordB [] = []

==Por exemplo:
>>> coordB ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 2 1", "* 1 1 0 1 10"]
[(5,2),(3,3)]

-}
coordB :: [String] -> [(Int,Int)]
coordB [] = []
coordB ((a:b):t) | a=='+' = tuplosB ((a:b)) : coordB t
                 | otherwise = coordB t

{-|
A função 'tuplosB' recebe uma String e dá um par de coordenadas (Int,Int)

==Por exemplo:
>>> tuplosB "+ 5 5"
(5,5)

-}

tuploBF:: [String] -> (Int,Int)
tuploBF [x,y] = (read x, read y)


tuplosB :: String -> (Int,Int)
tuplosB x = tuploBF (getCoord x)
{-|
A função 'tuploStringB' recebe um par ordenado de um power-up bomba e converte na respetiva string.

==Por exemplo:
>>> tuploStringB (2,2)
"+ 2 2"

-}
tuploStringB :: (Int,Int) -> String
tuploStringB (x,y) = "+ " ++ show x ++ " " ++ show y 

{-|
A função 'coordF' recebe uma lista de String (mapa) e dá uma lista com todos os pares ordenados dos power-ups Flames.

=Propriedade:

prop> coordB [] = []

==Por exemplo:
>>> coordB ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 1 1"]
[(5,5)]

-}
coordF :: [String] -> [(Int,Int)]
coordF [] = []
coordF ((a:b):t) | a=='!' = tuplosF ((a:b)) : coordF t
                 | otherwise = coordF t

{-|
A função 'tuplosF' converte uma string de um power-up Flame no respetivo par de coordenadas.

== Por exemplo:
>>> tuplosF "! 2 2"
(2,2)
-}

tuplosF :: String -> (Int,Int)
tuplosF x = tuploBF(getCoord x)
{-|
A função 'tuploStringF' recebe um par de coordenadas de um power-up flame e converte-o na respetiva string.

== Por exemplo:
>>> tuploStringF (5,5)
"! 5 5"
-}
tuploStringF :: (Int,Int) -> String
tuploStringF (x,y) = "! " ++ show x ++ " " ++ show y 

{-|
A função 'tuploDeixarBomba' recebe um Int (jogador) e um par de coordenadas onde o jogador quer deixar uma bomba e imprime uma String com a bomba,
as suas coordenadas, o jogador, o raio da bomba e o tempo para a sua explosão.

== Por exemplo:
>>> tuploDeixarBom 0 (1,1)
"* 1 1 0 1 10"

-}
tuploDeixarBomba :: Int -> (Int,Int) -> String
tuploDeixarBomba j (x,y) = "* " ++ show x ++ " " ++ show y ++ " " ++ show j  ++ " 1 " ++ "10" 

{-|
A função 'bIniciais' recebe uma lista de String (mapa) e imprime uma lista com todos os pares ordenados das bombas iniciais.

=Propriedade:

prop> bIniciais [] = []

==Por exemplo:
>>> bIniciais ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 1 1"]
[(1,1)]

-}
bIniciais :: [String] -> [(Int,Int)]
bIniciais [] = []
bIniciais (x:t) | head x=='*' = tuplosF (unwords (take 2(drop 1 (words x)))) : bIniciais t
                | otherwise = bIniciais t

{-|
A função 'listajogadores' recebe uma lista de String (mapa) e imprime uma lista com todos os pares ordenados dos jogadores iniciais.

=Propriedade:

prop> listajogadores [] = []

==Por exemplo:
>>> listajogadores ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"]
["0 5 5 +!"]

-}
listajogadores :: [String]  -> [String]
listajogadores [] = []
listajogadores (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = h : listajogadores t
                      | otherwise = listajogadores t


{-|
A função 'listajogadores' recebe uma lista de String (mapa) e imprime uma lista de strings com todos os power-ups do mapa.

=Propriedade:

prop> listapowerups [] = []

==Por exemplo:
>>> listapowerups ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"]
["+ 5 2", "+ 3 3","! 5 5"]

-}
listapowerups :: [String]  -> [String]
listapowerups [] = []
listapowerups (h:t)  | head h == '+' || head h == '!' = h : listapowerups t
                      | otherwise = listapowerups t


{-|
A função 'listajogadores' recebe uma lista de String (mapa) e imprime uma lista de String com todos as pedras do mapa.

=Propriedade:

prop> cardinais [] = []

==Por exemplo:
>>> cardinais ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"]
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]


-}

cardinais :: [String]  -> [String]
cardinais [] = []
cardinais (h:t)  | head h == '#' = h : cardinais t
                | otherwise = cardinais t

{-|
A função 'bombas' recebe uma lista de String (mapa) e imprime uma lista de String com todas as bombas desse mapa.

=Propriedade:

prop> bombas [] = []

==Por exemplo:
>>> bombas ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
["* 1 1 0 1 10"]

-}
bombas :: [String]  -> [String]
bombas [] = []
bombas (h:t)  | head h == '*' = h : bombas t
              | otherwise = bombas t

{-|
A função 'meterBomba' recebe uma lista de String (mapa) e uma string da nova bomba e verifica se essa bomba pode ser colocada ou não.

=Propriedade:

prop> meterBomba [] _ = True

==Por exemplo:
>>> meterBomba ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"] "* 1 1 0 1 10"
False

>>> meterBomba ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "0 5 5 +!"] "* 1  0 1 10"
True
-}
meterBomba :: [String] -> String -> Bool
meterBomba [] _ = True
meterBomba mapa novabomba | elem novabomba (bombas mapa) == False = True
                          | otherwise = False
            

{-|
A função 'contarB' recebe uma String e conta o número de '+' (power-up Bombas) nessa string.

=Propriedade:

prop> contarB [] = 0

==Por exemplo:
>>> contarB "0 2 2 ++++"
4

-}
contarB :: String  -> Int
contarB [] = 0
contarB (h:t)  | h == '+' = 1 + contarB t
               | otherwise = contarB t

{-|
A função 'contarB' recebe uma String e conta o número de '+' (power-up Bombas) nessa string.

=Propriedade:

prop> contarB [] = 0

==Por exemplo:
>>> contarB "0 2 2 ++++"
4

-}
contarF :: String  -> Int
contarF [] = 0
contarF (h:t)  | h == '!' = 1 + contarF t        
               | otherwise = contarF t

{-|
A função 'ifFlames' recebe uma lista de Strings (mapa) e um jogador e verifica se esse jogador tem power-ups flames.

=Propriedade:

prop> ifFlames [] j = False

==Por exemplo:
>>> ifFlames ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"] 0
  True


-}

ifFlames :: [String] -> Int -> Bool
ifFlames [] j = False
ifFlames mapa j | contarF (tirarCoord mapa j) >=1 = True 
                | otherwise = False

{-|
A função 'tuploDeixarBombaF' recebe um Int (jogador), um par de coordenadas  onde o jogador quer deixar uma bomba e o número de power-up Flames 
desse jogador e imprime uma String com a bomba, as suas coordenadas, o jogador, o raio da bomba e o tempo para a sua explosão.

== Por exemplo:
>>> tuploDeixarBom 0 (1,1) 2
"* 1 1 0 3 10"

-}
tuploDeixarBombaF :: Int -> (Int,Int) -> Int -> String
tuploDeixarBombaF j (x,y) n = "* " ++ show x ++ " " ++ show y ++ " " ++ show j ++ " "  ++ show( n + 1)  ++ " 10" 

{-|
A função 'bIniciaisJ' recebe uma lista de String (mapa) e um jogador e conta as bombas que esse jogador já tem no mapa.

=Propriedade:

prop> bIniciais [] j = 0

==Por exemplo:
>>> bIniciais ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "0 5 5 +!"] 0
1

-}
bIniciaisJ :: [String] -> Int -> Int
bIniciaisJ [] j = 0
bIniciaisJ (x:t) j | head x=='*' = 1 + bIniciaisJ t j
                   | otherwise = bIniciaisJ t j

{-|
A função 'bombasJogador' recebe uma string e um Int (jogador) e verifica se essa bomba é do jogador j.

=Propriedade;

prop> bombasJogador [] j = False

==Por exemplo:
>>> bombasJogador "* 1 1 0 1 10" 0
True

-}
bombasJogador :: String -> Int -> Bool
bombasJogador [] j = False
bombasJogador x j | elem (show j) (drop 3 ( take 4 (words x))) == True = True
                  | otherwise = False

{-| 
A função 'bombasJogadorMapa' recebe uma lista de String (mapa) e um Int (jogador) e conta no número de bombas que esse jogador tem no mapa.

=Propriedade:

prop> bombasJogadorMapa [] j = 0

==Por exemplo:
>>> bombasJogadorMapa ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "* 1 2 0 1 10" "0 5 5 +!"] 0
2

-}

bombasJogadorMapa :: [String] -> Int -> Int 
bombasJogadorMapa [] j = 0
bombasJogadorMapa (h:t) j | (bombasJogador h j == True) = 1 + bombasJogadorMapa t j
                        | otherwise = bombasJogadorMapa t j

{-| 
A função 'verificaJogador' recebe um Int (jogador) e uma lista de String (mapa) e verifica se esse jogador está presente no jogo.

=Propriedade:

prop> verificaJogador _ [] = False

==Por exemplo:
>>> verificaJogador 2 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "* 1 2 0 1 10" "0 5 5 +!"]
False

-}

verificaJogador :: Int -> [String] -> Bool
verificaJogador _ [] = False
verificaJogador j (h:t) | show j == (take 1 h) = True
                        | otherwise = verificaJogador j t


{-| 
A função 'move' é a função principal desta tarefa e recebe uma lista de String (mapa), um Int (jogador) e um Char (comando a dar ao jogador). Imprime
o novo mapa depois de efetuar esse comando.

=Propriedade:

prop> move [] j m = []

==Por exemplo:
>>> move ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "* 1 2 0 1 10" "0 5 5 +!"] 0 'R'
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10", "* 1 2 0 1 10" "0 6 5 +!"]
-}
--tuplo (getCoord ( tirarCoord mapa j))

move :: [String] -> Int -> Char -> [String]
move [] j m = []
move mapa j m  | m == 'B'  && (ifFlames mapa j == True) && bombasJogadorMapa mapa j <= contarB (tirarCoord mapa j)   = ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBombaF j (tuplo (getCoord ( tirarCoord mapa j))) (contarF (tirarCoord mapa j))) (bombas mapa)) ++ listajogadores mapa)
               | m == 'B' && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j) = (if elem (tuplo (getCoord ( tirarCoord mapa j))) (bIniciais mapa) == False  then ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBomba j (tuplo (getCoord ( tirarCoord mapa j)))) (bombas mapa)) ++ listajogadores mapa)   else  mapa)
               | m == 'B' && (bombasJogadorMapa mapa j) > contarB (tirarCoord mapa j) = mapa
               | verificaJogador j (listajogadores mapa) == False = mapa  
               | elem (andar mapa j m) (tP mapa)== True = mapa 
               | elem (andar mapa j m) (coordB mapa) == True  = delete (tuploStringB (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ "+ "  ++ replicate (cFlames (tirarCoord mapa j)) '!'    )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar mapa j m) (coordF mapa) == True  = delete (tuploStringF (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cFlames (tirarCoord mapa j)) '!' ++"! " ++ replicate (cMais (tirarCoord mapa j)) '+' )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar mapa j m) (tP mapa)== False =  delete (tuploStringF (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ replicate (cFlames (tirarCoord mapa j)) '!' )  (delete (tirarCoord mapa j) mapa))       
               | otherwise = mapa

{- |
O programa 'main' foi-nos facultado para a realização deste projeto e pode ser usado para testar a função 'move', por nós definida, ou seja,
após compilado, o programa aceita como parâmetros o mapa, o jogador e o comando m e depois invoca
a função ​@move@​ imprimindo o resultado no stdout.

-}


main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"