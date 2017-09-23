{-|
Module: Main

Description : Módulo Haskell com funções que permitem produzir efeitos da passagem de tempo num estado de jogo.

Copyright: Henrique Manuel Palmeira Pereira <a80261@alunos.uminho.pt>
           Ricardo Filipe Sousa Caçador <a81064@alunos.uminho.pt>

Um módulo contendo definições Haskell para o cálculo de funções que permitem produzir efeitos da passagem de tempo num estado de jogo.
partindo de um Mapa e de um Instante de Tempo, originando o novo Mapa.


-}

module Main where

import Data.Char 
import System.Environment
import Text.Read
import Data.Maybe
import BombermanAuxTarefa4
import Data.List

{- |
A função 'instanteBomba' recebe o Mapa e produz o efeito da passagem de tempo nas bombas originando uma lista de strings com as novas bombas e sem bombas que
explodem.

=Propriedade:

prop> instanteBomba []  = []

==Por exemplo:
>>> tirarCoord ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 10"] 
"* 1 1 0 1 9"

>>> tirarCoord ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 1"]
[]

-}

instanteBomba :: Mapa -> [String]
instanteBomba [] = []
instanteBomba mapa = aux mapa [] where
                    aux [] new = []
                    aux ((h:t):ts) new | h == '*' && read(last (words t)) == 1 = aux ts ([(h:t)] ++ new) 
                                       | h == '*' && read(last (words t)) >1 && explodePB (tuplosBr (h:t)) (tB (ts++new)) ((h:t):ts) /= [] =  tempo1 (h:t) : aux ts new
                                       | h == '*' && read(last (words t)) >1 && explodePB (tuplosBr (h:t)) (tB ts) ((h:t):ts) == [] = menosUm (h:t) : aux ts new
                                       | otherwise = aux ts new
{- |
A função 'explodePB' recebe uma Coordenada de uma Bomba, Bombas e um Mapa e produz uma lista de Coordenadas com as bombas que são atingidas pela CoordenadaBomba

=Propriedade:

prop> explodePB (r,x,y,t) [] _ = []

==Por exemplo:
>>> explodePB (1,1,1,1) [(1,2,1,10)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 1", "* 2 1 0 1 10"]
[(1,2,1,10)]


-}

explodePB :: CoordenadaBomba -> Bombas-> Mapa -> Bombas   
explodePB  (rb1,xb1,yb1,tb1) [] _ = []
explodePB (rb1,xb1,yb1,tb1) ((r,xb,yb,tb):ts) mapa  | yb1 == yb && xb > xb1  && ((xb - r) <= xb1) && (esqPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)]
                                              | yb1 == yb && xb <=  xb1 && ((xb + r) >= xb1) && (dirPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)] 
                                              | xb1 == xb && yb <= yb1 && ((yb + r) >= yb1) && (cimaPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)] 
                                              | xb1 == xb && yb >  yb1  && ((yb -r) <= yb1) && (baixoPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))== False && tb ==1  = [(rb1,xb1,yb1,tb1)]
                                              | otherwise = explodePB (rb1,xb1,yb1,tb1) ts mapa 

{- |
A função 'menosUm" recebe uma String de uma Bomba e altera o tempo dessa bomba, subtraindo 1.


==Por exemplo:
>>> menosUm "* 1 1 0 1 10"
"* 1 1 0 1 9"

-}


menosUm:: String ->String
menosUm (h:t) = [h] ++ " " ++ unwords (take 4(words t)) ++ " " ++ show((read(last (words t)))-1)

{- |
A função 'tuplosBr" recebe uma String de uma Bomba e dá um tuplo de coordenadas do género (raio,x,y,tempo).


==Por exemplo:
>>> tuplosBr "* 1 1 0 1 10"
"* 1 1 0 1 9"

-}

tuplosBr :: String -> (Int,Int,Int,Int)
tuplosBr x = tuploRaio2 (getCoord x)

{- |
A função 'tuploRaio2' é a função auxiliar de 'tuplosBr' que lê os elementos de uma lista de Strings e produz o tuplo (raio,x,y,tempo)


==Por exemplo:
>>> tuploRaio2 ["2","2","0","1","10"]
(1,2,2,10)

-}

tuploRaio2:: [String] -> (Int,Int,Int,Int)
tuploRaio2 [x,y,j,r,t] = (read r,read x, read y,read t)

{- |
A função 'tempo1" recebe uma String de uma Bomba e dá uma String da mesma Bomba com o tempo igual a 1.


==Por exemplo:
>>> tempo1 "* 1 1 0 1 10"
"* 1 1 0 1 1"

-}

tempo1 :: String -> String
tempo1 (h:t) = [h] ++ " " ++ unwords (take 4(words t)) ++ " " ++ "1"


{- |
A função 'tJ' recebe um Mapa e produz a lista com as coordenadas dos jogadores e a sua identificação.

=Propriedade:

prop> tJ [] = []


==Por exemplo:
>>> tJ ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1"]
[(0,1,1)]


-}

tJ :: Mapa -> Jogadores
tJ  [] = []
tJ ((h:t):ts) | (h >='0' && h<='3') = tuploJ (getCoord (h:t)) : tJ ts
              | otherwise = tJ ts



{- |
A função 'tuploRaio' é a função auxiliar de 'tuplosBr' que lê os elementos de uma lista de Strings e produz o tuplo (raio,x,y,tempo)


==Por exemplo:
>>> tuploRaio ["2","2","0","1","10"]
(1,2,2,10)

-}

tuploRaio:: [String] -> CoordenadaBomba
tuploRaio [x,y,z,t] = (read z,read x, read y,read t)

{- |
A função 'tB' recebe um Mapa e produz a lista com as coordenadas das bombas do género (raio,x,y,tempo).

=Propriedade:

prop> tB [] = []


==Por exemplo:
>>> tB ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 10"]
[(1,1,1,10)]


-}

tB:: Mapa -> Bombas
tB [] = []
tB (x:t) | head x=='*' = tuploRaio ((take 2 (drop 1(words x)))++take 1( drop 4 (words x))++ (drop 5 (words x))) : tB t
         | otherwise = tB t

{- |
A função 'arrebentaBonecos' recebe um Mapa, uma lista com os jogadores e produz o novo mapa sem os jogadores da lista.

=Propriedade:

prop> arrebentaBonecos [] = []
prop> arrebentaBonecos mapa [] = mapa


==Por exemplo:
>>> arrebentaBonecos ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1", "1 2 1"] [0]
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","1 2 1"]


-}
arrebentaBonecos :: Mapa -> NJogadores -> Mapa
arrebentaBonecos [] _ = []
arrebentaBonecos x [] = x 
arrebentaBonecos ((h:t):ts) (j:js) | intToDigit j /= h = (h:t) : arrebentaBonecos ts (j:js)
                                   | otherwise = arrebentaBonecos ts js

{- |
A função 'listajogadores3' recebe um Mapa e produz a lista dos jogadores existentes no estado de jogo.

=Propriedade:

prop> listajogadores3 [] = []


==Por exemplo:
>>> listajogadores3 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1", "1 2 1"] [0]
[0,1]


-}
listajogadores3 :: Mapa -> NJogadores
listajogadores3 [] = []
listajogadores3 (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = digitToInt (head h) : listajogadores3 t
                       | otherwise = listajogadores3 t


{- |
A função 'explodeJ' recebe os Jogadores, as Bombas e um Mapa e produz uma lista com os jogadores que são atingidos pelas Bombas.

=Propriedade:

prop> explodeJ jogadores [] _ = []
prop> explodeJ [] _ _ = []

==Por exemplo:
>>> explodeJ (1,1,1,1) [(1,2,1,10)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 1", "* 2 1 0 1 10"]
[(1,2,1,10)]


-}

explodeJ :: Jogadores -> Bombas -> Mapa -> NJogadores
explodeJ ((j,xj,yj):t) [] _ = []
explodeJ [] _ _ = []
explodeJ ((j,xj,yj):t) ((r,xb,yb,tb):ts) mapa | yj == yb && xb >  xj  && ((xb -r) <= xj) && (dirPedra   (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | yj == yb && xb <= xj  && ((xb +r) >= xj) && (esqPedra   (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | xj == xb && yb <= yj  && ((yb +r) >= yj) && (cimaPedra  (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | xj == xb && yb >  yj  && ((yb -r) <= yj) && (baixoPedra (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodeJ [(j,xj,yj)] ts mapa ++ explodeJ t ((r,xb,yb,tb):ts) mapa 



{- |
A função 'tPSemLimites' recebe as coordenadas das Pedras e dos Pontos de Interrogação, o Mapa e produz as coordenadas das Pedras e PI sem as Pedras Limitantes
de um Mapa.

=Propriedade:

prop> tPSemLimites [] _  = []

==Por exemplo:
>>> tPSemLimites [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(0,1),(6,1),(0,2),(2,2),(4,2),(6,2),(0,3),(2,3),(3,3),(6,3),(0,4),(2,4),(3,4),(4,4),(6,4),(0,5),(6,5),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6)] ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
[(2,2),(4,2),(2,3),(3,3),(2,4),(3,4),(4,4)]

-}

tPSemLimites :: PedrasEPI -> Mapa -> PedrasEPI
tPSemLimites  [] _ = []
tPSemLimites ((x,y):t) (h:ts) | (x== 0 || x== length h-1 || y ==0 || y == length h-1) = tPSemLimites t (h:ts)
                             | otherwise = (x,y): tPSemLimites t (h:ts)



{- |
A função 'esqPedra' recebe as coordenadas de um Jogador e as Coordenadas das Pedras e dos PI e verifica se o jogador tem uma Pedra ou um PI à sua esquerda.

=Propriedade:

prop> esqPedra (x,y) []  = False

==Por exemplo:
>>> esqPedra (3,2) [(2,2),(4,2),(2,3),(3,3),(2,4),(3,4),(4,4)]
True

-}

esqPedra :: CoordenadaJ -> PedrasEPI -> Bool
esqPedra x [] = False
esqPedra (x,y) ((h,t):ts) | (x-1,y) == (h,t) = True
                                | otherwise = esqPedra (x,y) ts

{- |
A função 'dirPedra' recebe as coordenadas de um Jogador e as Coordenadas das Pedras e dos PI e verifica se o jogador tem uma Pedra ou um PI à sua direita.

=Propriedade:

prop> dirPedra (x,y) []  = False

==Por exemplo:
>>> dirPedra (3,2) [(2,2),(4,2),(2,3),(3,3),(2,4),(3,4),(4,4)]
True

-}                                
dirPedra :: CoordenadaJ -> PedrasEPI -> Bool
dirPedra x [] = False
dirPedra (x,y) ((h,t):ts) | (x+1,y) == (h,t) = True
                                | otherwise = dirPedra (x,y) ts

{- |
A função 'baixoPedra' recebe as coordenadas de um Jogador e as Coordenadas das Pedras e dos PI e verifica se o jogador tem uma Pedra ou um PI por baixo de si.

=Propriedade:

prop> baixoPedra (x,y) []  = False

==Por exemplo:
>>> baixoPedra (2,1) [(2,2),(4,2),(2,3),(3,3),(2,4),(3,4),(4,4)]
True

-}                                  
baixoPedra :: CoordenadaJ -> PedrasEPI -> Bool
baixoPedra x [] = False
baixoPedra (x,y) ((h,t):ts) | (x,y+1) == (h,t) = True
                                | otherwise = baixoPedra (x,y) ts

{- |
A função 'cimaPedra' recebe as coordenadas de um Jogador e as Coordenadas das Pedras e dos PI e verifica se o jogador tem uma Pedra ou um PI por cima de si.

=Propriedade:

prop> cimaPedra (x,y) []  = False

==Por exemplo:
>>> cimaPedra (2,3) [(2,2),(4,2),(2,3),(3,3),(2,4),(3,4),(4,4)]
True

-}    
cimaPedra :: CoordenadaJ -> PedrasEPI -> Bool
cimaPedra x [] = False
cimaPedra (x,y) ((h,t):ts) | (x,y-1) == (h,t) = True
                              | otherwise = cimaPedra (x,y) ts


{- |
A função 'explodePU' recebe os PowerUps, as Bombas e um Mapa e produz uma lista com os PowerUps que são atingidos pelas Bombas.

=Propriedade:

prop> explodePU powerups [] _ = []
prop> explodePU [] _ _ = []

==Por exemplo:
>>> explodePU [(5,2),(3,3),(5,5)] [(4,5,1,1)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 1 1 0 1 1", "* 2 1 0 1 10"]
[(4,5)]


-}
explodePU :: PowerUps -> Bombas -> Mapa -> PowerUps
explodePU  [] _ _= []   
explodePU  ((xp,yp):t) [] _ = []
explodePU ((xp,yp):t) ((r,xb,yb,tb):ts) mapa  | elem (xp,yp) (tI mapa) == False && yp == yb && xb >  xp && ((xb - r)  <=  xp)  && (dirPedra   (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) && verificaCoordSeguinteX ((xp,yp):t) == False = (xp,yp) : explodePU t ((r,xb,yb,tb):ts) mapa
                                              | elem (xp,yp) (tI mapa) == False && yp == yb && xb <= xp && ((xb + r)  >= xp)  && (esqPedra   (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xp,yp)] ++ explodePU (listaSRXb t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | elem (xp,yp) (tI mapa) == False && xp == xb && yb <= yp && ((yb + r)  >= yp)  && (cimaPedra  (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xp,yp)] ++ explodePU (listaSRYb t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | elem (xp,yp) (tI mapa) == False && xp == xb && yb >  yp && ((yb - r)  <= yp)  && (baixoPedra (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) && verificaCoordSeguinteY ((xp,yp):t) == False = (xp,yp) : explodePU t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodePU [(xp,yp)] ts mapa ++ explodePU t ((r,xb,yb,tb):ts) mapa


                                            
{- |
A função 'arrebentaPU' recebe um Mapa e produz o novo mapa sem os powerups que são atingidos pelas bombas.

=Propriedade:

prop> arrebentaPU [] = []


==Por exemplo:
>>> arrebentaPU ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 4 5 0 1 1", "0 1 1","1 2 1"]
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","0,1,1","1 2 1"]


-}
arrebentaPU :: Mapa -> Mapa
arrebentaPU [] = [] 
arrebentaPU mapa  | auxVerify (coordPU mapa) (explodePU (coordPU mapa) (tB mapa) mapa) == True  = removePowerUps (explodePU (coordPU mapa) (tB mapa) mapa) mapa 
                  | otherwise = mapa

{- |
A função 'auxVerify' recebe duas lista de elementos e verifica cada elemento da primeira lista com a segunda lista.

=Propriedade:

prop> auxVerify [] _ = False
prop> auxVerify _ [] = False


==Por exemplo:
>>> auxVerify [1,2,3,4,5] [5,6,7,8]
False

>>> auxVerify [1,2,3,4,5] [1,6,7,8]
True

-}
auxVerify :: Eq a => [a] -> [a] -> Bool
auxVerify [] _ = False
auxVerify _ [] = False
auxVerify (h:t) (a:b) | elem h (a:b)==True = True
                      | otherwise = auxVerify t (a:b)


{- |
A função 'removePowerUps' recebe os PowerUps a eliminar, um Mapa e produz o novo mapa sem os powerups que são atingidos pelas bombas.

=Propriedade:

prop> removePowerUps [] mapa = mapa
prop> removePowerUps mapa [] = []


==Por exemplo:
>>> removePowerUps (5,5) ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 4 5 0 1 1", "0 1 1","1 2 1"]
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","0,1,1","1 2 1"]


-}                 
removePowerUps :: PowerUps -> Mapa -> Mapa
removePowerUps [] x  = x 
removePowerUps x [] = []
removePowerUps x ((a:b):c) | (a == '+' || a =='!') && aux (a:b) x == True = removePowerUps x c 
                 | otherwise = (a:b) : removePowerUps x c

                    where -- verifica se um powerup pertence a lista dos powerups a eliminar 
                        aux :: String -> PowerUps -> Bool
                        aux x [] = False     
                        aux x (h:t) | tuplosPU x == h = True
                                             | otherwise = aux x t  




{- |
A função 'explodePI' recebe os Pontos de Interrogação, as Bombas e um Mapa e produz uma lista com os PontosInt que são atingidos pelas Bombas.

=Propriedade:

prop> explodePI [] _ _ = []
prop> explodePI pontos [] _ = []

==Por exemplo:
>>> explodePI [(3,2)] [(3,3,1,1)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5", "* 3 3 0 1 1"]
[(3,2)]


-}
explodePI :: PontosInt -> Bombas-> Mapa -> PontosInt
explodePI  [] _ _= []   
explodePI  ((xi,yi):t) [] _ = []
explodePI ((xi,yi):t) ((r,xb,yb,tb):ts) mapa  | yi == yb && xb > xi  && ((xb - r) <= xi) && (esqPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) && verificaCoordSeguinteX ((xi,yi):t) == False = (xi,yi) : explodePI t ((r,xb,yb,tb):ts) mapa
                                              | yi == yb && xb <=  xi && ((xb + r) >= xi) && (dirPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xi,yi)] ++ explodePI (listaSRXb t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | xi == xb && yb <= yi && ((yb + r) >= yi) && (cimaPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xi,yi)] ++ explodePI (listaSRYb t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | xi == xb && yb >  yi  && ((yb -r) <= yi) && (baixoPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))== False && tb ==1 && verificaCoordSeguinteY (mesmoX ((xi,yi):t) xi) == False = (xi,yi) : explodePI t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodePI [(xi,yi)] ts mapa ++ explodePI t ((r,xb,yb,tb):ts) mapa

{- |
A função 'mesmoX' recebe uma lista de Pontos de Interrogação e um Inteiro e fica com as coordendas em que o X é igual a esse Inteiro.

=Propriedade:

prop> mesmoX [] _ = []

==Por exemplo:
>>> mesmoX [(3,2),(5,2),(3,3)] 2  
[(3,2),(5,2)]


-}
mesmoX :: PontosInt -> Int -> PontosInt
mesmoX [] _ = []
mesmoX ((x,y):c) n | n == x = (x,y) : mesmoX c n
                   | otherwise = mesmoX c n

{- |
A função 'verificaCoordSeguinteX' recebe uma lista de Pontos de Interrogação e verifica se a coordenada seguinte na lista está na mesma linha ao lado da 
primeira coordenada.

=Propriedade:

prop> verificaCoordSeguinteX ((x,y):[])  = False

==Por exemplo:
>>> verificaCoordSeguinteX [(3,2),(4,2),(3,3)]   
True


-}
verificaCoordSeguinteX :: PontosInt -> Bool
verificaCoordSeguinteX ((x,y):[]) = False
verificaCoordSeguinteX ((x,y):(a,b):c) | b == y && a == x+1 = True
                                       | otherwise = False

{- |
A função 'verificaCoordSeguinteY' recebe uma lista de Pontos de Interrogação e verifica se a coordenada seguinte na lista está na mesma coluna por baixo da 
primeira coordenada.

=Propriedade:

prop> verificaCoordSeguinteY ((x,y):[])  = False

==Por exemplo:
>>> verificaCoordSeguinteY [(3,2),(3,3),(5,3)]   
True


-}
verificaCoordSeguinteY :: PontosInt -> Bool
verificaCoordSeguinteY ((x,y):[]) = False
verificaCoordSeguinteY ((x,y):(a,b):c) | a == x && b==y+1 = True
                                       | otherwise = False


{- |
A função 'tOnlyP' recebe um Mapa e origina a lista das coordenadas de todos os cardinais desse mapa.


==Por exemplo:
>>> tOnlyP ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
[(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(0,1),(6,1),(0,2),(2,2),(4,2),(6,2),(0,3),(6,3),(0,4),(2,4),(4,4),(6,4),(0,5),(6,5),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6)]

-}

tOnlyP :: Mapa -> Pedras
tOnlyP  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '#'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

{- |
A função 'listaSRXb' recebe uma lista de Pontos de Interrogação e uma Coordenada de uma Bomba e remove dessa lista as coordenadas que explodem ou estão à direita
de uma coordenada que explode.

=Propriedade:

prop> listaSRXb [] _  = []

==Por exemplo:
>>> listaSRXb [(3,2),(3,3),(5,3)]  (1,2,3,1)  
[(3,3),(5,3)]


-}
listaSRXb :: PontosInt -> CoordenadaBomba -> PontosInt
listaSRXb [] _ = []
listaSRXb ((x,y):t) (r,xb,yb,tb) | yb==y && x>=xb = listaSRXb t (r,xb,yb,tb)
                              | otherwise = (x,y) : listaSRXb t (r,xb,yb,tb)

{- |
A função 'listaSRYb' recebe uma lista de Pontos de Interrogação e uma Coordenada de uma Bomba e remove dessa lista as coordenadas que explodem ou estão por baixo
de uma coordenada que explode.

=Propriedade:

prop> listaSRYb [] _  = []

==Por exemplo:
>>> listaSRXb [(3,2),(2,4),(2,5)]  (1,2,3,1)  
[(2,4),(2,5)]


-}

listaSRYb :: PontosInt -> CoordenadaBomba -> PontosInt
listaSRYb [] _ = []
listaSRYb ((x,y):t) (r,xb,yb,tb) | xb==x && y>=yb = listaSRYb t (r,xb,yb,tb)
                                  | otherwise = (x,y) : listaSRYb t (r,xb,yb,tb)

{- |
A função 'tI' recebe um Mapa e origina a lista das coordenadas de todos os pontos de interrogação desse mapa.

=Propriedade:

==Por exemplo:
>>> tI ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
[(2,3),(3,3),(3,4)]


-}

tI :: Mapa -> PontosInt
tI  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '?'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

{- |
A função 'arrebentaPI' recebe um Mapa e produz o novo mapa sem os powerups que são atingidos pelas bombas.

=Propriedade:

prop> arrebentaPI [] = []
prop> arrebentaPI mapa [] = mapa

==Por exemplo:
>>> arrebentaPI ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] [(2,3)]
["#######","#     #","# # # #","#  ?  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]

-}

arrebentaPI :: Mapa -> PontosInt -> Mapa
arrebentaPI [] _ = []
arrebentaPI x [] = x
arrebentaPI ((h:t):ts) ((x,y):c) = aux 0 0 ((h:t):ts) ((x,y):c) where
                                    aux x y [] _ = []
                                    aux x y a [] = a
                                    aux x y ((h:t):ts) ((a,b):c) | b == y  = arrPIStr (h:t) (mesmoY ((a,b):c) b) : aux x (y+1) ts (apagaUsadosY c y) 
                                                                 | otherwise = (h:t) : aux x (y+1) ts ((a,b):c)

{- |
A função 'arrebentaPIStr' recebe uma String e uma lista de coordenadas de Pontos de Interrogação e apaga-os dessa String.


==Por exemplo:
>>> arrebentaPIStr "# ??  #" [(2,3),(3,3)]
"#      #"

-}
arrPIStr ::  String -> PontosInt -> String
arrPIStr linha coord = aux 0  linha coord where
                        aux x  [] _ = []
                        aux x  a [] = a
                        aux x (h:t) ((a,b):c) | h== '?' && x==a = " " ++ aux (x+1) t c
                                              | otherwise =[h] ++ aux (x+1) t ((a,b):c)

{- |
A função 'apagaUsadosY' recebe uma lista de coordenadas de Pontos de Interrogação e um Inteiro e apaga-os pontos de interrogação com o y igual a esse inteiro.

=Propriedade:

prop> apagaUsadosY [] _ = []

==Por exemplo:
>>> apagaUsadosY [(2,3),(3,3),(5,5)] 3
[(5,5)]

-}
apagaUsadosY :: PontosInt -> Int -> PontosInt
apagaUsadosY [] _ = []
apagaUsadosY ((x,y):t) n | n == y = apagaUsadosY t n
                         | otherwise = (x,y) : apagaUsadosY t n

{- |
A função 'mesmoY' recebe uma lista de coordenadas de Pontos de Interrogação e um Inteiro e origina uma nova lista com os pontos de interrogação com o y igual
a esse inteiro.

=Propriedade:

prop> mesmoY [] _ = []

==Por exemplo:
>>> mesmoY [(2,3),(3,3),(5,5)] 3
[(2,3),(3,3)]

-}
mesmoY :: PontosInt -> Int -> PontosInt
mesmoY [] _ = []
mesmoY ((x,y):c) n | n == y = (x,y) : mesmoY c n
                   | otherwise = mesmoY c n


{- |
A função 'verBomba' recebe um Mapa e verifica se há bombas que vão explodir, ou seja, com t = 1.

=Propriedade:

prop> verBomba [] = False

==Por exemplo:
>>> verBomba ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 0 1 1"] 
True

-}

verBomba :: Mapa -> Bool
verBomba [] = False
verBomba ((h:t):ts) | h == '*' && read (unwords((drop 4 (words t)))) == 1 = True
                    | otherwise = verBomba ts

{- |
A função 'ḿetePontoEspiral' recebe um Mapa, e uma coordenada e vai ao Mapa colocar um '#' nessa coordenada.

=Propriedade:

prop> metePontoEspiral [] _ = []

==Por exemplo:
>>> metePontoEspiral ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 0 1 1"] (1,1)
["#######","##    #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 0 1 1"]


-}

metePontoEspiral:: Mapa -> (Int,Int) -> Mapa
metePontoEspiral [] _ = []
metePontoEspiral ((h:t):ts) (x,y) = aux 0 0 ((h:t):ts) (x,y) where
                                    aux x y ((h:t):ts) (a,b) | b == y  = substituiPedra (h:t) (a,b) : ts 
                                                             | otherwise = (h:t) : aux x (y+1) ts (a,b)

{- |
A função 'substituiPedra' recebe uma String (linha) e uma coordenada e coloca uma '#' nessa coordenada.

==Por exemplo:
>>> substituiPedra "#     #" (6,1)
"#     ##"

-}

substituiPedra ::  String -> (Int,Int) -> String
substituiPedra linha coord = aux 0  linha coord where
                        aux x  [] _ = []
                        aux x (h:t) (a,b) | x==a = "#" ++ t
                                          | otherwise =[h] ++ aux (x+1) t (a,b)
{- |
A função 'coordespiral' recebe dois inteiros, a dimensão do mapa e nº de instantes de tempo restantes e produz a coordenada onde vai cai a pedra.

==Por exemplo:
>>> coordespiral 5 9
(1,1)

-}

coordespiral :: Int -> Int-> (Int,Int)
coordespiral k int   = aux 1 1 k int   where
                     aux x y k int  |  x<(k - 2) = (if (k -2)^2 == int 
                                                        then (x,y)
                                                        else aux (x+1) y k (int+1) ) 
                                     | x == (k  -2) && y < k -2 = (if (k -2)^2 == int
                                                                              then (x,y)
                                                                              else aux x (y+1) k (int +1) )
                                     | otherwise =  (espiralback x y k int)
                                     
{- |
A função 'tentandoespiral' recebe três inteiros, o nº de voltas dadas, a dimensão do mapa e nº de instantes de tempo restantes e produz a coordenada onde vai cai a pedra.

==Por exemplo:
>>> tentandoespiral3 0 5 9
(1,1)

-}


tentandoespiral3 :: Int -> Int -> Int -> (Int,Int)
tentandoespiral3 v k int = adicionavoltas (coordespiral k int) v

                                     
{- |
A função 'espiral' recebe a dimensão do mapa e o nº de instantes de tempo restantes e origina a coordenada. Esta função é geral e é usada para qualquer dimensão.

==Por exemplo:
>>> espiral 7 1
(3,3)

-}

--13 - 49
--11 - 25
--15 - 81
espiral :: Int -> Int -> (Int,Int)
espiral k int = aux 0  k int where
                        aux y k int | int == 1 = (1+(f k), 1+(f k))
                                    | int > ((k^2) - 8*k + 16) = tentandoespiral3 y k int
                                    | int <= 9 = tentandoespiral3 ((f k)-1) 5 int
                                    | otherwise = aux (y+1) (k-2) int
{- |
A função 'f' recebe a dimensão do mapa e dá o nº de voltas até meter a última pedra.

==Por exemplo:
>>> f 7 
2

-}

f :: Int -> Int
f x | x==5 = 1
     | otherwise = 1 + f (x-2)

{- |
A função 'adicionavoltas' recebe um par de coordenadas e o nº da volta e dá o novo par de coordenadas.

==Por exemplo:
>>> adicionavoltas (1,1) 1
(2,2)

-}

adicionavoltas :: (Int,Int) -> Int -> (Int,Int)
adicionavoltas (x,y) v | v ==0 = (x,y)
                       | otherwise = adicionavoltas (x+1,y+1) (v-1)

                                   
{- |
A função 'espiralback' é uma função auxiliar da 'coordespiral', recebe o x e o y, a dimensão do mapa e o inteiro e dá a coordenada onde será colocada a próxima pedra.
Esta função tem como objetivo dar as coordenadas quando a espiral está a voltar para trás.

==Por exemplo:
>>>  espiralback 4 5 7 25
(4,5)


-}
espiralback :: Int -> Int -> Int -> Int -> (Int,Int)
espiralback x y l int = aux x y l int where
                    aux x y l int  | x/=1 = if (l -2) ^2 == int
                                            then (x,y)
                                            else aux (x-1) y l (int + 1)
                                   | x == 1 = espiralup x y l int

{- |
A função 'espiralup' é uma função auxiliar da 'coordespiral', recebe o x e o y, a dimensão do mapa e o inteiro e dá a coordenada onde será colocada a próxima pedra.
Esta função tem como objetivo dar as coordenadas quando a espiral está a subir.

==Por exemplo:
>>>  espiralup 1 4 7 25
(1,4)


-}

espiralup :: Int -> Int -> Int -> Int -> (Int,Int)
espiralup x y l int = aux x y l int where
                  aux x y l int = if (l -2 ) ^2 == int
                                 then (x,y)
                                else aux x (y-1) l (int+1)


{- |
A função 'removeGeral' recebe um par de coordenadas e um mapa e elimina essa coordenada do mapa

=Propriedade:

prop> removeGeral x []  = []

==Por exemplo:
>>> removeGeral (1,1) ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 4","* 1 1 0 1 10","0 1 1"]
["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 4"]

-}


removeGeral :: (Int,Int) -> Mapa -> Mapa 
removeGeral  x [] = []
removeGeral (x,y) ((a:b):c) | ((a >= '0' && a <='3') || a =='*' || a=='+' || a=='!') && (tuploGeral (a:b)) == (x,y)  = removeGeral (x,y) c
                            | otherwise = (a:b) : removeGeral (x,y) c

{- |
A função 'tuploG' recebe uma lista de strings e forma um par de coordenadas.

==Por exemplo:
>>> tuploG ["0","1","1"]
(1,1)

-}

tuploG :: [String] -> (Int,Int)
tuploG [x,y] = (read x, read y)
tuploG [x,y,a,b,c] = (read x, read y)
tuploG [j,x,y] =(read x, read y)

{- |
A função 'tuploGeral' recebe uma string e dá o par de coordenadas dessa string.

==Por exemplo:
>>> tuploGeral "* 1 1 0 1 10"
(1,1)

-}

tuploGeral :: String -> (Int,Int)
tuploGeral x = tuploG (getCoord x)

{- |
A função 'avanca' é a função principal desta Tarefa. Recebe um Mapa e um Inteiro e origina um novo Mapa efetuando todas as alterações nesse mapa originadas
pela passagem de tempo.

==Por exemplo:
>>> avanca ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 2 1","* 1 1 0 1 1","* 1 3 0 1 1"] 100
["#######","#     #","# # # #","#  ?  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]

-}



avanca :: Mapa -> Int -> Mapa
avanca mapa x | x > ((length (head mapa)-2)^2) = if verBomba mapa == True  then cardinais (arrebentaPI mapa (explodePI (tI mapa) (tB mapa) mapa)) ++ listapowerups (arrebentaPU mapa) ++ instanteBomba mapa ++ listajogadores2 (arrebentaBonecos mapa (explodeJ (tJ mapa) (tB mapa) mapa))
               else  cardinais mapa ++ listapowerups mapa ++ instanteBomba mapa ++ listajogadores2 mapa
             

              | x <= ((length (head mapa)-2)^2) && x>=1 = if verBomba mapa == True  then cardinais (metePontoEspiral (arrebentaPI mapa (explodePI (tI mapa) (tB mapa) mapa)) (espiral (length (head mapa))x))  ++ listapowerups ((removeGeral  (espiral (length (head mapa))x)) (arrebentaPU mapa)) ++ instanteBomba (removeGeral (espiral (length (head mapa))x) mapa) ++ listajogadores2 ((removeGeral  (espiral (length (head mapa))x)) (arrebentaBonecos mapa (explodeJ (tJ mapa) (tB mapa) mapa)))
               else  cardinais (metePontoEspiral mapa (espiral (length (head mapa))x)) ++ listapowerups ((removeGeral  (espiral (length (head mapa))x)) mapa) ++ instanteBomba (removeGeral (espiral (length (head mapa))x)mapa) ++ listajogadores2 ((removeGeral  (espiral (length (head mapa))x)) mapa)
              | otherwise = mapa


main :: IO ()   
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
