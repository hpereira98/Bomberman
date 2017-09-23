{-|
Module: Main

Description : Módulo Haskell com funções geradoras de um comando através de um mapa, de um jogador e de um instante de tempo restante.

Copyright: Henrique Manuel Palmeira Pereira <a80261@alunos.uminho.pt>
           Ricardo Filipe Sousa Caçador <a81064@alunos.uminho.pt>

Um módulo contendo definições Haskell para o cálculo de funções que permitam dar um certo comando (Up, Down, Left, Right ou Bomb) para o jogador efetuar.
-}

module Tarefa6_li1g031 where

import Data.Char 
import System.Environment
import Text.Read
import Data.Maybe
import BombermanAuxTarefa6
import Data.List


{- |
A função 'tVazio' recebe um Mapa e produz a lista das coordenadas de pontos vazios.


==Por exemplo:
>>> tVazio ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
[(1,1),(2,1),(3,1),(4,1),(5,1),(1,2),(3,2),(5,2),(1,3),(4,3),(5,3),(1,4),(5,4),(1,5),(2,5),(3,5),(4,5),(5,5),(1,7),(3,7),(1,8),(3,8)]


-}
tVazio :: Mapa -> Vazios
tVazio  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== ' ' = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

{- |
A função 'coordLivres' recebe um Mapa e produz a lista das coordenadas de pontos vazios sem as coordenadas atingidas por bombas ou com bombas.


==Por exemplo:
>>> coordLivres ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
[(1,1),(2,1),(3,1),(4,1),(5,1),(1,2),(3,2),(5,2),(1,3),(4,3),(5,3),(1,4),(5,4),(1,5),(2,5),(3,5),(4,5),(5,5)]


-}

coordLivres :: Mapa -> Vazios
coordLivres mapa = deletingLivres (coordRaios (tB mapa)) (igual (tVazio (cardinais mapa)) (bIniciais mapa))

{- |
A função 'coordLivres2' recebe um Mapa e produz a lista das coordenadas de pontos vazios sem as coordenadas com bombas.


==Por exemplo:
>>> coordLivres2 ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 6 5 0 1 1"]
[(1,1),(2,1),(3,1),(4,1),(5,1),(1,2),(3,2),(5,2),(1,3),(4,3),(5,3),(1,4),(5,4),(1,5),(2,5),(3,5),(4,5),(5,5)]


-}
coordLivres2 :: Mapa -> Vazios
coordLivres2 mapa = (igual (tVazio (cardinais mapa)) (bIniciais mapa))

{- |
A função 'deletingLivres' recebe as coordenadas que são atingidas por bombas e as coordenadas vazias e elimina as coordenadas atingidas por bombas da lista
de coordenadas vazias.

=Propriedade:

prop> deletingLivres x [] = []
prop> deletingLivres []x = x


==Por exemplo:
>>> deletingLivres [(1,1),(2,1)] [(1,1),(2,1),(3,1),(4,1),(5,1),(1,2),(3,2),(5,2),(1,3),(4,3),(5,3),(1,4),(5,4),(1,5),(2,5),(3,5),(4,5),(5,5)]
[(3,1),(4,1),(5,1),(1,2),(3,2),(5,2),(1,3),(4,3),(5,3),(1,4),(5,4),(1,5),(2,5),(3,5),(4,5),(5,5)]

-}
deletingLivres :: Raios -> Vazios -> Vazios
deletingLivres x [] = []
deletingLivres [] x = x 
deletingLivres ((x,y):xs) ((a,b):as) | elem (a,b) ((x,y):xs) ==True = deletingLivres xs as 
                                     | otherwise = (a,b) : deletingLivres ((x,y):xs) as 


{- |
A função 'coordRaios' a lista das Bombas e dá as coordenadas que são atingidas pelo raio das bombas.

=Propriedade:

prop> coordRaios [] = []

==Por exemplo:
>>> coordRaios [(1,2,2,1)]
[(2,1),(1,2),(3,2),(2,3)]

-}

coordRaios :: Bombas -> Raios
coordRaios [] = []
coordRaios ((rb,xb,yb,tb):bs)  | rb ==1 = (xb,yb-1) : (xb-1,yb) : (xb+1,yb) : (xb,yb+1)  : coordRaios bs
                               | rb ==2 = (xb,yb-2) : (xb-2,yb) : (xb+2,yb) : (xb,yb+2)  : coordRaios bs
                               | rb ==3 = (xb,yb-3) : (xb-3,yb) : (xb+3,yb) : (xb,yb+3)  : coordRaios bs

{- |
A função 'igual' elimina da lista das coordenadas vazias, as coordenadas das bombas.

=Propriedade:

prop> igual [] _ = []
prop> igual x [] = x

==Por exemplo:
>>> igual [(1,1),(2,1),(2,2)] [(1,1)]
[(2,1),(1,2)]

-}                               
igual :: Vazios -> BombasXY-> Vazios
igual [] _ = []
igual x [] = x
igual ((x,y):ys) ((a,b):bs) | (x,y) == (a,b) = igual ys bs
                            | otherwise = (x,y) : igual ys ((a,b):bs) 

{- |
A função 'tuplosJxy' recebe uma String e origina um par de coordenadas.


==Por exemplo:
>>> tuplosJxy "0 1 1"
(1,1)

-} 
tuplosJxy :: String -> CoordenadaJ
tuplosJxy x = tuploJ2 (getCoord x)

{- |
A função 'tuplosJxy' recebe uma lista de String e origina um par de coordenadas.


==Por exemplo:
>>> tuplosJxy ["0","1","1"]
(1,1)

-} 
tuploJ2 :: [String] -> CoordenadaJ
tuploJ2 [j,x,y] = (read x, read y)


{- |
A função 'moveR' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para a direita. Esta função tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveR ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
True

-} 
moveR :: Mapa -> CoordenadaJ -> Bool
moveR mapa (x,y) | elem (x+1,y) (coordLivres mapa) = True
                 | otherwise = False
{- |
A função 'moveR2' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para a direita. Esta função 'não' tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveR2 ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
True

-} 

moveR2 :: Mapa -> CoordenadaJ -> Bool
moveR2 mapa (x,y) | elem (x+1,y) (coordLivres2 mapa) = True
                 | otherwise = False

{- |
A função 'moveL' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para a esquerda. Esta função tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveL ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
False

-} 
moveL :: Mapa -> CoordenadaJ -> Bool
moveL mapa (x,y) | elem (x-1,y) (coordLivres mapa) = True
                 | otherwise = False
{- |
A função 'moveL2' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para a esquerda. Esta função 'não' tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveL2 ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
False

-} 
moveL2 :: Mapa -> CoordenadaJ -> Bool
moveL2 mapa (x,y) | elem (x-1,y) (coordLivres2 mapa) = True
                 | otherwise = False

{- |
A função 'moveU' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para cima. Esta função tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveU ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
False

-} 
moveU :: Mapa -> CoordenadaJ -> Bool
moveU mapa (x,y) | elem (x,y-1) (coordLivres mapa) = True
                 | otherwise = False
{- |
A função 'moveU2' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para cima. Esta função 'não' tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveU2 ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
False

-} 
moveU2 :: Mapa -> CoordenadaJ -> Bool
moveU2 mapa (x,y) | elem (x,y-1) (coordLivres2 mapa) = True
                 | otherwise = False

{- |
A função 'moveD' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para baixo. Esta função tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveD ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
True

-} 
moveD :: Mapa -> CoordenadaJ -> Bool
moveD mapa (x,y) | elem (x,y+1) (coordLivres mapa) = True
                 | otherwise = False
{- |
A função 'moveD2' recebe um mapa e a coordenada do jogador e verifica se ele pode ir para baixo. Esta função 'não' tem em conta as coordenadas atingidas por bombas.


==Por exemplo:
>>>  moveD2 ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"] (1,1)
True

-} 
moveD2 :: Mapa -> CoordenadaJ -> Bool
moveD2 mapa (x,y) | elem (x,y+1) (coordLivres2 mapa) = True
                 | otherwise = False


{- |
A função 'estaNoRaio' recebe uma coordenada de jogador, a lista das bombas, um mapa e verifica se esse jogador está no raio de uma bomba. 

=Propriedade:

prop> estaNoRaio (x,y) [] _ = ('N',False)

==Por exemplo:
>>>   estaNoRaio (2,1) [(1,1,1,1)] ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 1 0 1","0 2 1"]
('L',True)


-}  
estaNoRaio :: CoordenadaJ -> Bombas -> Mapa -> (Char,Bool)
estaNoRaio (x,y) [] _ = ('N',False)
estaNoRaio (x,y) ((r,xb,yb,tb):ts) mapa | y==yb && xb==x && (dirPedra   (x,y) (tPSemLimites (tP mapa) mapa)) == True = ('R',True)
                                        | y==yb && xb==x && (esqPedra   (x,y) (tPSemLimites (tP mapa) mapa)) == True = ('L',True)
                                        | y==yb && xb==x && (cimaPedra  (x,y) (tPSemLimites (tP mapa) mapa)) == True = ('U',True)
                                        | y==yb && xb==x && (baixoPedra (x,y) (tPSemLimites (tP mapa) mapa)) == True = ('D',True)
                                        | y == yb && xb >= x && ((xb -r) <= x) && (dirPedra   (x,y) (tPSemLimites (tP mapa) mapa)) == False = ('R',True) 
                                        | y == yb && xb <= x && ((xb +r) >= x) && (esqPedra   (x,y) (tPSemLimites (tP mapa) mapa)) == False = ('L',True) 
                                        | x == xb && yb <= y && ((yb +r) >= y) && (cimaPedra  (x,y) (tPSemLimites (tP mapa) mapa)) == False = ('U',True) 
                                        | x == xb && yb >= y && ((yb -r) <= y) && (baixoPedra (x,y) (tPSemLimites (tP mapa) mapa)) == False = ('D',True) 
                                        | otherwise = estaNoRaio (x,y) ts mapa 

{- |
A função 'emCimaDaBomba' recebe uma coordenada de um jogador e uma lista de bombas e verifica se esse jogador está por cima de uma bomba.

=Propriedade:

prop> estaNoRaio (x,y) [] = False

==Por exemplo:
>>>  emCimaDaBomba (1,1) [(1,1,1,1)] 
True

-}  
emCimaDaBomba :: CoordenadaJ -> Bombas -> Bool
emCimaDaBomba (x,y) [] = False
emCimaDaBomba (x,y) ((rb,xb,yb,tb):bs) | (x,y) == (xb,yb) = True
                                      | otherwise = emCimaDaBomba (x,y) bs

{- |
A função 'boom' recebe um Mapa, a coordenada de um jogador e verifica se esse jogador tem uma caixa à sua volta.

==Por exemplo:
>>>  boom  ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 1 0 1","0 2 1"] (2,1)
False


-}  
boom :: Mapa -> CoordenadaJ -> Bool
boom mapa (x,y) | elem (x+1,y) (tI mapa) || elem (x-1,y) (tI mapa) || elem (x,y+1) (tI mapa) || elem (x,y-1) (tI mapa) = True
                | otherwise = False
                
{- |
A função 'tPlayers' recebe um mapa e produz a lista com as coordenadas dos jogadores presentes nesse mapa.

=Propriedade:

prop> tPlayers [] = []

==Por exemplo:
>>> tPlayers  ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 1 0 1","0 2 1"] 
[(2,1)]


-}  
                
tPlayers :: Mapa -> JogadoresXY
tPlayers  [] = []
tPlayers ((h:t):ts) | (h >='0' && h<='3') = tuplosJxy (h:t) : tPlayers ts
                    | otherwise = tPlayers ts

{- |
A função 'boomJ' recebe um Mapa, uma coordenada de um Jogador
=Propriedade:

prop> tPlayers [] = []

==Por exemplo:
>>> tPlayers  ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 1 1 1 0 1","0 2 1"] 
[(2,1)]


-}  
           
boomJ :: Mapa -> CoordenadaJ -> Bool
boomJ mapa (x,y)| elem (x+1,y) (tPlayers mapa) || elem (x-1,y) (tPlayers mapa) || elem (x,y+1) (tPlayers mapa) || elem (x,y-1) (tPlayers mapa) || elem (x,y) (tPlayers mapa) = True
                | otherwise = False

{-|
A função 'ver', pegando num Mapa e no número do jogador, vai verificar se esse jogador se encontra no mapa, devolvendo um boleano - True se existir
no mapa e False se não existir.

=Propriedade:
prop> ver [] _ = False

==Por exemplo:
>>> ver ["#####"] 0
False

>>> ver ["#####","0 1 1"] 0
True

-}

ver :: Mapa -> Int -> Bool
ver [] _ = False
ver (x:xs) j | head x == intToDigit j = True
             | otherwise = ver xs j       

{-|
A função 'temSaidaR' pega na coordenada e num mapa e verifica se o jogador "não se vai meter num buraco" se for para a direita. É uma função útil para o jogador 
não ficar entre uma caixa e uma bomba.

==Por exemplo:
>>> temSaidaR (1,1) ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 2 1","* 1 1 0 1 1","* 1 3 0 1 1"] 100
True

-}


temSaidaR :: (Int,Int) -> Mapa -> Bool
temSaidaR (x,y) mapa | elem (x+2,y) (tI mapa) = False
                     | otherwise = True

{-|
A função 'temSaidaL' pega na coordenada e num mapa e verifica se o jogador "não se vai meter num buraco" se for para a esquerda. É uma função útil para o jogador 
não ficar entre uma caixa e uma bomba.


==Por exemplo:
>>> temSaidaL (3,1) ["#######","#?    #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 2 1","* 1 1 0 1 1","* 1 3 0 1 1"] 100
False

-}


temSaidaL :: (Int,Int) -> Mapa -> Bool
temSaidaL (x,y) mapa | elem (x-2,y) (tI mapa) = False
                     | otherwise = True
{-|
A função 'temSaidaD' pega na coordenada e num mapa e verifica se o jogador "não se vai meter num buraco" se for para baixo. É uma função útil para o jogador 
não ficar entre uma caixa e uma bomba.


==Por exemplo:
>>> temSaidaD (3,1) ["#######","#?    #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 2 1","* 1 1 0 1 1","* 1 3 0 1 1"] 100
False

-}

temSaidaD :: (Int,Int) -> Mapa -> Bool
temSaidaD (x,y) mapa | elem (x,y+2) (tI mapa) = False
                     | otherwise = True
{-|
A função 'temSaidaU' pega na coordenada e num mapa e verifica se o jogador "não se vai meter num buraco" se for para cima. É uma função útil para o jogador não ficar entre 
uma caixa e uma bomba.


==Por exemplo:
>>> temSaidaU (3,3) ["#######","#  ?  #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 2 1","* 1 1 0 1 1","* 1 3 0 1 1"] 100
False

-}


temSaidaU :: (Int,Int) -> Mapa -> Bool
temSaidaU (x,y) mapa | elem (x,y-2) (tI mapa) = False
                     | otherwise = True




{-|
A função 'bot' é a principal função da Tarefa 6. Esta função recebe um Mapa, um Jogador e um Inteiro (Instante de Tempo) e devolve um Maybe Char que poderá ser 
um movimento para o jogador ou Nothing.


==Por exemplo:
>>> bot ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 1 2","+ 3 4","* 1 1 0 1 1","0 2 1"] 0 100
Just 'R'


-}
bot :: Mapa -> Int -> Int -> Maybe Char
bot mapa j n  | ver mapa j == False = Nothing
              | (fst (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True  then Just 'R'
                                                                                          else if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U'
                                                                                          else if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L' 
                                                                                            else Just 'D')
                                                                                        else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'D'
              {-1º quadrante-}                                                           else if moveL mapa (tuplosJxy (tirarCoord mapa j)) == True
                                                                                          then Just 'L'
                                                                                          else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing) 
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)


              | (fst (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2)  = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L'
                                                                                          else if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U'
                                                                                          else if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R' 
                                                                                            else Just 'D')
                  
                                                                                        else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True 
                                                                                         then Just 'D'
             {-2º quadrante-}                                                           else if moveR mapa (tuplosJxy (tirarCoord mapa j)) == True
                                                                                              then Just 'R'
                                                                                            else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                          {-bomba a direita-}           else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
                                                          {-bomba em baixo-}            else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                          {-bomba em cima-}             else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                          {-bomba a esquerda-}          else (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
              |(fst (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L'
                                                                                          else if moveD2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaD (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'D'
                                                                                          else if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R' 
                                                                                            else Just 'U')
                                                                                                                                                                               
                                                                                        else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'R'
              {-3º quadrante-}                                                           else if moveU mapa (tuplosJxy (tirarCoord mapa j)) == True
                                                                                          then Just 'U'
                                                                                             else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing) 
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)





              |(fst (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R'
                                                                                          else if moveD2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaD (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'D'
                                                                                          else if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L' 
                                                                                            else Just 'U')
                                                                                          
                                                                                          else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'L'
              {-4º quadrante-}                                                           else if moveU mapa (tuplosJxy (tirarCoord mapa j)) == True
                                                                                          then Just 'U'
                                                                                              else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
             |(fst (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L'
                                                                                          else if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U'
                                                                                          else if moveD2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaD (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'D' 
                                                                                            else Just 'R')

                                                                                  
                                                                                          else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'R'
              {-y == metade && x < metade -}                                              
                                                                                            else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
              |(fst (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R'
                                                                                          else if moveD2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaD (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'D'
                                                                                          else if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U' 
                                                                                            else Just 'L')
                                                                                       
                                                                                          else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'L'
              {-y == metade  && x > metade-}                                                           
                                                                                             else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
             |(fst (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) < (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveD2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaD (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'D'
                                                                                          else if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R'
                                                                                          else if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L' 
                                                                                            else Just 'U')
                                                                                       
                                                                                          else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'D'
              {-x == metade  && y > metade-}                                                           
                                                                                         else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing) 
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                      else Nothing)
             |(fst (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) > (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U'
                                                                                          else if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R'
                                                                                          else if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L' 
                                                                                            else Just 'D')

                  {-x == metade  && y < metade-}                                                                    
                                                                                          else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('N',False) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True   
                                                                                         then Just 'U'
                                                              else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j)
                                                                                            then Just 'B'
                                                                                            else Nothing)            
                                                                                          
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
              |  (fst (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) &&  (snd (tuplosJxy (tirarCoord mapa j))) == (div (length (head mapa)) 2) = if emCimaDaBomba (tuplosJxy (tirarCoord mapa j)) (tB mapa) == True then ( 
                                                                                         if moveL2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaL (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'L'
                                                                                          else if moveR2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaR (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'R'
                                                                                          else if moveU2 mapa (tuplosJxy (tirarCoord mapa j))==True && temSaidaU (tuplosJxy (tirarCoord mapa j)) mapa == True then Just 'U' 
                                                                                            else Just 'D')

                                                                                        
                                                                                          else if (boomJ mapa (tuplosJxy (tirarCoord mapa j))== True || boom mapa (tuplosJxy (tirarCoord mapa j))==True) && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j) then Just 'B'
                                                                                         
              {-x == metade  && y == metade-}                                                           
                                                                                        
                                                 {-bomba a direita-}                     else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('R',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba em baixo-}                      else if estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('D',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                    else  if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                      else Nothing)
                                                 {-bomba em cima-}                       else if  estaNoRaio (tuplosJxy (tirarCoord mapa j)) (tB mapa) mapa == ('U',True) then (if moveL mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'L'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing)
                                                 {-bomba a esquerda-}                    else if estaNoRaio (tuplosJxy (tirarCoord mapa j )) (tB mapa) mapa == ('L',True) then (if moveU mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'U'
                                                                                                  else if moveR mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'R'
                                                                                                    else  if moveD mapa (tuplosJxy (tirarCoord mapa j))==True then Just 'D'
                                                                                                      else Nothing) 
                                                                                          else Nothing        
