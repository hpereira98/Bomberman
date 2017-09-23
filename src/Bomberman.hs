
module Bomberman where

import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import System.Random
import Data.Char

type Mapa = [String]
type Bombas = [(Int,Int,Int,Int)] -- (raio,x,y,tempo)
type Jogadores = [(Int,Int,Int)]
type PowerUps = [(Int,Int)]
type CoordenadaBomba = (Int,Int,Int,Int)
type NJogadores = [Int]
type PedrasEPI = [(Int,Int)]
type Pedras = [(Int,Int)]
type JogadoresXY = [(Int,Int)]
type CoordenadaJ = (Int,Int)
type PontosInt = [(Int,Int)]
type Vazios = [(Int,Int)]
type Raios = [(Int,Int)]
type BombasXY = [(Int,Int)]

grelha :: Int -> Int -> String
grelha y d | y==1 && d==5 = '#':replicate (d-2) ' ' ++ "#"
           | y==3 && d==5 = '#':replicate (d-2) ' ' ++ "#"
           | y==0 || y==(d-1) = replicate d '#'
           | y==1 || y==(d-2) = "#  "++replicate (d-6) '?'++"  #"
           | y==2 || y==(d-3) = "# "++aux1 (d-4) '#'++" #"
           | odd y && y<=d = aux d '?'
           | even y && y<=d = aux1 d '#'


aux :: Int -> Char -> String
aux 0 x = " "
aux d x = '#':replicate (d-2) x ++ "#" -- x tem q ser ' '


aux1 :: Int -> Char -> String
aux1 d x = unwords1 (replicate (d-div d 2) x) -- x tem q ser '#'

unwords1 :: String -> String
unwords1 [] = "?"
unwords1 [x] = [x]
unwords1 (h:t) = [h]++"?"++unwords1 t


mapaSemN :: Int -> Int -> [String]
mapaSemN x d | x<=(d-1) = grelha x d:mapaSemN (x+1) d -- x tem q ser 0
             | otherwise = []


numeros :: Int -> Int -> [Int]  -- número de randoms q precisa cada mapa dado a dimensao e a semente
numeros d s = take (nPontos d) $ randomRs (0,99) (mkStdGen s)
    where nPontos d = if d>5 then d^2-nPedras d-12  
                               else d^2-nPedras d
          nPedras d = 4*d-4 + (div (d-3) 2)^2 


alterarLinha :: String -> [Int] -> String
alterarLinha [] _ = []
alterarLinha l1 [] = l1
alterarLinha (x:xs) (y:ys) | x=='?' && y>=40 && y<=99 = ' ':alterarLinha xs ys
                           | x=='?' && (y==0 || y==1) = '+':alterarLinha xs ys
                           | x=='?' && (y==2 || y==3) = '!':alterarLinha xs ys
                           | x=='?' && y>=4 && y<=39 = '?':alterarLinha xs ys
                           | otherwise = x:alterarLinha xs (y:ys)


linha :: [String] -> Int -> String -- dada um nº devolve a linha nessa posicao
linha [] _ = []
linha (x:xs) 0 = x
linha (x:y:xs) 1 = y
linha (h:t) k = linha t (k-1)


numerotirar :: Int -> Int -> Int -- numero de numeros utilizados numa linha e nas anteriores
numerotirar k d | k==0 = 0
                | k==0 && d==5 = 0
                | k==1 || k==(d-2) = (d-6) + numerotirar (k-1) d
                | k==2 || k==(d-3) = (d-2-(div d 2)-1) + numerotirar (k-1) d
                | even k = (d-(div d 2)-1) + numerotirar (k-1) d
                | odd k = (d-2) + numerotirar (k-1) d


dropar :: Int -> Int -> Int -> [Int] -- lista de int sem os numeros utilizados nas anteriores
dropar d s k | k==0 = numeros d s
             | k<=(d-1) = drop (numerotirar (k-1) d) (numeros d s)
             | otherwise = []

posicao :: String -> Char -> Int -- posicao de um k numa string
posicao (x:xs) k | k==x = 0
                 | otherwise = 1 + posicao xs k


tirarPrimeiro1 :: String -> String -- altera a primeira ocorrencia de '+'
tirarPrimeiro1 [] = []
tirarPrimeiro1 (x:xs) | x=='+' = '?':xs
                      | otherwise = x:tirarPrimeiro1 xs


tirarPrimeiro2 :: String -> String -- altera a primeira ocorrencia de '!'
tirarPrimeiro2 [] = []
tirarPrimeiro2 (x:xs) | x=='!' = '?':xs
                      | otherwise = x:tirarPrimeiro2 xs


darCoordLinha1 :: String -> String -> Int -> [String] -- l1 tem q ser igual a (x:xs)  || dá as coordenadas do '+' numa linha
darCoordLinha1 [] _ _ = []
darCoordLinha1 (x:xs) l1 k | x=='+' = ["+ "++show (posicao l1 x)++" "++show k]++darCoordLinha1 xs (tirarPrimeiro1 l1) k
                           | otherwise = darCoordLinha1 xs l1 k

darCoordLinha2 :: String -> String -> Int -> [String] -- l1 tem q ser igual a (x:xs)  || dá as coordenadas do '!' numa linha
darCoordLinha2 [] _ _ = []
darCoordLinha2 (x:xs) l1 k | x=='!' = ["! "++show (posicao l1 x)++" "++show k]++darCoordLinha2 xs (tirarPrimeiro2 l1) k
                           | otherwise = darCoordLinha2 xs l1 k


mapa1 :: Int -> Int -> [String] -- dá o mapa c os '!' e '+'
mapa1 d s  = aux50 0 d s 
        where aux50 k d s | k<=(d-1) = alterarLinha (linha (mapaSemN 0 d) k) (dropar d s k): aux50 (k+1) d s
                          | otherwise = []


darCoordMapa1 :: Int -> Int -> Int -> [String]  -- k==0  || dá as coordenadas dos '+' num mapa
darCoordMapa1 d s k | k<=(d-1) = darCoordLinha1 (linha (mapa1 d s) k) (linha (mapa1 d s) k) k++darCoordMapa1 d s (k+1)
                    | otherwise = []


darCoordMapa2 :: Int -> Int -> Int -> [String]  -- k==0  || dá as coordenadas dos '!' num mapa
darCoordMapa2 d s k | k<=(d-1) = darCoordLinha2 (linha (mapa1 d s) k) (linha (mapa1 d s) k) k++darCoordMapa2 d s (k+1)
                    | otherwise = []


substituiLinha :: String -> String -- retira os '!' e '+' duma linha
substituiLinha [] = []
substituiLinha (x:xs) | x=='+' = '?':substituiLinha xs
                      | x=='!' = '?':substituiLinha xs
                      | otherwise = x:substituiLinha xs


substituiMapa :: [String] -> [String] -- retira os '!' e '+' do mapa
substituiMapa [] = []
substituiMapa (x:xs) = substituiLinha x: substituiMapa xs


mapa :: Int -> Int -> [String]
mapa d s = substituiMapa (mapa1 d s)++darCoordMapa1 d s 0++darCoordMapa2 d s 0


-- TAREFA 2

tirarCoord :: [String] -> Int -> String 
tirarCoord [] _ = []
tirarCoord ((a:b):t) j | [a]==show j = (a:b)
                       | otherwise = tirarCoord t j
               
verificaChar :: String -> Bool
verificaChar [] = False
verificaChar (h:t) | h== '+' || h== '!' = True
                   | otherwise = verificaChar t

cChar :: String -> Int
cChar [] = 0
cChar (h:t) | h== '!' || h== '+' = 1 + cChar t
            | otherwise = cChar t

getCoord :: String -> [String]
getCoord []= []
getCoord (h:t) | (h <='9' && h>='0') && (t == []) = [show (digitToInt h)]
               | (h <='9' && h>='0') && (head t <='9' && head t>='0') = (show (digitToInt h) ++ show (digitToInt (head t) )) : getCoord (drop 1 t)
               | (h <='9' && h>='0') && (head t == ' ' || head t == '+' || head t == '!') = (show (digitToInt h)) : getCoord (drop 1 t)
               | (h == ' ' || h == '+' || h == '!') = getCoord t
               | otherwise = getCoord t 


tuplo:: [String] -> (Int,Int)
tuplo [x,y,z] = (read y, read z)


tuploPowers :: [Int] -> (Int,Int)
tuploPowers [x,y] = (x,y)


cMais :: String -> Int
cMais [] = 0
cMais (h:t) | h == '+' = 1 + cMais t
            | otherwise = cMais t


cFlames :: String -> Int
cFlames [] = 0
cFlames (h:t) | h == '!' = 1 + cFlames t
            | otherwise = cFlames t

tuploString :: Int -> (Int,Int) -> String
tuploString j (x,y) = show j ++ " " ++  show x ++ " " ++ show y 


tuploStringMaisRight :: Int -> (Int,Int) -> String
tuploStringMaisRight j (x,y) =  show j ++ " " ++ show (x+1) ++ " " ++ show y



andarR :: String -> Int -> Char -> String
andarR [] j m = []
andarR x j m | m == 'R' = tuploStringMaisRight j (tuplo (getCoord x ))
             | otherwise = []

tuploStringMaisLeft :: Int -> (Int,Int) -> String
tuploStringMaisLeft j (x,y) | x >1 = show j ++ " " ++ show (x-1) ++ " " ++ show y
                            | otherwise = show j ++ " " ++ show x ++ " " ++ show y


andarL :: String -> Int -> Char -> String
andarL [] j m = []
andarL x j m | m == 'L' = tuploStringMaisLeft j (tuplo (getCoord x ))
             | otherwise = []

tuploStringMaisDown :: Int -> (Int,Int) -> String
tuploStringMaisDown j (x,y) = show j ++ " " ++ show x ++ " " ++ show (y+1)

andarD :: String -> Int -> Char -> String
andarD [] j m = []
andarD x j m | m == 'D' = tuploStringMaisDown j (tuplo (getCoord x ))
             | otherwise = []


tuploStringMaisUp :: Int -> (Int,Int) -> String
tuploStringMaisUp j (x,y) | y>1 = show j ++ " " ++ show x ++ " " ++ show (y-1)
                          | otherwise = show j ++ " " ++ show x ++ " " ++ show y 


andarU :: String -> Int -> Char -> String
andarU [] j m = []
andarU x j m | m == 'U' = tuploStringMaisUp j (tuplo (getCoord x ))
             | otherwise = []


andar :: [String] -> Int-> Char -> (Int,Int)
andar [] j m = (0,0)
andar mapa j m | m == 'U' = tuplo (getCoord (andarU (tirarCoord mapa j) j 'U') ) 
               | m == 'D' = tuplo (getCoord (andarD (tirarCoord mapa j) j 'D') )
               | m == 'L' = tuplo (getCoord (andarL (tirarCoord mapa j) j 'L') )
               | m == 'R' = tuplo (getCoord (andarR (tirarCoord mapa j) j 'R') )           

tP :: [String] -> [(Int,Int)]
tP  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '#' || h== '?'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)


coordB :: [String] -> [(Int,Int)]
coordB [] = []
coordB ((a:b):t) | a=='+' = tuplosB ((a:b)) : coordB t
                 | otherwise = coordB t


tuploBF:: [String] -> (Int,Int)
tuploBF (x:y:xs) = (read x, read y)


tuplosB :: String -> (Int,Int)
tuplosB x = tuploBF (getCoord x)

tuploStringB :: (Int,Int) -> String
tuploStringB (x,y) = "+ " ++ show x ++ " " ++ show y 


coordF :: [String] -> [(Int,Int)]
coordF [] = []
coordF ((a:b):t) | a=='!' = tuplosF ((a:b)) : coordF t
                 | otherwise = coordF t


tuplosF :: String -> (Int,Int)
tuplosF x = tuploBF (getCoord x)

tuploStringF :: (Int,Int) -> String
tuploStringF (x,y) = "! " ++ show x ++ " " ++ show y 


tuploDeixarBomba :: Int -> (Int,Int) -> String
tuploDeixarBomba j (x,y) = "* " ++ show x ++ " " ++ show y ++ " " ++ show j  ++ " 1 " ++ "10" 


bIniciais :: [String] -> [(Int,Int)]
bIniciais [] = []
bIniciais (x:t) | head x=='*' = tuplosF (unwords (take 2(drop 1 (words x)))) : bIniciais t
                | otherwise = bIniciais t


listajogadores :: [String]  -> [String]
listajogadores [] = []
listajogadores (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = h : listajogadores t
                      | otherwise = listajogadores t


listapowerups :: [String]  -> [String]
listapowerups [] = []
listapowerups (h:t)  | head h == '+' || head h == '!' = h : listapowerups t
                      | otherwise = listapowerups t


cardinais :: [String]  -> [String]
cardinais [] = []
cardinais (h:t)  | head h == '#' = h : cardinais t
                | otherwise = cardinais t


bombas :: [String]  -> [String]
bombas [] = []
bombas (h:t)  | head h == '*' = h : bombas t
              | otherwise = bombas t


meterBomba :: [String] -> String -> Bool
meterBomba [] _ = True
meterBomba mapa novabomba | elem novabomba (bombas mapa) == False = True
                          | otherwise = False
            

contarB :: String  -> Int
contarB [] = 0
contarB (h:t)  | h == '+' = 1 + contarB t
               | otherwise = contarB t

contarF :: String  -> Int
contarF [] = 0
contarF (h:t)  | h == '!' = 1 + contarF t        
               | otherwise = contarF t


ifFlames :: [String] -> Int -> Bool
ifFlames [] j = False
ifFlames mapa j | contarF (tirarCoord mapa j) >=1 = True 
                | otherwise = False

tuploDeixarBombaF :: Int -> (Int,Int) -> Int -> String
tuploDeixarBombaF j (x,y) n = "* " ++ show x ++ " " ++ show y ++ " " ++ show j ++ " "  ++ show( n + 1)  ++ " 10" 


bIniciaisJ :: [String] -> Int -> Int
bIniciaisJ [] j = 0
bIniciaisJ (x:t) j | head x=='*' = 1 + bIniciaisJ t j
                   | otherwise = bIniciaisJ t j

bombasJogador :: String -> Int -> Bool
bombasJogador [] j = False
bombasJogador x j | elem (show j) (drop 3 ( take 4 (words x))) == True = True
                  | otherwise = False


bombasJogadorMapa :: [String] -> Int -> Int 
bombasJogadorMapa [] j = 0
bombasJogadorMapa (h:t) j | (bombasJogador h j == True) = 1 + bombasJogadorMapa t j
                        | otherwise = bombasJogadorMapa t j


verificaJogador :: Int -> [String] -> Bool
verificaJogador _ [] = False
verificaJogador j (h:t) | show j == (take 1 h) = True
                        | otherwise = verificaJogador j t


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


-- TAREFA 4

-- SUBTRAI UM INSTANTE DE TEMPO ÀS BOMBAS DE UM MAPA

instanteBomba :: Mapa -> [String]
instanteBomba [] = []
instanteBomba mapa = aux mapa [] where
                    aux [] new = []
                    aux ((h:t):ts) new | h == '*' && read(last (words t)) == 1 = aux ts ([(h:t)] ++ new) 
                                       | h == '*' && read(last (words t)) >1 && explodePB (tuplosBr (h:t)) (tB (ts++new)) ((h:t):ts) /= [] =  tempo1 (h:t) : aux ts new
                                       | h == '*' && read(last (words t)) >1 && explodePB (tuplosBr (h:t)) (tB ts) ((h:t):ts) == [] = menosUm (h:t) : aux ts new
                                       | otherwise = aux ts new

explodePB :: CoordenadaBomba -> Bombas-> Mapa -> Bombas   
explodePB  (rb1,xb1,yb1,tb1) [] _ = []
explodePB (rb1,xb1,yb1,tb1) ((r,xb,yb,tb):ts) mapa  | yb1 == yb && xb > xb1  && ((xb - r) <= xb1) && (esqPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)]
                                              | yb1 == yb && xb <=  xb1 && ((xb + r) >= xb1) && (dirPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)] 
                                              | xb1 == xb && yb <= yb1 && ((yb + r) >= yb1) && (cimaPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = [(rb1,xb1,yb1,tb1)] 
                                              | xb1 == xb && yb >  yb1  && ((yb -r) <= yb1) && (baixoPedra (xb1,yb1) (tPSemLimites (tOnlyP mapa) mapa))== False && tb ==1  = [(rb1,xb1,yb1,tb1)]
                                              | otherwise = explodePB (rb1,xb1,yb1,tb1) ts mapa 



-- recebe a string da bomba e subtrai 1 no tempo
menosUm:: String ->String
menosUm (h:t) = [h] ++ " " ++ unwords (take 4(words t)) ++ " " ++ show((read(last (words t)))-1)

tuplosBr :: String -> (Int,Int,Int,Int)
tuplosBr x = tuploRaio2 (getCoord x)

tuploRaio2:: [String] -> (Int,Int,Int,Int)
tuploRaio2 [x,y,j,r,t] = (read r,read x, read y,read t)

-- funcao que pega nas coordenadas das bombas que apanham fogo e o seu t passa para 1
tempo1 :: String -> String
tempo1 (h:t) = [h] ++ " " ++ unwords (take 4(words t)) ++ " " ++ "1"

-- pega no raio de uma bomba 
raioB :: String -> Int
raioB x =  read (unwords(take 1 (drop 4 (words x))))

-- dá os tuplos dos jogadores do genero [(jogador,x,y)]
tJ :: Mapa -> Jogadores
tJ  [] = []
tJ ((h:t):ts) | (h >='0' && h<='3') = tuploJ (getCoord (h:t)) : tJ ts
              | otherwise = tJ ts


-- dá os tuplos das bombas do genero [(raio,x,y)]

tuploRaio:: [String] -> CoordenadaBomba
tuploRaio [x,y,z,t] = (read z,read x, read y,read t)

-- procura as bombas e dá um triplo do genero [(raio,x,y)]   "* 1 1 0 1 10"
tB:: Mapa -> Bombas
tB [] = []
tB (x:t) | head x=='*' = tuploRaio ((take 2 (drop 1(words x)))++take 1( drop 4 (words x))++ (drop 5 (words x))) : tB t
         | otherwise = tB t

-- recebe o mapa, um jogador e se esse jogador estiver no raio de uma bomba morre
arrebentaBonecos :: Mapa -> NJogadores -> Mapa
arrebentaBonecos [] _ = []
arrebentaBonecos x [] = x 
arrebentaBonecos ((h:t):ts) (j:js) | intToDigit j /= h = (h:t) : arrebentaBonecos ts (j:js)
                                   | otherwise = arrebentaBonecos ts js


listajogadores3 :: Mapa -> NJogadores
listajogadores3 [] = []
listajogadores3 (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = digitToInt (head h) : listajogadores3 t
                       | otherwise = listajogadores3 t


-- recebe os triplos com os jogadores e os tuplos com as bombas e verifica se a bomba explode com o jogadore
explodeJ :: Jogadores -> Bombas -> Mapa -> NJogadores
explodeJ ((j,xj,yj):t) [] _ = []
explodeJ [] _ _ = []
explodeJ ((j,xj,yj):t) ((r,xb,yb,tb):ts) mapa | yj == yb && xb >  xj  && ((xb -r) <= xj) && (dirPedra   (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | yj == yb && xb <= xj  && ((xb +r) >= xj) && (esqPedra   (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | xj == xb && yb <= yj  && ((yb +r) >= yj) && (cimaPedra  (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | xj == xb && yb >  yj  && ((yb -r) <= yj) && (baixoPedra (xj,yj) (tPSemLimites (tP mapa) mapa))  == False && tb ==1  = j : explodeJ t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodeJ [(j,xj,yj)] ts mapa ++ explodeJ t ((r,xb,yb,tb):ts) mapa 



--coordenadas de todas as pedras exceto as limitantes do mapa
tPSemLimites :: PedrasEPI -> Mapa -> PedrasEPI
tPSemLimites  [] _ = []
tPSemLimites ((x,y):t) (h:ts) | (x== 0 || x== length h-1 || y ==0 || y == length h-1) = tPSemLimites t (h:ts)
                             | otherwise = (x,y): tPSemLimites t (h:ts)



-- recebe a posição do jogador/PU e a lista das posições das pedras e verifica se o jogador/PU tem uma pedra a proteger 

esqPedra :: CoordenadaJ -> PedrasEPI -> Bool
esqPedra x [] = False
esqPedra (x,y) ((h,t):ts) | (x-1,y) == (h,t) = True
                                | otherwise = esqPedra (x,y) ts
dirPedra :: CoordenadaJ -> PedrasEPI -> Bool
dirPedra x [] = False
dirPedra (x,y) ((h,t):ts) | (x+1,y) == (h,t) = True
                                | otherwise = dirPedra (x,y) ts
baixoPedra :: CoordenadaJ -> PedrasEPI -> Bool
baixoPedra x [] = False
baixoPedra (x,y) ((h,t):ts) | (x,y+1) == (h,t) = True
                                | otherwise = baixoPedra (x,y) ts
cimaPedra :: CoordenadaJ -> PedrasEPI -> Bool
cimaPedra x [] = False
cimaPedra (x,y) ((h,t):ts) | (x,y-1) == (h,t) = True
                              | otherwise = cimaPedra (x,y) ts

-- recebe a lista dos powerups, a lista das bombas com o raio, e dá as coordenadas de powerups que explodem
explodePU :: PowerUps -> Bombas -> Mapa -> PowerUps
explodePU  [] _ _= []   
explodePU  ((xp,yp):t) [] _ = []
explodePU ((xp,yp):t) ((r,xb,yb,tb):ts) mapa  | elem (xp,yp) (tI mapa) == False && yp == yb && xb >  xp && ((xb - r) <= xp)  && (dirPedra (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) && verificaCoordSeguinteX ((xp,yp):t) == False = (xp,yp) : explodePU t ((r,xb,yb,tb):ts) mapa
                                              | elem (xp,yp) (tI mapa) == False && yp == yb && xb <= xp && ((xb + r) >= xp) && (esqPedra (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xp,yp)] ++ explodePU (listaSRXb' t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | elem (xp,yp) (tI mapa) == False && xp == xb && yb <= yp && ((yb + r) >= yp) && (cimaPedra (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xp,yp)] ++ explodePU (listaSRYb' t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | elem (xp,yp) (tI mapa) == False && xp == xb && yb >  yp && ((yb -r) <= yp)  && (baixoPedra (xp,yp) (tPSemLimites (tOnlyP mapa) mapa))  == False && tb ==1 && verificaCoordSeguinteY ((xp,yp):t) == False = (xp,yp) : explodePU t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodePU [(xp,yp)] ts mapa ++ explodePU t ((r,xb,yb,tb):ts) mapa


                                            

arrebentaPU :: Mapa -> Mapa
arrebentaPU [] = [] 
arrebentaPU mapa  | auxVerify (coordPU mapa) (explodePU (coordPU mapa) (tB mapa) mapa) == True  = removePowerUps (explodePU (coordPU mapa) (tB mapa) mapa) mapa 
                  | otherwise = mapa
-- verifica se  um elemento de uma lista pertence a uma lista
auxVerify :: Eq a => [a] -> [a] -> Bool
auxVerify [] _ = False
auxVerify _ [] = False
auxVerify (h:t) (a:b) | elem h (a:b)==True = True
                      | otherwise = auxVerify t (a:b)
                     
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


-- verifica se há bombas para explodir (com t1) 

verBomba :: Mapa -> Bool
verBomba [] = False
verBomba ((h:t):ts) | h == '*' && read (unwords((drop 4 (words t)))) == 1 = True
                    | otherwise = verBomba ts


-- recebe a lista de coordenadas dos p.i. , a lista das bombas , o mapa e dá a lista dos p.i. que explodem.
explodePI :: PontosInt -> Bombas-> Mapa -> PontosInt
explodePI  [] _ _= []   
explodePI  ((xi,yi):t) [] _ = []
explodePI ((xi,yi):t) ((r,xb,yb,tb):ts) mapa  | yi == yb && xb > xi  && ((xb - r) <= xi) && (esqPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) && verificaCoordSeguinteX ((xi,yi):t) == False = (xi,yi) : explodePI t ((r,xb,yb,tb):ts) mapa
                                              | yi == yb && xb <=  xi && ((xb + r) >= xi) && (dirPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xi,yi)] ++ explodePI (listaSRXb' t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | xi == xb && yb <= yi && ((yb + r) >= yi) && (cimaPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))==False && (tb ==1) = ([(xi,yi)] ++ explodePI (listaSRYb' t (r,xb,yb,tb)) ((r,xb,yb,tb):ts) mapa)
                                              | xi == xb && yb >  yi  && ((yb -r) <= yi) && (baixoPedra (xi,yi) (tPSemLimites (tOnlyP mapa) mapa))== False && tb ==1 && verificaCoordSeguinteY (mesmoX ((xi,yi):t) xi) == False = (xi,yi) : explodePI t ((r,xb,yb,tb):ts) mapa
                                              | otherwise = explodePI [(xi,yi)] ts mapa ++ explodePI t ((r,xb,yb,tb):ts) mapa

-- Recebe uma Lista com Pontos de Interrogação e fica com os que estiverem na mesma linha.
mesmoX :: PontosInt -> Int -> PontosInt
mesmoX [] _ = []
mesmoX ((x,y):c) n | n == x = (x,y) : mesmoX c n
                   | otherwise = mesmoX c n

-- se a coordenada seguinte no mesmo Y, for um ponto de interrogação passa a frente a primeira
verificaCoordSeguinteX :: PontosInt -> Bool
verificaCoordSeguinteX ((x,y):[]) = False
verificaCoordSeguinteX ((x,y):(a,b):c) | b == y && a == x+1 = True
                                       | otherwise = False

-- se a coordenada seguinte no mesmo X, for um ponto de interrogação passa a frente a primeira
verificaCoordSeguinteY :: PontosInt -> Bool
verificaCoordSeguinteY ((x,y):[]) = False
verificaCoordSeguinteY ((x,y):(a,b):c) | a == x && b==y+1 = True
                                       | otherwise = False


tOnlyP :: Mapa -> Pedras
tOnlyP  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '#'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

--remove as coordenadas dos pontos de interrogação à direita que explodem ou estao à direita dos que explodem  (util para parar o efeito explosao)
listaSRXb' :: PontosInt -> CoordenadaBomba -> PontosInt
listaSRXb' [] _ = []
listaSRXb' ((x,y):t) (r,xb,yb,tb) | yb==y && x>=xb = listaSRXb' t (r,xb,yb,tb)
                              | otherwise = (x,y) : listaSRXb' t (r,xb,yb,tb)

--remove as coordenadas dos pontos de interrogação por baixo  de bombas que explodem ou estao por baixo  de p.i. que explodem  (util para parar o efeito explosao)
listaSRYb' :: PontosInt -> CoordenadaBomba -> PontosInt
listaSRYb' [] _ = []
listaSRYb' ((x,y):t) (r,xb,yb,tb) | xb==x && y>=yb = listaSRYb' t (r,xb,yb,tb)
                                  | otherwise = (x,y) : listaSRYb' t (r,xb,yb,tb)

--coordenadas dos pontos de interrogação.
tI :: Mapa -> PontosInt
tI  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '?'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)



arrebentaPI :: Mapa -> PontosInt -> Mapa
arrebentaPI [] _ = []
arrebentaPI x [] = x
arrebentaPI ((h:t):ts) ((x,y):c) = aux 0 0 ((h:t):ts) ((x,y):c) where
                                    aux x y [] _ = []
                                    aux x y a [] = a
                                    aux x y ((h:t):ts) ((a,b):c) | b == y  = arrPIStr (h:t) (mesmoY ((a,b):c) b) : aux x (y+1) ts (apagaUsadosY c y) 
                                                                 | otherwise = (h:t) : aux x (y+1) ts ((a,b):c)

arrPIStr ::  String -> PontosInt -> String
arrPIStr linha coord = aux 0  linha coord where
                        aux x  [] _ = []
                        aux x  a [] = a
                        aux x (h:t) ((a,b):c) | h== '?' && x==a = " " ++ aux (x+1) t c
                                              | otherwise =[h] ++ aux (x+1) t ((a,b):c)
-- apaga os pontos de interrogaçao usados na linha n
apagaUsadosY :: PontosInt -> Int -> PontosInt
apagaUsadosY [] _ = []
apagaUsadosY ((x,y):t) n | n == y = apagaUsadosY t n
                         | otherwise = (x,y) : apagaUsadosY t n

-- pega nos pontos de interrogaçao a explodir da mesma linha
mesmoY :: PontosInt -> Int -> PontosInt
mesmoY [] _ = []
mesmoY ((x,y):c) n | n == y = (x,y) : mesmoY c n
                   | otherwise = mesmoY c n

metePontoEspiral:: Mapa -> (Int,Int) -> Mapa
metePontoEspiral [] _ = []
metePontoEspiral ((h:t):ts) (x,y) = aux 0 0 ((h:t):ts) (x,y) where
                                    aux x y ((h:t):ts) (a,b) | b == y  = substituiPedra (h:t) (a,b) : ts 
                                                             | otherwise = (h:t) : aux x (y+1) ts (a,b)
substituiPedra ::  String -> (Int,Int) -> String
substituiPedra linha coord = aux 0  linha coord where
                        aux x  [] _ = []
                        aux x (h:t) (a,b) | x==a = "#" ++ t
                                          | otherwise =[h] ++ aux (x+1) t (a,b)

coordespiral :: Int  -> Int-> (Int,Int)
coordespiral k int   = aux 1 1 k int   where
                     aux x y k int  |  x<(k - 2) = (if (k -2)^2 == int 
                                                        then (x,y)
                                                        else aux (x+1) y k (int+1) ) 
                                     | x == (k  -2) && y < k -2 = (if (k -2)^2 == int
                                                                              then (x,y)
                                                                              else aux x (y+1) k (int +1) )
                                     | otherwise =  (espiralback x y k int)
                                     


tentandoespiral3 :: Int -> Int -> Int -> (Int,Int)
tentandoespiral3 v k int = adicionavoltas (coordespiral k int) v


espiral :: Int -> Int -> (Int,Int)
espiral k int = aux 0  k int where
                        aux y k int | int == 1 = (1+(f k), 1+(f k))
                                    | int > ((k^2) - 8*k + 16) = tentandoespiral3 y k int
                                    | int <= 9 = tentandoespiral3 ((f k)-1) 5 int
                                    | otherwise = aux (y+1) (k-2) int
-- nº voltas pela dimensao do mapa
f :: Int -> Int
f x | x==5 = 1
     | otherwise = 1 + f (x-2)


adicionavoltas :: (Int,Int) -> Int -> (Int,Int)
adicionavoltas (x,y) v | v ==0 = (x,y)
                       | otherwise = adicionavoltas (x+1,y+1) (v-1)

                                   

espiralback :: Int -> Int -> Int -> Int -> (Int,Int)
espiralback x y l int = aux x y l int where
                    aux x y l int  | x/=1 = if (l -2) ^2 == int
                                            then (x,y)
                                            else aux (x-1) y l (int + 1)
                                   | x == 1 = espiralup x y l int

espiralup :: Int -> Int -> Int -> Int -> (Int,Int)
espiralup x y l int = aux x y l int where
                  aux x y l int = if (l -2 ) ^2 == int
                                 then (x,y)
                                else aux x (y-1) l (int+1)




removeGeral :: (Int,Int) -> Mapa -> Mapa 
removeGeral  x [] = []
removeGeral (x,y) ((a:b):c) | ((a >= '0' && a <='3') || a =='*' || a=='+' || a=='!') && (tuploGeral (a:b)) == (x,y)  = removeGeral (x,y) c
                            | otherwise = (a:b) : removeGeral (x,y) c


tuploG :: [String] -> (Int,Int)
tuploG [x,y] = (read x, read y)
tuploG [x,y,a,b,c] = (read x, read y)
tuploG [j,x,y] =(read x, read y)

tuploGeral :: String -> (Int,Int)
tuploGeral x = tuploG (getCoord x)




avanca :: Mapa -> Int -> Mapa
avanca mapa x | x > ((length (head mapa)-2)^2) = if verBomba mapa == True  then cardinais (arrebentaPI mapa (explodePI (tI mapa) (tB mapa) mapa)) ++ listapowerups (arrebentaPU mapa) ++ instanteBomba mapa ++ listajogadores2 (arrebentaBonecos mapa (explodeJ (tJ mapa) (tB mapa) mapa))
               else  cardinais mapa ++ listapowerups mapa ++ instanteBomba mapa ++ listajogadores2 mapa
             

              | x <= ((length (head mapa)-2)^2) && x>=1 = if verBomba mapa == True  then cardinais (metePontoEspiral (arrebentaPI mapa (explodePI (tI mapa) (tB mapa) mapa)) (espiral (length (head mapa))x))  ++ listapowerups ((removeGeral  (espiral (length (head mapa))x)) (arrebentaPU mapa)) ++ instanteBomba (removeGeral (espiral (length (head mapa))x) mapa) ++ listajogadores2 ((removeGeral  (espiral (length (head mapa))x)) (arrebentaBonecos mapa (explodeJ (tJ mapa) (tB mapa) mapa)))
               else  cardinais (metePontoEspiral mapa (espiral (length (head mapa))x)) ++ listapowerups ((removeGeral  (espiral (length (head mapa))x)) mapa) ++ instanteBomba (removeGeral (espiral (length (head mapa))x)mapa) ++ listajogadores2 ((removeGeral  (espiral (length (head mapa))x)) mapa)
              | otherwise = mapa


-- TAREFA 6

--Coordenadas dos espaços vazios
tVazio :: Mapa -> [(Int,Int)]
tVazio  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== ' ' = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)
-- Verifica se as coordenadas vazias estao mesmo vazias (i.e. vai pegar nas coordenadas por baixo do mapa e eliminar da lista tVazio)
coordLivres :: Mapa -> [(Int,Int)]
coordLivres mapa = deletingLivres (coordRaios (tB mapa)) (igual (tVazio (cardinais mapa)) (bIniciais mapa))

coordLivres2 :: Mapa -> [(Int,Int)]
coordLivres2 mapa = (igual (tVazio (cardinais mapa)) (bIniciais mapa))
--                   coordRaios    coordLivres
deletingLivres :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
deletingLivres x [] = []
deletingLivres [] x = x 
deletingLivres ((x,y):xs) ((a,b):as) | elem (a,b) ((x,y):xs) ==True = deletingLivres xs as 
                                     | otherwise = (a,b) : deletingLivres ((x,y):xs) as 

coordRaios :: [(Int,Int,Int,Int)] -> [(Int,Int)]
coordRaios [] = []
coordRaios ((rb,xb,yb,tb):bs)  | rb ==1 = (xb,yb-1) : (xb-1,yb) : (xb+1,yb) : (xb,yb+1)  : coordRaios bs
                               | rb ==2 = (xb,yb-2) : (xb-2,yb) : (xb+2,yb) : (xb,yb+2)  : coordRaios bs
                               | rb ==3 = (xb,yb-3) : (xb-3,yb) : (xb+3,yb) : (xb,yb+3)  : coordRaios bs
igual :: [(Int,Int)] -> [(Int,Int)]-> [(Int,Int)]
igual [] _ = []
igual x [] = x
igual ((x,y):ys) ((a,b):bs) | (x,y) == (a,b) = igual ys bs
                            | otherwise = (x,y) : igual ys ((a,b):bs) 
--Coordendas dos jogadores
coordJ :: Mapa -> [(Int,Int)]
coordJ [] = []
coordJ ((a:b):t) | a=='0' || a =='1' || a=='2' || a=='3' = tuplosJxy ((a:b)) : coordJ t
                 | otherwise = coordJ t

-- "0 10 10" -> (10,10)
tuplosJxy :: String -> (Int,Int)
tuplosJxy x = tuploJ2 (getCoord x)

-- ["0","10","10"] -> (10,10)
tuploJ2 :: [String] -> (Int,Int)
tuploJ2 [j,x,y] =(read x, read y)

--Pode ir para a direita? - com raios
moveR :: Mapa -> (Int,Int) -> Bool
moveR mapa (x,y) | elem (x+1,y) (coordLivres mapa) = True
                 | otherwise = False

moveR2 :: Mapa -> (Int,Int) -> Bool
moveR2 mapa (x,y) | elem (x+1,y) (coordLivres2 mapa) = True
                 | otherwise = False

--Pode ir para a esquerda? - com raios
moveL :: Mapa -> (Int,Int) -> Bool
moveL mapa (x,y) | elem (x-1,y) (coordLivres mapa) = True
                 | otherwise = False

moveL2 :: Mapa -> (Int,Int) -> Bool
moveL2 mapa (x,y) | elem (x-1,y) (coordLivres2 mapa) = True
                 | otherwise = False

--Pode ir para cima? - com raios
moveU :: Mapa -> (Int,Int) -> Bool
moveU mapa (x,y) | elem (x,y-1) (coordLivres mapa) = True
                 | otherwise = False

moveU2 :: Mapa -> (Int,Int) -> Bool
moveU2 mapa (x,y) | elem (x,y-1) (coordLivres2 mapa) = True
                 | otherwise = False

--Pode ir para baixo? - com raios
moveD :: Mapa -> (Int,Int) -> Bool
moveD mapa (x,y) | elem (x,y+1) (coordLivres mapa) = True
                 | otherwise = False

moveD2 :: Mapa -> (Int,Int) -> Bool
moveD2 mapa (x,y) | elem (x,y+1) (coordLivres2 mapa) = True
                 | otherwise = False


-- Tem uma bomba no seu raio  ? Se sim, dá um par com um Char(Onde está a bomba) e um true 
estaNoRaio :: (Int,Int) -> [(Int,Int,Int,Int)] -> Mapa -> (Char,Bool)
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

emCimaDaBomba :: (Int,Int) -> [(Int,Int,Int,Int)] -> Bool
emCimaDaBomba (x,y) [] = False
emCimaDaBomba (x,y) ((rb,xb,yb,tb):bs) | (x,y) == (xb,yb) = True
                                      | otherwise = emCimaDaBomba (x,y) bs

--Tem caixas para explodir?
boom :: Mapa -> (Int,Int) -> Bool
boom mapa (x,y) | elem (x+1,y) (tI mapa) || elem (x-1,y) (tI mapa) || elem (x,y+1) (tI mapa) || elem (x,y-1) (tI mapa) = True
                | otherwise = False
                
tPlayers :: Mapa -> [(Int,Int)]
tPlayers  [] = []
tPlayers ((h:t):ts) | (h >='0' && h<='3') = tuplosJxy (h:t) : tPlayers ts
                    | otherwise = tPlayers ts

boomJ :: Mapa -> (Int,Int) -> Bool
boomJ mapa (x,y)| elem (x+1,y) (tPlayers mapa) || elem (x-1,y) (tPlayers mapa) || elem (x,y+1) (tPlayers mapa) || elem (x,y-1) (tPlayers mapa) || elem (x,y) (tPlayers mapa) = True
                | otherwise = False             
--(tuplosJxy (tirarCoord mapa j))
pegarnoX :: (Int,Int) -> Int
pegarnoX (x,y) = x

pegarnoY :: (Int,Int) -> Int
pegarnoY (x,y) = y

ver :: Mapa -> Int -> Bool
ver [] _ = False
ver (x:xs) j | head x == intToDigit j = True
             | otherwise = ver xs j

temSaidaR :: (Int,Int) -> Mapa -> Bool
temSaidaR (x,y) mapa | elem (x+2,y) (tI mapa) = False
                     | otherwise = True

temSaidaL :: (Int,Int) -> Mapa -> Bool
temSaidaL (x,y) mapa | elem (x-2,y) (tI mapa) = False
                     | otherwise = True
temSaidaD :: (Int,Int) -> Mapa -> Bool
temSaidaD (x,y) mapa | elem (x,y+2) (tI mapa) = False
                     | otherwise = True
temSaidaU :: (Int,Int) -> Mapa -> Bool
temSaidaU (x,y) mapa | elem (x,y-2) (tI mapa) = False
                     | otherwise = True


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

            
-- AUXILIARES

coordPU :: Mapa -> [(Int,Int)]
coordPU [] = []
coordPU ((a:b):t) | a=='!' || a =='+' = tuplosPU ((a:b)) : coordPU t
                 | otherwise = coordPU t

tuplosPU :: String -> (Int,Int)
tuplosPU x = tuploPU(getCoord x)

tuploJ :: [String] -> (Int,Int,Int)
tuploJ [j,x,y] =(read j, read x, read y)

listajogadores2 :: [String]  -> [String]
listajogadores2 [] = []
listajogadores2 (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = h : listajogadores2 t
                      | otherwise = listajogadores2 t

tuploPU:: [String] -> (Int,Int)
tuploPU [x,y] = (read x, read y)

coordPU1 :: [String] -> [(Float,Float)]
coordPU1 [] = []
coordPU1 ((a:b):t) | a=='!' || a =='+' = tuplosPU1 ((a:b)) : coordPU1 t
                 | otherwise = coordPU1 t

tuplosPU1 :: String -> (Float,Float)
tuplosPU1 x = tuploPU1 (getCoord x)

tuploPU1 :: [String] -> (Float,Float)
tuploPU1 [x,y] = (read x, read y)

tOnlyP1 :: [String] -> [(Float,Float)]
tOnlyP1  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '#'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

tI1 :: Mapa -> [(Float,Float)]
tI1  mapa  = aux 0 0 mapa where
          aux x y ([]:ts) = aux 0 (y+1) ts
          aux x y [] = []
          aux x y ((h:t):ts) | h== '?'  = (x,y) : aux (x+1) y (t:ts) 
                             | otherwise =  aux (x+1) y (t:ts)

separarJogX :: [String] -> Char -> String
separarJogX [] _ = []
separarJogX (x:xs) k | head x == k = x
                     | otherwise = separarJogX xs k

-- funções da tarefa 2 com Maybe Char em vez de Char

andar1 :: [String] -> Int-> Maybe Char -> (Int,Int)
andar1 [] j m = (0,0)
andar1 mapa j m | m == Just 'U' = tuplo (getCoord (andarU1 (tirarCoord mapa j) j (Just 'U')) ) 
               | m == Just 'D' = tuplo (getCoord (andarD1 (tirarCoord mapa j) j (Just 'D')) )
               | m == Just 'L' = tuplo (getCoord (andarL1 (tirarCoord mapa j) j (Just 'L')) )
               | m == Just 'R' = tuplo (getCoord (andarR1 (tirarCoord mapa j) j (Just 'R')) )

andarU1 :: String -> Int -> Maybe Char -> String
andarU1 [] j m = []
andarU1 x j m | m == Just 'U' = tuploStringMaisUp j (tuplo (getCoord x ))
              | otherwise = []

andarD1 :: String -> Int -> Maybe Char -> String
andarD1 [] j m = []
andarD1 x j m | m == Just 'D' = tuploStringMaisDown j (tuplo (getCoord x ))
             | otherwise = []

andarL1 :: String -> Int -> Maybe Char -> String
andarL1 [] j m = []
andarL1 x j m | m == Just 'L' = tuploStringMaisLeft j (tuplo (getCoord x ))
             | otherwise = []

andarR1 :: String -> Int -> Maybe Char -> String
andarR1 [] j m = []
andarR1 x j m | m == Just 'R' = tuploStringMaisRight j (tuplo (getCoord x ))
              | otherwise = []

move1 :: [String] -> Int -> Maybe Char -> [String]
move1 [] j m = []
move1 mapa j m  | m == Just 'B'  && (ifFlames mapa j == True) && bombasJogadorMapa mapa j <= contarB (tirarCoord mapa j)   = ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBombaF j (tuplo (getCoord ( tirarCoord mapa j))) (contarF (tirarCoord mapa j))) (bombas mapa)) ++ listajogadores mapa)
               | m == Just 'B' && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j) = (if elem (tuplo (getCoord ( tirarCoord mapa j))) (bIniciais mapa) == False  then ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBomba j (tuplo (getCoord ( tirarCoord mapa j)))) (bombas mapa)) ++ listajogadores mapa)   else  mapa)
               | m == Just 'B' && (bombasJogadorMapa mapa j) > contarB (tirarCoord mapa j) = mapa
               | m == Nothing = mapa
               | verificaJogador j (listajogadores mapa) == False = mapa  
               | elem (andar1 mapa j m) (tP mapa)== True = mapa 
               | elem (andar1 mapa j m) (coordB mapa) == True  = delete (tuploStringB (andar1 mapa j m))  (insert (tuploString j (andar1 mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ "+ "  ++ replicate (cFlames (tirarCoord mapa j)) '!'    )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar1 mapa j m) (coordF mapa) == True  = delete (tuploStringF (andar1 mapa j m))  (insert (tuploString j (andar1 mapa j m) ++ " " ++ replicate (cFlames (tirarCoord mapa j)) '!' ++"! " ++ replicate (cMais (tirarCoord mapa j)) '+' )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar1 mapa j m) (tP mapa)== False =  delete (tuploStringF (andar1 mapa j m))  (insert (tuploString j (andar1 mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ replicate (cFlames (tirarCoord mapa j)) '!' )  (delete (tirarCoord mapa j) mapa))       
               | otherwise = mapa
