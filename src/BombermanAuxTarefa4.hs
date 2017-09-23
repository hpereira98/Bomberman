module BombermanAuxTarefa4 where
  
import Data.Char 
import System.Environment
import Text.Read
import Data.List
import System.Random

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

--FUNÇÕES TAREFA 2
contarF :: String  -> Int
contarF [] = 0
contarF (h:t)  | h == '!' = 1 + contarF t        
               | otherwise = contarF t



tuploXY:: [String] -> (Int,Int)
tuploXY [x,y,z] = (read y, read z)

getCoord :: String -> [String]
getCoord []= []
getCoord (h:t) | (h <='9' && h>='0') && (t == []) = [show (digitToInt h)]
               | (h <='9' && h>='0') && (head t <='9' && head t>='0') = (show (digitToInt h) ++ show (digitToInt (head t) )) : getCoord (drop 1 t)
               | (h <='9' && h>='0') && (head t == ' ' || head t == '+' || head t == '!') = (show (digitToInt h)) : getCoord (drop 1 t)
               | (h == ' ' || h == '+' || h == '!') = getCoord t
               | otherwise = getCoord t 

listajogadores :: Mapa -> [(Int,Int)]
listajogadores [] = []
listajogadores (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = tuploXY (getCoord h) : listajogadores t
                      | otherwise = listajogadores t

listajogadores2 :: [String]  -> [String]
listajogadores2 [] = []
listajogadores2 (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = h : listajogadores2 t
                      | otherwise = listajogadores2 t


bombas :: Mapa  -> [String]
bombas [] = []
bombas (h:t)  | head h == '*' = h : bombas t
              | otherwise = bombas t



cardinais :: [String]  -> [String]
cardinais [] = []
cardinais (h:t)  | head h == '#' = h : cardinais t
                | otherwise = cardinais t


listapowerups :: [String]  -> [String]
listapowerups [] = []
listapowerups (h:t)  | head h == '+' || head h == '!' = h : listapowerups t
                      | otherwise = listapowerups t

coordPU :: [String] -> [(Int,Int)]
coordPU [] = []
coordPU ((a:b):t) | a=='!' || a =='+' = tuplosPU ((a:b)) : coordPU t
                 | otherwise = coordPU t

tuplosJxy :: String -> (Int,Int)
tuplosJxy x = tuploJ2 (getCoord x)

tuploJ2 :: [String] -> (Int,Int)
tuploJ2 [j,x,y] =(read x, read y)


tuplosPU :: String -> (Int,Int)
tuplosPU x = tuploPU(getCoord x)

tuplosB :: String -> (Int,Int)
tuplosB x = tuploB (getCoord x)

tuploB :: [String] -> (Int,Int)
tuploB [x,y,a,b,c] = (read x, read y)

tuploPU:: [String] -> (Int,Int)
tuploPU [x,y] = (read x, read y)

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


tuplo:: [String] -> (Int,Int)
tuplo [x,y,z] = (read y, read z)

tuploJ :: [String] -> (Int,Int,Int)
tuploJ [j,x,y] =(read j, read x, read y)



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
tuploBF [x,y] = (read x, read y)


tuploStringB :: (Int,Int) -> String
tuploStringB (x,y) = "+ " ++ show x ++ " " ++ show y 

coordF :: [String] -> [(Int,Int)]
coordF [] = []
coordF ((a:b):t) | a=='!' = tuplosF ((a:b)) : coordF t
                 | otherwise = coordF t

tuplosF :: String -> (Int,Int)
tuplosF x = tuploBF(getCoord x)

tuploStringF :: (Int,Int) -> String
tuploStringF (x,y) = "! " ++ show x ++ " " ++ show y 

tuploDeixarBomba :: Int -> (Int,Int) -> String
tuploDeixarBomba j (x,y) = "* " ++ show x ++ " " ++ show y ++ " " ++ show j  ++ " 1 " ++ "10" 

bIniciais :: [String] -> [(Int,Int)]
bIniciais [] = []
bIniciais (x:t) | head x=='*' = tuplosF (unwords (take 2(drop 1 (words x)))) : bIniciais t
                | otherwise = bIniciais t

meterBomba :: [String] -> String -> Bool
meterBomba [] _ = True
meterBomba mapa novabomba | elem novabomba (bombas mapa) == False = True
                          | otherwise = False
            

contarB :: String  -> Int
contarB [] = 0
contarB (h:t)  | h == '+' = 1 + contarB t
               | otherwise = contarB t

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
listajogadores4 :: [String]  -> [String]
listajogadores4 [] = []
listajogadores4 (h:t)  | head h == '0' || head h == '1' || head h == '2' || head h == '3' = h : listajogadores4 t
                      | otherwise = listajogadores4 t

move :: [String] -> Int -> Char -> [String]
move [] j m = []
move mapa j m  | m == 'B'  && (ifFlames mapa j == True) && bombasJogadorMapa mapa j <= contarB (tirarCoord mapa j)   = ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBombaF j (tuplo (getCoord ( tirarCoord mapa j))) (contarF (tirarCoord mapa j))) (bombas mapa)) ++ listajogadores4 mapa)
               | m == 'B' && (bombasJogadorMapa mapa j) <= contarB (tirarCoord mapa j) = (if elem (tuplo (getCoord ( tirarCoord mapa j))) (bIniciais mapa) == False  then ((cardinais mapa) ++ (listapowerups mapa) ++ (insert (tuploDeixarBomba j (tuplo (getCoord ( tirarCoord mapa j)))) (bombas mapa)) ++ listajogadores4 mapa)   else  mapa)
               | m == 'B' && (bombasJogadorMapa mapa j) > contarB (tirarCoord mapa j) = mapa
               | verificaJogador j (listajogadores4 mapa) == False = mapa  
               | elem (andar mapa j m) (tP mapa)== True = mapa 
               | elem (andar mapa j m) (coordB mapa) == True  = delete (tuploStringB (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ "+ "  ++ replicate (cFlames (tirarCoord mapa j)) '!'    )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar mapa j m) (coordF mapa) == True  = delete (tuploStringF (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cFlames (tirarCoord mapa j)) '!' ++"! " ++ replicate (cMais (tirarCoord mapa j)) '+' )  (delete (tirarCoord mapa j) mapa)) 
               | elem (andar mapa j m) (tP mapa)== False =  delete (tuploStringF (andar mapa j m))  (insert (tuploString j (andar mapa j m) ++ " " ++ replicate (cMais (tirarCoord mapa j)) '+' ++ replicate (cFlames (tirarCoord mapa j)) '!' )  (delete (tirarCoord mapa j) mapa))       
               | otherwise = mapa



-- FUNÇÕES DA TAREFA 1

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
