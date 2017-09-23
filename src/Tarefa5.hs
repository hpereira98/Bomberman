{-|
Module: Main

Description : Módulo Haskell com funções para gerar os gráficos do jogo Bomberman

Copyright: Henrique Manuel Palmeira Pereira <a80261@alunos.uminho.pt>
           Ricardo Filipe Sousa Caçador <a81064@alunos.uminho.pt>

Um módulo contendo definições Haskell para o cálculo de funções que permitam transformar os mapas do jogo Bomberman em gráficos que nos permitam
realizar ações, isto é, que nos permitam jogar. Para isso, foi utilizada a biblioteca Gloss.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Char
import Bomberman

{-|
@Estado@ é um tipo de dados por nós definido, que consiste num tuplo constituído por uma lista de strings (mapa do jogo), por três boleanos (que nos
dão informações sobre o estado de jogo, isto é, o primeiro diz-nos se o jogo está no ecrã inicial ou não, o segundo diz-nos se está no ecrã de
selecionar o número de jogadores e o terceiro diz-nos se está no ecrã de seleção da dificuldade), por um inteiro (semente do mapa, que será gerada
aleatoriamente), pelas imagens em formato bitmap que serão utilizadas no jogo (tijolo, caixa, power-ups, bomba e explosão), por um segundo inteiro 
(número de jogadores escolhido), por uma lista de inteiros (lista de jogadores que estão vivos), por um Float (número de segundos que passaram desde
o ínicio do jogo) e por quatro caracteres (posição em que se encontra o boneco no jogo).

-}
type Estado = ([String],Bool,Bool,Bool,Picture,Picture,Picture,Int,Picture,Picture,Int,Picture,Picture,Picture,[Int],Float,Picture,Char,Char,Char,Char,Picture) -- 1º Bool: true - ecra inicial | false - n esta no e.i.
                                                                    -- 2º Bool: true - escolher nº  | false - n esta a escolher o nº
                                                                    -- 3º Bool: true - dificuldade  | false - n está a escolher dif.
                                                                    -- Int: seed do mapa
                                                                    -- Picture: tijolo
                                                                    -- Picture: caixa
                                                                    -- Int: nº jogadores
                                                                    -- Picture: pu flames
                                                                    -- Picture: pu bombs
                                                                    -- Picture: bomba
                                                                    -- [Int]: lista de jogadores vivos
                                                                    -- Float: segundos q passaram
                                                                    -- Picture: explosao
                                                                    -- Char: lado do boneco 0
                                                                    -- Char: lado boneco 1
                                                                    -- Char: lado boneco 2
                                                                    -- Char: lado boneco 3

{-|
A função 'estadoInicial' vai receber todas as imagens em formato bitmap utilizadas no jogo e a semente e vai devolver um @Estado@ correspondente ao ecrã
inicial do jogo.

==Por exemplo:
>>> estadoInicial p n d 1 t c f b bomba exp
([],True,False,False,p,n,d,1,t,c,0,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f')

-}

estadoInicial :: Picture -> Picture -> Picture -> Int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
estadoInicial p n d s t c f b bomba exp fundo = ([],True,False,False,p,n,d,s,t,c,0,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f',fundo)

{-|
A função 'escolherNumero' vai receber todas as imagens em formato bitmap utilizadas no jogo e a semente e vai devolver um @Estado@ correspondente ao ecrã
de seleção do número de jogadores.

==Por exemplo:
>>> escolherNumero p n d 1 t c f b bomba exp
([],False,True,False,p,n,d,1,t,c,0,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f')

-}

escolherNumero :: Picture -> Picture -> Picture -> Int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
escolherNumero p n d s t c f b bomba exp fundo = ([],False,True,False,p,n,d,s,t,c,0,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f',fundo)

{-|
A função 'escolherDificuldade' vai receber todas as imagens em formato bitmap utilizadas no jogo, a semente e o número de jogadores escolhido
e vai devolver um @Estado@ correspondente ao ecrã de seleção do número de jogadores.

==Por exemplo:
>>> escolherDificuldade p n d 1 t c 3 f b bomba exp
([],False,False,True,p,n,d,1,t,c,3,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f')

-}

escolherDificuldade :: Picture -> Picture -> Picture -> Int -> Picture -> Picture -> Int -> Picture -> Picture -> Picture -> Picture -> Picture -> Estado
escolherDificuldade p n d s t c num f b bomba exp fundo = ([],False,False,True,p,n,d,s,t,c,num,f,b,bomba,[0,1,2,3],0,exp,'f','f','f','f',fundo)

{-|
A função 'tuploGloss' recebe uma lista com duas strings e vai devolver um tuplo constituído pelos números representados nessas strings.

==Por exemplo:
>>> tuploGloss ["1","10"]
(1,10)

-}

tuploGloss:: [String] -> (Float,Float)
tuploGloss [x,y] = (read x, read y)

{-|
A função 'tuplosGloss' recebe uma string com as informações do jogo relativas aos jogadores ou aos power-ups e devolve um tuplo com as suas
coordenadas, recorrendo às funções 'getCoord' e 'tuploGloss'.

==Por exemplo:
>>> tuplosGloss "0 1 10"
(1,10)

>>> tuplosGloss "+ 1 1"
(1,1)

-}

tuplosGloss :: String -> (Float,Float)
tuplosGloss x = tuploGloss (getCoord x)

{-|
A função 'coordTudo' recebe um estado de jogo e um caracter e devolve uma lista de tuplos com as coordenadas a que o caracter diz respeito, isto é,
'+' corresponde aos power-ups Bombs, '!' aos power-ups Flames,'*' às bombas, '0' ao jogador 0, '1' ao jogador 1, etc.

=Propriedade:

prop> coordTudo [] _ = []

==Por exemplo:
>>> coordTudo ["#####","#   #","# # #","#   #","#####","0 1 1","1 2 1"] '0'
[(1.0,1.0)]

>>> coordTudo ["#####","#   #","# # #","#   #","#####","0 1 1","1 2 1"] '1'
[(2.0,1.0)]

-}

coordTudo :: [String] -> Char -> [(Float,Float)]
coordTudo [] _ = []
coordTudo (x:t) k | head x==k = tuplosGloss (unwords (take 2 (drop 1 (words x)))) : coordTudo t k
                  | otherwise = coordTudo t k

{-|
A função 'porJogador' recebe a lista de coordenadas de um determinado jogador, definido pelo caracter inserido, a dimensão do mapa, o número
de segundos que passaram desde o início do jogo e a posição do 'boneco' e vai colocar o boneco desse jogador nos gráficos do mapa.

=Propriedade:

prop> porJogador [] _ _ _ _ = Blank

-}

porJogador :: [(Float,Float)] -> Char -> Int -> Float -> Char -> Picture
porJogador [] _ _ _ _ = Blank
porJogador ((x,y):xs) j d t l | d==7 = (Translate (-357.5+(85*x)) (257.5-(85*y)) $ Scale (0.40) (0.40) $ boneco (read [j]) t l)
                              | d==13 = (Translate (-377+(46*x)) (277-(46*y)) $ Scale (0.25) (0.25) $ boneco (read [j]) t l)
                              | d==17 = (Translate (-382.5+(35*x)) (282.5-(35*y)) $ Scale (0.20) (0.20) $ boneco (read [j]) t l)
                              | otherwise = Blank


{-|
A função 'porPU' vai funcionar da mesma maneira que a 'porJogador', mas em vez de colocar os jogadores nos gráficos, vai colocar os power-ups,
recebendo para isso as coordenadas dos power-ups, a dimensão do mapa e a imagem que lhe corresponde.

=Propriedade:

prop> porPU [] _ _ = Blank

-}

porPU :: [(Float,Float)] -> Int -> Picture -> Picture
porPU [] _ _ = Blank
porPU ((x,y):xs) d i | d==7 = Pictures [(Translate (-357.5+(85*x)) (257.5-(85*y)) $ Scale (0.33) (0.33) $ i),porPU xs d i]
                     | d==13 = Pictures [(Translate (-377+(46*x)) (277-(46*y)) $ Scale (0.18) (0.18) $ i),porPU xs d i]
                     | d==17 = Pictures [(Translate (-382.5+(35*x)) (282.5-(35*y)) $ Scale (0.14) (0.14) $ i),porPU xs d i]
                     | otherwise = Blank

{-|
A função 'genLinha', recebendo uma linha do mapa, a sua dimensão, a posição de cada caracter na string, o número da linha e as imagens correspondentes
aos tijolos e às pedras, vai devolver os gráficos que dizem respeito a essa linha do mapa.

=Propriedade:

prop> genLinha [] _ _ _ _ _ = Blank

-}

genLinha :: String -> Int -> Float -> Float -> Picture -> Picture -> Picture -- posicao comeca no 0
genLinha [] _ _ _ _ _ = Blank
genLinha (x:xs) d p l t c | d==7 = if x=='#' then Pictures [(Translate (-357.5+(85*p)) (257.5-(85*l)) $ Scale (0.33) (0.33) $ t),genLinha xs d (p+1) l t c]
                                                       else if x=='?' then Pictures [(Translate (-357.5+(85*p)) (257.5-(85*l)) $ Scale (0.33) (0.33) $ c),genLinha xs d (p+1) l t c]
                                                                      else Pictures [(Translate (-357.5+(85*p)) (257.5-(85*l)) $ Blank),genLinha xs d (p+1) l t c]
                          | d==13 = if x=='#' then Pictures [(Translate (-377+(46*p)) (277-(46*l)) $ Scale (0.18) (0.18) $ t),genLinha xs d (p+1) l t c]
                                                        else if x=='?' then Pictures [(Translate (-377+(46*p)) (277-(46*l)) $ Scale (0.18) (0.18) $ c),genLinha xs d (p+1) l t c]
                                                                       else Pictures [(Translate (-377+(46*p)) (277-(46*l)) $ Blank),genLinha xs d (p+1) l t c]
                          | d==17 = if x=='#' then Pictures [(Translate (-382.5+(35*p)) (282.5-(35*l)) $ Scale (0.14) (0.14) $ t),genLinha xs d (p+1) l t c]
                                                        else if x=='?' then Pictures [(Translate (-382.5+(35*p)) (282.5-(35*l)) $ Scale (0.14) (0.14) $ c),genLinha xs d (p+1) l t c]
                                                                       else Pictures [(Translate (-382.5+(35*p)) (282.5-(35*l)) $ Blank),genLinha xs d (p+1) l t c]
                          | otherwise = Blank

{-|
A função 'genMapa', recebendo um mapa, a sua dimensão, o número da linha e as imagens que correspondem aos tijolos e às caixas, vai aplicar a 'genLinha'
a todo o mapa.

=Propriedade:
 
prop> genMapa [] _ _ _ _ = Blank

-}

genMapa :: [String] -> Int -> Float -> Picture -> Picture -> Picture
genMapa [] _ _ _ _ = Blank
genMapa (x:xs) d l t c | round (l+1) <= length x = Pictures [genLinha x d 0 l t c,genMapa xs d (l+1) t c]
                       | otherwise = Blank

-- bonecos

{-|
A função 'corpo' recebe o número correspondente a cada jogador (0,1,2 ou 3) e a posição em que o boneco se encontra (em forma de caracter: 'f' = frente,
't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o corpo do boneco.

-}

corpo :: Int -> Char -> Picture
corpo j l | l=='f' = (Color (dim (cor j)) $ rectangleSolid 90 90)
          | l=='t' = (Color (dim (cor j)) $ rectangleSolid 90 90)
          | otherwise = (Color (dim (cor j)) $ rectangleSolid 60 90)

{-|
A função 'cabeca' recebe a posição em que o boneco se encontra (em forma de caracter: 'f' = frente,
't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o cabeça do boneco.

-}

cabeca :: Char -> Picture
cabeca l | l=='f' || l=='t' = Translate 0 63 (Color (light (light rose)) $ rectangleSolid 45 45)
         | otherwise = Translate 0 63 (Color (light (light rose)) $ rectangleSolid 30 45)

{-|
A função 'olhoE' vai imprimir no ecrã de jogo o olho esquerdo do boneco.

-}

olhoE :: Picture
olhoE = Pictures [Translate (-12) 69 (Color white $ rectangleSolid 12 12),Translate (-12) 69 (Color black $ rectangleSolid 6 6)]

{-|
A função 'olhoD' vai imprimir no ecrã de jogo o olho direito do boneco.

-}

olhoD :: Picture
olhoD = Pictures [Translate 12 69 (Color white $ rectangleSolid 12 12),Translate 12 69 (Color black $ rectangleSolid 6 6)]

{-|
A função 'boca' vai imprimir no ecrã de jogo a boca do boneco.

-}

boca :: Picture
boca = Pictures [Translate 0 51 (Color black $ rectangleSolid 30 12),Translate 0 51 (Color red $ rectangleSolid 18 6)]

{-|
A função 'cabelo' recebe o número correspondente a cada jogador (0,1,2 ou 3) e a posição em que o boneco se encontra (em forma de caracter: 'f' = frente,
't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o cabelo do boneco.

-}

cabelo :: Int -> Char -> Picture
cabelo j l | j==0 = if l=='l' then Translate 0 (67.5) (Color (dark orange) $ rectangleSolid 6 36)
                              else Pictures [Rotate (-30) $ Translate 60 54 (Color (dark orange) $ rectangleSolid 6 36),Rotate 30 $ Translate (-60) 54 (Color (dark orange) $ rectangleSolid 6 36)]
           | otherwise = Blank

{-|
A função 'bigode' recebe o número correspondente a cada jogador (0,1,2 ou 3) e vai imprimir no ecrã de jogo o corpo do boneco.

-}

bigode :: Int -> Picture
bigode j | j==0 = Pictures [Translate 0 60 (Color (dark orange) $ rectangleSolid 36 6),Translate 0 30 (Color (dark orange) $ rectangleSolid 36 30),Translate (-18) 48 (Color (dark orange) $ rectangleSolid 6 24),Translate 18 48 (Color (dark orange) $ rectangleSolid 6 24)]
         | j==1 = Pictures [Translate 0 60 (Color black $ rectangleSolid 36 4),Translate 0 42 (Color black $ rectangleSolid 36 4),Translate (-18) 51 (Color black $ rectangleSolid 4 22),Translate 18 51 (Color black $ rectangleSolid 4 22)]
         | j==2 = Blank
         | j==3 = Pictures [Translate 0 60 (Color black $ rectangleSolid 36 3),Translate 0 45 $ Rotate 180 $ (Color yellow $ thickArc 360 180 23 3),Translate 0 39 (Color black $ rectangleSolid 3 12)]

{-|
A função 'sobrancelhaE' vai imprimir no ecrã de jogo a sobrancelha esquerda do boneco.

-}

sobrancelhaE :: Picture
sobrancelhaE = Translate (-12) 78 (Color black $ rectangleSolid 12 3)

{-|
A função 'sobrancelhaD' vai imprimir no ecrã de jogo a sobrancelha direita do boneco.

-}

sobrancelhaD :: Picture
sobrancelhaD = Translate 12 78 (Color black $ rectangleSolid 12 3)

{-|
A função 'bracoE' recebe o número de segundos que passaram desde o início do jogo e a posição em que o boneco se encontra 
(em forma de caracter: 'f' = frente, 't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o braço esquerdo do boneco.

-}

bracoE :: Float -> Char -> Picture
bracoE  t l | l=='l' = if even (round (t*(1/3))) then Translate 0 25 (Color (light (light rose)) $ rectangleSolid 12 15)
                                                 else Translate 0 10 (Color (light (light rose)) $ rectangleSolid 12 45)
            | otherwise = if even (round (t*(1/3))) then Translate (-67) 30 (Color (light (light rose)) $ rectangleSolid 45 12)
                                                    else Rotate (-30) $ Translate (-42) 51 (Color (light (light rose)) $ rectangleSolid 45 12)

{-|
A função 'bracoD' recebe o número de segundos que passaram desde o início do jogo e vai imprimir no ecrã de jogo o braço direito do boneco.

-}

bracoD :: Float -> Picture
bracoD t | even (round (t*(1/3))) = Translate 66 30 (Color (light (light rose)) $ rectangleSolid 45 12)
         | odd (round (t*(1/3))) = Rotate 30 $ Translate 42 51 (Color (light (light rose)) $ rectangleSolid 45 12)

{-|
A função 'pernaE' recebe o número correspondente a cada jogador (0,1,2 ou 3) e a posição em que o boneco se encontra (em forma de caracter: 'f' = frente,
't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo a perna esquerda do boneco.

-}

pernaE :: Int -> Char -> Picture
pernaE j l | l=='l' =  Translate 0 (-66) (Color (light (light rose)) $ rectangleSolid 12 45)
           | otherwise = Translate (-24) (-66) (Color (light (light rose)) $ rectangleSolid 12 45)

{-|
A função 'pernaD' recebe o número correspondente a cada jogador (0,1,2 ou 3) e vai imprimir no ecrã de jogo a perna direita do boneco.

-}

pernaD :: Int -> Picture
pernaD j = Translate 24 (-66) (Color (light (light rose)) $ rectangleSolid 12 45)

{-|
A função 'chapeu' recebe o número correspondente a cada jogador (0,1,2 ou 3) e a posição em que o boneco se encontra (em forma de caracter: 'f' = frente,
't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o chapéu do boneco.

-}

chapeu :: Int -> Char -> Picture
chapeu j l | j==0 = if l=='l' then Translate 0 83 (Color (dim (cor j)) $ circleSolid (15))
                              else Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate 0 88 (Color (dim (cor j)) $ rectangleSolid 72 6)]
           | j==1 = if l=='l' then Translate 0 83 (Color (dim (cor j)) $ circleSolid (15))
                              else Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate 0 87 (Color (dim (dark red)) $ rectangleSolid 46 6)]
           | j==3 = if l=='f' then Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate (-30) 88 (Color (dim (cor j)) $ rectangleSolid 18 6)]
                              else if l=='t' then Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate 30 88 (Color (dim (cor j)) $ rectangleSolid 18 6)]
                                             else Translate 0 83 (Color (dim (cor j)) $ circleSolid (15))
           | j==2 = if l=='f' then Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate 30 87 (Color (dim (cor j)) $ rectangleSolid 18 6)]
                              else if l=='t' then Pictures [Translate 0 90 (Color (dim (cor j)) $ circleSolid (23)),Translate (-30) 87 (Color (dim (cor j)) $ rectangleSolid 18 6)]
                                             else Translate 0 83 (Color (dim (cor j)) $ circleSolid (15))

{-|
A função 'cor' recebe o número correspondente a cada jogador (0,1,2 ou 3) e vai devolver a cor de roupa que corresponde a cada boneco.

-}

cor :: Int -> Color
cor x | x==0 = black
      | x==1 = red
      | x==2 = dark blue
      | x==3 = greyN (0.60)

{-|
A função 'boneco' recebe o número correspondente a cada jogador (0,1,2 ou 3), o número de segundos que passaram desde o início do jogo 
e a posição em que o boneco se encontra (em forma de caracter: 'f' = frente, 't' = trás e 'l' = lado) e vai imprimir no ecrã de jogo o boneco completo.

-}

boneco :: Int -> Float -> Char -> Picture
boneco j t l | l=='f' = Pictures [bracoD t,bracoE t l,pernaD j,pernaE j l,corpo j l,cabelo j l,chapeu j l,cabeca l,olhoD,olhoE,boca,bigode j,sobrancelhaD,sobrancelhaE]
             | l=='t' = Pictures [bracoD t,bracoE t l,pernaD j,pernaE j l,corpo j l,cabelo j l,chapeu j l,cabeca l]
             | otherwise = Pictures [pernaE j l,corpo j l,bracoE t l,chapeu j l,cabeca l,cabelo j l]

--

{-|
A função 'tempo', recebendo um mapa e o número de segundos que passaram desde o início do jogo, vai devolver o número de segundos que faltam para acabar
o jogo.

-}

tempo :: [String] -> Float -> Int
tempo m tmp | (length (head m)) ==7 = round (36 - (1/5)*tmp)
            | (length (head m)) ==13 = round (60 - (1/5)*tmp)
            | (length (head m)) ==17 = round (140 - (1/5)*tmp)
            | otherwise = 0

{-|
A função 'vivo' recebe a lista de jogadores que se encontram vivos, o número correspondente a um jogador e três coordenadas que irão ser utilizadas
para mover as imagens dentro do painel lateral de forma a ficar tudo alinhado, devolvendo uma imagem que nos diz se o jogador selecionado está vivo
ou morto no jogo.

=Propriedade:
 
prop> vivo [] _ _ _ _ = Blank

-}

vivo :: [Int] -> Int -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Picture
vivo [] _ _ _ _ = Blank
vivo l jog (x,y) (a,b) (w,z) = if elem jog l then Pictures [Translate x y $ Scale (0.3) (0.3) $ boneco jog 0 'f', Translate a b $ Scale (0.2) (0.2) $ text ("Jogador "++show (jog+1))]
                                             else Pictures [Translate w z $ Color black $ rectangleSolid 200 75, Translate (w-85) (z-20) $ Scale (0.4) (0.4) $ Color red $ text "MORTO"]

{-|
A função 'vivoBot' irá funcionar da mesma maneira que a 'vivo', mas desta vez aplicando as informações aos jogadores que são bots.

=Propriedade:
 
prop> vivoBot [] _ _ _ _ = Blank

-}

vivoBot :: [Int] -> Int -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Picture
vivoBot [] _ _ _ _ = Blank
vivoBot l jog (x,y) (a,b) (w,z) = if elem jog l then Pictures [Translate x y $ Scale (0.3) (0.3) $ boneco jog 0 'f', Translate a b $ Scale (0.2) (0.2) $ text "Bot"]
                                                else Pictures [Translate w z $ Color black $ rectangleSolid 200 75, Translate (w-85) (z-20) $ Scale (0.4) (0.4) $ Color red $ text "MORTO"]

{-|
A função 'puBApanhados', recebendo uma linha do mapa com as informações de um determinado jogador, um par de coordenadas e a imagem correspondete ao
power-up Bombs, vai devolver uma imagem que lista o número de power-ups Bombs que esse jogador apanhou.

=Propriedade:
 
prop> puBApanhados [] _ _ = Blank

-}

puBApanhados :: String -> (Float,Float) -> Picture -> Picture
puBApanhados [] _ _ = Blank
puBApanhados (h:t) (x,y) b | h=='+' = Pictures [Translate x y $ Scale (0.1) (0.1) $ b,puBApanhados t (x+10,y) b]
                           | otherwise = puBApanhados t (x,y) b

{-|
A função 'puFApanhados' vai atuar da mesma maneira que a 'puBApanhados', mas aplicando-se desta vez aos power-up Flames.

=Propriedade:
 
prop> puFApanhados [] _ _ = Blank

-}

puFApanhados :: String -> (Float,Float) -> Picture -> Picture
puFApanhados [] _ _ = Blank
puFApanhados (h:t) (x,y) f | h=='!' = Pictures [Translate x y $ Scale (0.1) (0.1) $ f,puFApanhados t (x+7,y) f]
                           | otherwise = puFApanhados t (x,y) f

{-|
A função 'painelLateral', recebendo um @Estado@ de jogo, vai devolver uma imagem com todas as informações adicionais do jogo, localizando-se no lado
esquerdo do ecrã de jogo. Estas informações consistem no tempo (em segundos) que falta para o jogo terminar, nos jogadores que estão vivos os mortos,
se são bots ou não e quais os power-ups que apanharam.

-}

painelLateral :: Estado -> Picture
painelLateral (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | num==1 = Pictures [Translate 300 0 $ Color yellow $ rectangleSolid 200 600,
                                                                Translate 300 250 $ Color black $ rectangleSolid 200 50,
                                                                Translate 275 240 $ Color yellow $ Scale (0.2) (0.2) $ text (show (tempo m tmp)),
                                                                vivo jog 0 (235,175) (265,170) (300,175),
                                                                vivoBot jog 1 (235,50) (265,45) (300,50),
                                                                vivoBot jog 2 (235,-75) (265,-80) (300,-75),
                                                                vivoBot jog 3 (235,-200) (265,-205) (300,-200),
                                                                puBApanhados (separarJogX m '0') (265,150) b,
                                                                puBApanhados (separarJogX m '1') (265,20) b,
                                                                puBApanhados (separarJogX m '2') (265,-100) b,
                                                                puBApanhados (separarJogX m '3') (265,-225) b,
                                                                puFApanhados (separarJogX m '0') (330,150) f,
                                                                puFApanhados (separarJogX m '1') (330,20) f,
                                                                puFApanhados (separarJogX m '2') (330,-100) f,
                                                                puFApanhados (separarJogX m '3') (330,-225) f]
                                                                      | num==2 = Pictures [Translate 300 0 $ Color yellow $ rectangleSolid 200 600,
                                                                Translate 300 250 $ Color black $ rectangleSolid 200 50,
                                                                Translate 275 240 $ Color yellow $ Scale (0.2) (0.2) $ text (show (tempo m tmp)),
                                                                vivo jog 0 (235,175) (265,170) (300,175),
                                                                vivo jog 1 (235,50) (265,45) (300,50),
                                                                vivoBot jog 2 (235,-75) (265,-80) (300,-75),
                                                                vivoBot jog 3 (235,-200) (265,-205) (300,-200),
                                                                puBApanhados (separarJogX m '0') (265,150) b,
                                                                puBApanhados (separarJogX m '1') (265,20) b,
                                                                puBApanhados (separarJogX m '2') (265,-100) b,
                                                                puBApanhados (separarJogX m '3') (265,-225) b,
                                                                puFApanhados (separarJogX m '0') (330,150) f,
                                                                puFApanhados (separarJogX m '1') (330,20) f,
                                                                puFApanhados (separarJogX m '2') (330,-100) f,
                                                                puFApanhados (separarJogX m '3') (330,-225) f]
                                                                      | num==3 = Pictures [Translate 300 0 $ Color yellow $ rectangleSolid 200 600,
                                                                Translate 300 250 $ Color black $ rectangleSolid 200 50,
                                                                Translate 275 240 $ Color yellow $ Scale (0.2) (0.2) $ text (show (tempo m tmp)),
                                                                vivo jog 0 (235,175) (265,170) (300,175),
                                                                vivo jog 1 (235,50) (265,45) (300,50),
                                                                vivo jog 2 (235,-75) (265,-80) (300,-75),
                                                                vivoBot jog 3 (235,-200) (265,-205) (300,-200),
                                                                puBApanhados (separarJogX m '0') (265,150) b,
                                                                puBApanhados (separarJogX m '1') (265,20) b,
                                                                puBApanhados (separarJogX m '2') (265,-100) b,
                                                                puBApanhados (separarJogX m '3') (265,-225) b,
                                                                puFApanhados (separarJogX m '0') (330,150) f,
                                                                puFApanhados (separarJogX m '1') (330,20) f,
                                                                puFApanhados (separarJogX m '2') (330,-100) f,
                                                                puFApanhados (separarJogX m '3') (330,-225) f]
                                                                      | num==4 = Pictures [Translate 300 0 $ Color yellow $ rectangleSolid 200 600,
                                                                Translate 300 250 $ Color black $ rectangleSolid 200 50,
                                                                Translate 275 240 $ Color yellow $ Scale (0.2) (0.2) $ text (show (tempo m tmp)),
                                                                vivo jog 0 (235,175) (265,170) (300,175),
                                                                vivo jog 1 (235,50) (265,45) (300,50),
                                                                vivo jog 2 (235,-75) (265,-80) (300,-75),
                                                                vivo jog 3 (235,-200) (265,-205) (300,-200),
                                                                puBApanhados (separarJogX m '0') (265,150) b,
                                                                puBApanhados (separarJogX m '1') (265,20) b,
                                                                puBApanhados (separarJogX m '2') (265,-100) b,
                                                                puBApanhados (separarJogX m '3') (265,-225) b,
                                                                puFApanhados (separarJogX m '0') (330,150) f,
                                                                puFApanhados (separarJogX m '1') (330,20) f,
                                                                puFApanhados (separarJogX m '2') (330,-100) f,
                                                                puFApanhados (separarJogX m '3') (330,-225) f]
                                                                      | otherwise = Blank

{-|
A função 'verRaioBomba', recebendo um estado de jogo, vai devolver uma lista com os raios de todas as bombas colocadas no mapa.

=Propriedade:
 
prop> verRaioBomba [] = []

==Por exemplo:
>>> verRaioBomba ["#####","#   #","# # #","#   #","#####","* 1 1 0 1 10","0 1 1"]
[1.0]

>>> verRaioBomba ["#####","#   #","# # #","#   #","#####","* 1 1 0 1 10","* 1 2 1 1 10","0 1 1","1 1 2"]
[1.0,1.0]

-}

verRaioBomba :: [String] -> [Float]
verRaioBomba [] = []
verRaioBomba (x:xs) | head x=='*' =  read (unwords (take 1 (drop 4 (words x)))): verRaioBomba xs
                    | otherwise = verRaioBomba xs

{-|
A função 'raios', recebendo um raio, vai devolver uma lista com os números de 0 até esse raio.

=Propriedade:
 
prop> raios 0 [] = []

==Por exemplo:
>>> raios 1
[0.0,1.0]

>>> raios 5
[0.0,1.0,2.0,3.0,4.0,5.0]

-}

raios :: Float -> [Float]
raios 0 = []
raios x = (enumFromTo 0 x)

{-|
A função 'verTempoBomba', recebendo um estado de jogo, vai devolver uma lista com os tempos de todas as bombas colocadas no mapa.

=Propriedade:
 
prop> verTempoBomba [] = []

==Por exemplo:
>>> verTempoBomba ["#####","#   #","# # #","#   #","#####","* 1 1 0 1 10","0 1 1"]
[10.0]

>>> verTempoBomba ["#####","#   #","# # #","#   #","#####","* 1 1 0 1 9","* 1 2 1 1 10","0 1 1","1 1 2"]
[9.0,10.0]

-}

verTempoBomba :: [String] -> [Float]
verTempoBomba [] = []
verTempoBomba (x:xs) | head x=='*' = read (last (words x)): verTempoBomba xs
                     | otherwise = verTempoBomba xs

{-|
A função 'explosaoCima', recebendo as coordenadas de uma bomba, a dimensão do mapa, a lista com os números de 0 até ao seu raio (dada pela 'raios'),
a lista das coordenadas dos tijolos, a lista das coordenadas das caixas, a lista das coordenadas dos power-ups e a imagem que corresponde à explosão,
vai imprimir no ecrã a explosão da bomba para cima, isto é, nas coordenadas de ordenada superior àquela onde a bomba foi colocada.

=Propriedade:
 
prop> explosaoCima _ _ [] _ _ _ _ = Blank

-}

explosaoCima :: (Float,Float) -> Int -> [Float] -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)] -> Picture -> Picture
explosaoCima _ _ [] _ _ _ _ = Blank
explosaoCima (x,y) d (r:rs) lt lc lp exp | d==7 = if elem (x,y+r) lt then Blank
                                                                                else if (elem (x,y+r) lp || elem (x,y+r) lc) then Translate (-357.5+(85*x)) (257.5-(85*(y+r))) $ Scale (0.33) (0.33) $ exp
                                                                                    else Pictures [Translate (-357.5+(85*x)) (257.5-(85*(y+r))) $ Scale (0.33) (0.33) $ exp, 
                                                                                    explosaoCima (x,y) d rs lt lc lp exp]
                                                    | d==13 = if elem (x,y+r) lt then Blank
                                                                                 else if (elem (x,y+r) lp || elem (x,y+r) lc) then Translate (-377+(46*x)) (277-(46*(y+r))) $ Scale (0.18) (0.18) $ exp
                                                                                    else Pictures [Translate (-377+(46*x)) (277-(46*(y+r))) $ Scale (0.18) (0.18) $ exp, 
                                                                                    explosaoCima (x,y) d rs lt lc lp exp]
                                                    | d==17 = if elem (x,y+r) lt then Blank
                                                                                else if (elem (x,y+r) lp || elem (x,y+r) lc) then Translate (-382.5+(35*x)) (282.5-(35*(y+r))) $ Scale (0.14) (0.14) $ exp
                                                                                    else Pictures [Translate (-382.5+(35*x)) (282.5-(35*(y+r))) $ Scale (0.14) (0.14) $ exp, 
                                                                                    explosaoCima (x,y) d rs lt lc lp exp]

{-|
A função 'explosaoBaixo' vai atuar da mesma maneira que a 'explosaoCima', mas imprimindo a explosão para baixo.

=Propriedade:
 
prop> explosaoBaixo _ _ [] _ _ _ _ = Blank

-}

explosaoBaixo :: (Float,Float) -> Int -> [Float] -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)] -> Picture -> Picture
explosaoBaixo _ _ [] _ _ _ _ = Blank
explosaoBaixo (x,y) d (r:rs) lt lc lp exp | d==7 = if elem (x,y-r) lt then Blank
                                                                                 else if (elem (x,y-r) lp || elem (x,y-r) lc) then Translate (-357.5+(85*x)) (257.5-(85*(y-r))) $ Scale (0.33) (0.33) $ exp
                                                                                    else Pictures [Translate (-357.5+(85*x)) (257.5-(85*(y-r))) $ Scale (0.33) (0.33) $ exp, 
                                                                                    explosaoBaixo (x,y) d rs lt lc lp exp]
                                                    | d==13 = if elem (x,y-r) lt then Blank
                                                                                 else if (elem (x,y-r) lp || elem (x,y-r) lc) then Translate (-377+(46*x)) (277-(46*(y-r))) $ Scale (0.18) (0.18) $ exp
                                                                                    else Pictures [Translate (-377+(46*x)) (277-(46*(y-r))) $ Scale (0.18) (0.18) $ exp, 
                                                                                    explosaoBaixo (x,y) d rs lt lc lp exp]
                                                    | d==17 = if elem (x,y-r) lt then Blank
                                                                                 else if (elem (x,y-r) lp || elem (x,y-r) lc) then Translate (-382.5+(35*x)) (282.5-(35*(y-r))) $ Scale (0.14) (0.14) $ exp
                                                                                    else Pictures [Translate (-382.5+(35*x)) (282.5-(35*(y-r))) $ Scale (0.14) (0.14) $ exp, 
                                                                                    explosaoBaixo (x,y) d rs lt lc lp exp]

{-|
A função 'explosaoDta' vai atuar da mesma maneira que a 'explosaoCima', mas imprimindo a explosão para a direita.

=Propriedade:
 
prop> explosaoDta _ _ [] _ _ _ _ = Blank

-}

explosaoDta :: (Float,Float) -> Int -> [Float] -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)] -> Picture -> Picture
explosaoDta _ _ [] _ _ _ _ = Blank
explosaoDta (x,y) d (r:rs) lt lc lp exp | d==7 = if elem (x+r,y) lt then Blank
                                                                                 else if (elem (x+r,y) lp || elem (x+r,y) lc) then Translate (-357.5+(85*(x+r))) (257.5-(85*y)) $ Scale (0.33) (0.33) $ exp
                                                                                    else Pictures [Translate (-357.5+(85*(x+r))) (257.5-(85*y)) $ Scale (0.33) (0.33) $ exp, 
                                                                                    explosaoDta (x,y) d rs lt lc lp exp]
                                                    | d==13 = if elem (x+r,y) lt then Blank
                                                                                 else if (elem (x+r,y) lp || elem (x+r,y) lc) then Translate (-377+(46*(x+r))) (277-(46*y)) $ Scale (0.18) (0.18) $ exp
                                                                                    else Pictures [Translate (-377+(46*(x+r))) (277-(46*y)) $ Scale (0.18) (0.18) $ exp, 
                                                                                    explosaoDta (x,y) d rs lt lc lp exp]
                                                    | d==17 = if elem (x+r,y) lt then Blank
                                                                                 else if (elem (x+r,y) lp || elem (x+r,y) lc) then Translate (-382.5+(35*(x+r))) (282.5-(35*y)) $ Scale (0.14) (0.14) $ exp
                                                                                    else Pictures [Translate (-382.5+(35*(x+r))) (282.5-(35*y)) $ Scale (0.14) (0.14) $ exp, 
                                                                                    explosaoDta (x,y) d rs lt lc lp exp]

{-|
A função 'explosaoEsq' vai atuar da mesma maneira que a 'explosaoCima', mas imprimindo a explosão para a esquerda.

=Propriedade:
 
prop> explosaoEsq _ _ [] _ _ _ _ = Blank

-}

explosaoEsq :: (Float,Float) -> Int -> [Float] -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)] -> Picture -> Picture
explosaoEsq  _ _ [] _ _ _ _ = Blank
explosaoEsq (x,y) d (r:rs) lt lc lp exp | d==7 = if elem (x-r,y) lt then Blank
                                                                                 else if (elem (x-r,y) lp || elem (x-r,y) lc) then Translate (-357.5+(85*(x-r))) (257.5-(85*y)) $ Scale (0.33) (0.33) $ exp
                                                                                    else Pictures [Translate (-357.5+(85*(x-r))) (257.5-(85*y)) $ Scale (0.33) (0.33) $ exp, 
                                                                                    explosaoEsq (x,y) d rs lt lc lp exp]
                                        | d==13 = if elem (x-r,y) lt then Blank
                                                                                 else if (elem (x-r,y) lp || elem (x-r,y) lc) then Translate (-377+(46*(x-r))) (277-(46*y)) $ Scale (0.18) (0.18) $ exp
                                                                                    else Pictures [Translate (-377+(46*(x-r))) (277-(46*y)) $ Scale (0.18) (0.18) $ exp, 
                                                                                    explosaoEsq (x,y) d rs lt lc lp exp]
                                        | d==17 = if elem (x-r,y) lt then Blank
                                                                               else if (elem (x-r,y) lp || elem (x-r,y) lc) then Translate (-382.5+(35*(x-r))) (282.5-(35*y)) $ Scale (0.14) (0.14) $ exp
                                                                                    else Pictures [Translate (-382.5+(35*(x-r))) (282.5-(35*y)) $ Scale (0.14) (0.14) $ exp, 
                                                                                    explosaoEsq (x,y) d rs lt lc lp exp]

{-|
A função 'porBomba' vai receber um estado de jogo, a lista com as coordenadas das bombas, a dimensão do mapa, a lista dos raios das bombas, a lista dos
tempos das bombas e as imagens correspondentes às bombas e às explosões, colocando as bombas, e respetivas explosões, nos gráficos do jogo.

=Propriedade:
 
prop> porBomba _ [] _ _ _ _ _ = Blank

-}

porBomba :: [String] -> [(Float,Float)] -> Int -> [Float] -> [Float] -> Picture -> Picture -> Picture
porBomba _ [] _ _ _ _ _ = Blank
porBomba m ((x,y):xs) d (r:rs) (t:ts) i exp | d==7 = if (round t>1) then Pictures [(Translate (-357.5+(85*x)) (257.5-(85*y)) $ Scale (0.33) (0.33) $ i),porBomba m xs d rs ts i exp]
                                                                    else Pictures [(Translate (-357.5+(85*x)) (257.5-(85*y)) $ Scale (0.33) (0.33) $ exp),
                                                                  explosaoCima (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoBaixo (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoEsq (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoDta (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  porBomba m xs d rs ts i exp]
                                            | d==13 = if (round t>1) then Pictures [(Translate (-377+(46*x)) (277-(46*y)) $ Scale (0.18) (0.18) $ i),porBomba m xs d rs ts i exp]
                                                                     else Pictures [(Translate (-377+(46*x)) (277-(46*y)) $ Scale (0.18) (0.18) $ exp),
                                                                  explosaoCima (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoBaixo (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoEsq (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoDta (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  porBomba m xs d rs ts i exp]

                                            | d==17 = if (round t>1) then Pictures [(Translate (-382.5+(35*x)) (282.5-(35*y)) $ Scale (0.14) (0.14) $ i),porBomba m xs d rs ts i exp]
                                                                     else Pictures [(Translate (-382.5+(35*x)) (282.5-(35*y)) $ Scale (0.14) (0.14) $ exp),
                                                                  explosaoCima (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoBaixo (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoEsq (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  explosaoDta (x,y) d (raios r) (tOnlyP1 m) (tI1 m) (coordPU1 m) exp,
                                                                  porBomba m xs d rs ts i exp]
                                            | otherwise = Blank

{-|
A função 'nJogadores' vai receber um estado de jogo e devolver a lista dos jogadores que estão ainda vivos.

=Propriedade:
 
prop> nJogadores [] = []

==Por exemplo:
>>> nJogadores ["#####","#   #","# # #","#   #","#####","0 1 1"]
[0]

>>> nJogadores ["#####","#   #","# # #","#   #","#####","0 1 1","1 1 2","3 2 1"]
[0,1,3]

-}

nJogadores :: [String] -> [Int]
nJogadores [] = []
nJogadores ((a:b):xs) = if isDigit a then digitToInt a : nJogadores xs
                                     else nJogadores xs

{-|
A função 'desenha', recebendo um @Estado@ do jogo, vai imprimir no ecrã a imagem correspondente a esse estado.

-}                                     

desenha :: Estado -> Picture
desenha (m,True,bool,bool1,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo) = p
desenha (m,False,True,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo) = n
desenha (m,False,False,True,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo) = d
desenha (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,(x:xs),tmp,exp,l0,l1,l2,l3,fundo) | length (x:xs) == 1 = Pictures
                                        [Scale (0.5) (0.5) $ Color red $ text "VENCEU!",
                                        Translate (-200) 0 $ Scale (1.5) (1.5) $ boneco x 0 'f',
                                        Translate (-380) (-200) $ Scale (0.25) (0.25) $ Color red $ text "Pressione a tecla home para jogar novamente"]
                                                                       | tempo m tmp <= 0 = Pictures
                                        [Translate (-250) 0 $ Color red $ text "EMPATE!",
                                        Translate (-380) (-200) $ Scale (0.25) (0.25) $ Color red $ text "Pressione a tecla home para jogar novamente"]
                                                                       | otherwise = Pictures
                                        [Translate (-100) 0 $ Scale (0.95) (0.95) $ fundo,
                                        painelLateral (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,(x:xs),tmp,exp,l0,l1,l2,l3,fundo),
                                        porPU (coordTudo m '+') (length (head m)) (Scale (0.75) (0.75) $ b),
                                        porPU (coordTudo m '!') (length (head m)) (Scale (0.75) (0.75) $ f),
                                        genMapa m (length (head m)) 0 t c,
                                        porJogador (coordTudo m '0') '0' (length (head m)) tmp l0,
                                        porJogador (coordTudo m '1') '1' (length (head m)) tmp l1,
                                        porJogador (coordTudo m '2') '2' (length (head m)) tmp l2,
                                        porJogador (coordTudo m '3') '3' (length (head m)) tmp l3,
                                        porBomba m (coordTudo m '*') (length (head m)) (verRaioBomba m) (verTempoBomba m) (Scale (0.75) (0.75) $ bomba) exp]
desenha (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,[],tmp,exp,l0,l1,l2,l3,fundo) = Pictures
                                        [Translate (-250) 0 $ Color red $ text "EMPATE!",
                                        Translate (-380) (-200) $ Scale (0.25) (0.25) $ Color red $ text "Pressione a tecla home para jogar novamente"]


{-|
A função 'mapaInicial', recebendo a dimensão do mapa desejado e a semente, vai devolver o mapa gerado pela função com o mesmo definida na Tarefa 1,
acrescentando-lhe as coordenadas iniciais dos quatro jogadores.

==Por exemplo:
>>> mapaInicial 7 0
["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 1 1","1 5 5","2 1 5","3 5 1"]

>>> mapaInicial 19 12
["###################","#    ?      ???   #","# # # # #?# # #?# #","#    ???   ?  ??? #","#?#?#?# # #?# #?#?#","#?    ?   ?   ?? ?#","# #?# #?# #?#?# # #","#  ???    ?? ??  ?#","#?# # # # # # #?# #","#  ??? ?  ?    ?  #","#?#?#?# #?#?#?# # #","#??   ?   ?  ??   #","#?#?# #?# #?#?#?#?#","#    ? ???      ? #","#?# #?# #?# #?# # #","#    ?        ?  ?#","# #?#?#?# # # #?# #","#   ????  ?   ??  #","###################","+ 11 4","+ 13 6","+ 11 7","+ 5 9","+ 10 11","+ 10 17","! 3 6","! 7 9","! 14 15","! 5 16","0 1 1","1 17 17","2 1 17","3 17 1"]

-}

mapaInicial :: Int -> Int -> [String]
mapaInicial d s = (mapa d s)++["0 1 1","1 "++show (d-2)++" "++show (d-2),"2 1 "++show (d-2),"3 "++show (d-2)++" 1"]

{-|
A função 'desenha', recebendo um evento e um @Estado@ do jogo, vai devolver o @Estado@ que sucede à transformação do anterior acionada pelo evento.

-}

continua :: Event -> Estado -> Estado
continua (EventKey (SpecialKey KeyHome) _ _ _) (m,True,bool,bool1,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = estadoInicial p n d s t c f b bomba exp fundo
continua (EventKey (Char '1') Down _ _) (m,False,True,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherDificuldade p n d s t c 1 f b bomba exp fundo -- escolher o nº de jogadores
continua (EventKey (Char '2') Down _ _) (m,False,True,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherDificuldade p n d s t c 2 f b bomba exp fundo
continua (EventKey (Char '3') Down _ _) (m,False,True,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherDificuldade p n d s t c 3 f b bomba exp fundo
continua (EventKey (Char '4') Down _ _) (m,False,True,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherDificuldade p n d s t c 4 f b bomba exp fundo
continua (EventKey (Char '1') Down _ _) (m,False,False,True,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (mapaInicial 7 s,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo) -- escolher o modo de jogo
continua (EventKey (Char '2') Down _ _) (m,False,False,True,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (mapaInicial 13 s,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo)
continua (EventKey (Char '3') Down _ _) (m,False,False,True,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (mapaInicial 17 s,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f','f','f','f',fundo)
continua (EventKey (Char _) Down _ _) (m,True,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherNumero p n d s t c f b bomba exp fundo-- avancar a pagina inicial
continua (EventKey (SpecialKey _) Down _ _) (m,True,bool,bool1,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = escolherNumero p n d s t c f b bomba exp fundo
continua (EventKey (SpecialKey KeyHome) Down _ _) (m,False,bool,bool1,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = estadoInicial p n d s t c f b bomba exp fundo -- volta pra pagina inicial
continua (EventKey (Char 'w') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (move m 0 'U',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'t',l1,l2,l3,fundo) -- escolher o nº de jogadores
continua (EventKey (Char 'a') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (move m 0 'L',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'l',l1,l2,l3,fundo)
continua (EventKey (Char 's') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (move m 0 'D',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'f',l1,l2,l3,fundo)
continua (EventKey (Char 'd') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (move m 0 'R',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,'l',l1,l2,l3,fundo)
continua (EventKey (Char 'q') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (move m 0 'B',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
continua (EventKey (SpecialKey KeyUp) Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==2 || num==3 || num==4) = (move m 1 'U',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,'t',l2,l3,fundo) -- movimentos dos jogadores
continua (EventKey (SpecialKey KeyLeft) Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==2 || num==3 || num==4) = (move m 1 'L',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,'l',l2,l3,fundo)
continua (EventKey (SpecialKey KeyDown) Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==2 || num==3 || num==4) = (move m 1 'D',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,'f',l2,l3,fundo)
continua (EventKey (SpecialKey KeyRight) Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==2 || num==3 || num==4) = (move m 1 'R',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,'l',l2,l3,fundo)
continua (EventKey (SpecialKey KeyEnter) Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==2 || num==3 || num==4) = (move m 1 'B',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
continua (EventKey (Char 't') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==3 || num==4) = (move m 2 'U',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,'t',l3,fundo)
continua (EventKey (Char 'f') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==3 || num==4) = (move m 2 'L',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,'l',l3,fundo)
continua (EventKey (Char 'g') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==3 || num==4) = (move m 2 'D',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,'f',l3,fundo)
continua (EventKey (Char 'h') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==3 || num==4) = (move m 2 'R',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,'l',l3,fundo)
continua (EventKey (Char 'y') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==3 || num==4) = (move m 2 'B',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
continua (EventKey (Char 'i') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==4) = (move m 3 'U',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,'t',fundo) 
continua (EventKey (Char 'j') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==4) = (move m 3 'L',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,'l',fundo)
continua (EventKey (Char 'k') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==4) = (move m 3 'D',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,'f',fundo)
continua (EventKey (Char 'l') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==4) = (move m 3 'R',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,'l',fundo)
continua (EventKey (Char 'o') Down _ _) (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (num==4) = (move m 3 'B',False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
continua e (m,bool,bool1,bool2,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) = (m,bool,bool1,bool2,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)

{-|
A função 'numeroBot', recebendo o número de jogadores que estão a jogar (selecionado num dos ecrãs anteriores ao jogo em si) e o número de segundos
que passaram desde o início do jogo, vai devolver o número correspondente a um bot, de forma a fazer com que a cada segundo se mova um bot diferente.

-}        

numeroBot :: Int -> Float -> Int
numeroBot num tmp | num == 1 = if (tmp == 0) then 1
                                else if (tmp == 1) then 2
                                    else if (tmp == 2) then 3
                                            else numeroBot num (tmp-3)
                  | num == 2 = if (tmp == 0) then 2
                                else if (tmp == 1) then 3
                                            else numeroBot num (tmp-2)
                  | num == 3 = 3
                  | otherwise = 4

{-|
A função 'mexerBot' vai receber um estado de jogo, o número de jogadores e o número de segundos que passaram desde o início do jogo e vai devolver
um novo estado de jogo após o movimento (ou não movimento) de um bot.

-}        

mexerBot :: [String] -> Int -> [Int] -> Float -> [String]
mexerBot m num jog tmp | (length (head m)) ==7 = if elem (numeroBot num tmp) jog then move1 m (numeroBot num tmp) (bot m (numeroBot num tmp) (180-round (tmp+1/5)))
                                                                                 else m
                       | (length (head m)) ==13 = if elem (numeroBot num tmp) jog then move1 m (numeroBot num tmp) (bot m (numeroBot num tmp) (300-round (tmp+1/5)))
                                                                                  else m
                       | (length (head m)) ==17 = if elem (numeroBot num tmp) jog then move1 m (numeroBot num tmp) (bot m (numeroBot num tmp) (700-round (tmp+1/5)))
                                                                                  else m
                       | otherwise = []

{-|
A função 'mudarLado' vai receber o número de jogadores selecionado no menu, o número de segundos que passaram desde o início do jogo, o mapa e o caracter
que diz respeito à posiçao do boneco de um bot e vai devolver um caracter que define a nova posição do boneco desse mesmo bot, de forma a fazê-lo virar-se.

==Por exemplo:
>>> mudarLado 2 15 ["#######","#     #","# # # #","#     #","#######","0 1 1","1 5 5","2 1 5","3 5 1"] 'f'
'f'

-}  

mudarLado :: Int -> Float -> [String] -> Char -> Char
mudarLado num tmp m l | (length (head m)) ==7 = if ((bot m (numeroBot num tmp) (180-round (tmp+1/5))) == Just 'U') then 't'
                                                 else if ((bot m (numeroBot num tmp) (180-round (tmp+1/5))) == Just 'D') then 'f'
                                                    else if ((bot m (numeroBot num tmp) (180-round (tmp+1/5))) == Just 'R' || (bot m (numeroBot num tmp) (180-round (tmp+1/5))) == Just 'L') then 'l'
                                                        else l
                      | (length (head m)) ==13 =if ((bot m (numeroBot num tmp) (300-round (tmp+1/5))) == Just 'U') then 't'
                                                 else if ((bot m (numeroBot num tmp) (300-round (tmp+1/5))) == Just 'D') then 'f'
                                                    else if ((bot m (numeroBot num tmp) (300-round (tmp+1/5))) == Just 'R' || (bot m (numeroBot num tmp) (300-round (tmp+1/5))) == Just 'L') then 'l'
                                                        else l
                      | (length (head m)) ==17 = if ((bot m (numeroBot num tmp) (700-round (tmp+1/5))) == Just 'U') then 't'
                                                 else if ((bot m (numeroBot num tmp) (700-round (tmp+1/5))) == Just 'D') then 'f'
                                                    else if ((bot m (numeroBot num tmp) (700-round (tmp+1/5))) == Just 'R' || (bot m (numeroBot num tmp) (700-round (tmp+1/5))) == Just 'L') then 'l'
                                                        else l

{-|
A função 'reageTempo', recebendo o frame rate e um @Estado@ do jogo, vai devolver o @Estado@ alterado pela passagem do tempo.

-}        

reageTempo :: Float -> Estado -> Estado
reageTempo fr (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo) | (length (head m)) ==7 = if (jog==[] || length jog==1) then (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
                                                                                                                else if num==4 then (avanca m (180-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,l3,fundo)
                                                                                                                    else if (numeroBot num tmp ==1 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (180-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,mudarLado num tmp m l1,l2,l3,fundo)
                                                                                                                        else if (numeroBot num tmp ==2 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (180-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,mudarLado num tmp m l2,l3,fundo)
                                                                                                                           else (avanca (mexerBot m (numeroBot num tmp) jog tmp) (180-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,mudarLado num tmp m l3,fundo)
                                                                                            | (length (head m)) ==13 = if (jog==[] || length jog==1) then (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
                                                                                                                else if num==4 then (avanca m (300-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,l3,fundo)
                                                                                                                    else if (numeroBot num tmp ==1 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (300-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,mudarLado num tmp m l1,l2,l3,fundo)
                                                                                                                        else if (numeroBot num tmp ==2 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (300-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,mudarLado num tmp m l2,l3,fundo)
                                                                                                                           else (avanca (mexerBot m (numeroBot num tmp) jog tmp) (300-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,mudarLado num tmp m l3,fundo)
                                                                                            | (length (head m)) ==17 = if (jog==[] || length jog==1) then (m,False,False,False,p,n,d,s,t,c,num,f,b,bomba,jog,tmp,exp,l0,l1,l2,l3,fundo)
                                                                                                                else if num==4 then (avanca m (700-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,l3,fundo)
                                                                                                                    else if (numeroBot num tmp ==1 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (700-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,mudarLado num tmp m l1,l2,l3,fundo)
                                                                                                                        else if (numeroBot num tmp ==2 ) then (avanca (mexerBot m (numeroBot num tmp) jog tmp) (700-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,mudarLado num tmp m l2,l3,fundo)
                                                                                                                           else (avanca (mexerBot m (numeroBot num tmp) jog tmp) (700-round (tmp+1/fr)),False,False,False,p,n,d,s,t,c,num,f,b,bomba,nJogadores m,tmp + 1,exp,l0,l1,l2,mudarLado num tmp m l3,fundo) 
reageTempo fr e = e


{-|
A função 'window' define o nome da janela do jogo, a sua dimensão e a sua posição no ecrã.

-}        

window :: Display
window = InWindow "Bomberman" (800,600) (350,50)

{-|
A função 'main' vai imprimir no ecrã os gráficos completos do jogo Bomberman, utilizando as funções por nós definidas e recorrendo às funções da 
biblioteca Gloss. O frame rate por nós definido é igual a 5.

-}        

main :: IO ()
main = do p <- loadBMP "./bitmaps/inicio.bmp"
          n <- loadBMP "./bitmaps/numero.bmp"
          d <- loadBMP "./bitmaps/modo.bmp"
          t <- loadBMP "./bitmaps/tijolo.bmp"
          c <- loadBMP "./bitmaps/caixa.bmp"
          f <- loadBMP "./bitmaps/flames.bmp"
          b <- loadBMP "./bitmaps/bombs.bmp"
          bomba <- loadBMP "./bitmaps/bomba.bmp"
          exp <- loadBMP "./bitmaps/xplsn1.bmp"
          fundo <- loadBMP "./bitmaps/fundo.bmp"
          s <- randomRIO (0,99)
          play window -- display
               aquamarine -- background
               5 -- fr
               (estadoInicial p n d s t c f b bomba exp fundo) -- estado inicial
               desenha -- transforma Estado em Picture
               continua-- reagir evento
               reageTempo -- reagir tempol