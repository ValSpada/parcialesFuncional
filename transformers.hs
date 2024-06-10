import Text.Show.Functions
import Data.List(genericLength)

data Autobot = Robot String (Int,Int,Int) ((Int,Int,Int)->(Int,Int)) | Vehiculo String (Int,Int) deriving (Show)

-- Robot nombre (fuerza,velocidad,resistencia) funcionVehiculos | Vehiculo nombre (velocidad,resistencia)

optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots = [optimus, jazz, wheeljack, bumblebee]

-- Auxiliar

fst3 :: (a,a,a) -> a
fst3 (x,_,_) = x

snd3 :: (a,a,a) -> a
snd3 (_,y,_) = y

trd3 :: (a,a,a) -> a
trd3 (_,_,z) = z

nombre :: Autobot -> String
nombre (Robot name _ _) = name
nombre (Vehiculo name _) = name

--------------------- PUNTO 1 ---------------------

maximoSegun :: (Ord b) => (a -> a -> b) -> a -> a -> a
maximoSegun unaFuncion unValor otroValor
    | unaFuncion unValor otroValor > unaFuncion otroValor unValor = unValor
    | otherwise                                                   = otroValor

--------------------- PUNTO 2 ---------------------

atributosAutobot :: Autobot -> (Int,Int,Int)
atributosAutobot (Robot _ atributos _) = atributos
atributosAutobot (Vehiculo _ (velocidad,resistencia)) = (0,velocidad,resistencia)

--------------------- PUNTO 3 ---------------------

transformar :: Autobot -> Autobot
transformar (Robot nombre atributos transformacion) = (Vehiculo nombre (transformacion atributos))

--------------------- PUNTO 4 ---------------------

velocidadContra :: Autobot -> Autobot -> Int
velocidadContra unAutobot otroAutobot = restarDiferencia (fuerza unAutobot) (resistencia otroAutobot) . velocidad $ unAutobot 

-- Auxiliar

fuerza :: Autobot -> Int
fuerza = fst3 . atributosAutobot

velocidad :: Autobot -> Int
velocidad = snd3 . atributosAutobot

resistencia :: Autobot -> Int
resistencia = trd3 . atributosAutobot

restarDiferencia :: Int -> Int -> Int -> Int
restarDiferencia fuerzaDelPrimero resistenciaDelSegundo velocidadPrimero = velocidadPrimero - (diferenciaSegura fuerzaDelPrimero resistenciaDelSegundo)

diferenciaSegura :: Int -> Int -> Int
diferenciaSegura unValor = max 0 . (unValor -)

--------------------- PUNTO 5 ---------------------

elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido unAutobot = maximoSegun velocidadContra unAutobot

--------------------- PUNTO 6 ---------------------

-- a. 
domina :: Autobot -> Autobot -> Bool
domina unAutobot otroAutobot = all (uncurry leDominaComo) combinacionesPosibles
    where combinacionesPosibles = [(unAutobot, otroAutobot),(unAutobot, transformar otroAutobot),(transformar unAutobot, otroAutobot),(transformar unAutobot, transformar otroAutobot)]
--domina unAutobot otroAutobot = leDominaComo (unAutobot, otroAutobot) && leDominaComo (unAutobot, transformar otroAutobot) && leDominaComo (transformar unAutobot, otroAutobot) && leDominaComo (transformar unAutobot, transformar otroAutobot)

leDominaComo :: Autobot -> Autobot -> Bool
leDominaComo unAutobot otroAutobot = (nombre . elMasRapido unAutobot) otroAutobot == (nombre unAutobot)

-- b.
losDominaATodos :: Autobot -> [Autobot] -> Bool
losDominaATodos unAutobot unosAutobots = all (domina unAutobot) unosAutobots
--losDominaATodos _ []                                    = True
--losDominaATodos unAutobot (primerAutobot:restoAutobots) = domina unAutobot primerAutobot && losDominaATodos unAutobot restoAutobots

--------------------- PUNTO 7 ---------------------

-- a.
quienesCumplen :: (Autobot -> Bool) -> [Autobot] -> [String]
quienesCumplen condicion = map nombre . filter condicion 
--quienesCumplen :: (Autobot -> Bool) -> [Autobot] -> [Autobots]
--quienesCumplen condicion = filter condicion 

-- b.
-- Consulta: losDominaATodos (head . quienesCumplen (flip elem "aeiouAEIOU" . last . nombre) $ autobots) autobots

--------------------- PUNTO 8 ---------------------

saraza :: (Ord b) => a -> a -> a -> (a -> a -> b) -> b
saraza x y w z = z w . maximoSegun z y $ x