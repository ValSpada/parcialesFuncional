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

fst3 :: (Int,Int,Int) -> Int
fst3 (x,_,_) = x

snd3 :: (Int,Int,Int) -> Int
snd3 (_,y,_) = y

trd3 :: (Int,Int,Int) -> Int
trd3 (_,_,z) = z

--------------------- PUNTO 1 ---------------------

maximoSegun :: Ord a => (a -> a -> a) -> a -> a -> a
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