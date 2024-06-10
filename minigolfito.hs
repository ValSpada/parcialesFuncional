import Text.Show.Functions
import Data.List(genericLength)

-- Modelo Inicial
data Jugador = Jugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad{
    fuerzaJugador :: Int,
    precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de EJemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro{
    velocidad :: Int,
    precision :: Int,
    altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones utiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

------------------ PUNTO 1 ------------------

type Palo = Habilidad -> Tiro
--type PaloGolf = Jugador -> Tiro

-- a.i.   --
putter :: Palo
putter unaHabilidad = Tiro 10 precisionRecibida 0
    where precisionResultante = (* 2) . precisionJugador $ unaHabilidad

-- a.ii.  --
madera :: Palo
madera unaHabilidad = Tiro 100 precisionResultante 5
    where precisionResultante = flip div 2 . precisionJugador $ unaHabilidad

-- a.iii. --
hierro :: Int -> Palo
hierro n unaHabilidad = Tiro velocidadResultante precisionResultante alturaResultante
    where 
    velocidadResultante = fuerzaJugador unaHabilidad * n
    precisionResultante = flip div n . precisionJugador $ unaHabilidad
    alturaResultante    = max 0 . (subtract 3) $ n 

-- b. --
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]
--palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10] 

------------------ PUNTO 2 ------------------

golpe :: Palo -> Jugador -> Tiro
golpe unPalo = unPalo . habilidad
--golpe unPalo = unPalo

------------------ PUNTO 3 ------------------

--type Obstaculo = Tiro -> Tiro
data Obstaculo = Obstaculo {
  criterioSuperacion :: Tiro -> Bool,
  efectoSobreTiro    :: Tiro -> Tiro
}

tirarEnObstaculo :: Obstaculo -> Tiro -> Tiro
tirarEnObstaculo unObstaculo unTiro
  | criterioSuperacion unObstaculo unTiro = efectoSobreTiro unObstaculo unTiro
  | otherwise                             = Tiro 0 0 0

-- a. --
tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo{
    criterioSuperacion = superaTunel,
    efectoSobreTiro    = pasarPorTunel
}

superaTunel :: Tiro -> Bool
superaTunel (Tiro _ unaPrecision unaAltura) = unaPrecision > 90 && unaAltura == 0

pasarPorTunel :: Tiro -> Tiro
pasarPorTunel unTiro = Tiro (2 * velocidad unTiro) 100 0

--tunelConRampita :: Obstaculo
--tunelConRampita unTiro
--    | superaTunel unTiro = Tiro (2 * velocidad unTiro) 100 0
--    | otherwise          = Tiro 0 0 0

-- b. --
laguna :: Int -> Obstaculo
laguna largo = Obstaculo{
    criterioSuperacion = superaLaguna,
    efectoSobreTiro    = pasarPorLaguna largo
}

superaLaguna :: Tiro -> Bool
superaLaguna (Tiro unaVelocidad _ unaAltura) = unaVelocidad > 80 && between 1 5 unaAltura 

pasarPorLaguna :: Int -> Tiro -> Tiro
pasarPorLaguna largo unTiro = Tiro (velocidad unTiro) (precision unTiro) (div (altura unTiro) largo)

--laguna :: Int -> Obstaculo
--laguna largo unTiro
--    | superaLaguna unTiro = Tiro (velocidad unTiro) (precision unTiro) (div (altura unTiro) largo)
--    | otherwise           = Tiro 0 0 0

-- c. --
laguna :: Int -> Obstaculo
laguna largo = Obstaculo{
    criterioSuperacion = superaHoyo,
    efectoSobreTiro    = pasarPorHoyo
}

superaHoyo :: Tiro -> Bool
superaHoyo (Tiro unaVelocidad unaPrecision unaAltura) = between 5 20 unaVelocidad && unaPrecision > 95 && unaAltura == 0

pasarPorHoyo :: Tiro -> Tiro
pasarPorHoyo _ = Tiro 0 0 0

--hoyo :: Obstaculo
--hoyo unTiro
--    | superaHoyo unTiro = Tiro 0 0 0
--    | otherwise         = Tiro 0 0 0

------------------ PUNTO 4 ------------------

-- a. --

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (flip superaObstaculo unObstaculo . flip golpe unJugador) palos

-- b. --

cantidadObstaculosSuperados :: [Obstaculo] -> Tiro -> Int
cantidadObstaculosSuperados unosObstaculos unTiro = genericLength . takeWhile (superaObstaculo unTiro) $ unosObstaculos

superaObstaculo :: Tiro -> Obstaculo -> Bool
superaObstaculo unTiro unObstaculo = unObstaculo unTiro /= (Tiro 0 0 0)

-- c. --
paloMasUtil :: Jugador -> [Obstaculo] -> Tiro
paloMasUtil unJugador unosObstaculos = maximoSegun (paloQueSupera unosObstaculos) . map (flip golpe unJugador) $ palos

------------------ PUNTO 5 ------------------

perdieronApuesta :: [(Jugador, Puntos)] -> [String]
perdieronApuesta unosJugadores = map (padre . fst) . filter ((/= (fst . niñoGanador $ unosJugadores)) . fst) $ unosJugadores

niñoGanador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
niñoGanador unosJugadores = maximoSegun snd unosJugadores