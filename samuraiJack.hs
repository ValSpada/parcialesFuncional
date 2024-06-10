import Text.Show.Functions
import Data.List(genericLength)

data Elemento = UnElemento{
    tipo    :: String,
    ataque  :: Transformacion,
    defensa :: Transformacion
} deriving (Show)

data Personaje = UnPersonaje{
    salud        :: Float,
    elementos    :: [Elemento],
    anioPresente :: Int
} deriving (Show)

-- Usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene.
-- En caso de no indicar efecto defensivo u ofensivo, no se altera al personaje recibido.

--------------------- PUNTO 1 ---------------------

type Transformacion = Personaje -> Personaje

-- a.
mandarAlAnio :: Int -> Transformacion
mandarAlAnio unAnio unPersonaje = unPersonaje { anioPresente = unAnio }

-- b.
meditar :: Transformacion
meditar = alterarSalud (1.5 *)

-- c.
causarDanio :: Float -> Transformacion
causarDanio danio = alterarSalud (max 0 . subtract danio)
--causarDanio danio unPersonaje
--    | (> 0).salud.recibirDanio $ unPersonaje =  recibirDanio unPersonaje
--    | otherwise                              = alterarSalud (* 0) unPersonaje
--    where recibirDanio = alterarSalud (subtract danio)

-- Auxiliar
alterarSalud :: (Float -> Float) -> Personaje -> Personaje
alterarSalud alteracion unPersonaje = unPersonaje { salud = alteracion . salud $ unPersonaje }

--------------------- PUNTO 2 ---------------------

-- a.
esMalvado :: Personaje -> Bool
esMalvado (UnPersonaje _ unosElementos _) = any ((== "Maldad") . tipo) $ unosElementos

-- b.
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje (UnElemento _ atacar _) = (salud unPersonaje) - ((salud . atacar) unPersonaje)

-- c.
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje unosEnemigos = filter (puedeMatarloConUnSoloElemento unPersonaje) unosEnemigos

-- Auxiliar
puedeMatarloConUnSoloElemento :: Personaje -> Personaje -> Bool
puedeMatarloConUnSoloElemento unPersonaje unEnemigo = estaMuerto . aplicarAtaques unEnemigo $ unPersonaje

estaMuerto :: Personaje -> Bool
estaMuerto (UnPersonaje vida _ _) = vida == 0

--------------------- PUNTO 3 ---------------------

-- a.
concentracion :: Int -> Elemento
concentracion nivelConcentracion = UnElemento {
    tipo = "Magia",
    ataque = id,
    defensa = concentrarse nivelConcentracion
}

-- b.
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados unaCantidad = replicate unaCantidad esbirro
--esbirrosMalvados 0                = []
--esbirrosMalvados cantidadEsbirros = esbirro:(esbirrosMalvados (cantidadEsbirros - 1))

-- c.
jack = UnPersonaje 300 [concentracion 3, katanaMagica] 200

-- d.
aku :: Int -> Float -> Personaje
aku anioActual cantidadSalud = UnPersonaje cantidadSalud (concentracion 4:portalAlFuturo anioActual:esbirrosMalvados (100*anioActual)) anioActual

-- Auxiliar
esbirro :: Elemento
esbirro = UnElemento{
    tipo = "Maldad",
    ataque = causarDanio 1,
    defensa = id
}

katanaMagica :: Elemento
katanaMagica = UnElemento{
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa = id
}

portalAlFuturo :: Int -> Elemento
portalAlFuturo unAnio = UnElemento{
    tipo = "Magia",
    ataque = mandarAlAnio anioFuturo,
    defensa = aku anioFuturo . salud
}where anioFuturo = 2800 + unAnio

-- Auxiliar

concentrarse :: Int -> Personaje -> Personaje
concentrarse nivelConcentracion = (!! (nivelConcentracion-1)) . iterate (meditar .) $ meditar
--concentrarse 1                  = meditar
--concentrarse nivelConcentracion = concentrarse (nivelConcentracion - 1) . meditar

--------------------- PUNTO 4 ---------------------

-- ELEMENTO DE PRUEBA
aku1 = aku 0 200

-- De cada elemento, el ataque va para el enemigo y el defensivo para si mismo

luchar :: Personaje -> Personaje -> (Personaje,Personaje)
luchar atacante defensor
    | estaMuerto . aplicarAtaques atacante $ defensor = (atacante,defensor)
    | otherwise                                       = luchar (aplicarAtaques atacante defensor) (aplicarDefensivos atacante)

-- Auxiliar

aplicarAtaques :: Personaje -> Personaje -> Personaje
aplicarAtaques atacante defensor = aplicarElementos defensor ataque (elementos atacante)
--aplicarAtaques atacante defensor = foldr (($) . ataque) defensor (elementos atacante)

aplicarDefensivos :: Personaje -> Personaje
aplicarDefensivos atacante = aplicarElementos atacante defensa (elementos atacante)
--aplicarDefensivos atacante = foldr (($) . defensa) atacante (elementos atacante)

aplicarElementos :: Personaje -> (Elemento -> Transformacion) -> [Elemento] -> Personaje
aplicarElementos unPersonaje particularidadElemento unosElementos = foldr (($) . particularidadElemento) unPersonaje unosElementos

--------------------- PUNTO 5 ---------------------
f :: (Eq a, Num b) => ([a] -> b -> (c,c)) -> (b -> [a]) -> [a] -> [b] -> [c]
f x y z
    | y 0 == z  = map (fst.x z)
    | otherwise = map (snd.x (y 0))