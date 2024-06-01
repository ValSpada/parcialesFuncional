import Text.Show.Functions
import Data.List(genericLength)

data Elemento = UnElemento{
    tipo    :: String,
    ataque  :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje)
} deriving (Show)

data Personaje = UnPersonaje{
    salud        :: Float,
    elementos    :: [Elemento],
    anioPresente :: Int
} deriving (Show)

-- Usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene.
-- En caso de no indicar efecto defensivo u ofensivo, no se altera al personaje recibido.

--------------------- PUNTO 1 ---------------------

-- a.
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje { anioPresente = unAnio }

-- b.
meditar :: Personaje -> Personaje
meditar = alterarSalud (1.5 *)

-- c.
causarDanio :: Float -> Personaje -> Personaje
causarDanio danio unPersonaje
    | (> 0).salud.recibirDanio $ unPersonaje =  recibirDanio unPersonaje
    | otherwise                              = alterarSalud (* 0) unPersonaje
    where recibirDanio = alterarSalud (subtract danio)

-- Auxiliar
alterarSalud :: (Float -> Float) -> Personaje -> Personaje
alterarSalud alteracion unPersonaje = unPersonaje { salud = alteracion . salud $ unPersonaje }

--------------------- PUNTO 2 ---------------------

-- a.
esMalvado :: Personaje -> Bool
esMalvado (UnPersonaje _ unosElementos _) = any (== "Maldad") . map tipo $ unosElementos

-- b.
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje (UnElemento _ atacar _) = (salud unPersonaje) - ((salud . atacar) unPersonaje)

-- c.
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje unosEnemigos = filter (verificarMortalidad unPersonaje) unosEnemigos

-- Auxiliar
verificarMortalidad :: Personaje -> Personaje -> Bool
verificarMortalidad unPersonaje unEnemigo = (== 0) . salud . aplicarAtaques unEnemigo $ unPersonaje

--------------------- PUNTO 3 ---------------------

-- a.
concentracion :: Float -> Elemento
concentracion nivelConcentracion = UnElemento {
    tipo = "Magia",
    ataque = noAfecta,
    defensa = concentrarse nivelConcentracion
}

-- b.
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados 0                = []
esbirrosMalvados cantidadEsbirros = esbirro:(esbirrosMalvados (cantidadEsbirros - 1))

-- c.
jack = UnPersonaje 300 [concentracion 3, katanaMagica] 200

-- d.
aku :: Int -> Float -> Personaje
aku anioActual cantidadSalud = UnPersonaje cantidadSalud (portalAlFuturo anioActual:esbirrosMalvados (100*anioActual)) anioActual

-- Auxiliar
esbirro :: Elemento
esbirro = UnElemento{
    tipo = "Maldad",
    ataque = causarDanio 1,
    defensa = noAfecta
}

katanaMagica :: Elemento
katanaMagica = UnElemento{
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa = noAfecta
}

portalAlFuturo :: Int -> Elemento
portalAlFuturo unAnio = UnElemento{
    tipo = "Magia",
    ataque = mandarAlAnio anioFuturo,
    defensa = aku anioFuturo . salud
}where anioFuturo = 2800 + unAnio

-- Auxiliar

concentrarse :: Float -> Personaje -> Personaje
concentrarse 1                  = meditar
concentrarse nivelConcentracion = concentrarse (nivelConcentracion - 1) . meditar

noAfecta :: Personaje -> Personaje
noAfecta unPersonaje = unPersonaje

--------------------- PUNTO 4 ---------------------

-- ELEMENTO DE PRUEBA
aku1 = aku 0 200

-- De cada elemento, el ataque va para el enemigo y el defensivo para si mismo

luchar :: Personaje -> Personaje -> (Personaje,Personaje)
luchar atacante defensor
    | (== 0) . salud . aplicarAtaques atacante $ defensor = (defensor,atacante)
    | otherwise                                           = luchar (aplicarAtaques atacante defensor) (aplicarDefensivos atacante)

-- Auxiliar

aplicarAtaques :: Personaje -> Personaje -> Personaje
aplicarAtaques atacante defensor = foldr (($) . ataque) defensor (elementos atacante)

aplicarDefensivos :: Personaje -> Personaje
aplicarDefensivos atacante = foldr (($) . defensa) atacante (elementos atacante)

--------------------- PUNTO 5 ---------------------
f :: (Eq a, Num b) => ([a] -> b -> (c,c)) -> (b -> [a]) -> [a] -> [b] -> [c]
f x y z
    | y 0 == z  = map (fst.x z)
    | otherwise = map (snd.x (y 0))