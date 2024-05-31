import Text.Show.Functions
import Data.List(genericLength)

data Aspecto = UnAspecto{
    tipoDeAspecto :: String,
    grado         :: Float
} deriving (Show,Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor                    = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2             = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado               = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo                   = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

situacion1 = [(UnAspecto "Incertidumbre" 100.2),(UnAspecto "Tension" 200.4),(UnAspecto "Peligro" 400.49)]
situacion2 = [(UnAspecto "Tension" 140.2),(UnAspecto "Peligro" 200.32),(UnAspecto "Incertidumbre" 30.2)]

------------------ PUNTO 1 ------------------

-- a.
modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto funcion unAspecto = cambiarGrado (funcion . grado $ unAspecto) unAspecto

-- b.
mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion unaSituacion otraSituacion = mejorAspecto (obtenerPeligro unaSituacion) (obtenerPeligro otraSituacion) && mejorAspecto (obtenerIncertidumbre unaSituacion) (obtenerIncertidumbre otraSituacion) && mejorAspecto (obtenerTension unaSituacion) (obtenerTension otraSituacion)

-- c.
modificarSituacion :: (Float -> Float) -> Aspecto -> Situacion -> Situacion
modificarSituacion alteracion unAspecto unaSituacion = reemplazarAspecto (modificarAspecto alteracion unAspecto) unaSituacion

-- Auxiliar

cambiarGrado :: Float -> Aspecto -> Aspecto
cambiarGrado unGrado unAspecto = unAspecto { grado = unGrado }

obtenerPeligro :: Situacion -> Aspecto
obtenerPeligro = buscarAspectoDeTipo "Peligro"

obtenerIncertidumbre :: Situacion -> Aspecto
obtenerIncertidumbre = buscarAspectoDeTipo "Incertidumbre"

obtenerTension :: Situacion -> Aspecto
obtenerTension = buscarAspectoDeTipo "Tension"

------------------ PUNTO 2 ------------------

-- a.

data Gema = UnaGema{
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
} deriving(Show)

-- b.

type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente = alterarTension (subtract 10) . alterarIncertidumbre (/2)

relajada :: Float -> Personalidad
relajada relajacion = alterarPeligro (+ relajacion) . alterarTension (subtract 30)

segura :: Personalidad
segura = alterarTension (subtract 40) . alterarIncertidumbre (/ 4) . alterarPeligro (subtract 100)

-- c.

garnet = UnaGema "Garnet" 1000 vidente

amatista = UnaGema "Amatista" 800 (relajada 15)

perla = UnaGema "Perla" 850 segura

-- Auxiliar
alterarTension :: (Float -> Float) -> Situacion -> Situacion
alterarTension alteracion unaSituacion = modificarSituacion alteracion (obtenerTension unaSituacion) unaSituacion

alterarIncertidumbre :: (Float -> Float) -> Situacion -> Situacion
alterarIncertidumbre alteracion unaSituacion = modificarSituacion alteracion (obtenerIncertidumbre unaSituacion) unaSituacion

alterarPeligro :: (Float -> Float) -> Situacion -> Situacion
alterarPeligro alteracion unaSituacion = modificarSituacion alteracion (obtenerPeligro unaSituacion) unaSituacion

------------------ PUNTO 3 ------------------

gemaDominante :: Gema -> Gema -> Situacion -> Gema
gemaDominante unaGema otraGema unaSituacion
    | ganaEnFuerza unaGema otraGema && ganaEnPersonalidad unaGema otraGema unaSituacion = unaGema
    | ganaEnFuerza otraGema unaGema && ganaEnPersonalidad otraGema unaGema unaSituacion = otraGema
    | otherwise                                                                         = (UnaGema "" 0 vidente)

-- Auxiliar

ganaEnFuerza :: Gema -> Gema -> Bool
ganaEnFuerza (UnaGema _ fuerza1 _) (UnaGema _ fuerza2 _) = fuerza1 >= fuerza2

ganaEnPersonalidad :: Gema -> Gema -> Situacion -> Bool
ganaEnPersonalidad (UnaGema _ _ personalidad1) (UnaGema _ _ personalidad2) unaSituacion = mejorSituacion (personalidad1 unaSituacion) (personalidad2 unaSituacion)

------------------ PUNTO 4 ------------------

fusionGemas :: Gema -> Gema -> Situacion -> Gema
fusionGemas unaGema otraGema unaSituacion = (UnaGema (nombreFusion unaGema otraGema) (fuerzaFusion unaGema otraGema unaSituacion) (personalidadFusion unaGema otraGema))

-- Auxiliar

nombreFusion :: Gema -> Gema -> String
nombreFusion unaGema otraGema
    | nombre unaGema == nombre otraGema = nombre unaGema
    | otherwise                         = nombre unaGema ++ nombre otraGema

personalidadFusion :: Gema -> Gema -> Personalidad
personalidadFusion unaGema otraGema = (personalidad unaGema) . (personalidad otraGema) . alterarIncertidumbre (subtract 10) . alterarPeligro (subtract 10) . alterarTension (subtract 10)

fuerzaFusion :: Gema -> Gema -> Situacion -> Int
fuerzaFusion unaGema otraGema unaSituacion
    | fusionCompatible unaGema otraGema unaSituacion && fusionCompatible otraGema unaGema unaSituacion = (* 10) $ fuerza unaGema + fuerza otraGema
    | otherwise                                                                                        = (* 7) . fuerza $ gemaDominante unaGema otraGema unaSituacion

fusionCompatible :: Gema -> Gema -> Situacion -> Bool
fusionCompatible unaGema otraGema unaSituacion = ganaEnPersonalidad (UnaGema "" 0 (personalidadFusion unaGema otraGema)) unaGema unaSituacion

------------------ PUNTO 5 ------------------

fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal _ [x] = x
fusionGrupal unaSituacion (unaGema:otraGema:gemas) = fusionGrupal unaSituacion . (: gemas) $ fusionGemas unaGema otraGema unaSituacion

------------------ PUNTO 6 ------------------

foo x y z = any (== y x) . z

{-
Invocaciones de la función
foo 5 (+7) [1..]
foo 3 even (map (<7))
foo 3 even [1,2,3]
foo [1..] head (take 5) [1..]
-}

-- a.
foo :: (Eq a) => [a] -> ([a] -> a) -> ([a] -> [a]) -> [a] -> Bool

-- b.

-- i.
{-
foo 3 even (map (<7)) -> Aca nos falta una lista para hacer el map, por ende dará error

foo 3 even [1,2,3] -> Aca nos falta la funcion para aplicarle a la lista.

foo 5 (+7) [1..] -> Error de tipos, nos falta la funcion a aplicar en la lista nueva.
-}

-- ii.
{- 
foo [1..] head (take 5) [1..] -> Ya que se espera una lista, una funcion que va de una lista a un valor, 
otra funcion que espera una lista y la otra lista para comparar y devolver el bool.

Al comparar el head de la lista potencialmente infinita que comienza con 1 con la lista de los 5 primeros 
elementos de la misma lista. 
-}

-- iii. 
-- Ningun planteo ingresa a este apartado