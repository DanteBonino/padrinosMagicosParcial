module Lib () where

--Concediendo Deseos:
--Punto 1:
data Chico = Chico{
    nombre      :: String,
    edad        :: Int,
    habilidades :: [Habilidad],
    deseos      :: [Deseo]
 }
type Habilidad = String

type Deseo = Chico -> Chico

--a
aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades = mapearHabilidades (++ unasHabilidades)

mapearHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapearHabilidades unaFuncion unChico = unChico {habilidades = (unaFuncion . habilidades) unChico}

--b
serGrosoEnElNeedForSpeed :: Deseo
serGrosoEnElNeedForSpeed = aprenderHabilidades jugarTodasLasVersionesDeNeedForSpeed

jugarTodasLasVersionesDeNeedForSpeed :: [Habilidad]
jugarTodasLasVersionesDeNeedForSpeed = map (("jugar al need for speed " ++) . show) [1..]

--c
serMayor :: Chico
serMayor = modificarEdad (const 18)

modificarEdad :: (Int -> Int) -> Chico -> Chico
modificarEdad modificacion unChico = unChico {edad = (modificacion . edad) unChico}

--Punto 2:

type PadrinoMagio = Chico -> Chico

wanda :: PadrinoMagio
wanda = (concederPrimerDeseo . madurar)

madurar :: Chico -> Chico
madurar = aumentarEdad 1

aumentarEdad :: Int -> Chico -> Chico
aumentarEdad unaCantidad = modificarEdad (+1)

concederPrimerDeseo :: Chico -> Chico
concederPrimerDeseo unChico = (cumplirDeseo unChico. head . deseos) unChico

cosmo :: PadrinoMagio
cosmo = desmadurar

desmadurar :: Chico -> Chico
desmadurar = modificarEdad (flip div 2)

muffinMagico :: Chico -> Chico
muffinMagico unChico = foldl (cumplirDeseo) unChico (deseos unChico)

cumplirDeseo :: Chico -> Deseo -> Chico
cumplirDeseo  = flip ($)
