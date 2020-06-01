module Lib where
import Text.Show.Functions

data Raton = UnRaton{
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: [String]
}deriving (Show,Eq)

------------------------------------PUNTO 1------------------------------------
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampión","tuberculosis"]
bicenterra = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["obesidad","sinusitis"]
------------------------------------PUNTO 2------------------------------------
type Hierba = Raton->Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton{edad= sqrt (edad raton)}--sqrt trabaja con float asi que cambio edad para que funcione

hierbaVerde :: String->Hierba
hierbaVerde terminacion raton = raton{enfermedades = sacoCiertasEnfermedades terminacion (enfermedades raton)}

sacoCiertasEnfermedades :: String->[String]->[String]
sacoCiertasEnfermedades terminacion enfermedades = filter (terminanEn terminacion) enfermedades

terminanEn :: String->String->Bool
terminanEn terminacion  = (== terminacion).tomoTerminacion

tomoTerminacion :: String->String
tomoTerminacion enfermedad = drop ((length enfermedad)-3) enfermedad --asumiendo q todas las termiaciones son de 3 chars sino rip

alcachofa :: Hierba
alcachofa raton = raton{peso=disminuyoPeso (peso raton)}

disminuyoPeso :: Float->Float
disminuyoPeso peso |peso > 2 = peso * 0.9
                   |otherwise = peso * 0.95

hierbaZort :: Hierba
hierbaZort raton = raton{nombre="Pinky",peso=0,enfermedades=[]}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton{peso= max 0 ((peso raton)-0.1),enfermedades = filter ((>10).length) (enfermedades raton) }

