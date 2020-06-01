module Lib where
import Text.Show.Functions

data Raton = UnRaton{
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: [String]
}deriving (Show,Eq)

------------------------------------PUNTO 1------------------------------------
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion","tuberculosis"]
bicenterra = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["obesidad","sinusitis"]
------------------------------------PUNTO 2------------------------------------
type Hierba = Raton->Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton{edad= sqrt (edad raton)}--sqrt trabaja con float asi que cambio edad para que funcione

hierbaVerde :: String->Hierba
hierbaVerde terminacion raton = raton{enfermedades = sacoCiertasEnfermedades terminacion (enfermedades raton)}

sacoCiertasEnfermedades :: String->[String]->[String]
sacoCiertasEnfermedades terminacion enfermedades = filter (not.terminanEn terminacion) enfermedades

terminanEn :: String->String->Bool
terminanEn terminacion  =(== terminacion).(tomoTerminacion (length terminacion))

tomoTerminacion :: Int->String->String
tomoTerminacion num enfermedad = drop ((length enfermedad)-num) enfermedad 

alcachofa :: Hierba
alcachofa raton = raton{peso=disminuyoPeso (peso raton)}

disminuyoPeso :: Float->Float
disminuyoPeso peso |peso > 2 = peso * 0.9
                   |otherwise = peso * 0.95

hierbaZort :: Hierba
hierbaZort raton = raton{nombre="Pinky",edad=0,enfermedades=[]}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton{peso= max 0 ((peso raton)-0.1),enfermedades = filter ((>10).length) (enfermedades raton) }

------------------------------------PUNTO 3------------------------------------
type Medicamento = Hierba

pondsAntiAge :: Medicamento
pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

reduceFatFast :: Int->Medicamento
reduceFatFast potencia  = (hierbaVerde "idad").(alcachofaPotenciada potencia)

alcachofaPotenciada :: Int->Medicamento
alcachofaPotenciada potencia  = last.(take (potencia)).(iterate alcachofa)--no esta teniando efecto
--con iterate creo una lista infinita con ratones despues de pasar por alcahcofa,tomo los primeros 2 y de ahi tomo el ultimo
--que seria el resultado de aplicarle dos veces alcachofa

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Hierba
pdepCilina raton = foldr ($) raton (map (hierbaVerde) sufijosInfecciosas)--hago una lista con la hierba y cada terminacion
                                                                        --y le aplico el raton. me salio d 1 #proud

------------------------------------PUNTO 4------------------------------------
--Parte A--
cantidadIdeal ::(Int->Bool)->Int
cantidadIdeal condicion = head.(filter condicion)
--Parte B--
lograEstabilizar :: Medicamento->[Raton]->Bool
lograEstabilizar medicamento = estabiliza.(map enfermedades).(map medicamento)

estabiliza :: [[String]]->Bool
estabiliza listaEnfermedades = sacaObesidad listaEnfermedades && menosDe3Enfermedades listaEnfermedades

sacaObesidad :: [[String]]->Bool
sacaObesidad enfermedades = all (==True) (map (not.(elem "obesidad")) enfermedades) 

menosDe3Enfermedades :: [[String]]->Bool
menosDe3Enfermedades enfermedades = all (==True )(map ((<3).length) enfermedades) 

--Parte C--
--buscoPotencia ::Hierba->[Raton]->Int
--buscoPotencia hierba = sum.(map (cantidadIdeal hierba))
--Armo una lista con la cantidad que requiere cada raton y sumo estos valores.

------------------------------------PUNTO 5------------------------------------
--Parte A--
{-
No va a ser posible saber si el medicamento va a estabilizar a la comunidad ya que este terminaria de aplicarse a la comunidad.

--Parte B--
En este caso si seria posible ya que buscaria en la lista hasta encontrar el caso que cumple la condicion.si tuviera que buscar
todos los ratones que quedan asi volvemos al caso A, ya que tendira que analizar una lista infinita

------------------------------------PUNTO 6------------------------------------
--Parte A--
para agregar una nueva hierba solo habira que codear lo que deberia hacer sin cambiar nada de lo que ya esta codeado.Lo mismo sucede
si quiero agregar un nuevo medicamento que utilize esta hierba solamente.En caso de que algun medicamento ya existente utilize esta
nueva hierba solo habria que pasarselo como paramtero.

--Parte B--
El concepto aplicado es el de orden superior ya que una funcion recive una funcion como parametro.En este caso, este concepto es el
que me permite agregar funciones nuevas que pueden ser usadas por otras funciones sin la necesidad de modificar las funciones
existentes.

--Parte c--
Solo habria que modificar las funciones que utilizen SOLO el peso del raton.El cambio que habira que hacer es cambiar el tipo de dato
a uno que se ajuste al nuevo requerimiento
-}