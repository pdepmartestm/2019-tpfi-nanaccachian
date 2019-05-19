import Text.Show.Functions

--DEFINICIONES
type FormaDeSaqueo = Tesoro->Bool

data Tesoro = Tesoro {
    nombreT::String,
    valorT::Int
} deriving (Show)

data Pirata = Pirata {
nombre::String,
botin::[Tesoro]
} deriving (Show)

data Barco = Barco {
piratas::[Pirata],
saqueoPor::FormaDeSaqueo
} deriving(Show)

data Isla = Isla {
objetoEspecifico::Tesoro,
nombreIs::String
} deriving (Show)

data Ciudad = Ciudad {
tesorosCiudad::[Tesoro]
} deriving (Show)

brujula = Tesoro {
    nombreT= "brujula",
    valorT = 10000
}

frascoDeArena = Tesoro {
    nombreT= "frasco de arena",
    valorT = 0
}

jackSparrow = Pirata {
nombre = "JackSparrow",
botin = [brujula,frascoDeArena]
}

davidJones = Pirata {
nombre = "David Jones",
botin = [cajaMusical]
}

cajaMusical = Tesoro {
    nombreT= "caja musical",
    valorT = 1
}
anneBonny = Pirata {
nombre = "Anne Bonny",
botin = [doblones,frascoDeArena]
}

doblones = Tesoro {
    nombreT = "doblones",
    valorT = 100
}

elizabethSwann = Pirata {
nombre = "Elizabeth Swann",
botin = [moneda,espadaDeHierro]
}

moneda = Tesoro {
    nombreT = "moneda",
    valorT = 100
}

espadaDeHierro = Tesoro {
    nombreT = "espada de hierro",
    valorT = 100
}

willTurner = Pirata {
nombre = "Will Turner",
botin =[cuchillo]
}

cuchillo = Tesoro {
    nombreT = "cuchillo",
    valorT = 5
}

perlaNegra = Barco {
piratas = [jackSparrow,anneBonny,elizabethSwann],
saqueoPor = soloValiosos
}

holandesErrante = Barco {
piratas = [davidJones],
saqueoPor = (soloObjetosEspecificos "caja")
}

islaTortuga = Isla {
objetoEspecifico = frascoDeArena,
nombreIs = "Isla Tortuga"
}

islaRon = Isla {
objetoEspecifico = ron,
nombreIs = "Isla del Ron"
}

ron = Tesoro {
    nombreT = "botella de ron",
    valorT = 25
}

lima = Ciudad {
tesorosCiudad = [arcoEncantado,espejo]
}

arcoEncantado = Tesoro {
    nombreT = "arco encantado",
    valorT = 120
}

espejo = Tesoro {
    nombreT = "espejo",
    valorT = 25
}

--TESOROS PIRATAS
primerosElementos::[Tesoro]->[String]
primerosElementos tesoros = map (nombreT) tesoros

segundosElementos::[Tesoro]->[Int]
segundosElementos tesoros = map (valorT) tesoros  

name::Pirata->String
name (Pirata nom _) = nom

tesoros::Pirata->[Tesoro]
tesoros (Pirata _ tes) = tes

listaTesoros::[Pirata]->[Tesoro]
listaTesoros pir = concat (map (tesoros) pir)

cantTesoros::Pirata->Int
cantTesoros pir = (length.tesoros) pir

esAfortunado::Pirata->Bool
esAfortunado pir = (sum.segundosElementos.tesoros) pir >= 10000

mismoTesoroValorDistinto::Pirata->Pirata->Bool
mismoTesoroValorDistinto pirata1 pirata2 = any (pirataTieneMismoTesoroConValorDistinto pirata2) (tesoros pirata1)

pirataTieneMismoTesoroConValorDistinto::Pirata->Tesoro->Bool
pirataTieneMismoTesoroConValorDistinto (Pirata _ tesoros) tesoro = any (tieneMismoNombreYDistintoValor tesoro) tesoros
 
tieneMismoNombreYDistintoValor::Tesoro->Tesoro->Bool
tieneMismoNombreYDistintoValor t1 t2 = (nombreT t1 == (nombreT t2)) && (valorT t1 /= (valorT t2))

tesoroMasValioso::Pirata->Int
tesoroMasValioso pir = (maximum.segundosElementos.tesoros) pir

agregarTesoro::Tesoro->Pirata->Pirata
agregarTesoro tesoro (Pirata nom bot) = Pirata nom (bot ++ [tesoro])

perderTesorosValiosos::Pirata->Pirata
perderTesorosValiosos (Pirata nom tes) = Pirata nom (filter ((<=100).valorT) tes)

perderTesorosNombres::Pirata->String->Pirata
perderTesorosNombres (Pirata nom tes) nomtes = Pirata nom (filter ((/=nomtes).nombreT) tes)

--TEMPORADA DE SAQUEOS
soloValiosos::Tesoro->Bool
soloValiosos = ((>=100).valorT)

soloObjetosEspecificos::String->Tesoro->Bool
soloObjetosEspecificos clave tesoro = ((==clave).nombreT) tesoro

soloNada::Tesoro->Bool
soloNada tesoro = False

cualquierTesoro::[FormaDeSaqueo]->Tesoro->Bool
cualquierTesoro saqueos tesoro = any (queLoCumpla tesoro) saqueos

queLoCumpla::Tesoro->FormaDeSaqueo->Bool
queLoCumpla tesoro saqueo = saqueo tesoro

saquear::(Tesoro->Bool)->Pirata->Tesoro->Pirata
saquear metodo (Pirata nom tes) tesoro = Pirata nom (tes ++ (filter (metodo) [tesoro]))

--NAVEGANDO LOS SIETE MARES
incorporaTripulacion::Pirata->Barco->Barco
incorporaTripulacion pirata (Barco pir fma) = Barco (pir ++ [pirata]) fma

abandonaTripulacion::Pirata->Barco->Barco
abandonaTripulacion (Pirata nom _) (Barco pir fma) = Barco (filter ((/=nom).name) pir) fma

anclarIslaDeshabitada::Barco->Isla->Barco
anclarIslaDeshabitada (Barco pir fma) (Isla obj _) = Barco (agregarVariosTesoros pir obj) fma

agregarVariosTesoros::[Pirata]->Tesoro->[Pirata]
agregarVariosTesoros pir tes = map (agregarTesoro tes) pir

cantTesorosCiudad::Ciudad->Int
cantTesorosCiudad (Ciudad tes) = length tes

atacarCiudad::Ciudad->Barco->Barco
atacarCiudad (Ciudad tes) (Barco piratas fma) | length piratas > length tes = Barco (zipWith (saquear fma) (take (length tes) piratas) tes) fma
                                              | otherwise = Barco (zipWith (saquear fma) piratas (take (length piratas) tes)) fma

{-Los piratas del barco con mayor cantidad de tripulantes mata a los piratas del otro barco y a cada pirata del barco de mayor tripulacion se le asignan los tesoros de un pirata asesinado del otro barco. En el caso de que las tripulaciones sean iguales, todos los piratas se suben al primer barco y el segundo queda abandonado.-}
abordarOtroBarco::Barco->Barco->Barco
abordarOtroBarco (Barco pir1 fma1) (Barco pir2 fma2) | length pir1 > length pir2 = Barco ((zipWith (saquear fma1) (take (length pir2) pir1) (listaTesoros pir2)) ++ (drop (length pir2) pir1)) fma1
                                                     | length pir1 < length pir2 = Barco ((zipWith (saquear fma2) (take (length pir1) pir2) (listaTesoros pir1)) ++ (drop (length pir1) pir2)) fma2
                                                     | otherwise = Barco (pir1 ++ pir2) fma1

--FIN TP

--COMIENZO TP PARTE II

bono1=("bono",(valor 102 234))

valor::Int->Int->Int
valor max min = (div ((max-min)*50) 100) + (max-min)