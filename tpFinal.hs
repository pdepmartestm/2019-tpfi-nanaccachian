import Text.Show.Functions

--DEFINICIONES
type Tesoro = (String,Int)
type FormaDeSaqueo = Tesoro->Bool

data Pirata = Pirata {
nombre::String,
botin::[(String,Int)]
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

jackSparrow = Pirata {
nombre = "JackSparrow",
botin = [("brujula",10000),("frasco de arena",0)]
}

davidJones = Pirata {
nombre = "David Jones",
botin = [("caja musical",1)]
}

anneBonny = Pirata {
nombre = "Anne Bonny",
botin = [("doblones",100),("frasco de arena",1)]
}

elizabethSwann = Pirata {
nombre = "Elizabeth Swann",
botin = [("moneda",100),("espadaDeHierro",50)]
}

willTurner = Pirata {
nombre = "Will Turner",
botin =[("cuchillo",5)]
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
objetoEspecifico = ("frasco de arena",1),
nombreIs = "Isla Tortuga"
}

islaRon = Isla {
objetoEspecifico = ("botella de ron",25),
nombreIs = "Isla del Ron"
}

lima = Ciudad {
tesorosCiudad = [("arco encantado",120),("espejo",25)]
}

--TESOROS PIRATAS
segundosElementos::[Tesoro]->[Int]
segundosElementos tesoros = map (snd) tesoros  

primerosElementos::[Tesoro]->[String]
primerosElementos tesoros = map (fst) tesoros  

name::Pirata->String
name (Pirata nom _) = nom

tesoros::Pirata->[Tesoro]
tesoros (Pirata _ tes) = tes

listaTesoros::[Pirata]->[Tesoro]
listaTesoros pir = concat (map (tesoros) pir)

cantTesoros::Pirata->Int
cantTesoros pir = (length.segundosElementos.tesoros) pir

esAfortunado::Pirata->Bool
esAfortunado pir = (sum.segundosElementos.tesoros) pir >= 10000

mismoTesoroValorDistinto::Pirata->Pirata->Bool
mismoTesoroValorDistinto pirata1 pirata2 = any (pirataTieneMismoTesoroConValorDistinto pirata2) (tesoros pirata1)

pirataTieneMismoTesoroConValorDistinto::Pirata->Tesoro->Bool
pirataTieneMismoTesoroConValorDistinto (Pirata _ tesoros) tesoro = any (tieneMismoNombreYDistintoValor tesoro) tesoros
 
tieneMismoNombreYDistintoValor::Tesoro->Tesoro->Bool
tieneMismoNombreYDistintoValor t1 t2 = (fst t1 == fst t2) && (snd t1 /= snd t2)

tesoroMasValioso::Pirata->Int
tesoroMasValioso pir = (maximum.segundosElementos.tesoros) pir

agregarTesoro::Tesoro->Pirata->Pirata
agregarTesoro tesoro (Pirata nom bot) = Pirata nom (bot ++ [tesoro])

perderTesorosValiosos::Pirata->Pirata
perderTesorosValiosos (Pirata nom tes) = Pirata nom (filter ((<=100).snd) tes)

perderTesorosNombres::Pirata->String->Pirata
perderTesorosNombres (Pirata nom tes) nomtes = Pirata nom (filter ((/=nomtes).fst) tes)

--TEMPORADA DE SAQUEOS
soloValiosos::Tesoro->Bool
soloValiosos = ((>=100).snd)

soloObjetosEspecificos::String->Tesoro->Bool
soloObjetosEspecificos clave tesoro = ((==clave).fst) tesoro

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
