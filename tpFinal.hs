import Text.Show.Functions

type Tesoro = (String,Int)
type FormaDeSaqueo = Pirata->Tesoro->Pirata

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

erevan = Ciudad {
tesorosCiudad = [("arco encantado",120),("espejo",25)]
}

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

islaTortuga = Isla {
objetoEspecifico = ("frasco de arena",1),
nombreIs = "Isla Tortuga"
}

islaRon = Isla {
objetoEspecifico = ("botella de ron",25),
nombreIs = "Isla del Ron"
}

perlaNegra = Barco {
piratas = [jackSparrow,anneBonny,elizabethSwann],
saqueoPor = soloValiosos
}

holandesErrante = Barco {
piratas = [davidJones],
saqueoPor= nada
}

segundosElementos::[Tesoro]->[Int]
segundosElementos tesoros = map (snd) tesoros  

primerosElementos::[Tesoro]->[String]
primerosElementos tesoros = map (fst) tesoros 

tesoros::Pirata->[Tesoro]
tesoros (Pirata _ tes) = tes

name::Pirata->String
name (Pirata nom _) = nom

cantTesoros::Pirata->Int
cantTesoros pir = (length.segundosElementos.tesoros) pir

esAfortunado::Pirata->Bool
esAfortunado pir = (sum.segundosElementos.tesoros) pir >= 10000

mismoTesoroValorDistinto::Pirata->Pirata->Bool
mismoTesoroValorDistinto (Pirata nom1 tes1) (Pirata nom2 tes2) = any (==True) (listaNombresIgualesyValoresDistintos tes1 tes2)

listaNombresIgualesyValoresDistintos::[Tesoro]->[Tesoro]->[Bool]
listaNombresIgualesyValoresDistintos tesoro1 tesoro2 = zipWith (&&) (listaNombresIguales tesoro1 tesoro2) (listaValoresDistintos tesoro1 tesoro2)

listaNombresIguales::[Tesoro]->[Tesoro]->[Bool]
listaNombresIguales tesoro1 tesoro2 = zipWith (==) (primerosElementos tesoro1) (primerosElementos tesoro2)

listaValoresDistintos::[Tesoro]->[Tesoro]->[Bool]
listaValoresDistintos tesoro1 tesoro2 = zipWith (/=) (segundosElementos tesoro1) (segundosElementos tesoro2)

tesoroMasValioso::Pirata->Int
tesoroMasValioso pir = (maximum.segundosElementos.tesoros) pir

agregarTesoro::Tesoro->Pirata->Pirata
agregarTesoro tesoro (Pirata nom bot) = Pirata nom (bot ++ [tesoro])

perderTesorosValiosos::Pirata->Pirata
perderTesorosValiosos (Pirata nom tes) = Pirata nom (filter ((<=100).snd) tes)

perderTesorosNombres::Pirata->String->Pirata
perderTesorosNombres (Pirata nom tes) nomtes = Pirata nom (filter ((/=nomtes).fst) tes)

soloValiosos::Pirata->Tesoro->Pirata
soloValiosos (Pirata nom tes) tesoro = Pirata nom (tes ++ filter ((>=100).snd) [tesoro])

verificarPalabra::Tesoro->String->[Tesoro]
verificarPalabra tesoro palabra = filter ((==palabra).fst) [tesoro]

soloObjetosEspecificos::Pirata->Tesoro->String->Pirata
soloObjetosEspecificos (Pirata nom tes) tesoro clave = Pirata nom (tes ++ (verificarPalabra tesoro clave))

nada::Pirata->Tesoro->Pirata
nada pirata tesoro = pirata

saquear::Pirata->FormaDeSaqueo->Tesoro->Pirata
saquear pirata metodo tesoro = metodo pirata tesoro

saquear2::String->Pirata->(Pirata->Tesoro->String->Pirata)->Tesoro->Pirata
saquear2 clave pirata metodo tesoro = metodo pirata tesoro clave

incorporaTripulacion::Pirata->Barco->Barco
incorporaTripulacion pirata (Barco pir fma) = Barco (pir ++ [pirata]) fma

abandonaTripulacion::Pirata->Barco->Barco
abandonaTripulacion (Pirata nom _) (Barco pir fma) = Barco (filter ((/=nom).name) pir) fma

anclarIslaDeshabitada::Barco->Isla->Barco
anclarIslaDeshabitada (Barco pir fma) (Isla obj nom) = Barco (agregarVariosTesoros pir obj) fma

agregarVariosTesoros::[Pirata]->Tesoro->[Pirata]
agregarVariosTesoros pir tes = map (agregarTesoro tes) pir

atacarCiudad::Ciudad->Barco->Barco
atacarCiudad (Ciudad tes) (Barco piratas fma) | length piratas > length tes = Barco (( zipWith (fma) (take (length tes) piratas) tes)) fma
                                              | otherwise = Barco ((zipWith (fma) piratas (take (length piratas) tes)) ++ (drop (length piratas) piratas)) fma


abordarOtroBarco::Barco->Barco->Barco
abordarOtroBarco (Barco pir1 fma1) (Barco pir2 fma2) | length pir1 > length pir2 = Barco ((zipWith (fma1) (take (length pir2) pir1) (listaTesoros pir2))++(drop (length pir2) pir1)) fma1
                                                     | length pir1 < length pir2 = Barco ((zipWith (fma2) (take (length pir1) pir2) (listaTesoros pir1))++(drop (length pir1) pir2)) fma2
													 | otherwise = Barco (pir1++pir2) fma1

listaTesoros::[Pirata]->[Tesoro]
listaTesoros pir = (concat.map (tesoros)) pir
