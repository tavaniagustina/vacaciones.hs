data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}

type Idioma = String

--------------
-- Punto 01 --
--------------

ana :: Turista
ana = Turista 0 21 False ["Español"]

beto :: Turista
beto = Turista 15 15 True ["Aleman"]

cathi :: Turista
cathi = Turista 15 15 True ["Aleman", "Catalan"]

--------------
-- Punto 02 --
--------------

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = bajaCansancio 5 unTurista 
    | otherwise           = bajaStress 1 unTurista

bajaCansancio :: Int -> Turista -> Turista 
bajaCansancio unCansancio unTurista = unTurista { cansancio = cansancio unTurista - unCansancio }

bajaStress :: Int -> Turista -> Turista
bajaStress unStress unTurista = unTurista { stress = stress unTurista - unStress }

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje unPaisaje = bajaStress (cantidadDeLetras unPaisaje)

cantidadDeLetras :: String -> Int
cantidadDeLetras unaPalabra = length unaPalabra

type IdiomaQueSeAprende = String

salirAHablarUnIdioma :: IdiomaQueSeAprende -> Excursion
salirAHablarUnIdioma unIdioma unTurista = agregarIdioma unIdioma . cambiarCompania $ unTurista

agregarIdioma :: IdiomaQueSeAprende -> Turista -> Turista
agregarIdioma unIdioma = mapIdioma (unIdioma :)

mapIdioma :: ([Idioma] -> [Idioma]) -> Turista -> Turista
mapIdioma unaFuncion unTurista = unTurista { idiomas = unaFuncion (idiomas unTurista) }

cambiarCompania :: Turista -> Turista
cambiarCompania unTurista = unTurista {viajaSolo = False}

caminar :: Int -> Excursion
caminar cantidadMinutos = aumentaCansancio (intensidadCaminata cantidadMinutos) . bajaStress (intensidadCaminata cantidadMinutos)

aumentaCansancio :: Int -> Turista -> Turista 
aumentaCansancio unCansancio unTurista = unTurista { cansancio = cansancio unTurista + unCansancio }

intensidadCaminata :: Int -> Int
intensidadCaminata minutos = (div minutos 4)

data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)

paseoEnBarco :: Marea -> Excursion
paseoEnBarco estadoMarea unTurista
    | estadoMarea == Fuerte   = aumentaStress 6 . aumentaCansancio 10 $ unTurista
    | estadoMarea == Moderada = unTurista
    | otherwise               = salirAHablarUnIdioma "aleman" . apreciarElementoPaisaje "mar" . caminar 10 $ unTurista

aumentaStress :: Int -> Turista -> Turista 
aumentaStress unStress unTurista = unTurista { stress = stress unTurista + unStress }

-- a) 

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = bajaStress (div (stress unTurista) 10) . unaExcursion $ unTurista

-- b) 

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun unaFuncion unTurista unaExcursion = deltaSegun unaFuncion (hacerExcursion unaExcursion unTurista) unTurista

-- c)

esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista = (> 0) . deltaExcursionSegun (length . idiomas) unTurista

excursionDesestresante :: Turista -> [Excursion] -> [Excursion]
excursionDesestresante unTurista = filter (esDesestresante unTurista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista = (<= 3) . deltaExcursionSegun stress unTurista

--------------
-- Punto 03 --
--------------

type Tour = [Excursion]

completo :: Tour
completo = [salirAHablarUnIdioma "melmacquiano" . caminar 40 . apreciarElementoPaisaje "cascada" . caminar 20]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [caminar 120 . hacerExcursion unaExcursion. paseoEnBarco Tranquila]

islaVecina :: Marea -> Tour
islaVecina estadoMarea = [paseoEnBarco estadoMarea . excursionSegunEstadoMarea estadoMarea . paseoEnBarco estadoMarea]

excursionSegunEstadoMarea :: Marea -> Excursion
excursionSegunEstadoMarea Fuerte = apreciarElementoPaisaje "lago"
excursionSegunEstadoMarea _ = irALaPlaya

-- a) 

hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista unTour = foldl (flip hacerExcursion) (aumentaStress (length unTour) unTurista) unTour

-- b)

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente unTurista = any (esConvincente unTurista)

esConvincente :: Turista -> Tour -> Bool
esConvincente unTurista = any (dejaAcompaniadoAlTurista unTurista) . excursionDesestresante unTurista

dejaAcompaniadoAlTurista :: Turista -> Excursion -> Bool
dejaAcompaniadoAlTurista unTurista = not . viajaSolo . flip hacerExcursion unTurista

-- c)

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour unTour = sum . map (espiritualidadAportada unTour) . filter (flip esConvincente unTour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada unTour = negate . deltaRutina unTour

deltaRutina :: Tour -> Turista -> Int
deltaRutina unTour unTurista = deltaSegun nivelDeRutina (hacerTour unTurista unTour) unTurista

nivelDeRutina :: Turista -> Int
nivelDeRutina unTurista = cansancio unTurista + stress unTurista

--------------
-- Punto 04 --
--------------

-- a)

playasEternas :: Tour
playasEternas = repeat irALaPlaya

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}