{--
    Proyecto I: Haskell
    Jack “El Monádico”Lambda
    Carlos Rivero 13-11216
    Jose Barrera  15-10123
--}

module Cartas
(   
    Palo,
    Rango,
    Carta,
    Jugador,
    Mano,
    Mazo,
    Eleccion,
    vacia,
    baraja,
    cantidadCartas, 
    valor,
    busted,
    blackjack,
    ganador,
    separar,
    barajar,
    inicialLambda,
    desdeMano,
    puedePicar,
    aplanar,
    reconstruir,
    robar,
    juegaLambda,
    manoALista,
    addCarta,
    desdeLista,
    unirManos
) where

import System.Random
import Data.List

data Palo = Treboles | Diamantes | Picas | Corazones deriving (Eq, Enum)
data Rango = N Int | Jack | Queen | King | Ace deriving Eq

data Carta = Carta {
    rango   :: Rango,
    palo    :: Palo
} deriving Eq

data Jugador = Dealer | Player deriving (Show, Eq)

newtype Mano = Mano [Carta]

data Mazo = Vacio | Mitad Carta Mazo Mazo deriving (Show, Eq)

data Eleccion = Izquierdo | Derecho deriving (Eq, Read)


instance Show Palo where
    show Treboles = "♣"
    show Diamantes = "♢"
    show Picas = "♠"
    show Corazones = "♡"

instance Show Rango where
    show (N i) = show i
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

instance Show Carta where
    show (Carta r p) = show p ++ show r

instance Show Mano where
    show (Mano []) = "vacio"
    show (Mano [c]) = show c
    show (Mano (c:cs)) = (show c) ++ (show (Mano cs))


-- Funciones de construcción:

-- Produce una mano vacía
vacia :: Mano
vacia = Mano []

-- Produce una mano con las 52 cartas de la baraja
baraja :: Mano
baraja = Mano [ (Carta x y) | x <- [N 2, N 3, N 4, N 5, N 6, N 7, N 8, N 9, N 10] ++ [Jack, Queen, King, Ace], y <- [Treboles .. Corazones] ]

-- Funciones de acceso:

-- Determina la cantidad de cartas en una mano
cantidadCartas :: Mano ->Int
cantidadCartas (Mano x) = length x

-- Recibe una mano y devuelve su valor
valor :: Mano ->Int
valor (Mano x) | sum (map (valorCarta) x) > 21 = (sum (map (valorCarta) x)) - (10 * (numTimesFound 11 (map (valorCarta) x)))
               | otherwise = sum (map (valorCarta) x)

----------------------------------------------
-- Numero de veces que un valor esta en una lista (utilizado para encontrar el numero de A en una mano)
numTimesFound :: Eq a => a -> [a] -> Int
numTimesFound x xs = (length . filter (==x)) xs

-- Recibe una carta y devuelve su valor
valorCarta :: Carta -> Int
valorCarta (Carta r p) = case r of
    N i -> i
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11
----------------------------------------------

-- Recibe una mano y dice si su valor excede 21
busted :: Mano ->Bool
busted x = valor (x) > 21

-- Recibe una mano y devuelve True si la mano tiene Blackjack (suma 21 y tiene 2 cartas)
blackjack :: Mano ->Bool
blackjack (Mano x) = valor(Mano x) == 21 && length x == 2

-- Recibe la mano del dealer y la del player y devuelve quien es el ganador
ganador :: Mano ->Mano ->Jugador
ganador x y | busted(y) || blackjack(x) = Dealer
            | busted(x) || blackjack(y) = Player
            | valor(x) > valor(y) || valor(x) == valor(y) = Dealer
            | valor(x) < valor(y) = Player

-- Recibe una mano y la separa en la tupla (Mitad izquierda, carta del medio, mitad derecha)
separar :: Mano ->(Mano, Carta, Mano)
separar (Mano x) = (Mano (take ((length x)`div`2) x), x!!((length x)`div`2), Mano (drop ((length x)`div`2+1) x))

--Funciones de modificación:

-- recibe una mano con la baraja y la barajea de forma aleatoria
barajar :: StdGen ->Mano ->Mano
barajar g x = barajarAux g x vacia

----------------------------------------------
-- Auxiliar para barajar que recibe 2 manos, la baraja vieja y la nueva baraja
-- cuando la vieja baraja esta vacia devuelve la nueva baraja
barajarAux :: StdGen -> Mano -> Mano -> Mano
barajarAux g (Mano x) (Mano y) | x == []   = (Mano y)
                               | otherwise = barajarAux g (Mano (removeN rnd x)) (Mano (y ++ [x!!rnd])) 
                                where rnd = (take 1 (randomRs (0, (length x) -1) g))!!0

-- Elimina de una lista el elemento en el indice indicado
removeN :: Int -> [a] -> [a]
removeN _ []     = []
removeN i (x:xs)
   | i == 0    = xs
   | otherwise = (x : removeN (i-1) xs)
----------------------------------------------

-- Recibe la baraja inicial y devuelve la mano inicial de Lambda con las dos primeras cartas
inicialLambda :: Mano ->(Mano, Mano)
inicialLambda (Mano (x:y:xs)) = (Mano [x,y], Mano xs)

-- Mazo

--Funciones de construcción:

-- Crea un mazo desde una mano
desdeMano :: Mano ->Mazo
desdeMano (Mano []) = Vacio
desdeMano x = desdeManoAux (separar x)

----------------------------------------------
-- Auxiliar para desdeMano que va creando el mazo de forma recursiva
desdeManoAux :: (Mano, Carta, Mano) -> Mazo
desdeManoAux (Mano x, y, Mano z)  | z == [] && x == [] = Mitad y Vacio Vacio
                        | z == []                      = Mitad y (desdeManoAux (separar (Mano x))) Vacio
                        | otherwise                    = Mitad y (desdeManoAux (separar (Mano x))) (desdeManoAux (separar (Mano z)))
----------------------------------------------

--Funciones de acceso:

-- Funcion que devuelve True si el Mazo tiene mas de una carta
puedePicar :: Mazo ->Bool
puedePicar Vacio = False
puedePicar (Mitad x y z) = not (y == Vacio && z == Vacio)

--Funciones de modificación:

-- Recibe un mazo y devuelve una mano en el mismo orden
aplanar :: Mazo ->Mano
aplanar x = Mano (aplanarAux x)

----------------------------------------------
-- Auxiliar para aplanar que crea la lista de cartas de la mano
aplanarAux :: Mazo -> [Carta]
aplanarAux Vacio = []
aplanarAux (Mitad x y z) = (aplanarAux y) ++ [x] ++ (aplanarAux z)
----------------------------------------------

-- Recibe un mazo, le quita los elementos de una mano y devuelve el mazo reconstruido
reconstruir :: Mazo ->Mano ->Mazo
reconstruir x (Mano y) = desdeMano (Mano ((aplanarAux x)\\y))

-- Devuelve una mano resultante de agarrar del mazo una carta y la mano con esa carta agregada
robar :: Mazo ->Mano ->Eleccion ->Maybe (Mazo,Mano)
robar x y z             | x == Vacio = Nothing

robar (Mitad x a b) (Mano y) z | b == Vacio && z == Derecho   = Nothing --Just (Vacio, Mano (y ++ ([x])))
                               | a == Vacio && z == Izquierdo = Nothing --Just (Vacio, Mano (y ++ ([x])))
                               | z == Izquierdo               = Just (a, Mano (y ++ [getMitadMazo a]))
                               | z == Derecho                 = Just (b, Mano (y ++ [getMitadMazo b]))

-- Devuelve la mano resultante de robar hasta que supere un valor de 16
juegaLambda :: Mazo ->Mano ->Maybe Mano
juegaLambda x (Mano y)  | x == Vacio                                                    = Nothing
                        | (valor (Mano y)) > 16                                         = Just (Mano y)
                        | otherwise                                                     = (juegaLambda a b) where
                            a = reconstruir x (Mano (take 1 (manoALista (aplanar x))))
                            b = Mano (y ++ (take 1 (manoALista (aplanar x))))

----------------------------------------------
-- Devuelve la carta del medio de un mazo
getMitadMazo :: Mazo -> Carta
getMitadMazo (Mitad x _ _) = x

-- Convierte una mano a una lista de cartas
manoALista :: Mano -> [Carta]
manoALista (Mano x) = x

-- Recibe una mano y una carta, devuelve la mano resultante de agregar esa carta
addCarta :: Mano -> Carta -> Mano
addCarta (Mano x) y = Mano (x ++ [y])

-- Convierte una lista de cartas en una mano
desdeLista :: [Carta] -> Mano
desdeLista x = Mano x

-- Agarra dos manos y devuelve la mano resultante de unirlas
unirManos :: Mano -> Mano -> Mano
unirManos (Mano x) (Mano y) = Mano (x ++ y)
----------------------------------------------