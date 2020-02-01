{--
    Proyecto I: Haskell
    Jack “El Monádico”Lambda
    Carlos Rivero 13-11216
    Jose Barrera 15-10123
--}

--Acciones:
--Hit: Pide otra carta al dealer.
--Stand: Decide no recibir mas cartas, cediendo el turno al dealer.
--Double down: Duplica la apuesta, recibe otra carta, y cede el turno.
--Surrender: Después de recibir la mano inicial, el jugador puede decidir
--no jugar la ronda y rendirse, cediendo la mitad de su apuesta.

import System.Random

data Palo = Treboles | Diamantes | Picas | Corazones deriving (Eq, Enum)
data Rango = N Int | Jack | Queen | King | Ace deriving Eq

data Carta = Carta {
    rango :: Rango,
    palo :: Palo
} deriving Eq

data Jugador = Dealer | Player deriving Show

newtype Mano = Mano [Carta]

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
    show (Mano [c]) = show c
    show (Mano (c:cs)) = (show c) ++ (show (Mano cs))

--Funciones de construcción:
vacia :: Mano
vacia = Mano []

baraja :: Mano
baraja = Mano [ (Carta x y) | x <- [N 2, N 3, N 4, N 5, N 6, N 7, N 8, N 9, N 10] ++ [Jack, Queen, King, Ace], y <- [Treboles .. Corazones] ]

--Funciones de acceso:
cantidadCartas :: Mano ->Int
cantidadCartas (Mano x) = length x


valor :: Mano ->Int
valor (Mano x) | sum (map (valorCarta) x) > 21 = (sum (map (valorCarta) x)) - (10 * (numTimesFound 11 (map (valorCarta) x)))
               | otherwise = sum (map (valorCarta) x)

numTimesFound :: Eq a => a -> [a] -> Int
numTimesFound x xs = (length . filter (==x)) xs


valorCarta :: Carta -> Int
valorCarta (Carta r p) = case r of
    N i -> i
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11


busted :: Mano ->Bool
busted x = valor (x) > 21

blackjack :: Mano ->Bool
blackjack (Mano x) = valor(Mano x) == 21 && length x == 2

ganador :: Mano ->Mano ->Jugador
ganador x y | busted(y) || blackjack(x) = Dealer
            | busted(x) || blackjack(y) = Player
            | valor(x) > valor(y) || valor(x) == valor(y) = Dealer
            | valor(x) < valor(y) = Player

separar :: Mano ->(Mano, Carta, Mano)
separar (Mano x) = (Mano (take ((length x)`div`2) x), x!!((length x)`div`2), Mano (drop ((length x)`div`2+1) x))

--Funciones de modificación:
--barajar :: StdGen ->Mano ->Mano
--barajar g0 (Mano x) = Mano (shuffle x)

{-
shuffle :: [a] -> [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)
-}

--  rollDice :: IO Int
--  rollDice = getStdRandom (randomR (1,6))

inicialLambda :: Mano ->(Mano, Mano)
inicialLambda (Mano (x:y:xs)) = (Mano [x,y], Mano xs)

data Mazo = Vacio | Mitad Carta Mazo Mazo
data Eleccion = Izquierdo | Derecho

{-
--Funciones de construcción:
desdeMano :: Mano ->Mazo

--Funciones de acceso:
puedePicar :: Mazo ->Bool

--Funciones de modificación:
aplanar :: Mazo ->Mano

reconstruir :: Mazo ->Mano ->Mazo

robar :: Mazo ->Mano ->Eleccion ->Maybe (Mazo,Mano)

juegaLambda :: Mazo ->Mano ->Maybe Mano
-}