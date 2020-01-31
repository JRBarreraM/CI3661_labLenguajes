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

data Palo = Treboles | Diamantes | Picas | Corazones deriving (Eq, Enum)
data Rango = N Int | Jack | Queen | King | Ace deriving Eq

data Carta = Carta {
    rango :: Rango,
    palo :: Palo
} deriving Eq

data Jugador = Dealer | Player

newtype Mano = Mano [Carta] deriving Show

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

--Funciones de construcción:
vacia :: Mano
vacia = Mano []

baraja :: Mano
baraja = Mano [ (Carta x y) | x <- [N 2, N 3, N 4, N 5, N 6, N 7, N 8, N 9, N 10] ++ [Jack, Queen, King, Ace], y <- [Treboles .. Corazones] ]

--Funciones de acceso:
cantidadCartas :: Mano ->Int
cantidadCartas (Mano x) = length x

valor :: Mano ->Int
valor (Mano []) = 0
valor (Mano (c:cs)) =
    if valorCarta(c) + valor (Mano cs) > 21 && valorCarta(c) == 11
        then 1 + valor (Mano cs)
        else valorCarta(c) + valor (Mano cs)

valorCarta :: Carta -> Int
valorCarta (Carta r p) = case r of
    N i -> i
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11

{-
busted :: Mano ->Bool

blackjack :: Mano ->Bool

ganador :: Mano ->Mano ->Jugador

separar :: Mano ->(Mano, Carta, Mano)

--Funciones de modificación:
barajar :: StdGen ->Mano ->Mano
inicialLambda :: Mano ->(Mano, Mano)

data Mazo = Vacio | Mitad Carta Mazo Mazo
data Eleccion = Izquierdo | Derecho

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