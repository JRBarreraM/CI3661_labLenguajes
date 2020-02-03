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
import Data.List

data Palo = Treboles | Diamantes | Picas | Corazones deriving (Eq, Enum)
data Rango = N Int | Jack | Queen | King | Ace deriving Eq

data Carta = Carta {
    rango :: Rango,
    palo :: Palo
} deriving Eq

data Jugador = Dealer | Player deriving Show

newtype Mano = Mano [Carta]

data Mazo = Vacio | Mitad Carta Mazo Mazo deriving (Show, Eq)

data Eleccion = Izquierdo | Derecho deriving (Eq)


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

----------------------------------------------
numTimesFound :: Eq a => a -> [a] -> Int
numTimesFound x xs = (length . filter (==x)) xs

valorCarta :: Carta -> Int
valorCarta (Carta r p) = case r of
    N i -> i
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11
----------------------------------------------

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
barajar :: StdGen ->Mano ->Mano
barajar g x = barajarAux g x vacia

----------------------------------------------
barajarAux :: StdGen -> Mano -> Mano -> Mano
barajarAux g (Mano x) (Mano y) | x == []   = (Mano y)
                               | otherwise = barajarAux g (Mano (removeN rnd x)) (Mano (y ++ [x!!rnd])) 
                                where rnd = (take 1 (randomRs (0, (length x) -1) g))!!0

removeN :: Int -> [a] -> [a]
removeN _ []     = []
removeN i (x:xs)
   | i == 0    = xs
   | otherwise = (x : removeN (i-1) xs)
----------------------------------------------

inicialLambda :: Mano ->(Mano, Mano)
inicialLambda (Mano (x:y:xs)) = (Mano [x,y], Mano xs)

--Funciones de construcción:
desdeMano :: Mano ->Mazo
desdeMano (Mano []) = Vacio
desdeMano x = desdeManoAux (separar x)

----------------------------------------------
desdeManoAux :: (Mano, Carta, Mano) -> Mazo
desdeManoAux (Mano x, y, Mano z)  | z == [] && x == [] = Mitad y Vacio Vacio
                        | z == []                      = Mitad y (desdeManoAux (separar (Mano x))) Vacio
                        | otherwise                    = Mitad y (desdeManoAux (separar (Mano x))) (desdeManoAux (separar (Mano z)))
----------------------------------------------

--Funciones de acceso:
puedePicar :: Mazo ->Bool
puedePicar Vacio = False
puedePicar (Mitad x y z) = not (y == Vacio && z == Vacio)

--Funciones de modificación:
aplanar :: Mazo ->Mano
aplanar x = Mano (aplanarAux x)

----------------------------------------------
aplanarAux :: Mazo -> [Carta]
aplanarAux Vacio = []
aplanarAux (Mitad x y z) = (aplanarAux y) ++ [x] ++ (aplanarAux z)
----------------------------------------------

reconstruir :: Mazo ->Mano ->Mazo
reconstruir x (Mano y) = desdeMano (Mano ((aplanarAux x)\\y))

robar :: Mazo ->Mano ->Eleccion ->Maybe (Mazo,Mano)
robar x y z             | x == Vacio = Nothing

robar (Mitad x a b) (Mano y) z | b == Vacio && z == Derecho   = Just(Vacio, Mano (y ++ ([x])))
                               | a == Vacio && z == Izquierdo = Just (Vacio, Mano (y ++ ([x])))
                               | z == Izquierdo               = Just (reconstruir a (Mano ((aplanarAux b) ++ [x])), Mano (y ++ ([x])))
                               | z == Derecho                 = Just (m,n) where
                                   m = reconstruir b (Mano ((aplanarAux a) ++ [x]))
                                   n = Mano (y ++ ([x]))

{-
robar :: Mazo ->Mano ->Eleccion ->Maybe (Mazo,Mano)
robar x y z             | x == Vacio = Nothing

robar (Mitad x a b) (Mano y) z | b == Vacio && z == Derecho   = Nothing
                               | a == Vacio && z == Izquierdo = Nothing
                               | z == Izquierdo               = Just (reconstruir a (Mano (take 1 (aplanarAux a))), Mano (y ++ (take 1 (aplanarAux a))))
                               | z == Derecho                 = Just (m,n) where 
                                   m = reconstruir b (Mano [x])
                                   n = Mano (y ++ ([x]))
-}

juegaLambda :: Mazo ->Mano ->Maybe Mano
juegaLambda x (Mano y)  | x == Vacio                                                    = Nothing
                        | (valor (Mano y)) > 16                                         = Just (Mano y)
--                        | valor (Mano (y ++ (take 1 (manoALista (aplanar x))))) > 16    = Just b
                        | otherwise                                                     = (juegaLambda a b) where
                            a = reconstruir x (Mano (take 1 (manoALista (aplanar x))))
                            b = Mano (y ++ (take 1 (manoALista (aplanar x))))

----------------------------------------------
manoALista :: Mano -> [Carta]
manoALista (Mano x) = x
----------------------------------------------

-- main :: IO ()
main = do
    g <- getStdGen
    print $ barajar g baraja

