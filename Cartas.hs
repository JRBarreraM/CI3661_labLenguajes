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

data Palo = Treboles | Diamantes | Picas | Corazones
data Rango = N Int | Jack | Queen | King | Ace

data Carta = Carta {
    rango :: Rango,
    palo :: Palo
}

data Jugador = Dealer | Player

newtype Mano = Mano [Card]

--Funciones de construcción:
vacia :: Mano

baraja :: Mano

--Funciones de acceso:
cantidad Cartas :: Mano ->Int

valor :: Mano ->Int

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