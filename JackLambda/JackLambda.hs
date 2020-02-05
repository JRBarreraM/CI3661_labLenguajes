{--
    Proyecto I: Haskell
    Jack “El Monádico”Lambda
    Carlos Rivero 13-11216
    Jose Barrera 15-10123
--}

import Cartas
import System.Random
import System.IO
import Control.Monad
import Data.List
import Data.Maybe

data GameState = GS {
    juegosJugados   :: Int,
    victoriasLambda :: Int,
    nombre          :: String,
    generador       :: StdGen,
    dinero          :: Int,
    objetivo        :: Int,
    apuesta         :: Int
}

instance Show GameState where
    show (GS a b c d e f g) = "GS "++ show a ++" "++ show b ++" "++ c ++" "++ show d ++" "++ show e ++" "++ show f ++" "++ show g

menu :: GameState -> IO ()
menu game = do

    putStrLn ("\nPartidas jugadas: " ++ (show (juegosJugados game)))
    putStrLn ("Partidas ganadas por Jack Lambda: " ++ (show (victoriasLambda game)))
    putStrLn ("Partidas ganadas por " ++ (nombre game) ++ ": " ++ (show ((juegosJugados game) - (victoriasLambda game))))
    putStrLn ("Dinero restante: " ++ show (dinero game))
    putStrLn "\nElige una opcion (número):\n 1. Jugar ronda\n 2. Guardar partida\n 3. Cargar partida\n 4. Salir"
    opcion <- readLn

    if (opcion == 1) then do
        jugar game
    else if (opcion == 2) then do
        guardar game
        menu game
    else if (opcion == 3) then do
        putStrLn "Bicho, donde esta tu juego?"
        archivo <- getLine
        handle <- openFile archivo ReadMode
        contents <- hGetContents handle
        let gsAttr = words contents
        let juegosJugados = read (gsAttr!!1)
        let victoriasLambda = read (gsAttr!!2)
        let nombre = (gsAttr!!3)
        let generador = read (gsAttr!!4 ++ " " ++ gsAttr!!5) :: StdGen
        let dinero = read (gsAttr!!6)
        let objetivo = read (gsAttr!!7)
        let apuesta = read (gsAttr!!8)

        --print generador
        let gs = GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta
        putStrLn ("Se cargo exitosamente la partida: " ++ show gs)
        hClose handle
        menu gs

    else if (opcion == 4) then
        putStrLn "Chao bicho"
    else do
        putStrLn "Ingresa una opción válida, gafo"
        menu game

jugar :: GameState -> IO ()
jugar game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do
    --let deck = barajar generador baraja
    let tupla = (inicialLambda (barajar generador baraja))
    let manoLambda = fst tupla
    let deck = snd tupla
    putStrLn (nombre ++ ", esta es mi primera carta: " ++ show ((manoALista manoLambda)!!0))
    if (blackjack manoLambda) then do
        putStrLn (nombre ++ ", he sacado blackjack. Yo gano.")
        if ((dinero - apuesta) < (apuesta)) then
            putStrLn (nombre ++ ", no te queda dinero. Es el fin del juego para ti")
            --exit
        else do
            let tempGame = GS (juegosJugados +1) (victoriasLambda +1) nombre generador (dinero-apuesta) objetivo apuesta
            menu tempGame
    else do
        let deck1 = desdeMano deck
        let mano = addCarta vacia (getCarta deck1)
        putStrLn (nombre ++ ", ¿robarás de la izquierda o de la derecha?")
        
        ---------------------------------------------------------------------------------------------------------------------

        input <- getLine
        let lado = read (input) :: Eleccion
        let estado = robar deck1 mano lado
        
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (blackjack (snd (fromJust estado))) then
            putStrLn (nombre ++ ", tu mano es un blackjack")
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            let tempGame = GS (juegosJugados +1) victoriasLambda nombre generador (dinero-apuesta) objetivo apuesta
            menu2 tempGame (fromJust estado) manoLambda

menu2 :: GameState -> (Mazo, Mano) -> Mano -> IO ()
menu2 game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) (deck, hand) manoLambda = do
    putStrLn "\nElige una opcion (número):\n 1. Hit\n 2. Stand\n 3. Double Down\n 4. Surrender"

    --putStrLn "\nElige una opcion (número):\n 1. Hit\n 2. Stand\n 3. Double Down\n 4. Surrender"

    opcion <- readLn
    if (opcion == 1) then
        hit (deck, hand) manoLambda game
    else if (opcion == 2) then
        stand (deck, hand) manoLambda True game
    else if (opcion == 3 && dinero >= apuesta) then
        --doubleDown game
        return ()
    else if (opcion == 4 && (cantidadCartas hand) == 2) then
        --surrender game
        return ()  
    else do
        putStrLn "Por favor ingrese una opción válida"
        menu2 game (deck,hand) manoLambda

hit :: (Mazo, Mano) -> Mano -> GameState -> IO ()
hit (deck, hand) manoLambda game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    if (puedePicar deck) then do
        let deck1 = deck
        
        putStrLn (nombre ++ ", ¿robarás de la izquierda o de la derecha?")

        ---------------------------------------------------------------------------------------------------------------------

        input <- getLine
        ----------------
        let lado = read (input) :: Eleccion
        let estado = robar deck1 hand lado
        
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (busted (snd (fromJust estado))) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))) ++ ". Perdiste.")
            if ((dinero - apuesta) < (apuesta)) then
                putStrLn (nombre ++ ", no te queda dinero. Es el fin del juego para ti")
                --exit
            else do
                let tempGame = GS (juegosJugados +1) (victoriasLambda +1) nombre generador (dinero-apuesta) objetivo apuesta
                menu tempGame
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            menu2 game (fromJust estado) manoLambda

    else do
        let deck1 = reconstruir (desdeMano (barajar generador baraja)) (unirManos hand manoLambda)

        putStrLn (nombre ++ ", ¿robarás de la izquierda o de la derecha?")
        
        ---------------------------------------------------------------------------------------------------------------------

        input <- getLine
        ----------------
        let lado = read (input) :: Eleccion
        let estado = robar deck1 hand lado
        
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (busted (snd (fromJust estado))) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))) ++ ". Perdiste.")
            if ((dinero - apuesta) < (apuesta)) then
                putStrLn (nombre ++ ", no te queda dinero. Es el fin del juego para ti")
                --exit
            else do
                let tempGame = GS (juegosJugados +1) (victoriasLambda +1) nombre generador (dinero-apuesta) objetivo apuesta
                menu tempGame
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            menu2 game (fromJust estado) manoLambda

stand :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO ()
stand (deck, hand) manoLambda doubleDown game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    putStrLn "Es mi turno ahora"
    let manoLambdaFinal = juegaLambda deck manoLambda
    putStrLn ("Mi mano es " ++ show (fromJust manoLambdaFinal))
    putStrLn ("Suma " ++ show (valor (fromJust manoLambdaFinal)))
    
    ganadorFinal (valor hand) (valor (fromJust manoLambdaFinal)) doubleDown game

--doubleDown :: GameState -> IO ()

--surrender :: GameState -> IO ()

ganadorFinal :: Int -> Int -> Bool -> GameState -> IO ()
ganadorFinal player lambda doubleDown game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    if (lambda > player) then do
        putStrLn "Yo gano."
        let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
        menu tempGame
    else if (lambda == player) then do
        putStrLn "Empatamos, así que yo gano."
        let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
        menu tempGame
    else do
        putStrLn "Tu ganas."
        if (doubleDown) then do
            let tempGame = GS juegosJugados victoriasLambda nombre generador (dinero + (apuesta * 4)) objetivo apuesta
            menu tempGame
        else do
            let tempGame = GS juegosJugados victoriasLambda nombre generador (dinero + (apuesta * 2)) objetivo apuesta
            menu tempGame



guardar :: GameState -> IO ()
guardar game = do
    putStrLn "Bicho, donde quieres guardar tu juego?"
    archivo <- getLine
    handle <- openFile archivo WriteMode
    hPutStrLn handle (show game)
    hClose handle

{-
cargar :: IO GameState
cargar = do
    putStrLn "Bicho, donde esta tu juego?"
    archivo <- getLine
    --archivo <- "guardado"
    handle <- openFile archivo ReadMode
    contents <- hGetContents handle
    let gsAttr = words contents
    hClose handle
    let juegosJugados = read (gsAttr!!1)
    let victoriasLambda = read (gsAttr!!2)
    let nombre = (gsAttr!!3)
    let generador1 = read (gsAttr!!4) :: StdGen
    let generador2 = (gsAttr!!5)
    let dinero = read (gsAttr!!6)
    let objetivo = read (gsAttr!!7)
    let apuesta = read (gsAttr!!8)

    print generador1

    GS juegosJugados victoriasLambda nombre generador1 dinero objetivo apuesta
-}

start :: IO ()
start = do
    -- we define "loop" as a recursive IO action
    let datos = do
            putStrLn "Epale bicho, cual es tu nombre? (Sin espacios)"
            nombre <- getLine

            if ((length nombre) < 1 || (length (words nombre)) > 1) then do
                putStrLn "ERROR: Nombre inválido, intentalo de nuevo"
                datos
                return ()
            else do
                putStrLn "Epale bicho, con cuanto dinero quieres iniciar?"
                dinero <- readLn
                if (dinero <= 0) then do
                    putStrLn "ERROR: No puedes empezar en negativo o en cero, intentalo de nuevo"
                    datos
                    return ()
                else do
                    putStrLn "Epale bicho, cuanto dinero hace falta para ganar?"
                    objetivo <- readLn
                    if (objetivo <= dinero) then do
                        putStrLn "ERROR: el objetivo no puede ser menor al dinero inicial, gafo, intentalo de nuevo"
                        datos
                        return ()
                    else do
                        putStrLn "Epale bicho, cuanto dinero se apostara en cada ronda?"
                        apuesta <- readLn
                        if (apuesta <= 0 || apuesta > dinero) then do
                            putStrLn "ERROR: la apuesta debe ser mayor a cero y menor o igual que el dinero inicial, gafo, intentalo de nuevo"
                            datos
                            return ()
                        else do
                            g <- getStdGen
                            let game = GS 0 0 nombre g dinero objetivo apuesta
                            menu game

            {-        
            putStrLn $ "Welcome to our personality test " ++ name 
                    ++ ", inspired by the Big Five Theory."
            putStrLn "You will receive fifty questions in total to which you can reply with Yes or No."
            putStrLn "Whenever you feel ready to begin please write Start"
            goGlenn <- getLine
            putStrLn goGlenn
            -- if we did not finish, start another loop
            when (goGlenn /= "start") menu
            -}
    datos  -- start the first iteration 

main :: IO ()
main = do
    g <- getStdGen
    let game = GS 0 0 "Ian" g 10 20 1
    menu game
--    start 
