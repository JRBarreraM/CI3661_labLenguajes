{--
    Proyecto I: Haskell
    Jack “El Monádico”Lambda
    Carlos Rivero 13-11216
    Jose Barrera  15-10123
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

--Permite al usuario escoger: jugar, guardar, cargar o salir.
--Chequea si el usuario puede segir apostando o ha llegado a la meta.
menu :: GameState -> IO ()
menu game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do
    if (dinero < apuesta) then
        putStrLn (nombre ++ ", no te queda dinero. Es el fin del juego para ti")
    else if (dinero >= objetivo) then
        putStrLn ("Felicidades, " ++ nombre ++ ", me has derrotado. Es el fin del juego para mı́.")
    else do

        putStrLn ("\nPartidas jugadas: " ++ (show juegosJugados))
        putStrLn ("Partidas ganadas por Jack Lambda: " ++ (show victoriasLambda))
        putStrLn ("Partidas ganadas por " ++ nombre ++ ": " ++ (show (juegosJugados - victoriasLambda)))
        putStrLn ("Dinero restante: " ++ show dinero)
        putStrLn "\nElige una opcion (número):\n 1. Jugar ronda\n 2. Guardar partida\n 3. Cargar partida\n 4. Salir"
        opcion <- readLn

        if (opcion == 1) then do
            g <- newStdGen
            jugar (GS juegosJugados victoriasLambda nombre g (dinero-apuesta) objetivo apuesta)
        else if (opcion == 2) then do
            guardar game
            menu game
        else if (opcion == 3) then do
            putStrLn "Bicho, donde esta tu juego?"
            archivo <- getLine
            --Abrimos y leemos el archivo de datos guardados
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
            let gs = GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta
            putStrLn ("Se cargo exitosamente la partida: " ++ show gs)
            --Cerramos el archivo de datos guardados
            hClose handle
            --Se va al menu para continuar la partida cargada
            menu gs

        else if (opcion == 4) then
            putStrLn "Adios bicho"
        else do
            putStrLn "Ingresa una opción válida, bicho"
            menu game

--Controla el inicio de cada ronda, repartiendo las cartas iniciales.
--Luego cede el control a menuRonda
jugar :: GameState -> IO ()
jugar game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do
    let tupla = (inicialLambda (barajar generador baraja))
    let manoLambda = fst tupla
    let deck = snd tupla
    putStrLn (nombre ++ ", esta es mi primera carta: " ++ show ((manoALista manoLambda)!!0))
    if (blackjack manoLambda) then do
        putStrLn (nombre ++ ", he sacado blackjack. Yo gano.")
        let tempGame = GS (juegosJugados +1) (victoriasLambda +1) nombre generador dinero objetivo apuesta
        menu tempGame
    else do
        let deck1 = desdeMano deck
        let mano = addCarta vacia (getMitadMazo deck1)
        
        lado <- pedir nombre
        let estado = robar deck1 mano lado
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (blackjack (snd (fromJust estado))) then do
            putStrLn (nombre ++ ", tu mano es un blackjack")
            let tempGame = GS (juegosJugados +1) victoriasLambda nombre generador dinero objetivo apuesta
            menuRonda tempGame (fromJust estado) manoLambda
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            let tempGame = GS (juegosJugados +1) victoriasLambda nombre generador dinero objetivo apuesta
            menuRonda tempGame (fromJust estado) manoLambda

--Pregunta al usuario de que mazo quiere robar su carta
pedir :: String -> IO (Eleccion)
pedir nombre = do
            putStrLn (nombre ++ ", ¿robarás del mazo Izquierdo o del Derecho?")        

            input <- getLine

            case input of
                "Izquierdo" ->  return (read ("Izquierdo") :: Eleccion)
                "Derecho"   ->  return (read ("Derecho") :: Eleccion)
                otherwise   ->  do
                    putStrLn "Bicho, Izquierdo o Derecho"
                    pedir nombre

--Permite al usuario decidir sus acciones durante una ronda:
--hit, stand, doubleDown o surrender, verificando las condiciones
--de cada una.
menuRonda :: GameState -> (Mazo, Mano) -> Mano -> IO ()
menuRonda game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) (deck, hand) manoLambda = do
    putStrLn "\nElige una opcion (número):\n 1. Hit\n 2. Stand"

    if (((cantidadCartas hand) == 2) && (dinero >= apuesta)) then do
        putStrLn " 3. Double Down\n 4. Surrender"
        opcion <- readLn
        case opcion of
            1 ->    hit (deck, hand) manoLambda False game
            2 ->    stand (deck, hand) manoLambda False game
            3 ->    doubleDown (deck, hand) manoLambda game
            4 ->    surrender game
            otherwise -> do
                putStrLn "Por favor ingrese una opción válida"
                menuRonda game (deck,hand) manoLambda
            
    else if (dinero >= apuesta) then do
        putStrLn " 3. Double Down"
        opcion <- readLn
        case opcion of
            1 ->    hit (deck, hand) manoLambda False game
            2 ->    stand (deck, hand) manoLambda False game
            3 ->    doubleDown (deck, hand) manoLambda game
            otherwise -> do
                putStrLn "Por favor ingrese una opción válida"
                menuRonda game (deck,hand) manoLambda

    else if ((cantidadCartas hand) == 2) then do
        putStrLn " 4. Surrender"
        opcion <- readLn
        case opcion of
            1 ->    hit (deck, hand) manoLambda False game
            2 ->    stand (deck, hand) manoLambda False game
            4 ->    surrender game
            otherwise -> do
                putStrLn "Por favor ingrese una opción válida"
                menuRonda game (deck,hand) manoLambda

    else do
        opcion <- readLn
        case opcion of
            1 ->    hit (deck, hand) manoLambda False game
            2 ->    stand (deck, hand) manoLambda False game
            otherwise -> do
                putStrLn "Por favor ingrese una opción válida"
                menuRonda game (deck,hand) manoLambda

--Funcion que ejecuta la asigcion de robar del jugador.
--Llama a reconstruir y a robar para obtener la nueva carta. Verifica si
--la mano es busted. Muestra al usuario el valor de su mano despues
--de robar. Chequea si fue llamada desde un doubleDown para llamar 
--a stand o al menu de juego.
hit :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO ()
hit (deck, hand) manoLambda doubleDown game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    if (puedePicar deck) then do
        let deck1 = deck
        
        lado <- pedir nombre
        let estado = robar deck1 hand lado
        
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (busted (snd (fromJust estado))) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))) ++ ". Perdiste.")
            let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
            menu tempGame
        else if (doubleDown) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            stand (fromJust estado) manoLambda doubleDown game
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            menuRonda game (fromJust estado) manoLambda

    else do
        let deck1 = reconstruir (desdeMano (barajar generador baraja)) (unirManos hand manoLambda)

        lado <- pedir nombre
        let estado = robar deck1 hand lado
        
        putStrLn (nombre ++ ", tu mano es " ++ show (snd (fromJust estado)))
        if (busted (snd (fromJust estado))) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))) ++ ". Perdiste.")
            let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
            menu tempGame
        else if (doubleDown) then do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            stand (fromJust estado) manoLambda doubleDown game
        else do
            putStrLn ("Suma " ++ show (valor (snd (fromJust estado))))
            menuRonda game (fromJust estado) manoLambda

--Si el usuario hace un stand, se debe distinguir si este es consecuencia de
--un double down o no, y lambda roba el resto de su mano.
stand :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO ()
stand (deck, hand) manoLambda doubleDown game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    putStrLn "Es mi turno ahora"
    let manoLambdaFinal = juegaLambda deck manoLambda
    putStrLn ("Mi mano es " ++ show (fromJust manoLambdaFinal))
    putStrLn ("Suma " ++ show (valor (fromJust manoLambdaFinal)))
    ganadorFinal hand (fromJust manoLambdaFinal) doubleDown game

--Si el usuario hace un doubleDown, se recibe la mano y el mazo.
--para hacer un hit y luego un stand. Se resta el dinero del hit de una vez.
doubleDown :: (Mazo, Mano) -> Mano -> GameState -> IO ()
doubleDown (deck, hand) manoLambda game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do 
    let dineroRestante = dinero - apuesta
    hit (deck, hand) manoLambda True (GS juegosJugados victoriasLambda nombre generador dineroRestante objetivo apuesta)

--Si el usuario se rine se suma una victoria a lambda, y se asigna el dinero restante
--en el game state. Luego se vuelve al menu
surrender :: GameState -> IO ()
surrender game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do
    putStrLn (nombre ++ ", te has rendido. Yo gano")
    let dineroRestante = dinero + apuesta `div` 2

    let tempGame = (GS juegosJugados (victoriasLambda+1) nombre generador dineroRestante objetivo apuesta)
    menu tempGame

--Dadas las manos del jugador y de lambda determina quien gana,
--hace los cambios al game state correspondientes para volver al menu
ganadorFinal :: Mano -> Mano -> Bool -> GameState -> IO ()
ganadorFinal player lambda doubleDown game @ (GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta) = do
    if ((valor player) > (valor lambda) || (valor lambda) > 21 || (blackjack player)) then do
        putStrLn "Tu ganas."
        if (doubleDown) then do
            let tempGame = GS juegosJugados victoriasLambda nombre generador (dinero + (apuesta * 4)) objetivo apuesta
            menu tempGame
        else do
            let tempGame = GS juegosJugados victoriasLambda nombre generador (dinero + (apuesta * 2)) objetivo apuesta
            menu tempGame
    else if ((valor lambda) > (valor player)) then do
        putStrLn "Yo gano."
        let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
        menu tempGame
    else do
        putStrLn "Empatamos, así que yo gano."
        let tempGame = GS juegosJugados (victoriasLambda +1) nombre generador dinero objetivo apuesta
        menu tempGame

--Guarda en un archivo indicado por consola el game state actual
guardar :: GameState -> IO ()
guardar game = do
    putStrLn "Bicho, donde quieres guardar tu juego?"
    archivo <- getLine
    handle <- openFile archivo WriteMode
    hPutStrLn handle (show game)
    hClose handle

--Primer menu con el que interactua el usuario al inicio de cada juego
--se encarga de cargar de un archivo o pedir por consola la data
--de juego necesaria para ir al menu primer menu de juego
start :: IO ()
start = do
    let datos = do
            --Ya que es una partida nueva se piden los datos para configurar la partida al usuario
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
                        putStrLn "ERROR: el objetivo no puede ser menor al dinero inicial, bicho, intentalo de nuevo"
                        datos
                        return ()
                    else do
                        putStrLn "Epale bicho, cuanto dinero se apostara en cada ronda?"
                        apuesta <- readLn
                        if (apuesta <= 0 || apuesta > dinero) then do
                            putStrLn "ERROR: la apuesta debe ser mayor a cero y menor o igual que el dinero inicial, bicho, intentalo de nuevo"
                            datos
                            return ()
                        else do
                            g <- getStdGen
                            let game = GS 0 0 nombre g dinero objetivo apuesta
                            menu game

    putStrLn "Bicho, quieres cargar una partida? (y,n)"
    input <- getLine
    case input of
        --En caso afirmativo se busca, abre y lee el archivo
        "y" -> do
            putStrLn "Bicho, donde esta tu juego?"
            archivo <- getLine
            --Abrimos y leemos el archivo de datos guardados
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
            let gs = GS juegosJugados victoriasLambda nombre generador dinero objetivo apuesta
            putStrLn ("Se cargo exitosamente la partida: " ++ show gs)
            --Cerramos el archivo de datos guardados
            hClose handle
            --Se va al menu para continuar la partida cargada
            menu gs

        --En caso negativo se procede a jugar una partida nueva
        "n" -> datos

        --En caso de otra entrada se repite la pregunta
        otherwise -> do
            putStrLn "Bicho, \"y\" o \"n\""
            start

main :: IO ()
main = do
    start