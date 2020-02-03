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
    putStrLn "\nElige una opcion (número):\n 1. Jugar ronda\n 2. Guardar partida\n 3. Cargar partida\n 4. Salir"
    opcion <- readLn
    if (opcion == 1) then
        --menu (jugar game)
        return ()
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

--jugar :: GameState -> GameState

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
    putStrLn "Before the loop!"
    -- we define "loop" as a recursive IO action
    let datos = do
            putStrLn "Epale bicho, cual es tu nombre?"
            nombre <- getLine
            if ((length nombre) < 1) then do
                putStrLn "ERROR: No ingresaste nada, intentalo de nuevo"
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
    let game = GS 0 0 "Ian" g 0 1 0
    menu game
--    start 
