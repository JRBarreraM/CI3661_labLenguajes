## Jack “El Monádico” Lambda

Proyecto 1 del Laboratorio de Lenguajes de Programacion I / CI3661.
Programa para jugar black jack en la terminal, escrito en haskell segun los lineamientos del enunciado Proyecto_I.pdf

### ¿Como correr el programa?

Teniendo los archivos Cartas.hs, JackLambda.hs y el Makefile. Corra 'make' para producir el ejecutable JackLambda y para usarlo corra ./JackLambda

```
> make
> ./JackLambda
```

### Detalles de implementacion

Adicionalmente a las funciones obligatorias del proyecto se implementaron las siguientes, en modulo Cartas.hs:

* manoALista :: Mano -> [Carta]
* getCarta :: Mazo -> Carta
* addCarta :: Mano -> Carta -> Mano
* desdeLista :: [Carta] -> Mano
* unirManos :: Mano -> Mano -> Mano
* getMitadMazo :: Mazo -> Carta
* aplanarAux :: Mazo -> [Carta]
* desdeManoAux :: (Mano, Carta, Mano) -> Mazo
* barajarAux :: StdGen -> Mano -> Mano -> Mano
* removeN :: Int -> [a] -> [a]
* numTimesFound :: Eq a => a -> [a] -> Int

Mientras que en el cliente, JackLambda.hs:
* menu :: GameState -> IO ()
* jugar :: GameState -> IO ()
* pedir :: String -> IO (Eleccion)
* menu2 :: GameState -> (Mazo, Mano) -> Mano -> IO ()
* hit :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO()
* stand :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO()
* doubleDown :: (Mazo, Mano) -> Mano -> GameState -> IO ()
* surrender :: GameState -> IO ()
* ganadorFinal :: Int -> Int -> Bool -> GameState -> IO ()
* guardar :: GameState -> IO ()
* start :: IO ()

## Autores

* **José Barrera** - *15-10123* - [JRBarreraM](https://github.com/JRBarreraM)

* **Carlos Rivero** - *13-11216* - [CarlosRivero96](https://github.com/CarlosRivero96)