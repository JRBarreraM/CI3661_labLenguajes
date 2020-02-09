## Jack “El Monádico” Lambda

Proyecto 1 del Laboratorio de Lenguajes de Programacion I / CI3661.
Programa para jugar black jack en la terminal, escrito en haskell segun los lineamientos del enunciado Proyecto_I.pdf

### ¿Como correr el programa?

Teniendo los archivos Cartas.hs, JackLambda.hs y el Makefile. Corra 'make' para producir el ejecutable JackLambda y para usarlo corra ./JackLambda

```
> make
> ./JackLambda
```

### ¿Como funciona el programa?
Una serie de llamadas que ilustrarian una breve sesion de juego podrian ser, de forma muy simplificada:
main -> start -> menu -> jugar -> menuRonda -> hit -> ganadorFinal

### Detalles de implementacion

No se cambio ninguna de las firmas declaradas en el enunciado pero se implementaron las siguientes para resolver las limitaciones que estas proponian, en el modulo Cartas.hs:

Convierte una mano a una lista de cartas 
* manoALista :: Mano -> [Carta]

Devuelve la carta del medio de un mazo
* getMitadMazo :: Mazo -> Carta

Recibe una mano y una carta, devuelve la mano resultante de agregar esa carta
* addCarta :: Mano -> Carta -> Mano

Convierte una lista de cartas en una mano
* desdeLista :: [Carta] -> Mano

Agarra dos manos y devuelve la mano resultante de unirlas
* unirManos :: Mano -> Mano -> Mano

Auxiliar para aplanar que crea la lista de cartas de la mano
* aplanarAux :: Mazo -> [Carta]

Auxiliar para desdeMano que va creando el mazo de forma recursiva
* desdeManoAux :: (Mano, Carta, Mano) -> Mazo

Auxiliar para barajar que recibe 2 manos, la baraja vieja y la nueva baraja. Cuando la vieja baraja esta vacia devuelve la nueva baraja
* barajarAux :: StdGen -> Mano -> Mano -> Mano

Elimina de una lista el elemento en el indice indicado
* removeN :: Int -> [a] -> [a]

Numero de veces que un valor esta en una lista (utilizado para encontrar el numero de A en una mano)
* numTimesFound :: Eq a => a -> [a] -> Int

Mientras que en el cliente, JackLambda.hs:

Permite al usuario escoger: jugar, guardar, cargar o salir. Chequea si el usuario puede segir apostando o ha llegado a la meta.
* menu :: GameState -> IO ()

Controla el inicio de cada ronda, repartiendo las cartas iniciales. Luego cede el control a menuRonda
* jugar :: GameState -> IO ()

Pregunta al usuario de que mazo quiere robar su carta
* pedir :: String -> IO (Eleccion)

Permite al usuario decidir sus acciones durante una ronda:
hit, stand, doubleDown o surrender, verificando las condiciones de cada una.
* menuRonda :: GameState -> (Mazo, Mano) -> Mano -> IO ()

Funcion que ejecuta la asigcion de robar del jugador.
Llama a reconstruir y a robar para obtener la nueva carta. Verifica si la mano es busted. Muestra al usuario el valor de su mano despues de robar. Chequea si fue llamada desde un doubleDown para llamar a stand o menuRonda.
* hit :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO()

Si el usuario hace un stand, se debe distinguir si este es consecuencia de un double down o no, y lambda roba el resto de su mano.
* stand :: (Mazo, Mano) -> Mano -> Bool -> GameState -> IO()

Si el usuario hace un doubleDown, se recibe la mano y el mazo, para hacer un hit y luego un stand. Se resta el dinero del hit de una vez.
* doubleDown :: (Mazo, Mano) -> Mano -> GameState -> IO ()

Si el usuario se rine se suma una victoria a lambda, y se asigna el dinero restante en el game state. Luego se vuelve al menu
* surrender :: GameState -> IO ()

Dadas las manos del jugador y de lambda determina quien gana, hace los cambios al game state correspondientes para volver al menu
* ganadorFinal :: Int -> Int -> Bool -> GameState -> IO ()

Guarda en un archivo indicado por consola el game state actual
* guardar :: GameState -> IO ()

Primer menu con el que interactua el usuario al inicio de cada juego se encarga de cargar de un archivo o pedir por consola la data de juego necesaria para ir al menu primer menu de juego
* start :: IO ()

Se nota ademas que la funcion:
* ganador :: Mano ->Mano ->Jugador

No se implemento, en su luagar se uso ganadorFinal, pues ganador no daba informacion sobre el empate.

## Autores

* **José Barrera** - *15-10123* - [JRBarreraM](https://github.com/JRBarreraM)

* **Carlos Rivero** - *13-11216* - [CarlosRivero96](https://github.com/CarlosRivero96)