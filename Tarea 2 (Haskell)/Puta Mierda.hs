data ArbolMB a =    vacio
                |   RamaM a (ArbolMB a)
                |   RamaB a (ArbolMB a) (ArbolMB a)

-- 0.a)
Vacio :: ArbolMB a
RamaM :: a -> ArbolMB a -> ArbolMB a
RamaB :: a -> ArbolMB a -> ArbolMB a -> ArbolMB a

-- 0.b)
transformarVacio :: a -> b
transformarRamaM :: a -> b -> b
transformarRamaB :: a -> b -> b -> b

-- 0.c)
plegarArbolMB   :: (a -> b)
                -> (a -> b -> b)
                -> (a -> b -> b -> b)
                -> ArbolMB a
                -> b

plegarArbolMB transVacio transRamaM transRamaB = plegar
where
plegar Vacio            = transVacio
plegar (RamaM x y)      = transRamaM x (plegar y)
plegar (RamaB x y z)    = transRamaB x (plegar y) (plegar z)

-- 0.d)
sumarArbolMB :: (Num a) => ArbolMB a -> a
sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
where
transVacio          = 0
transRamaM (x y)    = x + sumarArbolMB y
transRamaB (x y z)  = x + sumarArbolMB y + sumarArbolMB z

-- 0.e)
aplanarArbolMB :: ArbolMB a -> [a]
aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
where
transVacio          = []
transRamaM (x y)    = aplanarArbolMB y : x
transRamaB (x y z)  = aplanarArbolMB y : x : aplanarArbolMB z

-- 0.f)
import Data.Tuple.Select
import Data.Maybe

analizarArbolMB :: (Ord a) => ArbolMB a -> Maybe (a, a, Bool)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
where
transVacio          = Nothing
transRamaM (x y)    = Just ( a, b, c) where
    a = min x (Sel1 d) where
        d = (fromMaybe ((maxBound :: Int), 0, True) (analizarArbolMB y))

    b = max x (Sel2 d) where
        d = (fromMaybe (0, (minBound :: Int), True) (analizarArbolMB y))

    c = (Sel3 d) && (x >= (Sel2 d)) where
        d = (fromMaybe (0, (minBound :: Int), True) (analizarArbolMB y))

transRamaB (x y z)  = Just ( a, b, c) where
    a = min x (min (Sel1 d) (Sel1 e)) where
        d = (fromMaybe ((maxBound :: Int), 0, True) (analizarArbolMB y))
        e = (fromMaybe ((maxBound :: Int), 0, True) (analizarArbolMB z))

    b = max x (max (Sel2 d) (Sel2 e)) where
        d = (fromMaybe (0, (minBound :: Int), True) (analizarArbolMB y))
        e = (fromMaybe (0, (minBound :: Int), True) (analizarArbolMB z))

    c = (Sel3 d) && (x >= (Sel2 d)) && (x <= (Sel1 e)) where
        d = (fromMaybe (0, (minBound :: Int), True) (analizarArbolMB y))
        e = (fromMaybe ((maxBound :: Int), 0, True) (analizarArbolMB z))

-- 0.g)
Deberia tomar n funciones

-- 0.h)
foldr

-- 0.i) (Extra)
foldable


-- 1.a)
Es necesario poner la S en la instancia porque el Monad Secuencial depende de su estado interno para generar el resultado final.
Y porque la instancia debe ser de kind (* -> *)

-- 1.b)
return :: a -> Secuencial s a
(>>=) :: Secuencial s a -> (a -> Secuencial s b) -> Secuencial s b
(>>) :: Secuencial s a -> Secuencial s b -> Secuencial s b
fail :: String -> Secuencial s a

-- 1.c)
return x = Secuencial ( \s -> (x, s) )

-- 1.d)
(Secuencial programa) >>= transformador =
    Secuencial $ \estadoInicial ->
        let (resultado, nuevoEstado)    = programa estadoInicial
            (Secuencial nuevoPrograma)  = transformador resultado
        in nuevoPrograma nuevoEstado

-- 1.e) (Extra)
State


-- INVESTIGACION
id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

subs :: (a -> b -> c) -> (a -> b) -> a -> c
subs x y z = x z (y z)

-- a)
subs (id const) const id
    
    (id const) id (const id)
    const id (const id)
    id

--b)
subs subs const (subs (const (subs subs (subs (subs subs const)))) const)

-- c)
id :: a -> a
id x = const x ()

-- d)
S = subs
K = const
I = id
-- Añadir foto de Ski