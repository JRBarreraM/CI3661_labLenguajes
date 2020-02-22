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
plegar Vacio            = transformarVacio
plegar (RamaM x y)      = transformarRamaM x (plegar y)
plegar (RamaB x y z)    = transformarRamaB x (plegar y) (plegar z)

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
analizarArbolMB :: (Ord a) => ArbolMB a -> Maybe (a, a, Bool)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
where
transVacio = ¿?
transRamaM = ¿?
transRamaB = ¿?

-- 0.g)
Deberia tomar n funciones

-- 0.h)
foldr

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
--subs (const (subs id id)) (subs (subs (const subs) const) (const (subs id id)))
subs subs const (subs (const (subs subs (subs (subs subs const)))) const)

-- c)
id :: a -> a
id x = const x ()

-- d)
S = subs
K = const
I = id
-- Añadir foto de Ski