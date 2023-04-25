import Text.Show.Functions ()
data Jugador = Jugador {
    nombre      :: String,
    cantDinero  :: Int,
    tactica     :: String,
    propiedades :: [Propiedad],
    acciones    :: [Accion]
} deriving Show

type Accion = Jugador -> Jugador

type Propiedad = (Nombre,Precio)
type Nombre = String
type Precio = Int

carolina :: Jugador
carolina = Jugador {nombre = "Carolina", tactica = "Accionista", acciones = [pasarPorElBanco,pagarAAccionistas], cantDinero = 500, propiedades=[] }

manuel :: Jugador
manuel = Jugador {nombre = "Manuel", tactica = "Oferente singular", acciones = [pasarPorElBanco,enojarse], cantDinero = 500, propiedades=[] }

pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
  |esAccionista unJugador = agregarPlata 200 unJugador
  |otherwise = quitarPlata 100 unJugador

esAccionista :: Jugador -> Bool
esAccionista unJugador = tactica unJugador == "Accionista"

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = cambiarTactica . agregarPlata 40 $ unJugador

{--agregarPlata :: Int -> Jugador -> Jugador
agregarPlata unaSuma (Jugador nombre cantDinero tactica propiedad acciones) = Jugador nombre (cantDinero + unaSuma) tactica propiedad acciones
--}

agregarPlata :: Int -> Jugador -> Jugador
agregarPlata unaSuma unJugador = unJugador {cantDinero = cantDinero unJugador + unaSuma}

quitarPlata :: Int -> Jugador -> Jugador
quitarPlata unaSuma unJugador = unJugador {cantDinero = cantDinero unJugador - unaSuma}

enojarse :: Accion
enojarse unJugador = agregarAccion gritar . agregarPlata 50 $ unJugador

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion unaAccion unJugador = unJugador {acciones = acciones unJugador ++ [unaAccion]}

cambiarTactica :: Jugador -> Jugador
cambiarTactica unJugador = unJugador {tactica = "Comprador compulsivo"}

gritar :: Accion
gritar unJugador = unJugador {nombre = "AAHH" ++ nombre unJugador } 

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata (_,unPrecio) = unPrecio <= 150 

obtenerPrecioPropiedad :: Propiedad -> Int
obtenerPrecioPropiedad (_,unPrecio) = unPrecio

precioAlquiler :: Propiedad -> Int
precioAlquiler unaPropiedad
  |esPropiedadBarata unaPropiedad = 10
  |otherwise = 20

ingresosPorAlquileres :: Jugador -> Int
ingresosPorAlquileres unJugador = sum . map precioAlquiler . propiedades $ unJugador
 
cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador {cantDinero = cantDinero unJugador + ingresosPorAlquileres unJugador } 

adquirirPropiedad :: Propiedad -> Jugador -> Jugador
adquirirPropiedad unaPropiedad unJugador = unJugador {propiedades = propiedades unJugador ++ [unaPropiedad]}

puedeGanarPropiedad :: String -> Bool
puedeGanarPropiedad "Oferente singular" = True
puedeGanarPropiedad "Accionista" = True
puedeGanarPropiedad _ = False

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
  |puedeGanarPropiedad . tactica $ unJugador = quitarPlata (obtenerPrecioPropiedad unaPropiedad) . adquirirPropiedad unaPropiedad $ unJugador
  |otherwise = unJugador




