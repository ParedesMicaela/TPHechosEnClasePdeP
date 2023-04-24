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
carolina = Jugador {nombre = "Carolina", tactica = "Accionista", acciones = [pasarPorElBanco], cantDinero = 500, propiedades=[] }

--pagarAAccionistas :: Accion
--pagarAAccionistas unJugador
  --  |unJugador {tactica == "Accionistas"} = agregarPlata 200 unJugador|otherwise = quitarPlata 100 unJugador

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = cambiarTactica . agregarPlata 40 $ unJugador

--agregarPlata :: Int -> Jugador -> Jugador
--agregarPlata unaSuma (Jugador nombre cantDinero tactica propiedad acciones) = Jugador nombre (cantDinero + unaSuma) tactica propiedad acciones

agregarPlata :: Int -> Jugador -> Jugador
agregarPlata unaSuma unJugador = unJugador {cantDinero = cantDinero unJugador + unaSuma}

quitarPlata :: Int -> Jugador -> Jugador
quitarPlata unaSuma unJugador = unJugador {cantDinero = cantDinero unJugador - unaSuma}

enojarse :: Accion
enojarse unJugador = agregarAccion gritar . agregarPlata 50 $ unJugador

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion unaAccion unJugador = unJugador {acciones = acciones unJugador ++ [unaAccion]}

cambiarTactica :: Jugador -> Jugador
cambiarTactica unJugador = Jugador {tactica = "Comprador compulsivo"}

gritar :: Accion
gritar unJugador = unJugador {nombre = "AAHH" ++ nombre unJugador } 


 