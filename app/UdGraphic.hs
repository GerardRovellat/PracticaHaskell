module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    display,
    execute,
    verd, marro, groc, rosa, blau, vermell, negre, blanc
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

-- S'han afegit nous colors.

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0
marro   = Color' 0.5 0.25 0.0
groc    = Color' 0.0 1.0 1.0
rosa    = Color' 1.0 0.0 0.5

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició donada a la practica de Comanda

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | CanviaColor Llapis
               | Branca Comanda
               deriving (Eq, Show)




-- Problema 8

-- Declaració del tipus LlapisActual que es guardara l'estat en cada comanda del llapis
data LlapisActual = LlapisActual Llapis Angle Pnt

execute :: Comanda -> [Ln]
execute c = generarLiniesAPintar c LlapisActual negre 0 (Pnt 0.0 0.0) 

-- Genera una llista de línies a pintar a partir d'una comanda i l'estat actual del llapis.
generarLiniesAPintar :: Comanda -> LlapisActual -> [Ln]
generarLiniesAPintar c act = case c of
  (CanviaColor color) :#: y -> generarLiniesAPintar y (modificarColorActual color act) -- Modifica el color del del llapis actual
  (Branca br) :#: y -> generarLiniesAPintar br act ++ generarLiniesAPintar y act -- Si trobem una branca, executem la comanda per separat
  (Branca br) -> generarLiniesAPintar br act -- Si el final es una branca, s'executa ella mateixa
  Para :#: x -> generarLiniesAPintar x act -- En cas de que hi hagi un Para, es pasa a la seguent comanda disponible
  (Avança n) :#: x -> let punt2 = puntDesti (posicioLlapis act) (angleLlapis act) n in
                        Ln (colorLlapis act) (posicioLlapis act) punt2 : generarLiniesAPintar x (modificarPosicioActual punt2 act) -- Calculem el punt desti i pintem la linea
  (Gira n) :#: x -> generarLiniesAPintar x (modificarAngleActual n act) -- Si la comanda es Gira, es modifica el angle
  (x1 :#: x2) :#: y -> generarLiniesAPintar (x1 :#: x2 :#: y) act -- En cas de trobar una comanda composta, la transformem en una
  (Avança n) -> let punt2 = puntDesti (posicioLlapis act) (angleLlapis act) n in
                  [Ln (colorLlapis act) (posicioLlapis act) punt2] -- Si trobem un Avança al final, pintem la linea
  _ -> [] -- Per finalitzar el pintar linies, retornem llista buida
  where
    -- Modifica el color actual del llapis a l'estat actual.
    modificarColorActual :: Llapis -> LlapisActual -> LlapisActual
    modificarColorActual llapis (LlapisActual _ angle pos) = LlapisActual llapis angle pos
    
    -- Modifica la posició actual del llapis a l'estat actual.
    modificarPosicioActual :: Pnt -> LlapisActual -> LlapisActual
    modificarPosicioActual pos (LlapisActual llapis angle _) = LlapisActual llapis angle pos
    
    -- Modifica l'angle actual del llapis a l'estat actual.
    modificarAngleActual :: Angle -> LlapisActual -> LlapisActual
    modificarAngleActual radians (LlapisActual llapis angle pos) = LlapisActual llapis (angle + radians) pos
    
    -- Obté el color del llapis de l'estat actual.
    colorLlapis :: LlapisActual -> Llapis
    colorLlapis (LlapisActual llapis _ _ ) = llapis
    
    -- Obté la posició del llapis de l'estat actual.
    posicioLlapis :: LlapisActual -> Pnt
    posicioLlapis (LlapisActual _ _ pos) = pos
    
    -- Obté l'angle del llapis de l'estat actual.
    angleLlapis :: LlapisActual -> Angle
    angleLlapis (LlapisActual _ angle _) = angle



-- Pasa de graus a radians
graus2radians :: Float -> Float
graus2radians x = (x*2*pi)/360


-- Donat un punt de referència, un angle i una distància, retorna el punt corresponent.
puntDesti :: Pnt -> Angle -> Distancia -> Pnt
puntDesti (Pnt p1 p2) angle dist = Pnt (p1 + dist * cos (graus2radians angle)) (p2 - dist * sin (graus2radians angle))





-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
