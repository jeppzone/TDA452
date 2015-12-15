import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.List
import Data.Maybe
import Pages
import AltExpr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> Double -> IO ()
readAndDraw elem canvas sc = 
  do
    expr <- readInput elem
    if isJust expr
      then render canvas (stroke (path (ps expr)))
    else error "Faulty expression"
   where 
     ps exp = points (fromJust exp) sc (canWidth, canHeight)                   

readInput :: Elem -> IO (Maybe Expr)
readInput elem = 
  do
    string <- getValue elem
    if isJust string
      then return $ readExpr $ fromJust string
    else return Nothing

diffInput :: Elem -> IO()
diffInput input = 
  do
    string <- getProp input "value"
    let exp = fromJust $ readExpr string
    set input [prop "value" =: (showExpr $ differentiate exp)]

drawZoom :: Elem -> Elem -> Canvas -> Double -> IO()
drawZoom input zoomLevel canvas amount =
  do
    string <- getValue zoomLevel
    let newScale = abs (amount + (read (fromJust string)) :: Double)
    simpleDraw input zoomLevel canvas newScale

simpleDraw :: Elem -> Elem -> Canvas -> Double -> IO()
simpleDraw input zoomLevel canvas scale = 
  do
    set zoomLevel [prop "value" =: (show scale)]
    readAndDraw input canvas scale

-- Calculates and creates all points in a function
points :: Expr -> Double -> (Int,Int) -> [Point]
points expr sc (w, h) = [ (x,realToPix (eval expr (pixToReal x))) | x <- [0.0..dWidth]]
  where
    pixToReal :: Double -> Double  -- converts a pixel x-coordinate to a real x-coordinate
    pixToReal x = sc * (x - dWidth / 2)
    realToPix :: Double -> Double  -- converts a real y-coordinate to a pixel y-coordinate
    realToPix y = -y / sc + dHeight / 2
    dWidth = fromIntegral w
    dHeight = fromIntegral h

main = do
    -- Elements
    canvas    <- mkCanvas canWidth canHeight   -- The drawing area
    fx        <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input     <- mkInput 20 "x"                -- The formula input
    draw      <- mkButton "Draw graph"         -- The draw button
    diff      <- mkButton "Differentiate"
    zoomIn    <- mkButton "Zoomz in"
    zoomOut   <- mkButton "Zoom out"
    zoomLevel <- mkInput 0 "0.04"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw, diff, zoomIn, zoomOut]
 
    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input
    
    let scale = 0.04
    let amount = 0.003
    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click  $ \_    -> simpleDraw input zoomLevel can scale
    onEvent input KeyUp  $ \code -> when (code==13) $ simpleDraw input zoomLevel can scale
    onEvent diff Click $ \_      -> diffInput input 
    onEvent zoomIn Click $ \_    -> drawZoom input zoomLevel can (-amount)
    onEvent zoomOut Click $ \_   -> drawZoom input zoomLevel can amount
      -- "Enter" key has code 13



