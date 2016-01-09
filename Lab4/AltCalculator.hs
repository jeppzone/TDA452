import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages

import AltExpr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> Double -> IO ()
readAndDraw elm canv sc =
    do
        expr <- readInput elm
        if isJust expr
            then draw $ points $ fromJust expr sc csize
            else error
    where
        draw pnts = render canvas (stroke $ path pnts)
        csize = (canHeight, canWidth)

readInput :: Elem -> IO (Maybe Expr)
readInput elm = do
    str <- getValue elm
    if isJust str
        then return $ fromJust $ readExpr str
        else return Nothing

points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (width,height) =  fmap xCoord xs `zip` ys
    where
        pixToReal x = x*scale
        realToPix y = y/scale
        yCoord c1   = height'/2 - c1
        xCoord c2   = width'/2 + c2
        height'     = (fromIntegral width)
        width'      = (fromIntegral height)
        xs          = [0..(pixToReal width')]
        ys          = fmap yCoord [eval expr x | x <- xs]


-- pixToReal :: Double -> Double -- converts a pixel x-coordinate to a real x-coordinate
-- pixToReal x = (canWidth/2) + x
--
-- realToPix :: Double -> Double -- converts a real y-coordinate to a pixel y-coordinate
-- realToPix y = (canHeight/2) - y

-- main = do
--     -- Elements
--     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
--     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
--     input   <- mkInput 20 "x"                -- The formula input
--     draw    <- mkButton "Draw graph"         -- The draw button
--       -- The markup "<i>...</i>" means that the text inside should be rendered
--       -- in italics.
--
--     -- Layout
--     formula <- mkDiv
--     row formula [fx,input]
--     column documentBody [canvas,formula,draw]
--
--     -- Styling
--     setStyle documentBody "backgroundColor" "lightblue"
--     setStyle documentBody "textAlign" "center"
--     setStyle input "fontSize" "14pt"
--     focus input
--     select input
--
--     -- Interaction
--     Just can <- getCanvas canvas
--     onEvent draw  Click $ \_    -> readAndDraw input can
--     onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
--       -- "Enter" key has code 13
