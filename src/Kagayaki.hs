module Kagayaki where

import qualified Kagayaki.Type as K (Shape(..))
import Kagayaki.Type
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Time.Clock
import Data.List (sort)
import Debug.Trace (trace)

(width, height) = (500,320) :: (Int,Int)
(minSize,maxSize) = (15,25)

framerate = 30

window :: Display
window = InWindow "いのちの輝き君" (width, height) (300, 0)

runKagayaki :: IO ()
runKagayaki = do
    now <- floor . utctDayTime <$> getCurrentTime :: IO Int
    let ds = map (flip (/) (fromIntegral now).fromIntegral) . randomRs (0,now) $ mkStdGen now :: [Float]
    play window white framerate (initKagayaki ds) draw dealEvent update
    where
        initKagayaki ds = Kagayaki { bodies = [], divs = ds }

draw :: Kagayaki -> Picture
draw k = Pictures . map toPicture $ zip (sort $ bodies k) (divs k)
    where
        toPicture (body,d) = case shape body of
            K.Circle -> flucture d . move (pos body) . fillCircle red $ size body
            K.Ellipse v ->  flucture d . move (pos body) . rotate' v . scale 1 1.4 . fillCircle red $ size body
            K.Eye _ -> flucture d $ eyePictures body

        eyePictures body = move (pos body) . Pictures $
            [ fillCircle red (size body)
            , rotate' (direction body) $ Pictures
                [ translate (size body/4) 0 $ fillCircle white (size body/2)
                , rotate' (direction body) $ translate (size body/8) 0 $ fillCircle blue (size body/4)
                ]
            ]

update :: Float -> Kagayaki -> Kagayaki
update _ k = k { divs = drop (length $ bodies k) $divs k }

dealEvent :: Event -> Kagayaki -> Kagayaki
dealEvent (EventKey (MouseButton LeftButton) Up _ p) k = clickUpdate p k
dealEvent (EventKey key Down _ _) k = case key of
    Char c -> k {bodies = []}
    _ -> k
dealEvent _ k = k

clickUpdate pos' k = trace (showBody newBody) $ k
    { bodies = bodies k ++ [newBody]
    , divs = drop 5 $ divs k
    }
    where
        size' = (maxSize-minSize) * divs k !! 0 + minSize
        shape' = getShape (divs k !! 3) (divs k !! 4)
        newBody = Body { size = size', pos = pos', shape = shape' }

getShape v a
    | v < 1/4 = K.Eye a
    | v < 1/2 = K.Ellipse a
    | otherwise = K.Circle

showBody :: Body -> String
showBody body = sizeS ++ posS ++ shapeS ++ "\n"
    where
        sizeS = "size = " ++ (take 6.show $ size body)
        posS = " (x,y) = (" ++ (take 6.show.fst $ pos body)
                    ++ "," ++ (take 6.show.snd $ pos body) ++ ")"
        shapeS = " shape = " ++ (show $ shape body)

move (x,y) = translate x y
toX = (*) (fromIntegral width) . flip (-) 0.5
toY = (*) (fromIntegral height) . flip (-) 0.5

fillCircle c v = color c $ Pictures
    [ thickCircle v v
    , rectangleSolid v v
    ]

rotate' = rotate . (*) 180

direction body = (\(Eye v) -> v) $ shape body

flucture d = move (3*(d-0.5),10*(d-0.5))
