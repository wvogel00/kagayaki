module Kagayaki.Type where

data Kagayaki = Kagayaki
    { bodies :: [Body]
    , divs :: [Diviation]
    }

data Body = Body
    { size :: Float
    , pos :: Pos
    , shape :: Shape
} deriving Eq

type Diviation = Float
type Pos = (Float, Float)
type Direction = Float

data Shape = Circle | Ellipse Direction | Eye Direction deriving (Eq, Show, Ord)

instance Ord Body where
    compare a b = compare (shape a) (shape b)