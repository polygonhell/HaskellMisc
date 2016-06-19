module ILayout where 


data ILayout a = Leaf {_v :: a}
            | HSplit {_row:: Int, _top, _bottom :: ILayout a }
            | VSplit {_col:: Int, _left, _right :: ILayout a }
            deriving (Show)