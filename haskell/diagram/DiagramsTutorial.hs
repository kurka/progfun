{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine



-- example :: Diagram B
-- example = circle 1 # fc blue
--                    # lw veryThick
--                    # lc purple
--                    # dashingG [0.2, 0.05] 0
example :: Diagram B
-- example = atop $ map alignT  [(square 1), (pentagon 1)]
-- example = atop (square 1 # alignB) (pentagon 1 # alignB)
example = mconcat . map alignB . map ($ 1) $ map regPoly [3..50] 



main :: IO ()
main = mainWith example

