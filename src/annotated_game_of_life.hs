import qualified Data.Map as M

type Coords = (Int, Int)
type Cell = (Bool, Int)
type World = M.Map Coords Cell

tick :: Cell -> Bool
tick (a, 2) = a
tick (_, 3) = True
tick (_, _) = False

explode :: Coords -> World
explode (x, y) =
  M.fromList $
    do xmod <- [-1, 0, 1]
       ymod <- [-1, 0, 1]
       let center = xmod  == 0 && ymod == 0
       return ((x + xmod, y + ymod),
               (center, if center then 0 else 1))

mergeCell :: Cell -> Cell -> Cell
mergeCell (a1, n1) (a2, n2) = (a1 || a2, n1 + n2)

mergeWorlds :: [World] -> World
mergeWorlds = foldr (M.unionWith mergeCell) M.empty

worldTick :: [Coords] -> [Coords]
worldTick coords =
  M.keys $ M.filter tick $ mergeWorlds $ map explode coords

main = print (worldTick [(2,1), (2,2), (2,3)])

-- 6 whitespace
-- 23 loc

