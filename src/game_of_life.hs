import qualified Data.Map as M

tick (a, 2) = a
tick (_, 3) = True
tick (_, _) = False

explode (x, y) =
  do xmod <- [-1, 0, 1]
     ymod <- [-1, 0, 1]
     let center = xmod  == 0 && ymod == 0
     return ((x + xmod, y + ymod),
             (center, if center then 0 else 1))

mergeCell (a1, n1) (a2, n2) = (a1 || a2, n1 + n2)

worldTick coords =
  M.keys $
   M.filter tick $
    foldr (M.unionWith mergeCell) M.empty $
     map M.fromList $ map explode coords

main = print (worldTick [(2,1), (2,2), (2,3)])

-- 4 whitespace
-- 16 loc
