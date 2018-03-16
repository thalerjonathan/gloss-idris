-- KDTree code
--   by Matthew Sottile <matt@galois.com> <mjsottile@computer.org>
--
module KDTree2d

import Vec2

public export
data KDTreeNode a
  = Empty
  | Node (KDTreeNode a) Vec2 a (KDTreeNode a)

||| An empty KDTree
export
newKDTree : KDTreeNode a
newKDTree = Empty

||| Flatten out a KDTree to a list.
export
kdtreeToList : KDTreeNode a -> List a
kdtreeToList Empty          = []
kdtreeToList (Node l _ x r) = [x] ++ kdtreeToList l ++ kdtreeToList r

||| Apply a worker function to all elements of a KDTree.
export
mapKDTree : KDTreeNode a -> (a -> b) -> List b
mapKDTree Empty _               = []
mapKDTree (Node l p n r) f      = f n :: (mapKDTree l f ++ mapKDTree r f)

kdtAddWithDepth : KDTreeNode a -> Vec2 -> a -> Int -> KDTreeNode a
kdtAddWithDepth Empty pos dat _ = Node Empty pos dat Empty
kdtAddWithDepth (Node left npos ndata right) pos dat d =
    if vecDimSelect pos d < vecDimSelect npos d
      then Node (kdtAddWithDepth left pos dat d') npos ndata right
      else Node left npos ndata (kdtAddWithDepth right pos dat d')
  where
    d' = if (d == 1) then 0 else 1

export
kdtAddPoint : KDTreeNode a -> Vec2 -> a -> KDTreeNode a
kdtAddPoint t p d = kdtAddWithDepth t p d 0

export
kdtInBounds : Vec2 -> Vec2 -> Vec2 -> Bool
kdtInBounds p bMin bMax = vecLessThan p bMax && vecGreaterThan p bMin

mutual
  -- X dimension
  kdtRangeSearchRecX : KDTreeNode a -> Vec2 -> Vec2 -> List (Vec2, a)
  kdtRangeSearchRecX Empty _ _ = []
  kdtRangeSearchRecX (Node 
                        left 
                        npos@(MkVec2 nc _) ndata right) 
                        bMin@(MkVec2 mnc _)  
                        bMax@(MkVec2 mxc _) = 
      if nc < mnc
        then nextfun right bMin bMax
        else if nc > mxc
          then nextfun left bMin bMax
          else if kdtInBounds npos bMin bMax
            then (npos, ndata) :: (nextfun right bMin bMax ++ nextfun left bMin bMax)
            else nextfun right bMin bMax ++ nextfun left bMin bMax
    where
      nextfun         = kdtRangeSearchRecY

  -- Y dimension
  kdtRangeSearchRecY : (KDTreeNode a) -> Vec2 -> Vec2 -> List (Vec2, a)
  kdtRangeSearchRecY Empty _ _    = []
  kdtRangeSearchRecY (Node 
                        left 
                        npos@(MkVec2 nc _) ndata right) 
                        bMin@(MkVec2 mnc _)  
                        bMax@(MkVec2 mxc _) = 
      if nc < mnc
        then nextfun right bMin bMax
        else if nc > mxc
          then nextfun left bMin bMax
          else if (kdtInBounds npos bMin bMax)
            then (npos, ndata) :: (nextfun right bMin bMax ++ nextfun left bMin bMax)
            else nextfun right bMin bMax ++ nextfun left bMin bMax
    where  
      nextfun         = kdtRangeSearchRecX

export
kdtRangeSearch : (KDTreeNode a) -> Vec2 -> Vec2 -> List (Vec2, a)
kdtRangeSearch t bMin bMax = kdtRangeSearchRecX t bMin bMax

export
kdtAddPoints : List (Vec2, a) -> (KDTreeNode a) -> (KDTreeNode a)
kdtAddPoints [] t       = t
kdtAddPoints ((pt, dat) :: ps) t = kdtAddPoints ps $ kdtAddPoint t pt dat

singleCollision : Vec2 -> Vec2 -> Vec2 -> Double -> a -> Maybe (Vec2, a)
singleCollision pt start a eps dat =
    if sqrd_dist < eps * eps
      then Just (vecAdd start p, dat)
      else Nothing
  where
    b : Vec2
    b = vecSub pt start

    xhat : Double
    xhat = (vecDot a b) / (vecDot a a)

    p : Vec2
    p = vecScale a xhat

    e : Vec2
    e  = vecSub p b

    sqrd_dist : Double
    sqrd_dist = vecDot e e

export
kdtCollisionDetect : (KDTreeNode a) -> Vec2 -> Vec2 -> Double -> List (Vec2, a)
kdtCollisionDetect root start@(MkVec2 sx sy) end@(MkVec2 ex ey) eps
    = mapMaybe 
        (\(pt, dat) => singleCollision pt start a eps dat) 
        (kdtRangeSearch root rmin rmax) 
  where
    rmin : Vec2
    rmin = MkVec2 (min sx ex - eps) (min sy ey - eps)

    rmax : Vec2
    rmax = MkVec2 (max sx ex + eps) (max sy ey + eps)

    --pts : List (Vec2, a)
    --pts = kdtRangeSearch root rmin rmax

    a : Vec2
    a = vecSub end start

    --colls : List (Vec2, a)
    --colls = mapMaybe (\(pt, dat) => singleCollision pt start a eps dat) (kdtRangeSearch root rmin rmax)

-- Dumping --------------------------------------------------------------------
{-
-- | Dump a KDTree to a file
dumpKDTree :: KDTreeNode Int -> FilePath -> IO ()
dumpKDTree kdt name 
 = do   h       <- openFile name WriteMode
        hPutStrLn h "n x y z"
        dumpKDTreeInner kdt h
        hClose h


-- | Dump a KDTree to a handle.
dumpKDTreeInner :: KDTreeNode Int -> Handle -> IO ()
dumpKDTreeInner kdt h 
 = case kdt of
        Empty -> return ()

        Node l v d r 
         -> do  printVec v h d
                dumpKDTreeInner l h
                dumpKDTreeInner r h

-- | Print a vector to a handle.
printVec :: Vec2 -> Handle -> Int -> IO ()
printVec (Vec2 x y) h i 
        = hPutStrLn h $ show i ++ " " ++ show x ++ " " ++ show y
        -}