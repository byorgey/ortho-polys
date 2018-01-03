{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module SMTDrawing where

import Data.Maybe

import Data.SBV
import Data.SBV.Internals (SMTModel(..), CW(..), CWVal(..))
import Data.List.Split (chunksOf)

import Control.Lens (both, (^..))
import Diagrams.Prelude hiding (E, (.>), (|||), turn)
import Diagrams.Backend.Rasterific.CmdLine

import GenBracelets

------------------------------------------------------------
-- Data types

data Turn = L | R
  deriving (Eq, Ord, Show, Read, Enum)

data Dir = N | E | S | W
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

turn :: Turn -> Dir -> Dir
turn t = toEnum . (`mod` 4) . (+ turnDelta t) . fromEnum
  where
    turnDelta L = (-1)
    turnDelta R = 1

type Vertex = (SInteger, SInteger)
type Edge   = (Vertex, Vertex)

mkVertex :: [a] -> (a,a)
mkVertex [x,y] = (x,y)

------------------------------------------------------------
-- Constraint solving

orthoPolyDrawing :: [Turn] -> Goal
orthoPolyDrawing turns = do

  -- Create (x,y) variables for each vertex
  vertices <- (map mkVertex . chunksOf 2)
              <$> (mkFreeVars (2*length turns) :: Symbolic [SInteger])

  let vertexCycle = vertices ++ [head vertices]
      edges = zip vertexCycle (tail vertexCycle)

  -- Generate constraints.
  genEdgeConstraints turns E edges
  genCrossingConstraints edges

  -- Minimize the sum of all edge lengths.
  minimize "total edge length" $ sum (zipWith dist vertexCycle (tail vertexCycle))

-- Distance between two points.  In the special case of only
-- horizontal or vertical edges, as we have, Manhattan distance and
-- Euclidean distance coincide.
dist :: Vertex -> Vertex -> SInteger
dist (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

-- Generate edge constraints from the remaining turns, the current
-- direction, and the remaining vertices.
genEdgeConstraints :: [Turn] -> Dir -> [Edge] -> Goal
genEdgeConstraints [] _ _ = return ()
genEdgeConstraints (t:ts) dir ((v1,v2):es) = do

  -- Constrain the edge from v1 to v2 to point in direction dir.
  genEdgeConstraint dir v1 v2

  -- Generate the rest of the constraints, turning the current
  -- direction appropriately.
  genEdgeConstraints ts (turn t dir) es

-- Constrain the second vertex to lie in the given direction from the
-- first.
genEdgeConstraint :: Dir -> Vertex -> Vertex -> Goal
genEdgeConstraint d (x1,y1) (x2,y2) =
  case d of
    N -> do constrain $ x1 .== x2
            constrain $ y1 .<  y2
    E -> do constrain $ y1 .== y2
            constrain $ x1 .<  x2
    S -> do constrain $ x1 .== x2
            constrain $ y1 .>  y2
    W -> do constrain $ y1 .== y2
            constrain $ x1 .>  x2

-- Constrain all nonadjacent edges to not intersect.
genCrossingConstraints :: [Edge] -> Goal
genCrossingConstraints (e:es) = do
  let edgePairs = map (e,) (init $ tail es) ++ skipPairs es
  mapM_ genCrossingConstraint edgePairs

skipPairs :: [a] -> [(a,a)]
skipPairs [] = []
skipPairs [_] = []
skipPairs (a:b:cs) = map (a,) cs ++ skipPairs (b:cs)

-- Constrain two edges to be completely disjoint.  Note that in
-- general this requires doing cross products etc., but in the special
-- case of axis-aligned segments the test for intersection becomes
-- much simpler.  Just think of them as two infinitely thin rectangles.
genCrossingConstraint :: (Edge, Edge) -> Goal
genCrossingConstraint (((x1a,y1a),(x1b,y1b)), ((x2a,y2a),(x2b,y2b))) = do
  let x1L = smin x1a x1b
      x1R = smax x1a x1b
      y1L = smin y1a y1b
      y1R = smax y1a y1b
      x2L = smin x2a x2b
      x2R = smax x2a x2b
      y2L = smin y2a y2b
      y2R = smax y2a y2b
  constrain $ (smax x1L x2L .> smin x1R x2R) ||| (smax y1L y2L .> smin y1R y2R)

------------------------------------------------------------
-- Solution extraction

extractPoly :: OptimizeResult -> Maybe [P2 Double]
extractPoly (LexicographicResult (Satisfiable _ (SMTModel _ vars)))
  = Just $ extractPoints vars
extractPoly _ = Nothing

extractPoints :: [(String, CW)] -> [P2 Double]
extractPoints = map mkP2 . chunksOf 2 . init . map (getInteger . snd)
  where
    getInteger (CW _ (CWInteger i)) = i
    mkP2 [x,y] = fromIntegral x ^& fromIntegral y

------------------------------------------------------------
-- Drawing & generation

drawPoly :: OptimizeResult -> Trail V2 Double
drawPoly = closeTrail . fromVertices . fromMaybe [] . extractPoly

-- Generate all polygons with 2n vertices
allPolys :: Int -> [[Turn]]
allPolys 2 = [[R,R,R,R]]
allPolys n = map (map toEnum) $ genFixedBracelets (2*n) [(0,n-2), (1,n+2)]

drawAllPolysGrid :: Int -> IO (Diagram B)
drawAllPolysGrid n = do
  ores <- mapM (optimize Lexicographic . orthoPolyDrawing) (allPolys n)
  let polyTrails = map drawPoly ores
      polys = map (centerXY . strokeTrail) polyTrails
      side  = ceiling (sqrt (fromIntegral (length polys)))
      w     = maximum . map width  $ polys
      h     = maximum . map height $ polys
      disp  = unlines . map (unwords . map (show . (round :: Double -> Int)) . concatMap ((^.. both) . unp2) . trailVertices . (`at` origin)) $ polyTrails
  putStr disp
  return
    . frame 1
    . lw thin
    .      vcat' (with & catMethod .~ Distrib & sep .~ (h+1))
    . map (hcat' (with & catMethod .~ Distrib & sep .~ (w+1)))
    . chunksOf side
    $ polys
