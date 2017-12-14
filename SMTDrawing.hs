import Data.SBV
import Data.List.Split (chunksOf)

data Turn = L | R
  deriving (Eq, Ord, Show, Read)

data Dir = N | E | S | W
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

turn :: Turn -> Dir -> Dir
turn t = toEnum . (`mod` 4) . (+ turnDelta t) . fromEnum
  where
    turnDelta L = (-1)
    turnDelta R = 1

orthoPolyDrawing :: [Turn] -> Goal
orthoPolyDrawing turns = do

  -- Create (x,y) variables for each vertex
  vertices <- chunksOf 2 <$> (mkFreeVars (2*length turns) :: Symbolic [SInteger])

  let vertexCycle = vertices ++ [head vertices]

  -- Generate constraints.
  genConstraints turns E vertexCycle

  -- Minimize the sum of all edge lengths.
  minimize "total edge length" $ sum (zipWith dist vertexCycle (tail vertexCycle))

-- Distance between two points.  In the special case of only
-- horizontal or vertical edges, as we have, Manhattan distance and
-- Euclidean distance coincide.
dist :: [SInteger] -> [SInteger] -> SInteger
dist [x1,y1] [x2,y2] = abs (x2 - x1) + abs (y2 - y1)

-- Generate edge constraints from the remaining turns, the current
-- direction, and the remaining vertices.
genConstraints :: [Turn] -> Dir -> [[SInteger]] -> Goal
genConstraints [] _ _ = return ()
genConstraints (t:ts) dir (v1:v2:vs) = do

  -- Constrain the edge from v1 to v2 to point in direction dir.
  genEdgeConstraint dir v1 v2

  -- Generate the rest of the constraints, turning the current
  -- direction appropriately.
  genConstraints ts (turn t dir) (v2:vs)

-- Constrain the second vertex to lie in the given direction from the
-- first.
genEdgeConstraint :: Dir -> [SInteger] -> [SInteger] -> Goal
genEdgeConstraint d [x1,y1] [x2,y2] =
  case d of
    N -> do constrain $ x1 .== x2
            constrain $ y1 .<  y2
    E -> do constrain $ y1 .== y2
            constrain $ x1 .<  x2
    S -> do constrain $ x1 .== x2
            constrain $ y1 .>  y2
    W -> do constrain $ y1 .== y2
            constrain $ x1 .>  x2


