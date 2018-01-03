import Diagrams.Prelude hiding (E, (.>), (|||), turn)
import Diagrams.Backend.Rasterific.CmdLine
import SMTDrawing

import System.Environment

main = mainWith $ drawAllPolysGrid 3
