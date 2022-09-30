import Control.Monad.State
import Data.Sequence
import qualified Data.List as L

type Vertex = Int
type Label = String
type Path = [Label]
type Edge = (Vertex, Label, Vertex)
type Graph = [Edge]
a = [(1,"Spritz",2),(2,"Martini",3),(2,"Gin Tonic",4),(4,"Cuba Libre",1)]

type BfsResult = [Path]
type BfsState = (Seq (Vertex, Path), [Path])

getEdges :: Vertex -> Graph -> [Edge]
getEdges v graph = L.filter (\(x,_,_) -> x == v) graph

edgesToChildren :: Edge -> Path -> (Vertex, Path)
edgesToChildren (_, label, child) path = (child, path++[label])

bfs :: Graph -> State BfsState BfsResult
bfs graph = do
    (queue, result) <- get
    let view = viewl queue
    if view == EmptyL
      then return result
    else do
      let (top :< rest) = view
      let (currentVertex, path) = top
      if L.length path > 10
        then return result
      else do
        let newQueue = rest
        let edges = getEdges currentVertex graph
        let children = fromList ([edgesToChildren x path | x <- edges])
        put (rest >< children, result++[path])
        bfs graph

startState = (fromList [(1, [])], [])

main = print $ evalState (bfs a) startState
