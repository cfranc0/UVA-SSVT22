import Control.Monad.State
import Data.Sequence
import qualified Data.List as L

type Vertex = Int
type Label = String
type Path = [Label]
type Edge = (Vertex, Label, Vertex)
type Graph = [Edge]

b = [1, 2, 3]
a = [(1,"?coin",2),(2,"!coffee",4),(2,"!tea",3)]

type BfsResult = [Path]
type BfsState = (Seq (Vertex, Path), [Path])

isOutput :: Edge -> Bool
isOutput (_, label, _) = head label == '!'

quiesencent :: Vertex -> Graph -> Bool
quiesencent v graph = foldr (\x acc -> if isOutput(x) then False else True) True (getEdges v graph)

addQuiesence :: [Vertex] -> Graph -> Graph
addQuiesence vertices graph = [(x,"delta",x) | x <- vs ]++graph
  where vs = L.filter (\x -> quiesencent x graph) vertices

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
      if L.length path > 4
        then return result
      else do
        let newQueue = rest
        let edges = getEdges currentVertex graph
        let children = fromList ([edgesToChildren x path | x <- edges])
        put (rest >< children, result++[path])
        bfs graph

startState = (fromList [(1, [])], [])

main = print $ evalState (bfs (addQuiesence b a)) startState