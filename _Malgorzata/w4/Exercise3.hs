module Exercise3 where

import Control.Monad.State
import Data.Sequence
import qualified Data.List as L

-- Time spent: 600min

type Vertex = Int
type Label = String
type Path = [Label]
type Edge = (Vertex, Label, Vertex)
type Graph = [Edge]

b = [1, 2, 3, 4]
a = [(1,"?coin",2),(2,"!coffee",4),(2,"!tea",3)]

type BfsResult = [Path]
type BfsState = (Seq (Vertex, Path), [Path])

-- check if the transition is an output, because then we know we can't have a quiesence before it
isOutput :: Edge -> Bool
isOutput (_, label, _) = head label == '!'

-- check if the transitions (edges) are inputs or outputs
quiesencent :: Vertex -> Graph -> Bool
quiesencent v graph = foldr (\x acc -> if isOutput(x) then False else True) True (getEdges v graph)

-- we are filtering the outputs and adding quiesence to them as a transition and we make a union with the original graph 
addQuiesence :: [Vertex] -> Graph -> Graph
addQuiesence vertices graph = [(x,"delta",x) | x <- vs ]++graph
  where vs = L.filter (\x -> quiesencent x graph) vertices

-- get the edges for the given state
getEdges :: Vertex -> Graph -> [Edge]
getEdges v graph = L.filter (\(x,_,_) -> x == v) graph


edgesToChildren :: Edge -> Path -> (Vertex, Path)
edgesToChildren (_, label, child) path = (child, path++[label])

{-
implement bfs using state monad and sequence data type.
helping source: https://mmhaskell.com/blog/2021/11/22/ai-revisited-breaking-down-bfs.

We keep the current queue and result in the monad. "view" is the queue (fifo). 
If the queue if empty, we return current result.
If not, we pop the first element from it. This will be the currentVertex and 
Path is a trace to this vertex. We put constraint to it (if L.length path > 4) so it doesn't go forever.

At the begging the path is empty, then for each edge we put each neighbor in the queue 
and add the name of this edge to its path, we add the path of the present and the result is [[""]]
We do the next pop, we get the number of the vertex and the path to which we added.
For example (2, [?"coin"]) and it's neighbors are added to the queue with the path (3, ["? coin", "! coffee"]), (4, ["? coin", "! tea"])
we add its path and the result is [[""], ["? coin"]]

This is not the best of the explanation, however it is late and im running out of time!
-}
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

-- we are adding quiesences to the ..... and running bfs over it 
main = print $ evalState (bfs (addQuiesence b a)) startState