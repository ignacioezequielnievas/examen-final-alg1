import Data.Array (Array, listArray, bounds, (!), assocs)
import Data.Ix (Ix)

module Graph (Graph, mkGraph, adyacente, nodos, aristas, peso, aristaIn)where 

-- Definimos el tipo de dato Grafo usando una matriz de adyacencia
data Graph n w = Graph {
    dirigido :: Bool,          -- Indica si el grafo es dirigido
    rango :: (n, n),          -- Rango de nodos
    matriz :: Array (n, n) w  -- Matriz de adyacencia con pesos
} deriving (Show)

mkGraph :: (Ix n , Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Grafo n w) 
{-Toma los limites inferior y superior del conjunto de índices, una lista de aristas (cada arista está dada por una 
tupla, origen -  destino – peso) y retorna un Grafo. El primer argumento Booleano indica si el grafo es dirigido, 
False indica que se debe agregar un arco en ambas direcciones (Gafo no dirigido). -}

mkGraph dir (nInf, nSup) aristas =
    Graph dir (nInf, nSup) (listArray ((nInf, nInf), (nSup, nSup)) pesos)
  where
    -- Inicializamos con peso 0 o infinito
    pesos = [pesoArista i j | i <- range (nInf, nSup), j <- range (nInf, nSup)]
    
    -- Función para determinar el peso entre nodos
    pesoArista i j = case lookup (i, j) aristas of
        Just w  -> w
        Nothing -> if dir then 0 else case lookup (j, i) aristas of
                      Just w  -> w
                      Nothing -> 0


adjacente :: (Ix n , Num w) => (Grafo n w) -> n -> [n] 
{-Retorna la lista de nodos  adyacentes a un nodo dado.-} 

adyacente (Graph _ (nInf, nSup) matriz) nodo =
    [j | j <- range (nInf, nSup), matriz ! (nodo, j) /= 0]

nodos :: (Ix n , Num w) => (Grafo n w) -> [n] 
{-Retorna la lista de nodos del grafo. -} 

nodos (Graph _ (nInf, nSup) _) = range (nInf, nSup)

aristasD, aristasU :: (Ix n , Num w) => (Grafo n w) -> [(n,n,w)]  
{-Retorna una lista de todas las aristas de un grafo dirigido y  de uno no dirigido respectivamente.-}

aristasD (Graph _ (nInf, nSup) matriz) =
    [(i, j, matriz ! (i, j)) | i <- range (nInf, nSup), j <- range (nInf, nSup), matriz ! (i, j) /= 0]

aristasU :: (Ix n, Num w, Eq w) => Graph n w -> [(n, n, w)]
aristasU g@(Graph _ _ _) = [(i, j, w) | (i, j, w) <- aristasD g, i <= j]

aristaIn :: (Ix n , Num w) => (Grafo n w) -> (n,n) -> Bool 
{-Retorna True si la arista dada existe en el grafo.-}  
aristaIn (Graph _ _ matriz) (i, j) = matriz ! (i, j) /= 0

peso :: (Ix n , Num w) => (Grafo n w) -> n -> n -> w 
{-Retorna el peso de la arista cuyo origen y destino se proporcionan. -} 

peso (Graph _ _ matriz) i j = matriz ! (i, j)

