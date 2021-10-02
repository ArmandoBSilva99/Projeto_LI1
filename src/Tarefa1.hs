-- | Este módulo tem como objetivo criar o Maze 

module Tarefa1 where

import System.Random
import Types

-- * Funções da construção inicial do Maze

-- | Dado uma semente retorna uma lista de n inteiros aleatórios
--
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed -- cria um gerador aleatório
                        in  take n $ randomRs (0,99) gen -- Pega nos primeiros n elementos de uma série infinta de números aleatórios entre 0-99


-- | Converter inteiro para peça
--                            
--  * 3 == Food Big  
--  * 0 <= x < 70 = Food Little
--  * 70 < x <= 99 = Wall 
--
intToPiece :: Int -> Piece
intToPiece x | x == 3 = Food Big      
             | x >= 0 && x < 70 = Food Little
             | otherwise = Wall

-- | Coloca no corredores do Maze espaços para colocar as Walls
--
createCorridor :: Int -> Corridor -> Maze
createCorridor _ [] = []
createCorridor l ps =
                  let h = take l ps
                      t = drop l ps
                  in h: createCorridor l t 

-- | Converte uma lista de Int para Corridor
--
convertCorridor :: [Int] -> Corridor 
convertCorridor [] = []
convertCorridor (h:t) = [intToPiece h] ++ convertCorridor t

-- * Adiciona Walls 

-- | Adiciona Wall nos lados do maze
--
addSideWalls :: Maze -> Maze
addSideWalls [] = []
addSideWalls (h:t) = (Wall : h ++ [Wall]) : addSideWalls t

-- | Adiciona Wall no top e bottom do Maze
--
createWall :: Int -- ^ Comprimento do Maze
             -> Corridor
createWall n = replicate n Wall

-- * Adiciona Tunel 

-- | Cria um Corridor com o Tunel
-- 
tunnelCorridor :: Corridor -> Corridor
tunnelCorridor corridor = (Empty: init (tail corridor)) ++ [Empty]

-- | Encontra o Corridor do meio do Maze e retorna-o
--
findMiddleMaze :: Int -- ^ Altura do Maze
                 -> Maze -> Corridor
findMiddleMaze 1 maze = head maze
findMiddleMaze n maze = findMiddleMaze (n-1) (tail maze) 

-- | Adiciona o Tunel para um Maze com altura impar
--
addTunnelsOdd :: Int -> Maze -> Maze
addTunnelsOdd n maze = (take ((div n 2)) maze) ++ [(tunnelCorridor(findMiddleMaze (div n 2) maze))] ++ drop ((div n 2) + 1) maze

-- | Adiciona o Tunel para um Maze com altura par
--
addTunnelsEven :: Int -> Maze -> Maze 
addTunnelsEven n maze = (take ((div n 2) - 1) maze) ++ [(tunnelCorridor(findMiddleMaze (div n 2) maze))] ++ [(tunnelCorridor(findMiddleMaze ((div n 2) + 1) maze))] ++ drop ((div n 2) + 1) maze

-- | Adiciona o Tunel para qualquer Maze
addTunnels :: Int -> Maze -> Maze
addTunnels n maze | (mod n 2 == 0) = addTunnelsEven n maze
                  | otherwise = addTunnelsOdd n maze


-- * Ghost House

-- | Cria a ghostHouse
--
ghostHouse :: Int -- ^ altura do maze
             -> Int -- ^ comprimento do maze
             -> Maze -> Maze
ghostHouse a c m | (mod c 2 == 0) && (mod a 2 == 0) = ghostHouseEven (emptyForGhostEven m ((div a 2) - 3) ((div c 2) - 5) 5) ((div a 2) - 2) ((div c 2) - 4)
                 | (mod c 2 == 0) && (mod a 2 /= 0) = ghostHouseEven (emptyForGhostEven m ((div a 2) - 2) ((div c 2) - 5) 5) ((div a 2) - 1) ((div c 2) - 4)
                 | (mod c 2 /= 0) && (mod a 2 == 0) = ghostHouseOdd (emptyForGhostOdd m ((div a 2) - 3) ((div c 2) - 5) 5) ((div a 2) - 2) ((div c 2) - 4)
                 | otherwise = ghostHouseOdd (emptyForGhostOdd m ((div a 2) - 2) ((div c 2) - 5) 5) ((div a 2) - 1) ((div c 2) - 4)

-- | Põe o primeiro corridor da GhostHouse
-- 
putCorridor :: Corridor 
               -> Int -- ^ inteiro que percorre as posições onde deve estar a ghostHouse   
               -> Int -- ^ posição do corridor onde começa a colocar Empty na ghostHouse
               -> Int -- ^ posição do corridor onde acaba de colocar Empty na ghostHouse
               -> Int -- ^ posição do corridor onde acaba a GhostHouse
               -> Corridor
putCorridor c y v1 v2 max | y >= v1 && y <= v2 = putCorridor (replaceNElem y Empty c) (y+1) v1 v2 max
                          | y < max = putCorridor (replaceNElem y Wall c) (y+1) v1 v2 max
                          | otherwise = c

-- ** GhostHouse Par

-- | Põe o segundo corredor e terceiro da GhostHouse para maze par
--
putSndAndRdCorridorEven :: Maze 
                           -> Int -- ^ posição onde começa a ghostHouse
                           -> Maze
putSndAndRdCorridorEven (nd:rd:t) i = (putCorridor nd i (i+1) (i+6) (i+8)):(putCorridor rd i 0 0 (i+8)):t

-- | GhostHouse para maze par
--
ghostHouseEven :: Maze 
                  -> Int -- ^ inteiro que identifica o numero do corredor (0 - corredor com entrada da ghostHouse; 1 - corredor a seguir à entrada; 2 - corredor com o fim da ghostHouse) 
                  -> Int -- ^ posição no corridor onde começa a ghostHouse
                  -> Maze
ghostHouseEven [] _ _ = []
ghostHouseEven (h:t) x y | x == 0 = (putCorridor h y (y+3) (y+4) (y+8)):(putSndAndRdCorridorEven t y)
                         | otherwise = h:(ghostHouseEven t (x-1) y)

--Coloca onde deve ficar a ghostHouse tudo Empty para maze par
--
emptyForGhostEven :: Maze 
                    -> Int -- ^ altura do maze onde começa a colocar Empty 
                    -> Int -- ^ comprimento da GhostHouse + 1 para colocar Empty ao longo do corridor
                    -> Int -- ^ altura do maze onde deixa de colocar Empty e dá o caso de paragem 
                    -> Maze
emptyForGhostEven [] _ _  _ = []
emptyForGhostEven m _ _ 0 = m
emptyForGhostEven (h:t) x y l | x == 0 = (putCorridor h y y (y+9) (y+9)):(emptyForGhostEven t 0 y (l-1))
                              | otherwise = h:(emptyForGhostEven t (x-1) y l)

-- ** GhostHouse Impar

-- | Põe o segundo corredor e terceiro da GhostHouse para maze impar
--
putSndAndRdCorridorOdd :: Maze 
                          -> Int -- ^ posição onde começa a ghostHouse
                          -> Maze
putSndAndRdCorridorOdd (nd:rd:t) i = (putCorridor nd i (i+1) (i+7) (i+9)):(putCorridor rd i 0 0 (i+9)):t

--GhostHouse para maze impar
--
ghostHouseOdd :: Maze 
                  -> Int -- ^ inteiro que identifica o numero do corredor (0 - corredor com entrada da ghostHouse; 1 - corredor a seguir à entrada; 2 - corredor com o fim da ghostHouse) 
                  -> Int -- ^ posição no corridor onde começa a ghostHouse
                  -> Maze
ghostHouseOdd [] _ _ = []
ghostHouseOdd (h:t) x y | x == 0 = (putCorridor h y (y+3) (y+5) (y+9)):(putSndAndRdCorridorOdd t y)
                        | otherwise = h:(ghostHouseOdd t (x-1) y)


--Coloca onde deve ficar a ghostHouse tudo Empty para maze impar
--
emptyForGhostOdd :: Maze 
                    -> Int -- ^ altura do maze onde começa a colocar Empty 
                    -> Int -- ^ comprimento da GhostHouse + 1 para colocar Empty ao longo do corridor
                    -> Int -- ^ altura do maze onde deixa de colocar Empty e dá o caso de paragem 
                    -> Maze
emptyForGhostOdd [] _ _  _ = []
emptyForGhostOdd m _ _ 0 = m
emptyForGhostOdd (h:t) x y  l | x == 0 = (putCorridor h y y (y+10) (y+10)):(emptyForGhostOdd t 0 y (l-1))
                              | otherwise = h:(emptyForGhostOdd t (x-1) y l)


-- * Gera a Maze

-- | Gera o Maze final
--
generateMaze :: Int -- ^ altura do Maze
                -> Int -- ^ comprimento do Maze
                -> Int -- ^ semente do Maze 
                -> Maze
generateMaze l a s = 
      let nr = (l-2) * (a-2)
          randNrs = generateRandoms nr s
          pieces = convertCorridor randNrs
          maze1 = createCorridor (l-2) pieces 
          sideWall = addSideWalls maze1
          wall = createWall l
          allWalls = (wall : sideWall) ++ [wall] 
          tunnels = addTunnels a allWalls 
          gh = ghostHouse a l tunnels 
      in gh

