{- | Module : Tarefa5
   | Description : Retornar jogadas válidas aos Ghosts 
   | Copyright : Armando Silva <a87949@alunos.uminho.pt> 

= Introdução:
O objetivo desta tarefa é implementar um comportamento para os fantasmas.

= Objetivos:
Esta tarefa, contém 3 funções principais:

  - chaseMode -> retorna jogadas que permitem aos Ghosts perseguirem o Pacman
  - scatterMode -> retorna jogadas que permitem aos Ghosts fugirem do Pacman
  - ghostPlay -> Junta as funções scatterMode e chaseMode, chamando cada uma na situação mais pertinente

= Estratégias:
  - chaseMode : nesta função a estratégia adotada foi verificar as coordenadas x e y do Pacman e fazer com que as coordenadas x e y do Ghost aumentem ou diminuem até serem iguais ao Pacman, tendo atenção às Walls de maneira a que não fique preso na Wall. 
  Quando o Ghost é comido, esse ghost executa duas Plays para cima de maneira a sair da GhostHouse e depois prossegue com o seu comportamento de perseguição.

  - scatterMode : nesta função a estratégia adotada foi verificar as coordenadas x e y do Pacman e fazer com que as coordenadas x e y do Ghost aumentem ou diminuem, 
  de maneira a que haja maior amplitude entre as coordenadas entre o Pacman e Ghost, também é verificado caso o Ghost está em algum canto do mapa, se mover para o lado ou comprimento oposto do Pacman  

= Conclusão:
    Os Ghosts em chaseMode, de facto, perseguem o Pacman, no entando o scatterMode, fica "confuso" quando se encontra nos cantos.
-}

module Tarefa5 where 

import Types
import Tarefa2

-- * Função principal

-- | Jogadas do Ghost
ghostPlay :: State -> [Play]
ghostPlay (State m [] l) = []
ghostPlay s@(State m (Ghost h: t) l) | getPacmanMode(findPacman ((Ghost h):t)) == Mega = scatterMode s (getPlayerID (Ghost h)): ghostPlay (State m t l) 
                                     | otherwise = chaseMode s (getPlayerID (Ghost h)): ghostPlay (State m t l)
ghostPlay (State m (Pacman h: t) l) = ghostPlay (State m t l)

-- * Função ChaseMode

-- | Perseguir o Pacman 
chaseMode :: State 
             -> Int -- ^ ID do Ghost 
             -> Play
chaseMode s@(State m ps l) pid | stillInHouse s pid = Move pid U
                               | otherwise = Move pid (checkWallsToFollow s pid (getPlayerCoords(findPacman ps)))

-- | Devolve orientação em direção à posição do Pacman 
--
whereToFollow :: Int -- ^ ID do Ghost 
                -> [Player] 
                -> Coords -- ^ Coordenadas do Pacman 
                -> Orientation 
whereToFollow i ps (x,y) | getPlayerCoordsX (findPlayerID ps i) < x = D
                         | getPlayerCoordsX (findPlayerID ps i) > x = U
                         | getPlayerCoordsY (findPlayerID ps i) > y = L
                         | otherwise = R

-- | Condições de verificação se o Ghost ainda está dentro da casa
stillInHouse :: State 
                -> Int -- ^ ID do Ghost 
                -> Bool
stillInHouse s@(State m ps l) pid | getPlayerCoords (findPlayerID ps pid) == getPlayerCoords (spawnMiddle m (findPlayerID ps pid)) 
                                   || (getPlayerCoordsX (findPlayerID ps pid) == (getPlayerCoordsX (spawnMiddle m (findPlayerID ps pid)) - 1) && getPlayerCoordsY (findPlayerID ps pid) == (getPlayerCoordsY (spawnMiddle m (findPlayerID ps pid)))) = True
                                  | otherwise = False   

-- | Verifica condições caso esteja contra a parede 
checkWallsToFollow ::  State 
                       -> Int -- ^ ID do Ghost 
                       -> Coords -- ^ Coordenadas do Pacman
                       -> Orientation                      
checkWallsToFollow s@(State m ps l) i (x,y) | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p < y = R -- Verifica se tem Wall em cima e em baixo
                                            | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p > y = L --
                                            
                                            | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p < y = R -- Verifica se tem Wall apenas em cima
                                            | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p > y = L --
                                    
                                            | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p < y = R -- Verifica se tem Wall apenas em baixo
                                            | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && getPlayerCoordsY p > y = L --

                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsX p > x = U -- Verifica se tem Wall à direita e à esquerda
                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsX p < x = D --    

                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && getPlayerCoordsY p > x = U -- Verifica se tem Wall apenas à direita
                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && getPlayerCoordsY p < x = D --
                                    
                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsY p > y = U -- Verifica se tem Wall apenas à esquerda
                                            | findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsY p < y = D -- 
                                    
                                            | otherwise = whereToFollow i ps (x,y) 
                                            where p = findPlayerID ps i

-- * Função Scatter Mode

-- | Fugir do Pacman
scatterMode :: State -> Int -> Play
scatterMode s pid = runAway pid s

-- | Devolve orientação contrária à posição do Pacman
whereToRun :: Int -> [Player] -> Coords -> Orientation
whereToRun i ps (x,y) | getPlayerCoordsX (findPlayerID ps i) < x = U
                      | getPlayerCoordsX (findPlayerID ps i) > x = D
                      | getPlayerCoordsY (findPlayerID ps i) > y = R
                      | otherwise = L

-- | Verifica condições caso esteja contra a parede ou nos cantos
checkWallsToRun ::  State 
               -> Int -- ^ ID do Ghost 
               -> Coords -- ^ Coordenadas do Pacman
               -> Orientation                      
checkWallsToRun s@(State m ps l) i (x,y) | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall = D
                                         | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsY p == y = L
                                    
                                         | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall = U
                                         | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall && getPlayerCoordsY p == y = R

                                         | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall = D
                                         | findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && getPlayerCoordsY p == y = L
                                    
                                         | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall = U 
                                         | findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall && getPlayerCoordsY p == y = R
                                    
                                         | otherwise = whereToRun i ps (x,y) 
                                         where p = findPlayerID ps i

-- | Função auxiliar da scatter Mode que contem a play
runAway :: Int -- ^ ID do Pacman 
           -> State 
           -> Play
runAway i s@(State m ps l) = Move i (checkWallsToRun s i (getPlayerCoords(findPacman ps)))


