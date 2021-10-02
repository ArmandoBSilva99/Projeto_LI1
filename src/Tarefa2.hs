{- | Module : Tarefa2
   | Description : Implementação de Jogadas  
   | Copyright : Armando Silva <a87949@alunos.uminho.pt> 

= Introdução
  O objetivo deste módulo é, dada uma descrição do estado do jogo e uma jogada do Pacman, determinar o efeito dessa jogada no estado do jogo. 

= Objetivos
  Os objetivos aqui falados serão apenas os que foram alterandos ou adicionados a esta Tarefa de maneira a que a Tarefa 4 e 5 funcionem corretamente.

  As alteração principais feitas nesta tarefa foram:
  
  - pacEatGhost -> foi adicionada mais uma variável que representa a lista de jogadores antes de ser feita a jogada, de maneira a que evita com que quando o ghost e o pacman choquem, efetivamente o pacman morre ou o ghost morre
  - play -> adicionada condiçáo que verifica se o Player a ser movido é Ghost ou Pacman
  - moveAuxG -> executa o movimento do Ghost

= Conclusão 
  Estas adições e alterações fizeram com que a Tarefa 4 e Tarefa 5 funcionassem, de acordo com o pedido.      
-}
module Tarefa2 where

import Types

-- * Funções do movimento do Pacman

-- | Verifica se está contra a parede
againstWall :: Maze -> Player -> Orientation -> Int
againstWall m p@(Pacman h) o | (getPlayerOrientation p == L) && getPlayerCoordsY p == 0 = 0
                            | (getPlayerOrientation p == R) && getPlayerCoordsY p == (length (head m) - 1) = 0
                            | (getPlayerOrientation p == U) && findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall = 1
                            | (getPlayerOrientation p == D) && findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall = 1
                            | (getPlayerOrientation p == L) && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall = 1
                            | (getPlayerOrientation p == R) && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall = 1
                            | otherwise = 0
againstWall m p@(Ghost h) o | (o == L) && getPlayerCoordsY p == 0 = 0
                           | (o == R) && getPlayerCoordsY p == (length (head m) - 1) = 0
                           | (o == U) && findElemInMaze (getPlayerCoordsX p - 1, getPlayerCoordsY p) m == Wall = 1
                           | (o == D) && findElemInMaze (getPlayerCoordsX p + 1, getPlayerCoordsY p) m == Wall = 1
                           | (o == L) && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p - 1) m == Wall = 1
                           | (o == R) && findElemInMaze (getPlayerCoordsX p, getPlayerCoordsY p + 1) m == Wall = 1
                           | otherwise = 0
                

-- | Encontra as coordenadas todas de cada entrada do tunel
findTunnelCoords :: Maze -> [Coords]    
findTunnelCoords m | mod (length m) 2 /= 0 = [(div (length m) 2, 0) , (div (length m) 2,length (head m) - 1)] 
                   | otherwise = [((div (length m) 2) - 1, 0), ((div (length m) 2) - 1, length (head m) - 1), ((div (length m) 2), 0), ((div (length m) 2), length (head m) - 1)]
                                                          

-- | Verifica se a coordenada está dentro da lista de todas as entradas do túnel                   
moveTunnel :: Coords -> [Coords] -> Bool
moveTunnel _ [] = False
moveTunnel c (h:t) | c == h = True
                   | otherwise = moveTunnel c t
               
-- | Retorna as coordenadas para onde o pacman e ghost se movem
getPlayerCoordsOri :: Maze -> Player 
                      -> Orientation -- ^ orientação da play
                      -> Orientation -- ^ orientação do Player
                      -> Coords
getPlayerCoordsOri m (Pacman (PacState (id,(x,y),z,t,h,l) b c d )) oriM oriP | oriP == oriM && oriM == U = (x-1,y) 
                                                                             | oriP == oriM && oriM == D = (x+1,y)
                                                                             | oriP == oriM && oriM == L && (moveTunnel (x,y) (findTunnelCoords m)) && y == 0 = (x, length(head m) - 1)
                                                                             | oriP == oriM && oriM == R && (moveTunnel (x,y) (findTunnelCoords m)) && y == (length (head m)) - 1 = (x, 0)
                                                                             | oriP == oriM && oriM == L && y > 0 = (x,y-1)
                                                                             | oriP == oriM && oriM == R && y < (length (head m)) - 1 = (x,y+1)
                                                                             | otherwise = (x,y)
getPlayerCoordsOri m (Ghost (GhoState (id,(x,y),z,t,h,l) b)) oriM oriP | oriM == U = (x-1,y) 
                                                                       | oriM == D = (x+1,y)
                                                                       | oriM == L && (moveTunnel (x,y) (findTunnelCoords m)) && y == 0 = (x, length(head m) - 1)
                                                                       | oriM == R && (moveTunnel (x,y) (findTunnelCoords m)) && y == (length (head m)) - 1 = (x, 0)
                                                                       | oriM == L && y > 0 = (x,y-1)
                                                                       | oriM == R && y < (length (head m)) - 1 = (x,y+1)
                                                                       | otherwise = (x,y)

-- ** Funções Auxiliares

-- | Muda a orientação de um player
setPlayerOrientation :: Player -> Orientation -> Player
setPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) o = (Pacman (PacState (x,y,z,o,h,l) q c d ))
setPlayerOrientation (Ghost (GhoState (x,y,z,t,h,l) q )) o = (Ghost (GhoState (x,y,z,o,h,l) q ))                             

-- * Funções de quando o Pacman come 

-- | Aumenta os pontos do pacman e muda o seu PacMode 
pacEat :: Player -> Maze -> [Player] -> Player
pacEat p m ps| findElemInMaze (getPlayerCoords p) m == Food Little && (isGhostInThoseCoords (getPlayerCoords p) ps) /= True = setPlayerPoints (dMega(openClose p)) 1
             | findElemInMaze (getPlayerCoords p) m == Food Big && (isGhostInThoseCoords (getPlayerCoords p) ps) /= True = setPacMega(setPlayerPoints (dMega(openClose p)) 5) 
             | findElemInMaze (getPlayerCoords p) m == Food Big && getPacmanMode p == Mega = setPlayerPoints p 5 
             | otherwise = (dMega(openClose p))

-- | Verifica se existe um Ghost naquelas coordenadas
isGhostInThoseCoords :: Coords -> [Player] -> Bool
isGhostInThoseCoords _ [] = False
isGhostInThoseCoords (x,y) (Pacman h:t) = isGhostInThoseCoords (x,y) t
isGhostInThoseCoords (x,y) (Ghost h:t) | getPlayerCoords (Ghost h) == (x,y) = True
                                       | otherwise = isGhostInThoseCoords (x,y) t                                       

-- | Verifica quantos ghosts estão naquela coordenada
howManyGhostsInThoseCoords :: Coords -> [Player] -> Int -> Int
howManyGhostsInThoseCoords _ [] i = i
howManyGhostsInThoseCoords (x,y) (Pacman h: t) i = howManyGhostsInThoseCoords (x,y) t i
howManyGhostsInThoseCoords (x,y) (Ghost h: t) i | getPlayerCoords (Ghost h) == (x,y) = howManyGhostsInThoseCoords (x,y) t (i+1)
                                                | otherwise = howManyGhostsInThoseCoords (x,y) t i

-- | Comer ou não o fantasma dando a lista de jogares atualizada
pacEatGhost :: Maze 
              -> Player -- ^ Player que foi movido 
              -> [Player] 
              -> [Player] -- ^ Lista de Players inalterada
              -> [Player] -- ^ Lista de Players antes da jogada
              -> Bool -- ^ Boolean para utilizar isGhostInThoseCoords
              -> [Player]
pacEatGhost _ _ [] _ _ _ = []
pacEatGhost m p ((Pacman h):t) ps bps b | b && getPlayerID (Pacman h) == getPlayerID p && getPacmanMode (Pacman h) == Normal = (pacMightDie (Pacman h)):(pacEatGhost m p t ps bps b)
                                        | b && getPlayerID (Pacman h) == getPlayerID p && getPacmanMode (Pacman h) == Mega 
                                         = (setPlayerPoints (Pacman h) (10 * (howManyGhostsInThoseCoords (getPlayerCoords p) bps 0))):(pacEatGhost m p t ps bps b)
                                        | otherwise = (Pacman h):(pacEatGhost m p t ps bps b)
pacEatGhost m p ((Ghost h):t) ps bps b | b && isPacMega p && getPlayerCoords p == getPlayerCoords (findPlayerID bps (getPlayerID(Ghost h))) = (spawnMiddle m (setGhostAlive (Ghost h))):(pacEatGhost m p t ps bps b)
                                       | otherwise = (Ghost h):(pacEatGhost m p t ps bps b)



-- ** Funções auxiliares

-- | Vê se o player está mega
isPacMega :: Player -> Bool
isPacMega (Pacman (PacState a b c d ))  | d == Mega = True
                                        | otherwise = False
isPacMega (Ghost (GhoState a b )) = False

-- | Verifica se o pacman morre ou não
pacMightDie :: Player -> Player
pacMightDie (Pacman (PacState (x,y,z,t,h,l) q c d )) | l == 1 || l == 0 = (Pacman (PacState (x,y,z,t,h,0) q c Dying ))
                                                     | otherwise = (Pacman (PacState (x,y,z,t,h,l-1) q c d ))

-- | Torna todos os Ghosts Dead 
allGhostDead :: [Player] -> [Player]
allGhostDead [] = []
allGhostDead ((Pacman h): t) = Pacman h: allGhostDead t
allGhostDead ((Ghost h): t) = setGhostDead (Ghost h) : allGhostDead t

-- | Torna todos os Ghosts Alive
allGhostAlive :: [Player] -> [Player]
allGhostAlive [] = []
allGhostAlive ((Pacman h): t) = Pacman h: allGhostAlive t
allGhostAlive ((Ghost h): t) = setGhostAlive (Ghost h) : allGhostAlive t

-- | Torna o ghost Dead
setGhostDead :: Player -> Player
setGhostDead (Ghost (GhoState (x,y,z,t,h,l) q )) = (Ghost (GhoState (x,y,z/2,t,h,l) Dead ))

-- | Torna o ghostAlive
setGhostAlive :: Player -> Player
setGhostAlive (Ghost (GhoState (x,y,z,t,h,l) q )) | l == 1 || l == 0 = (Ghost (GhoState (x,y,z+z,t,h,0) Alive))
                                                  | otherwise = (Ghost (GhoState (x,y,z+z,t,h,l-1) Alive ))

-- | Torna o pacman Mega
setPacMega :: Player -> Player
setPacMega (Pacman (PacState a b c d)) = (Pacman (PacState a 10 c Mega))

-- | Dado inteiro, soma esse inteiro aos pontos atuais do player
setPlayerPoints :: Player -> Int -> Player
setPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) i = (Pacman (PacState (x,y,z,t,h + i,l) q c d ))
setPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) i = (Ghost (GhoState (x,y,z,t,h + i,l) q ))

-- | Verifica se o player comeu a comida grande e retorna lista com os ghosts Dead
ifAteBigFoodPutAllDead :: [Player] 
                          -> Coords -- ^ coordenadas da comida grande 
                          -> Coords -- ^ coordenadas do Player
                          -> Maze -> [Player]
ifAteBigFoodPutAllDead l co coP m   | findElemInMaze co m == (Food Big) && co /= coP && isPacmanMega l
                                      = allGhostDead l
                                    | not (isPacmanMega l) = allGhostAlive l
                                    | otherwise = l 
                                        

-- | Verifica se existe algum Pacman em modo Mega
isPacmanMega :: [Player] -> Bool
isPacmanMega [] = False
isPacmanMega ((Ghost h): t) = isPacmanMega t
isPacmanMega ((Pacman h): t) | getPacmanMode (Pacman h) == Mega = True
                             | otherwise = isPacmanMega t                                    

-- | Pega num player e coloca-o no meio do maze
spawnMiddle :: Maze -> Player -> Player
spawnMiddle m p | (mod (length m) 2 == 0) = setPlayerCoords p ((div (length m) 2) - 1, div (length (head m)) 2)  
                | otherwise = setPlayerCoords p ((div (length m) 2), div (length (head m)) 2)
-- | Respawn do Pacman
respawn :: Maze -> Player -> Player
respawn m p = setPlayerCoords p ((div (length m) 2) + 1, div (length (head m)) 2)  

-- | Muda a orientação de um jogador
changeOrientation :: Orientation -> Int -> [Player] -> [Player]
changeOrientation o x (Pacman h: t) | getPlayerID (Pacman h) == x = (setPlayerOrientation (Pacman h) o):t
                                    | otherwise = (Pacman h): changeOrientation o x t
changeOrientation o x (Ghost h: t) | getPlayerID (Ghost h) == x = (setPlayerOrientation (Ghost h) o):t
                                   | otherwise = (Ghost h): changeOrientation o x t                                      

-- * Funções finais que juntam as funções de comer e mover do pacman

-- | Função da jogada final e atualiza o maze  
play :: Play -> State -> State                                                                  
play (Move x o) (State m s l) = if isPacOrGhost (findPlayerID s x) then let pls = (moveAuxP m x o s) in State (updateMaze m x o s) (pacEatGhost m (findPlayerID pls x) pls pls s (isGhostInThoseCoords (getPlayerCoords (findPlayerID pls x)) s)) l
                                else let pls = (moveAuxG m x o s) in State (updateMaze m x o s) pls l
-- | Atualiza o Maze
updateMaze :: Maze -> Int -> Orientation -> [Player] -> Maze
updateMaze m p o [] = m
updateMaze m p o ((Pacman h): t) | p == getPlayerID (Pacman h) = replaceElemInMaze (getPlayerCoords (Pacman h)) Empty m 
                                 | otherwise = updateMaze m p o t
updateMaze m p o ((Ghost h): t) = updateMaze m p o t


-- | Movimento tendo em conta a parede e o tunel PACMAN
moveAuxP :: Maze 
           -> Int -- ^ ID do Player
           -> Orientation -> [Player] -> [Player]
moveAuxP m p o [] = []
moveAuxP m p o ls = if getPacmanMode (findPlayerID ls p) == Normal && isGhostInThoseCoords (getPlayerCoords (findPlayerID ls p)) ls
                   then putDead m (findPlayerID ls p) ls  (getPlayerCoords (findPlayerID ls p))
                   else ifAteBigFoodPutAllDead (moveOrTurnPlayerState m ls (findPlayerID ls p) o) (getPlayerCoordsOri m (findPlayerID ls p) o (getPlayerOrientation (findPlayerID ls p))) (getPlayerCoords (findPlayerID ls p)) m 

-- | Movimento do Ghost
moveAuxG :: Maze 
           -> Int -- ^ ID do Player
           -> Orientation -> [Player] -> [Player]
moveAuxG m p o [] = []
moveAuxG m p o ls = (moveOrTurnPlayerState m ls (findPlayerID ls p) o)


-- | Muda o estado do Pacman após movimentação e possivelmente comer comida                                                                                              
moveOrTurnPlayerState :: Maze -> [Player] 
                        -> Player -- ^ Player que é movido                                                                                                                  
                        -> Orientation -> [Player]
moveOrTurnPlayerState _ [] _ _ = []
moveOrTurnPlayerState m ((Pacman h):t) p o | getPlayerID p == getPlayerID (Pacman h) && (againstWall m p o == 0 && (getPlayerOrientation p) == o) && getPacmanMode (Pacman h) /= Dying = (pacEat (setPlayerCoords (Pacman h) (getPlayerCoordsOri m p o (getPlayerOrientation p))) m ((Pacman h):t)):t
                                           | getPlayerID p == getPlayerID (Pacman h) && getPacmanMode (Pacman h) /= Dying = (setPlayerOrientation (dMega(openClose p)) o):t
                                           | getPacmanMode (Pacman h) == Dying = (Pacman h):(moveOrTurnPlayerState m t p o)
                                           | otherwise = (dMega(openClose (Pacman h))):(moveOrTurnPlayerState m t p o)
moveOrTurnPlayerState m ((Ghost g):t) p o | getPlayerID p == getPlayerID (Ghost g) && (againstWall m p o == 0) = (setPlayerCoords (Ghost g) (getPlayerCoordsOri m p o (getPlayerOrientation p))):t
                                          | otherwise = (Ghost g):(moveOrTurnPlayerState m t p o)



-- ** Função auxiliar

isPacOrGhost :: Player -> Bool
isPacOrGhost (Pacman (PacState a b c d )) = True
isPacOrGhost (Ghost (GhoState a b )) = False

-- | Pega no Player e coloca-o morto ou tira-lhe a vida nas coordenadas no Ghost que o matou
putDead :: Maze -> Player -> [Player] -> Coords -> [Player]
putDead _ _ [] _= []
putDead m p (h:t) c | getPlayerID p == getPlayerID h = (setPlayerCoords (pacMightDie h) c):t
                    | getPlayerID p == getPlayerID h && getPacLife h /= 0 = (respawn m (pacMightDie h)):t
                    | otherwise = h:(putDead m p t c)

-- | Open Close Mouth 
openClose :: Player -> Player
openClose (Pacman (PacState a b c d )) | c == Open = (Pacman (PacState a b Closed d ))
                                       | otherwise = (Pacman (PacState a b Open d ))

-- | Desce o tempo de mega
dMega :: Player -> Player
dMega (Pacman (PacState a b c d )) | b > 0 = (Pacman (PacState a (b-1) c d ))
                                   | otherwise = (Pacman (PacState a b c Normal ))

-- * Funções auxiliares

-- | Encontra o Pacman numa lista de players
findPacman :: [Player] -> Player 
findPacman (Pacman h:t) = (Pacman h)
findPacman (Ghost h:t) = findPacman t

-- | Encontra o Player numa lista de Players
findPlayerID :: [Player] 
                -> Int -- ^ ID do Player 
                -> Player
findPlayerID ((Pacman h):t) x | (getPlayerID (Pacman h)) == x = (Pacman h)
                              | otherwise = findPlayerID t x 
findPlayerID ((Ghost h):t) x  | (getPlayerID (Ghost h)) == x = (Ghost h)
                              | otherwise = findPlayerID t x 

-- | Encontra um elemento no maze
findElemInMaze :: Coords -> Maze -> Piece
findElemInMaze (a,b) (x: xs) | a == 0 = findNElem b x
                             | otherwise = findElemInMaze (a-1,b) xs

-- | Encontra um elemento no corridor
findNElem :: Int -> [a] -> a
findNElem i (x: xs) | i == 0 = x
                    | otherwise = findNElem (i-1) xs

-- | Vai buscar a coordenada x dum player 
getPlayerCoordsX :: Player -> Int
getPlayerCoordsX (Pacman (PacState (x,(y1,y2),z,t,h,l) b c d )) = y1
getPlayerCoordsX (Ghost (GhoState (x,(y1,y2),z,t,h,l) b )) = y1

-- | Vai buscar a coordenada y dum player
getPlayerCoordsY :: Player -> Int
getPlayerCoordsY (Pacman (PacState (x,(y1,y2),z,t,h,l) b c d )) = y2
getPlayerCoordsY (Ghost (GhoState (x,(y1,y2),z,t,h,l) b )) = y2

-- * Funções auxiliares usadas para fazer testes

--Dá print as coordenadas de todos os players
printStatsCoords :: State -> [Coords]
printStatsCoords (State m [] l) = []
printStatsCoords (State m ps l) = (getPlayerCoords (head ps)):(printStatsCoords (State m (tail ps) l)) 

-- Dá print do Mode de todos os pacmans
printPacMode :: State -> [PacMode]
printPacMode (State m [] l) = []
printPacMode (State m ((Pacman h):t) l) = (getPacmanMode (Pacman h)):(printPacMode (State m t l))
printPacMode (State m ((Ghost h):t) l) = (printPacMode (State m t l)) 

-- Vai buscar o ghostMode de um ghost
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState a b)) = b

-- Dá print do ghostmode de todos os ghosts
printGhostMode :: State -> [GhostMode]
printGhostMode (State m [] l) = []
printGhostMode (State m ((Ghost h):t) l)  = (getGhostMode (Ghost h)):(printGhostMode (State m t l))
printGhostMode (State m ((Pacman h):t) l) = printGhostMode (State m t l)

getPacTime :: Player -> Double
getPacTime (Pacman (PacState a b c d )) = b

getPacLife :: Player -> Int
getPacLife (Pacman (PacState (x,y,z,t,h,l) q c d )) = l

-- Vai buscar a lista de jogadores
getList :: State -> [Player]
getList (State m ps o) = ps

setPlayerVelAux :: Player -> Double -> Player
setPlayerVelAux (Pacman (PacState (x,y,z,t,h,l) q c d )) a = Pacman (PacState (x,y,a,t,h,l) q c d )
setPlayerVelAux (Ghost (GhoState (x,y,z,t,h,l) q )) a = Ghost (GhoState (x,y,a,t,h,l) q )

--Muda a velocidade do um Player
setPlayerVel :: [Player] -> Int -> Double -> [Player]
setPlayerVel [] _ _ = []
setPlayerVel (h:t) i d = if getPlayerID h == i
                         then (setPlayerVelAux h d):t
                         else h:(setPlayerVel t i d)

-- Dá print do estado de um player
printStats :: Player -> String
printStats p = let (a,b,c,d,e,l) = getPlayerState p
               in "ID:" ++ show a ++ " Coords:" ++ show b ++ " Vel:" ++ show c ++ " Ori:" ++ show d ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n" 