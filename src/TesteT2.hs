-- | Módulo que executa testes da Tarefa3
--
module TesteT2 where

import Tarefa2
import Types

-- * Testes Tarefa 2

-- ** Maze
-- | Maze usado para o teste

m = [
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
     [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Wall, Wall, Food Little, Food Big, Wall],
     [Wall, Food Big, Food Little, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Little, Food Big, Wall],
     [Wall, Food Little, Food Little, Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty, Food Little, Wall, Wall],
     [Empty, Food Little, Food Little, Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Food Little, Food Little, Empty],
     [Empty, Food Little, Food Little, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Food Little, Food Little, Empty],
     [Wall, Food Little, Food Little, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Food Little, Food Little, Wall],
     [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Wall, Food Little, Food Big, Wall],
     [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Big, Wall],
     [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
    ]

{- 
  Representação do Maze:
  
  #################
  #o..........##.o#
  #o.           .o#
  #.. ###   ### .##
   .. #       # .. 
   .. ######### .. 
  #..           ..#
  #o...........#.o#
  #o.............o#
  #################

-}

-- ** Jogadores do Maze

-- *** Jogadores para TesteT2_1 e TesteT2_2
player1 = (Pacman (PacState (0, (1,2), 1, L, 0, 1) 0 Open Normal))
player2 = (Ghost (GhoState (1, (2,1), 1, Null, 0, 2) Alive))
player3 = (Ghost (GhoState (2, (4,7), 1, Null, 0, 2) Alive))
player4 = (Ghost (GhoState (3, (4,9), 1, Null, 0, 2) Alive))
listp1 = [player4, player2, player3, player1]


-- *** Jogadores para TesteT2_3
player5 = (Pacman (PacState (0, (1,2), 1, L, 0, 1) 0 Open Normal))
player6 = (Ghost (GhoState (1, (2,1), 1, Null, 0, 2) Alive))
player7 = (Ghost (GhoState (2, (2,1), 1, Null, 0, 2) Alive))
listp2 = [player5, player6, player7]

-- ** Sequência de Jogadas

-- | Estado Inicial para TesteT2_1 e TesteT2_2
e1 = State m listp1 1

-- | Estado Inicial para TesteT2_3
e2 = State m listp2 1

-- *** Para TesteT2_1
e2_1 = play (Move 0 L) e1
e3_1 = play (Move 0 D) e2_1
e4_1 = play (Move 0 D) e3_1
e5_1 = play (Move 0 D) e4_1
e6_1 = play (Move 0 D) e5_1
e7_1 = play (Move 0 L) e6_1
e8_1 = play (Move 0 L) e7_1
e9_1 = play (Move 0 L) e8_1

-- *** Para TesteT2_2
e2_2 = play (Move 0 U) e1
e3_2 = play (Move 0 U) e2_2
e4_2 = play (Move 0 D) e3_2
e5_2 = play (Move 0 D) e4_2
e6_2 = play (Move 0 L) e5_2
e7_2 = play (Move 0 L) e6_2
e8_2 = play (Move 0 D) e7_2

-- *** Para TesteT2_3
e2_3 = play (Move 0 L) e2
e3_3 = play (Move 0 D) e2_3
e4_3 = play (Move 0 D) e3_3
e5_3 = play (Move 0 D) e4_3

-- ** Testes

--  Para facilitar a compreensão dos testes, coloquei um "\n" antes no printMaze na instance Show State no Types.hs fornecido pelos professores
--Ao chamar esta função no terminal vai aparecer uma sequência de jogadas onde testa comer o ghost e fazê lo voltar ao inicio, testa também a transição no túnel
testeT2_1 :: [(String, State)]
testeT2_1 = [
             ("Inicio:", e1),("play (Move 0 L) e1",e2_1),("play (Move 0 D) e2_1",e3_1),("play (Move 0 D) e3_1",e4_1),
             ("play (Move 0 D) e4_1",e5_1),("play (Move 0 D) e5_1",e6_1),("play (Move 0 L) e6_1",e7_1),("play (Move 0 L) e7_1",e8_1),("play (Move 0 L) e8_1",e9_1)
            ]
          
--Ao chamar esta função no terminal testa a colisão de parede, a morte do Pacman quando choca contra um Ghost e nessa mesma posição existe Food Big 
testeT2_2 :: [(String, State)]
testeT2_2 = [
             ("Inicio:",e1),("play (Move 0 U) e1",e2_2),("play (Move 0 U) e2_2", e3_2),("play (Move 0 D) e3_2",e4_2),
             ("play (Move 0 D) e4_2", e5_2),("play (Move 0 L) e5_2",e6_2),("play (Move 0 L) e6_2", e7_2), ("play (Move 0 D) e7_2", e8_2)
            ]

testeT2_3 :: [(String, State)]
testeT2_3 = [
             ("Inicio: ", e2), ("play (Move 0 L) e2", e2_3), ("play (Move 0 D) e2_3", e3_3), ("play (Move 0 D) e3_3", e4_3), ("play (Move 0 D) e4_3", e5_3)    
            ]            

