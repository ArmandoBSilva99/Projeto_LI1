-- | Módulo que executa testes da Tarefa3
--
module TesteT3 where

import Tarefa3
import Types

-- * Testes Tarefa 3

-- ** Mazes

-- | Maze Usado na Tarefa 2
maze1 = [
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

maze2 = [
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Wall, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Food Big, Food Little, Empty, Wall],
         [Wall, Wall, Wall, Wall, Wall]
        ]

maze3 = [
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall, Wall]
        ]

-- ** Testes
teste1 = compactMaze maze1

teste2 = compactMaze maze2

teste3 = compactMaze maze3       	

-- | Junta todos os Testes
testesT3 :: [(String, Instructions)]
testesT3 = [("compactMaze maze1: ", compactMaze maze1), ("compactMaze maze2: ", compactMaze maze2), ("compactMaze maze3: ", compactMaze maze3)]
