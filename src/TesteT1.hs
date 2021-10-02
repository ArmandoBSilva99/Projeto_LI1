-- | Módulo que executa testes da Tarefa1
--
module TesteT1 where

import Tarefa1
import Types

-- * Testes Tarefa 1

-- | Teste 1, Maze 15x15
teste1 = generateMaze 15 15 1

-- | Teste 2, Maze 19x30
teste2 = generateMaze 19 30 2

-- | Teste 3, Maze 21x24
teste3 = generateMaze 21 24 3

-- | Teste 4, Maze 50x47
teste4 = generateMaze 50 47 9
	