-- | O objectivo desta tarefa é, dado um labirinto válido, convertê-lo numa sequência de instruções de modo a recriá-lo num formato mais compacto para leitura.
--
module Tarefa3 where

import Types

-- * Funções principais

-- | Compactar Corridor
--
compactMazeHorizontal :: Corridor -> Int -> [(Int,Piece)]
compactMazeHorizontal [] _ = []
compactMazeHorizontal [x] y = [(y,x)]
compactMazeHorizontal (fs:sd:t) x | fs == sd = compactMazeHorizontal (sd:t) (x+1)  
                                  | otherwise = (x,fs): compactMazeHorizontal (sd:t) 1

-- | Compara dois Instructions iguais para colocar Repeat
--
findRepeat :: Instructions -> Instructions 
              -> Int -- ^ index do primeiro Instructions 
              -> Int -- ^ index do fim da Instructions
              -> Instructions
findRepeat _ [] _ _ = [] 
findRepeat is1 is2 i j | i > 0 && i == j - 1 = [auxFind (head is1) is2 0 i]
                       | i > 0 = (auxFind (head is1) is2 0 i):(findRepeat (tail is1) is2 (i+1) j)   
                       | otherwise = (head is1):(findRepeat (tail is1) is2 (i+1) j)  

-- | Função auxiliar de findRepeat que procura num Instructions uma Instruction igual à Instruction chamada 
--
auxFind :: Instruction -> Instructions 
           -> Int -- ^ index que percorre Instructions 
           -> Int -- ^ index da Instructions de onde foi chamada esta função
           -> Instruction
auxFind ins [] _ _ = ins
auxFind ins (h:t) i j | i < j && ins == h = Repeat i
                      | i < j = auxFind ins t (i+1) j
                      | otherwise = ins

-- | Compacta o Corridor para Instruction
--
compactToInstruction :: Corridor -> Instruction
compactToInstruction [] = Instruct []
compactToInstruction l = Instruct (compactMazeHorizontal l 1)

-- | Compacta o Maze para Instruction
--
compactToInstructions :: Maze -> Instructions
compactToInstructions [] = []
compactToInstructions (h:t) = [(compactToInstruction h)] ++ compactToInstructions t 

-- | Função principal 
--
compactMaze :: Maze -> Instructions
compactMaze m = let maze = compactToInstructions m 
            in findRepeat maze maze 0 (length maze)   