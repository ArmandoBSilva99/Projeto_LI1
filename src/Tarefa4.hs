{- | Module : Tarefa4
   | Description : Movimentar o Pacman e os Ghosts ao mesmo tempo
   | Copyright : Armando Silva <a87949@alunos.uminho.pt> 

= Introdução:
O objetivo desta tarefa é fazer movimentar todos os jogadores no mapa, atualizando a Tarefa 2 com funções que permitem a Tarefa4 cumprir o seu objetivo 

= Objetivos:
Esta tarefa, contém apenas 2 funções:

  - makeMovesGh -> executa plays, que são calculadas na Tarefa 5, em todos os Ghosts na lista de jogadores
  - passTime -> função principal que executa a função play no Pacman e junta com as plays do Ghost chamando a função makeMovesGh tomando atenção ao step, de maneira a que faça com que o ghost fique mais lento caso esteja em Modo Dying

= Conclusão:
	O função corre, no entanto, só considera duas velocidades, quando o step é par ou impar
-}


module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5

-- | Tempo do delay genérico
defaultDelayTime = 250 -- 250 ms

-- | Faz play a todos os jogadores 
passTime :: Int -- ^ Step 
            -> State -> State
passTime x s@(State m ps l) | (mod x 2 == 0) = makeMovesGh (ghostPlay s) (play (Move (getPlayerID (findPacman ps)) (getPlayerOrientation(findPacman ps))) s)
							| otherwise = (play (Move (getPlayerID (findPacman ps)) (getPlayerOrientation(findPacman ps))) s)

-- | Faz as plays do ghosts que são calculadas na tarefa 5
makeMovesGh :: [Play] -> State -> State
makeMovesGh [] s = s
makeMovesGh (h:t) s = makeMovesGh t $ play h s 
