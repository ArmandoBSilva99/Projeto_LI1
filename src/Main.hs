module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


-- * Main

-- | Carrega o mapa 1F1.txt (= 1.txt mas é válido)
loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1F1.txt") 0 0 0 0 defaultDelayTime )


-- | Muda apenas o jogador de direção
updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k (Manager (State m ps l) pid step before delta delay) | k == KeyUpArrow = Manager (State m (changeOrientation U pid ps) l) pid step before delta delay
                                                                              | k == KeyDownArrow = Manager (State m (changeOrientation D pid ps) l) pid step before delta delay
                                                                              | k == KeyLeftArrow = Manager (State m (changeOrientation L pid ps) l) pid step before delta delay
                                                                              | k == KeyRightArrow = Manager (State m (changeOrientation R pid ps) l) pid step before delta delay
                                                                              | otherwise = Manager (State m ps l) pid step before delta delay
updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                      if (getPacmanMode(findPlayerID (getPlayerList (state man)) (pid man)) == Dying) then drawString $ "Game Over!" else return ()
                    render

-- | Devolve a lista de jogadores do State     
getPlayerList :: State -> [Player]
getPlayerList (State m ps l) = ps

-- | Tempo     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- | Atualiza o tempo
updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state pid step before delta delay) = (Manager state pid step now (delta+(now-before)) delay) 

-- | Coloca o delta a 0
resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state pid step before delta delay) = (Manager state pid step now 0 delay) 

-- | Avança o jogo frame a frame
nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step before delta delay)  = resetTimer now (Manager (passTime step state) pid (step+1) before delta delay)

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w $ updateTime now man
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

-- | Main de testes
main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

-- * Load Game

-- | Inicia um jogo a partir do nome do mapa .txt
runGame :: String -> IO()
runGame s =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w (loadManagerRun s)

-- | Cria o manager com o nome do mapa .txt
loadManagerRun :: String -> Manager
loadManagerRun s = ( Manager (loadMaze s) 0 0 0 0 defaultDelayTime )


-- * Create Game

-- | Cria um jogo recebendo um Maze gerado
createGame :: Maze -> IO()
createGame m =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w (loadManagerCreate m)

-- | Cria 4 players
putPlayersInMap :: Maze -> [Player]
putPlayersInMap m | (mod (length m) 2) == 0 = [(Ghost (  GhoState (1,((div (length m) 2) - 1, div (length (head m)) 2),0,Null,0,1 ) Alive)),
                                               (Ghost (  GhoState (2,((div (length m) 2) - 1, (div (length (head m)) 2) + 1),0,Null,0,1 ) Alive)),
                                               (Ghost (  GhoState (3,((div (length m) 2) - 1, (div (length (head m)) 2) - 1),0,Null,0,1 ) Alive)),
                                               (Pacman ( PacState (0, ((div (length m) 2) + 1, div (length (head m)) 2), 1, R,0,1) 0 Open Normal ))]
                  | otherwise = [ (Ghost (  GhoState (1,((div (length m) 2), div (length (head m)) 2),0,Null,0,1 ) Alive)),
                                 (Ghost (  GhoState (2,((div (length m) 2), (div (length (head m)) 2) - 1),0,Null,0,1 ) Alive)),
                                 (Ghost (  GhoState (3,((div (length m) 2), (div (length (head m)) 2) + 1),0,Null,0,1 ) Alive)),
                                 (Pacman ( PacState (0, ((div (length m) 2) + 2, div (length (head m)) 2), 1, R,0,1) 0 Open Normal )) ] 

-- | Carrega o manager com o mapa gerado e coloca os 4 players
loadManagerCreate :: Maze -> Manager
loadManagerCreate m = ( Manager (State m (putPlayersInMap m) 1) 0 0 0 0 defaultDelayTime)
