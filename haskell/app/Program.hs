{-# OPTIONS -Wall -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Program (startup, mainLoop, shouldClose, teardown) where

import Native (jslog)
import Raylib
  ( beginDrawing,
    clearBackground,
    closeWindow,
    drawFPS,
    drawText,
    endDrawing,
    getScreenHeight,
    initWindow,
    setTargetFPS, isKeyDown, isMouseButtonPressed,
    drawRectangle,
    Key (..), keyToCode,
  )

data Direction = Right | Left | Down | Up
  deriving Show

data ProgramState = ProgramState {
  clicks :: Int,
  direction :: Direction,
  pos :: (Int, Int),
  ticks :: Int,
  ended :: Bool
}
  deriving Show

windowHeight = 800
windowWidth = windowHeight

-- These are the main rendering functions of the program
startup :: IO ProgramState
startup = do
  initWindow windowWidth windowHeight "haskell course example game"
  setTargetFPS 60
  height <- getScreenHeight
  jslog ("screen height: " ++ show height)
  jslog "Window initialized through Haskell"
  return $ ProgramState {
    clicks = 0,
    direction = Program.Right,
    pos = (windowWidth `div` 2, windowHeight `div` 2),
    ticks = 0,
    ended = False
  }

processArrowKeys :: IO (Maybe Direction)
processArrowKeys = do
  right <- isKeyDown $ keyToCode KEY_RIGHT
  left <- isKeyDown $ keyToCode KEY_LEFT
  down <- isKeyDown $ keyToCode KEY_DOWN
  up <- isKeyDown $ keyToCode KEY_UP
  return (case (right, left, down, up) of
    (True, _, _, _) -> Just Program.Right
    (_, True, _, _) -> Just Program.Left
    (_, _, True, _) -> Just Program.Down
    (_, _, _, True) -> Just Program.Up
    _ -> Nothing)

move (x, y) (a, b) = (x + a, y + b)

tile = 40

directionToDelta Program.Right = (tile, 0)
directionToDelta Program.Left = (-tile, 0)
directionToDelta Program.Down = (0, tile)
directionToDelta Program.Up = (0, -tile)

handlePressed pressed state = if pressed then state { clicks = clicks state + 1 } else state

handleDirection dir state =
  case dir of
    Just direction -> state { direction = direction }
    _ -> state

updatePos state
  | ended state = state 
  | ticks state `mod` 30 == 0 = state { pos = move (pos state) (directionToDelta (direction state)) }
  | otherwise = state

updateTicks state
  | ended state = state
  | otherwise = state { ticks = ticks state + 1 }

checkGameEnd state
  | ended state = state -- idempotence
  | otherwise = let (x, y) = pos state
                    (x2, y2) = (x + tile, y + tile)
                    outside = x < 0 || y < 0 || x2 > windowWidth || y2 > windowHeight
                in state { ended = outside }

mainLoop :: ProgramState -> IO ProgramState
mainLoop inState = do
  -- Input
  dir <- processArrowKeys
  pressed <- isMouseButtonPressed 0

  -- Systems
  let state = (checkGameEnd
            . updatePos
            . handleDirection dir
            . handlePressed pressed
            . updateTicks
            ) inState

  -- Drawing
  beginDrawing
  clearBackground 0xff796642 -- blue-ish color in hex in (a, b, g, r) form (this is a workaround to avoid having to implement the `Color` struct)
  drawFPS 10 10
  drawText "Hello from haskell!" 10 40 20 0x44000000
  drawText "Press [ESC] to close game" 10 70 20 0x44000000
  drawText ("This window has been clicked " ++ show (clicks state) ++ " times") 10 100 20 0x44000000
  drawText ("State: " ++ show state) 10 130 20 0x44000000
  drawText ("Tick counter: " ++ show (ticks state)) 10 160 20 0x44000000
  if ended state
    then drawText "Game over!" (windowWidth `div` 2) (windowHeight `div` 2) 40 0x880000ee
    else return ()

  drawRectangle (fst $ pos state) (snd $ pos state) tile tile 0xffee33ee

  endDrawing
  return state

shouldClose :: ProgramState -> IO Bool
shouldClose _ = isKeyDown 256

teardown :: ProgramState -> IO ()
teardown _ = do
  closeWindow
  jslog "Window closed through Haskell"
  return ()
