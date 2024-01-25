{-# OPTIONS -Wall #-}
module Raylib (initWindow, closeWindow, setTargetFPS, beginDrawing, endDrawing, clearBackground, getScreenHeight, drawFPS, drawText, drawRectangle, isKeyDown, isMouseButtonPressed, Key (..), keyToCode) where

import Foreign (with, toBool)
import Foreign.C (CInt, CUInt, withCString, CBool)
import Native (callRaylibFunction)

initWindow :: Int -> Int -> String -> IO ()
initWindow width height title =
  withCString
    title
    (callRaylibFunction "_InitWindow_" (fromIntegral width :: CInt) (fromIntegral height :: CInt))

closeWindow :: IO ()
closeWindow = callRaylibFunction "_CloseWindow_"

setTargetFPS :: Int -> IO ()
setTargetFPS fps = callRaylibFunction "_SetTargetFPS_" (fromIntegral fps :: CInt)

beginDrawing :: IO ()
beginDrawing = callRaylibFunction "_BeginDrawing_"

endDrawing :: IO ()
endDrawing = callRaylibFunction "_EndDrawing_"

clearBackground :: Integer -> IO ()
clearBackground color =
  with
    (fromIntegral color :: CUInt)
    (callRaylibFunction "_ClearBackground_")

getScreenHeight :: IO Int
getScreenHeight = fromIntegral <$> (callRaylibFunction "_GetScreenHeight_" :: IO CInt)

drawFPS :: Int -> Int -> IO ()
drawFPS x y = callRaylibFunction "_DrawFPS_" (fromIntegral x :: CInt) (fromIntegral y :: CInt)

drawText :: String -> Int -> Int -> Int -> Integer -> IO ()
drawText text x y size color =
  withCString
    text
    ( \t ->
        with
          (fromIntegral color :: CUInt)
          (callRaylibFunction "_DrawText_" t (fromIntegral x :: CInt) (fromIntegral y :: CInt) (fromIntegral size :: CInt))
    )

isKeyDown :: Int -> IO Bool
isKeyDown key = toBool <$> (callRaylibFunction "_IsKeyDown_" (fromIntegral key :: CInt) :: IO CBool)

isMouseButtonPressed :: Int -> IO Bool
isMouseButtonPressed button = toBool <$> (callRaylibFunction "_IsMouseButtonPressed_" (fromIntegral button :: CInt) :: IO CBool)

drawRectangle :: Int -> Int -> Int -> Int -> Integer -> IO ()
drawRectangle posX posY width height color =
  with
  (fromIntegral color :: CUInt)
  (callRaylibFunction "_DrawRectangle_" (fromIntegral posX :: CInt) (fromIntegral posY :: CInt) (fromIntegral width :: CInt) (fromIntegral height :: CInt))

-- How to pass double from callRaylibFunction in index.ts?
-- getTime :: IO Double
-- getTime = do
--   time <- (callRaylibFunction "_GetTime_" :: IO CDouble)
--   return $ coerce time

-- cat Raylib.hs |choose 0 |rg '\n' --multiline --replace ' | '
data Key = KEY_NULL | KEY_APOSTROPHE | KEY_COMMA | KEY_MINUS | KEY_PERIOD | KEY_SLASH | KEY_ZERO | KEY_ONE | KEY_TWO | KEY_THREE | KEY_FOUR | KEY_FIVE | KEY_SIX | KEY_SEVEN | KEY_EIGHT | KEY_NINE | KEY_SEMICOLON | KEY_EQUAL | KEY_A | KEY_B | KEY_C | KEY_D | KEY_E | KEY_F | KEY_G | KEY_H | KEY_I | KEY_J | KEY_K | KEY_L | KEY_M | KEY_N | KEY_O | KEY_P | KEY_Q | KEY_R | KEY_S | KEY_T | KEY_U | KEY_V | KEY_W | KEY_X | KEY_Y | KEY_Z | KEY_LEFT_BRACKET | KEY_BACKSLASH | KEY_RIGHT_BRACKET | KEY_GRAVE | KEY_SPACE | KEY_ESCAPE | KEY_ENTER | KEY_TAB | KEY_BACKSPACE | KEY_INSERT | KEY_DELETE | KEY_RIGHT | KEY_LEFT | KEY_DOWN | KEY_UP | KEY_PAGE_UP | KEY_PAGE_DOWN | KEY_HOME | KEY_END | KEY_CAPS_LOCK | KEY_SCROLL_LOCK | KEY_NUM_LOCK | KEY_PRINT_SCREEN | KEY_PAUSE | KEY_F1 | KEY_F2 | KEY_F3 | KEY_F4 | KEY_F5 | KEY_F6 | KEY_F7 | KEY_F8 | KEY_F9 | KEY_F10 | KEY_F11 | KEY_F12 | KEY_LEFT_SHIFT | KEY_LEFT_CONTROL | KEY_LEFT_ALT | KEY_LEFT_SUPER | KEY_RIGHT_SHIFT | KEY_RIGHT_CONTROL | KEY_RIGHT_ALT | KEY_RIGHT_SUPER | KEY_KB_MENU | KEY_KP_0 | KEY_KP_1 | KEY_KP_2 | KEY_KP_3 | KEY_KP_4 | KEY_KP_5 | KEY_KP_6 | KEY_KP_7 | KEY_KP_8 | KEY_KP_9 | KEY_KP_DECIMAL | KEY_KP_DIVIDE | KEY_KP_MULTIPLY | KEY_KP_SUBTRACT | KEY_KP_ADD | KEY_KP_ENTER | KEY_KP_EQUAL | KEY_BACK | KEY_MENU | KEY_VOLUME_UP | KEY_VOLUME_DOWN

keyToCode :: Key -> Int
keyToCode k = case k of
    KEY_NULL            -> 0        -- Key: NULL, used for no key pressed
    KEY_APOSTROPHE      -> 39       -- Key: '
    KEY_COMMA           -> 44       -- Key: ,
    KEY_MINUS           -> 45       -- Key: -
    KEY_PERIOD          -> 46       -- Key: .
    KEY_SLASH           -> 47       -- Key: /
    KEY_ZERO            -> 48       -- Key: 0
    KEY_ONE             -> 49       -- Key: 1
    KEY_TWO             -> 50       -- Key: 2
    KEY_THREE           -> 51       -- Key: 3
    KEY_FOUR            -> 52       -- Key: 4
    KEY_FIVE            -> 53       -- Key: 5
    KEY_SIX             -> 54       -- Key: 6
    KEY_SEVEN           -> 55       -- Key: 7
    KEY_EIGHT           -> 56       -- Key: 8
    KEY_NINE            -> 57       -- Key: 9
    KEY_SEMICOLON       -> 59       -- Key: ;
    KEY_EQUAL           -> 61       -- Key: =
    KEY_A               -> 65       -- Key: A | a
    KEY_B               -> 66       -- Key: B | b
    KEY_C               -> 67       -- Key: C | c
    KEY_D               -> 68       -- Key: D | d
    KEY_E               -> 69       -- Key: E | e
    KEY_F               -> 70       -- Key: F | f
    KEY_G               -> 71       -- Key: G | g
    KEY_H               -> 72       -- Key: H | h
    KEY_I               -> 73       -- Key: I | i
    KEY_J               -> 74       -- Key: J | j
    KEY_K               -> 75       -- Key: K | k
    KEY_L               -> 76       -- Key: L | l
    KEY_M               -> 77       -- Key: M | m
    KEY_N               -> 78       -- Key: N | n
    KEY_O               -> 79       -- Key: O | o
    KEY_P               -> 80       -- Key: P | p
    KEY_Q               -> 81       -- Key: Q | q
    KEY_R               -> 82       -- Key: R | r
    KEY_S               -> 83       -- Key: S | s
    KEY_T               -> 84       -- Key: T | t
    KEY_U               -> 85       -- Key: U | u
    KEY_V               -> 86       -- Key: V | v
    KEY_W               -> 87       -- Key: W | w
    KEY_X               -> 88       -- Key: X | x
    KEY_Y               -> 89       -- Key: Y | y
    KEY_Z               -> 90       -- Key: Z | z
    KEY_LEFT_BRACKET    -> 91       -- Key: [
    KEY_BACKSLASH       -> 92       -- Key: '\'
    KEY_RIGHT_BRACKET   -> 93       -- Key: ]
    KEY_GRAVE           -> 96       -- Key: `
    KEY_SPACE           -> 32       -- Key: Space
    KEY_ESCAPE          -> 256      -- Key: Esc
    KEY_ENTER           -> 257      -- Key: Enter
    KEY_TAB             -> 258      -- Key: Tab
    KEY_BACKSPACE       -> 259      -- Key: Backspace
    KEY_INSERT          -> 260      -- Key: Ins
    KEY_DELETE          -> 261      -- Key: Del
    KEY_RIGHT           -> 262      -- Key: Cursor right
    KEY_LEFT            -> 263      -- Key: Cursor left
    KEY_DOWN            -> 264      -- Key: Cursor down
    KEY_UP              -> 265      -- Key: Cursor up
    KEY_PAGE_UP         -> 266      -- Key: Page up
    KEY_PAGE_DOWN       -> 267      -- Key: Page down
    KEY_HOME            -> 268      -- Key: Home
    KEY_END             -> 269      -- Key: End
    KEY_CAPS_LOCK       -> 280      -- Key: Caps lock
    KEY_SCROLL_LOCK     -> 281      -- Key: Scroll down
    KEY_NUM_LOCK        -> 282      -- Key: Num lock
    KEY_PRINT_SCREEN    -> 283      -- Key: Print screen
    KEY_PAUSE           -> 284      -- Key: Pause
    KEY_F1              -> 290      -- Key: F1
    KEY_F2              -> 291      -- Key: F2
    KEY_F3              -> 292      -- Key: F3
    KEY_F4              -> 293      -- Key: F4
    KEY_F5              -> 294      -- Key: F5
    KEY_F6              -> 295      -- Key: F6
    KEY_F7              -> 296      -- Key: F7
    KEY_F8              -> 297      -- Key: F8
    KEY_F9              -> 298      -- Key: F9
    KEY_F10             -> 299      -- Key: F10
    KEY_F11             -> 300      -- Key: F11
    KEY_F12             -> 301      -- Key: F12
    KEY_LEFT_SHIFT      -> 340      -- Key: Shift left
    KEY_LEFT_CONTROL    -> 341      -- Key: Control left
    KEY_LEFT_ALT        -> 342      -- Key: Alt left
    KEY_LEFT_SUPER      -> 343      -- Key: Super left
    KEY_RIGHT_SHIFT     -> 344      -- Key: Shift right
    KEY_RIGHT_CONTROL   -> 345      -- Key: Control right
    KEY_RIGHT_ALT       -> 346      -- Key: Alt right
    KEY_RIGHT_SUPER     -> 347      -- Key: Super right
    KEY_KB_MENU         -> 348      -- Key: KB menu
    KEY_KP_0            -> 320      -- Key: Keypad 0
    KEY_KP_1            -> 321      -- Key: Keypad 1
    KEY_KP_2            -> 322      -- Key: Keypad 2
    KEY_KP_3            -> 323      -- Key: Keypad 3
    KEY_KP_4            -> 324      -- Key: Keypad 4
    KEY_KP_5            -> 325      -- Key: Keypad 5
    KEY_KP_6            -> 326      -- Key: Keypad 6
    KEY_KP_7            -> 327      -- Key: Keypad 7
    KEY_KP_8            -> 328      -- Key: Keypad 8
    KEY_KP_9            -> 329      -- Key: Keypad 9
    KEY_KP_DECIMAL      -> 330      -- Key: Keypad .
    KEY_KP_DIVIDE       -> 331      -- Key: Keypad /
    KEY_KP_MULTIPLY     -> 332      -- Key: Keypad *
    KEY_KP_SUBTRACT     -> 333      -- Key: Keypad -
    KEY_KP_ADD          -> 334      -- Key: Keypad +
    KEY_KP_ENTER        -> 335      -- Key: Keypad Enter
    KEY_KP_EQUAL        -> 336      -- Key: Keypad =
    KEY_BACK            -> 4        -- Key: Android back button
    KEY_MENU            -> 5        -- Key: Android menu button
    KEY_VOLUME_UP       -> 24       -- Key: Android volume up button
    KEY_VOLUME_DOWN     -> 25       -- Key: Android volume down button