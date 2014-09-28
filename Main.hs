
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace
tsi x = trace (show x) x

type Pos = (Int,Int)
posX = fst
posY = snd

mkPos :: Int -> Int -> Pos
mkPos x y = (x,y)

modPos :: Pos -> Pos -> Pos
modPos (x1,y1) (x2,y2) = (x1 `mod` x2, y1 `mod` y2)

data Move = MUp | MRight | MDown | MLeft
          deriving (Show, Eq)

data Block = Block { blockPos   :: Pos
                   , blockColor :: Color
                   }
             deriving (Show, Eq)

type Snake = [Block]
type Apple = Block

data World
  = World
    { wSnake     :: Snake
    , wApples    :: [Apple]
    , wScore     :: Int
    , wDirection :: Move
    , wLastDir   :: Move
    , wCountDown :: Float
    , wToGrow    :: Int
    , wGen       :: StdGen
    , wWorldSize :: Pos
    }
  | MenuScreen
    { mMenuText :: String
    , mSubText  :: String
    , mGen      :: StdGen
    }
  deriving (Show)

mkWorld :: StdGen -> World
mkWorld gen =
  let (apple, gen') = genApple sz gen
  in World { wSnake     = [Block (mkPos (posX sz `div` 2) (posY sz `div` 2)) snakeColor]
           , wApples    = [apple]
           , wScore     = 0
           , wDirection = MRight
           , wLastDir   = MRight
           , wCountDown = getSpeed 0
           , wToGrow    = 3
           , wGen       = gen'
           , wWorldSize = sz
           }
  where sz = initWorldSize

mkMenu :: StdGen -> String -> String -> World
mkMenu gen text sub = MenuScreen { mMenuText = text, mSubText = sub, mGen = gen }

initWorldSize = (15,15) :: Pos
initSqSize    = (24,24) :: Pos

sqSize :: Pos -> Pos
sqSize wSize = initSqSize

getSpeed :: Int -> Float
getSpeed score = limit $ slow - rate * fromIntegral score
  where
    rate = 0.0005
    fast = 0.005
    slow = 0.1
    limit x = max fast (min slow x)

collidesWithColor :: Pos -> [Block] -> Maybe Color
thisPos `collidesWithColor` those = liftM blockColor $ find go those
  where
    go Block { blockPos=thatPos } = thisPos == thatPos

menuMain  = "~SNAKE~"  :: String
menuSub   = "[Enter]"  :: String
loseMain  = "You died" :: String
loseScore = "Score: "  :: String

colz :: [Color]
colz = [ rose, orange, aquamarine, azure, violet
       , yellow, magenta, red, blue, white
       ]

genApple :: Pos -> StdGen -> (Apple, StdGen)
genApple sz gen =
  let
    (num1, newGen)  = next gen
    (num2, newGen') = next newGen
  in
   ( Block { blockPos   = (mkPos num1 num2) `modPos` sz
           , blockColor = colz !! ((num1 + num2) `mod` length colz)
           }
   , newGen'
   )

snakeColor = chartreuse :: Color

moveSnake :: Pos -> Snake -> Move -> [Apple] -> Bool -> (Snake, Bool)
moveSnake wsize snake move apples isGrowing =
  let
    newHeadPosn   = movePos move (blockPos $ head snake)
    newRestPosns  = map blockPos $ if isGrowing then snake else init snake

    isEatingColor = newHeadPosn `collidesWithColor` apples
    colors        = (map blockColor snake) ++ repeat snakeColor
    colors'       = case isEatingColor of Nothing    -> colors
                                          (Just col) -> (mixColors 0.7 0.3 col snakeColor) : colors
    colors''      = map (mixColors 0.01 0.99 snakeColor) colors'

  in
   ( (zipWith Block (newHeadPosn : newRestPosns) colors'')
   , isJust isEatingColor
   )

movePos :: Move -> Pos -> Pos
movePos MUp    (x,y) = mkPos x     (y+1)
movePos MRight (x,y) = mkPos (x+1) y
movePos MDown  (x,y) = mkPos x     (y-1)
movePos MLeft  (x,y) = mkPos (x-1) y

outOfBounds (wx,wy) (x,y) = x < 0  || y < 0 ||
                            x >= wx || y >= wy
updateState :: World -> World
updateState w@(World { wSnake     = snake
                     , wDirection = direction
                     , wApples    = apples
                     , wToGrow    = toGrow
                     , wScore     = score
                     , wGen       = gen
                     , wWorldSize = sz
                     }) =
  let
    (newSnake, isEating) =
      moveSnake sz snake direction apples (toGrow > 0)
    newScore      = score + (if isEating then 1 else 0)
    isDead        = (isJust $ (blockPos $ head newSnake) `collidesWithColor` tail snake)
                    || (outOfBounds sz $ blockPos (head newSnake))
    (replApple, gen') = if isEating
                        then ([apple'],gen')
                        else ([]      ,gen)
      where (apple', gen') = genApple sz gen
    remApples     = filter (go (blockPos (head newSnake))) apples
      where go pos apple = pos /= blockPos apple
  in
   if isDead
   then mkMenu gen' loseMain $ loseScore ++ show newScore
   else w { wSnake     = newSnake
          , wApples    = if isEating then replApple ++ remApples else apples
          , wScore     = newScore
          , wGen       = gen'
          , wToGrow    = max 0 $ toGrow + (if isEating then 1 else -1)
          , wWorldSize = sz
          , wLastDir   = direction
          }

updateState m@(MenuScreen {}) = m

keyToMove :: Move -> SpecialKey -> Move
keyToMove _   KeyUp    = MUp
keyToMove _   KeyRight = MRight
keyToMove _   KeyDown  = MDown
keyToMove _   KeyLeft  = MLeft
keyToMove cur _        = cur

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey k) Down _ _) w@(World {wLastDir=oldDir}) =
  let
    newDir = keyToMove oldDir k
  in
   w { wDirection =
          -- Don't allow snake to turn around and immediately die
          if newDir == MUp && oldDir == MDown ||
             newDir == MDown && oldDir == MUp ||
             newDir == MLeft && oldDir == MRight ||
             newDir == MRight && oldDir == MLeft
          then
            oldDir
          else
            newDir
     }

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) m@(MenuScreen {mGen=g}) = mkWorld g
handleEvent _ w = w

pxWidth,pxHeight,hWidth,hHeight,cellPadding :: Float
pxWidth  = fromIntegral $ posX initSqSize * posX initWorldSize
pxHeight = fromIntegral $ posY initSqSize * posY initWorldSize
hWidth = pxWidth / 2
hHeight = pxHeight / 2
cellPadding = 2.0

renderWorld :: World -> Picture
renderWorld w@(World {wScore=score, wSnake=snake, wApples=apples, wWorldSize=sz}) =
   translate (-hWidth) (-hHeight) $ pictures (
     renderBG sz
     :  map (renderBlock sz) snake
     ++ map (renderBlock sz) apples
     ++ [color white . text $ show score]
     )
renderWorld m@(MenuScreen txt sub gen) = translate (-hWidth) (-hHeight) $ pictures [
  renderBG initWorldSize,
  translate (hWidth - 50.0) hHeight $ scale 0.2 0.2 $ color white (text txt),
  translate (hWidth - 30.0) (hHeight -30.0) $ scale 0.1 0.1 $ color white (text sub) ]

renderBlock :: Pos -> Block -> Picture
renderBlock wsz (Block pos col) = (color col) . (square wsz) $ pos

renderBG :: Pos -> Picture
renderBG sz = pictures $ wallpaper : vertLines ++ horizLines
  where
    halfPad = cellPadding / 2
    wallpaper = color (greyN 0.1) $ Polygon [ (0.0,0.0), (pxWidth,0.0), (pxWidth,pxHeight), (0.0,pxHeight) ]
    vertLines = map withX [0,fromIntegral (posX $ sqSize sz) .. pxWidth]
    horizLines = map withY [0,fromIntegral (posY $ sqSize sz) .. pxHeight]
    withX x = color (greyN 0.15) $ Polygon [
      (x-halfPad,0), (x+halfPad,0), (x+halfPad,pxHeight), (x-halfPad,pxHeight)]
    withY y = color (greyN 0.15) $ Polygon [
      (0,y-halfPad), (0,y+halfPad), (pxWidth,y+halfPad), (pxWidth,y-halfPad)]

square :: Pos -> Pos -> Picture
square wsz (x',y') = Polygon [ (w * x + padding,       h * y + padding)
                             , (w * (x + 1) - padding, h * y + padding)
                             , (w * (x + 1) - padding, h * (y + 1) - padding)
                             , (w * x + padding,       h * (y + 1) - padding)
                             ]
  where
    x = fromIntegral x'
    y = fromIntegral y'
    w = fromIntegral . posX $ sqSize wsz
    h = fromIntegral . posY $ sqSize wsz
    padding = cellPadding

updateTick :: Float -> World -> World
updateTick _ menu@(MenuScreen {}) = menu
updateTick time world@(World {wScore=score, wCountDown=countDown}) =
  let t = countDown - time :: Float
  in if t < 0.0
     then case updateState world of
       world@(World {}) -> world { wCountDown = getSpeed score - t }
       w                -> w
     else world { wCountDown = t }

main :: IO ()
main =
  do
    stdGen <- getStdGen
    play display bg stepsPerSecond (mkMenu stdGen menuMain menuSub) renderWorld handleEvent updateTick
  where
    stepsPerSecond = 60
    display        = InWindow "Snake" (width, height) (0, 0)
    bg             = greyN 0.2
    width          = posX initSqSize * (1 + posX initWorldSize)
    height         = posY initSqSize * (1 + posY initWorldSize)

-- ideas
-- - randomly outline map w/ pseudo-apples where if you eat all then walls are removed to let you wrap around
-- - corner apples give bonus
-- - if you eat an apple in minimum time possible you get a bonus
-- - bonus could be extra life where if you die then your old body turns into apples but you keep speed & score
-- - ai enemy snake like tron

