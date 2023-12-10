module UI (module UI) where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

startPoint :: IO ()
startPoint = simpleMain ui