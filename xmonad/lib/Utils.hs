module Utils( onlyOne
            , reverseArgs3
            , getFocusedWindowProperty
            )
                where

import XMonad (X, getStringProperty, gets)
import XMonad.Core (withDisplay, windowset)
import XMonad.StackSet (peek)

-- Get the WM_NAME of the window that is in focus
getFocusedWindowProperty :: String -> X (Maybe String)
getFocusedWindowProperty prop = do
    ss <- gets windowset
    let win = peek ss

    let getName = \w -> withDisplay $ reverseArgs3 getStringProperty prop w
    case win of Nothing -> return Nothing
                Just w -> getName w

-- TODO: Get rid of this stupid thing
onlyOne :: String -> String -> String
onlyOne proc args = "pidof " ++ proc ++ " || " ++ proc ++ " " ++ args

reverseArgs3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
reverseArgs3 f = g
    where g a b c = f c b a


