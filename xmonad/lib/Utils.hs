module Utils( dynamicWorkspaceFromFocused )
                where

import XMonad (X, getStringProperty, gets, windows)
import XMonad.Core (withDisplay, windowset)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.StackSet (peek, shift)

-- Get the WM_NAME of the window that is in focus
getFocusedWindowProperty :: String -> X (Maybe String)
getFocusedWindowProperty prop = do
    ss <- gets windowset
    let win = peek ss

    let getName = \w -> withDisplay $ reverseArgs3 getStringProperty prop w
    case win of Nothing -> return Nothing
                Just w -> getName w

reverseArgs3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
reverseArgs3 f = g
    where g a b c = f c b a

dynamicWorkspaceFromFocused :: X (Maybe String)
dynamicWorkspaceFromFocused = do 
    focusedWorkspaceName <- Utils.getFocusedWindowProperty "WM_NAME"
    case focusedWorkspaceName of 
                 Nothing -> return ()
                 Just n -> do
                     addHiddenWorkspace n
                     windows . shift $ n
    return focusedWorkspaceName


