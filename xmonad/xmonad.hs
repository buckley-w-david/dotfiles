------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import System.IO                        (hPutStrLn)
import System.Exit                      (exitSuccess)

    -- Actions
import XMonad.Actions.CopyWindow        (copyToAll, killAllOtherCopies, kill1)
import XMonad.Actions.CycleWindows      (cycleRecentWindows)
import XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.SpawnOn           (manageSpawn)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, selectWorkspace, withWorkspace, removeWorkspace, renameWorkspace)

    -- Hooks
import XMonad.Hooks.DynamicLog          (dynamicLogWithPP, ppOutput, ppTitle, ppCurrent, ppVisible, xmobarPP, xmobarColor, shorten, wrap)
import XMonad.Hooks.DynamicProperty     (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops        (ewmh, ewmhDesktopsEventHook, ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers       (isFullscreen, isDialog, doFullFloat, doCenterFloat, transience')
import XMonad.Hooks.ManageDocks         (avoidStruts, docks, ToggleStruts(..))

    -- Layout
import XMonad.Layout.NoBorders          (noBorders, smartBorders)
import XMonad.Layout.ResizableTile      (ResizableTall(..))


    -- Prompt
import XMonad.Prompt                    (XPConfig(..), XPPosition(..))

    -- StackSet
import XMonad.StackSet                  (shift, greedyView)

    -- Utils
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.Scratchpad           (scratchpadManageHookDefault)
import XMonad.Util.NamedScratchpad      (namedScratchpadAction, defaultFloating, NamedScratchpad(..))
import XMonad.Util.SpawnOnce

    -- Custom
import qualified Utils

------------------------------------------------------------------------
-- TODO
------------------------------------------------------------------------
-- Pillage the following configs
--   - https://gitlab.com/dwt1/dotfiles/blob/master/.xmonad/xmonad.hs
--   - https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs
--   - https://github.com/quarkQuark/dotfiles/blob/master/.config/xmonad/src/xmonad.hs
--   - https://github.com/hallettj/dot-xmonad/blob/master/home/.xmonad/xmonad.hs
--
-- Check out Dynamic Projects: https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-DynamicProjects.html

------------------------------------------------------------------------
-- Dynamic Workspace Functions
------------------------------------------------------------------------
-- TODO: modularize this mess
-- Create a new workspace named after the WM_NAME property of the currently focused window, and yield that name
dynamicWorkspaceFromFocused :: X (Maybe String)
dynamicWorkspaceFromFocused = do 
    focusedWorkspaceName <- Utils.getFocusedWindowProperty "WM_NAME"
    case focusedWorkspaceName of 
                 Nothing -> return ()
                 Just n -> do
                     addHiddenWorkspace n
                     windows . shift $ n
    return focusedWorkspaceName

-- Switch to new window
dynamicWorkspace :: X ()
dynamicWorkspace = do
    _ <- dynamicWorkspaceFromFocused
    return ()

-- FIXME: Doesn't work right now
dynamicWorkspaceAndSwitch :: X ()
dynamicWorkspaceAndSwitch = do
    focusedWorkspaceName <- dynamicWorkspaceFromFocused
    case focusedWorkspaceName of 
                 Nothing -> return ()
                 Just n -> do
                     windows . greedyView $ n



------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows ke

myTerminal :: String
myTerminal = "terminte"       -- Sets modkey to super/windows ke

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    ewmhDesktopsStartup
    spawnOnce "nm-applet --sm-disable &"
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 17 &"
    spawnOnce "xss-lock --xscreensaver-command -lock &"
    spawnOnce "xscreensaver -no-splash &"


-- WORKSPACES
------------------------------------------------------------------------
-- TODO: Create dynamicPropertyChange hooks for each of my workspaces so that anything
-- with my CUSTOM_TYPE xprop gets moved to it's correct workspace. This is a measure to help prevent
-- have to list all that various and sundry applications I'd like to move in my xmonad config file
-- makeDynamicPropertyChange
myWorkspaceNames = ["main", "comms", "media", "dev1", "dev2", "dev3" ,"games", "wiki", "launchers"] 
myWorkspaces = map ( \(x, y) -> show (x::Int) ++ ":" ++ y) (zip [1..] myWorkspaceNames)
-- myWorkspaceHooks = map makeDynamicPropertyChange myWorkspaces


------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ full ||| tall -- ||| grid ||| tab 
  where
   full = noBorders Full
   tall = smartBorders $ ResizableTall 1 (10/100) (1/2) []


------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
myManageHook = manageSpawn <+> composeAll [ isFullscreen   --> doFullFloat
                                     , isDialog                       --> doCenterFloat
                                     , className =? "Gimp"            --> doFloat
                                     , className =? "zoom"            --> doFloat
                                     , role      =? "pop-up"          --> doFloat
                                     , className =? "Steam"           --> doShift "7:games"
                                     , className =? "Lutris"          --> doShift "7:games"
                                     , className =? "discord"         --> doShift "2:comms"
                                     , className =? "Ripcord"         --> doShift "2:comms"
                                     , className =? "Thunderbird"     --> doShift "2:comms"
                                     , transience'
                                     , scratchpadManageHookDefault
                                     ]
    where
      role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- EVENTHOOK
------------------------------------------------------------------------
spotifyEventHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> defaultFloating) -- Spotify doesn't set their props before it maps itself, we have to wait for the change dynamically
gameEventHook = dynamicPropertyChange "CUSTOM_TYPE" (stringProperty "CUSTOM_TYPE" =? "Game" --> doShift "7:games") -- I'd like to not have to explicitly list game stuff, and instead give them a custom xprop that we look for

myEventHooks = handleEventHook def <+> spotifyEventHook <+> gameEventHook <+> ewmhDesktopsEventHook  -- <+> fullscreenEventHook -- I like this "bug" https://github.com/xmonad/xmonad-contrib/issues/183


------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------

myXPConfig :: XPConfig
myXPConfig = def
              {font        = "xft:Monospace:pixelsize=16"
              , bgColor     = "black"
              , fgColor     = "grey"
              , bgHLight    = "#4444aa"
              , fgHLight    = "#ddddff"
              , borderColor = "#444488"
              , position    = Top
              }

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads = [
                      NS "spotify" "spotify" (className =? "Spotify") defaultFloating
                    , NS "termite" "termite --name scratchpad" (appName =? "scratchpad") defaultFloating
                    ]


------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys = [ ("M-b"          , sendMessage ToggleStruts              ) -- toggle the status bar gap
         -- Workspace Bindings
         , ("M-<Tab>"        , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab  ) -- classic alt-tab behaviour
         , ("M-<Right>"      , nextWS                                       ) -- go to next workspace
         , ("M-<Left>"       , prevWS                                       ) -- go to prev workspace
         , ("M-S-<Right>"    , shiftToNext                                  ) -- move client to next workspace
         , ("M-S-<Left>"     , shiftToPrev                                  ) -- move client to prev workspace

         -- Dynamic Workspace Bindings
         , ("M-v"            , selectWorkspace myXPConfig                   ) -- Create a new dynamic workspace
         , ("M-S-v"          , withWorkspace myXPConfig (windows . shift) ) -- Shift current window to workspace
         , ("M-m"            , dynamicWorkspace                             ) -- Create a new dynamic workspace from the name of the currently focused window
         , ("M-S-m"          , dynamicWorkspaceAndSwitch                    ) -- Create a new dynamic workspace from the name of the currently focused window
         , ("M-<Backspace>"  , removeWorkspace                              ) -- Delete dynamic workspace
         , ("M-S-r"          , renameWorkspace myXPConfig                   ) -- Rename

         -- Window Copying Bindings
         , ("M-a"            , windows copyToAll                            ) -- Pin to all workspaces
         , ("M-C-a"          , killAllOtherCopies                           ) -- remove window from all but current
         , ("M-S-a"          , kill1                                        ) -- remove window from current, kill if only one

         -- dmenu Shenanigans
         , ("M-p"            , spawn "dmenu_run_history"                    ) -- app launcher
         , ("M-c"            , spawn "dmenu_python"                         ) -- Run one-off Python commands

         -- Password Manager
         , ("M-C-p"          , spawn "passmenu"                            ) -- Display password prompt
         , ("M-S-p"          , spawn "passmenu --type"                     ) -- Display password prompt

         -- System Controls
         , ("M-C-d"          , spawn disableXscreensaver                    ) -- disable xscreensaver
         , ("M-C-l"          , spawn "xscreensaver-command -lock"           ) -- lock screen

         -- Media Controls
         , ("<XF86AudioMute>"         , spawn "/home/david/scripts/vol-ctl toggle")
         , ("<XF86AudioRaiseVolume>"  , spawn "/home/david/scripts/vol-ctl up")
         , ("<XF86AudioLowerVolume>"  , spawn "/home/david/scripts/vol-ctl down")
         , ("<XF86AudioNext>"         , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotifyd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
         , ("<XF86AudioPrev>"         , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotifyd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
         , ("<XF86AudioStop>"         , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotifyd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop")
         , ("<XF86AudioPlay>"         , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotifyd /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause") 

         -- Applications
         , ("<Print>"        , spawn "maim -s | xclip -selection clipboard -t image/png"                            ) -- Screenshot
         , ("M-<Print>"      , spawn "maim -i $((16#$(xwininfo | grep \"Window id\" | awk '{print $4}' | cut -c3-))) ~/Pictures/Screenshots/$(date +%s).png" ) -- Screenshot
         , ("M-S-s"          , namedScratchpadAction myScratchPads "spotify"  ) -- Spawn a scratchpad with spotify
         , ("M-<F12>"        , namedScratchpadAction myScratchPads "termite"  ) -- Spawn a scratchpad with termite
         , ("M-w"            , spawn "firefox"                              ) -- launch browser
         -- , ("M-e"            , spawn "rox"                                  ) -- launch file manager
         , ("M-e"            , spawn "termite -e 'vifm'"                    ) -- launch file manager

         -- Exiting
         , ("M-r"            , spawn "xmonad --recompile && xmonad --restart" ) -- restart xmonad
         , ("M-q"            , restart "xmonad" True                        )
         , ("M-C-<Delete>"   , spawn "shutdown -r now"                      ) -- reboot
         , ("M-C-<Insert>"   , spawn "shutdown -h now"                      ) -- poweroff
         , ("M-S-q"          , io  exitSuccess                              ) -- Exit Xmonad
         ]

    where
      disableXscreensaver = "sed -i 's/mode:\\s*off/\\x0/g; s/mode:\\s*one/mode:\\t\\toff/g; s/\\x0/mode:\\t\\tone/g' ~/.xscreensaver"


------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/david/.xmobarrc0"
    xmproc1 <- spawnPipe "xmobar -x 1 /home/david/.xmobarrc1"

    xmonad $ ewmh $ docks $ def {
      modMask           = myModMask
    , focusFollowsMouse = myFocusFollowsMouse
    , terminal          = myTerminal
    , startupHook = myStartupHook
    , logHook = dynamicLogWithPP $ xmobarPP 
                    { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    , ppCurrent = xmobarColor "white" "" . wrap "<" ">"
                    , ppVisible = wrap "[" "]" 
                    }
    , manageHook = myManageHook 
    , handleEventHook  = myEventHooks
    , layoutHook  = myLayoutHook
    , workspaces = myWorkspaces
    } `additionalKeysP` myKeys     
