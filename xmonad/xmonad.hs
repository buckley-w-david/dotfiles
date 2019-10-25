-- TODO: Clean up imports
import System.IO                        (hPutStrLn)
import System.Exit                      (exitSuccess)

import XMonad
import XMonad.Actions.CopyWindow        (copyToAll, killAllOtherCopies, kill1)
import XMonad.Actions.CycleWindows      (cycleRecentWindows)
import XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.SpawnOn           (manageSpawn)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, selectWorkspace, withWorkspace, removeWorkspace, renameWorkspace)
import XMonad.Hooks.DynamicLog          (ppOutput, ppTitle, dynamicLogWithPP, xmobarPP, xmobarColor, shorten)
import XMonad.Hooks.DynamicProperty     (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops        (ewmh, fullscreenEventHook, ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers       (isFullscreen, isDialog, doFullFloat, doCenterFloat, transience')
import XMonad.Hooks.ManageDocks         (avoidStruts, docks, ToggleStruts(..))
import XMonad.Layout.NoBorders          (noBorders, smartBorders)
import XMonad.Layout.Fullscreen         (fullscreenSupport)
import XMonad.Layout.ResizableTile      (ResizableTall(..))
import XMonad.Prompt                    (XPConfig(..), XPPosition(..))
import XMonad.StackSet                  (shift, greedyView)
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.Scratchpad           (scratchpadManageHookDefault)
import XMonad.Util.NamedScratchpad      (namedScratchpadAction, defaultFloating, NamedScratchpad(..))

import qualified Utils

-- TODO: modularize this mess

-- Create a new workspace named after the WM_NAME property of the currently focused window, and yield that name
dynamicWorkspaceFromFocused :: X (Maybe String)
dynamicWorkspaceFromFocused = do 
    name <- Utils.getFocusedWindowProperty "WM_NAME"
    case name of Nothing -> return ()
                 Just n -> do
                     addHiddenWorkspace n
                     windows . shift $ n
    return name

-- Switch to new window
dynamicWorkspace :: X ()
dynamicWorkspace = do
    dynamicWorkspaceFromFocused
    return ()

-- FIXME: Doesn't work right now
dynamicWorkspaceAndSwitch :: X ()
dynamicWorkspaceAndSwitch = do
    name <- dynamicWorkspaceFromFocused
    case name of Nothing -> return ()
                 Just n -> do
                     windows . greedyView $ n

-- TODO: Create dynamicPropertyChange hooks for each of my workspaces so that anything
-- with my CUSTOM_TYPE xprop gets moved to it's correct workspace. This is a measure to help prevent
-- have to list all that various and sundry applications I'd like to move in my xmonad config file
-- makeDynamicPropertyChange

myWorkspaces = ["main", "comms", "media", "dev1", "dev2", "dev3" ,"games", "wiki", "launchers"] 

-- myWorkspaceHooks = map makeDynamicPropertyChange myWorkspaces
spotifyEventHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> defaultFloating) -- Spotify doesn't set their props before it maps itself, we have to wait for the change dynamically
gameEventHook = dynamicPropertyChange "CUSTOM_TYPE" (stringProperty "CUSTOM_TYPE" =? "Game" --> doShift "7:games") -- I'd like to not have to explicitly list game stuff, and instead give them a custom xprop that we look for

myLayoutHook = avoidStruts $ full ||| tall -- ||| grid ||| tab 
  where
   full = noBorders Full
   tall = smartBorders $ ResizableTall 1 (10/100) (1/2) []

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

main = do
    -- TODO: Fix this janky "Utils.onlyOne" solution
    -- Probably just want to throw this stuff in my xinitrc, except xmobar
    spawn $ Utils.onlyOne "twmnd" ""
    spawn $ Utils.onlyOne "trayer" "--edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 17"
    spawn $ Utils.onlyOne "xscreensaver" "-no-splash"
    spawn $ Utils.onlyOne "xss-lock" "-- xscreensaver-command -lock"
    spawn $ Utils.onlyOne "scrolld.py" ""
    spawn $ Utils.onlyOne "nm-applet" "--sm-disable"
    spawn $ Utils.onlyOne "xbindkeys" ""

    xmproc <- spawnPipe "xmobar /home/david/.xmobarrc"
    xmonad $ ewmh $ fullscreenSupport $ docks def {
      modMask           = mod4Mask -- use the Windows button as mod
    , focusFollowsMouse = False
    , terminal          = "termite"
    , logHook           = dynamicLogWithPP xmobarPP 
                                  { ppOutput = hPutStrLn xmproc 
                                  , ppTitle = xmobarColor "green" "" . shorten 50}
    , manageHook        = manageSpawn <+> composeAll [ isFullscreen   --> doFullFloat
                                     , isDialog                       --> doCenterFloat
                                     , className =? "Gimp"            --> doFloat
                                     , className =? "zoom"            --> doFloat
                                     , role      =? "pop-up"          --> doFloat
                                     , className =? "Steam"           --> doShift "7:games"
                                     , className =? "Lutris"          --> doShift "7:games"
                                     , className =? "discord"         --> doShift "2:comms"
                                     , className =? "Thunderbird"     --> doShift "2:comms"
                                     , transience'
                                     , scratchpadManageHookDefault
                                     ]
    , handleEventHook   = spotifyEventHook <+> gameEventHook <+> handleEventHook def <+> fullscreenEventHook
    , layoutHook  = myLayoutHook
    , startupHook = ewmhDesktopsStartup
    , workspaces = map ( \(x, y) -> show x ++ ":" ++ y) (zip [1..] myWorkspaces)
    } `additionalKeysP` [ ("M-b"          , sendMessage ToggleStruts              ) -- toggle the status bar gap
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
         , ("M-<Page_Up>"    , spawn "pulseaudio-ctl up"                    ) -- Volume up 5%
         , ("M-<Page_Down>"  , spawn "pulseaudio-ctl down"                  ) -- Volume down 5%
         , ("M-S-m"          , spawn "pulseaudio-ctl mute"                  ) -- Volume down 5%
         , ("M-C-l"          , spawn "xscreensaver-command -lock"           ) -- lock screen

         -- Applications
         , ("<Print>"        , spawn "spectacle"                            ) -- Screenshot
         , ("M-S-s"          , namedScratchpadAction scratchpads "spotify"  ) -- Spawn a scratchpad with spotify
         , ("M-<F12>"        , namedScratchpadAction scratchpads "termite"  ) -- Spawn a scratchpad with termite
         , ("M-w"            , spawn "firefox"                              ) -- launch browser
         , ("M-e"            , spawn "rox"                                  ) -- launch file manager

         -- Exiting
         , ("M-r"            , spawn "xmonad --recompile && xmonad --restart" ) -- restart xmonad
         , ("M-q"            , restart "xmonad" True                        )
         , ("M-C-<Delete>"   , spawn "shutdown -r now"                      ) -- reboot
         , ("M-C-<Insert>"   , spawn "shutdown -h now"                      ) -- poweroff
         , ("M-S-q"          , io  exitSuccess                              ) -- Exit Xmonad
         ]

    where
      role = stringProperty "WM_WINDOW_ROLE"
      disableXscreensaver = "sed -i 's/mode:\\s*off/\\x0/g; s/mode:\\s*one/mode:\\t\\toff/g; s/\\x0/mode:\\t\\tone/g' ~/.xscreensaver"
      scratchpads = [
                      NS "spotify" "spotify" (className =? "Spotify") defaultFloating
                    , NS "termite" "termite --name scratchpad" (appName =? "scratchpad") defaultFloating
                    ]
     
