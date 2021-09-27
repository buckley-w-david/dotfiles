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
import XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext, shiftToPrev, prevScreen)
import XMonad.Actions.SpawnOn           (manageSpawn)
import XMonad.Actions.DynamicWorkspaces (selectWorkspace, withWorkspace, removeWorkspace, renameWorkspace)
import XMonad.Actions.Warp              (banishScreen, Corner(LowerLeft))

    -- Hooks
import XMonad.Hooks.DynamicLog          (dynamicLogWithPP, ppOutput, ppTitle, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppSep, ppUrgent, ppExtras, ppOrder, xmobarPP, xmobarColor, shorten, wrap)
import XMonad.Hooks.DynamicProperty     (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops        (ewmh, ewmhDesktopsEventHook)
import XMonad.Hooks.ManageHelpers       (isDialog, doCenterFloat, transience', doFullFloat)
import XMonad.Hooks.ManageDocks         (avoidStruts, docks, ToggleStruts(..))

    -- Layout
import XMonad.Layout.NoBorders          (noBorders, smartBorders)
import XMonad.Layout.ResizableTile      (ResizableTall(..))


    -- Prompt
import XMonad.Prompt                    (XPConfig(..), XPPosition(..))

    -- StackSet
import XMonad.StackSet                  (shift, greedyView, integrate', stack, workspace, current)

    -- Utils
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)
import XMonad.Util.Scratchpad           (scratchpadManageHookDefault)
import XMonad.Util.NamedScratchpad      (namedScratchpadAction, defaultFloating, NamedScratchpad(..))

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
--   - https://github.com/thearthur/.xmonad/blob/master/xmonad.hs
--
-- Check out Dynamic Projects: https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-DynamicProjects.html

------------------------------------------------------------------------
-- Dynamic Workspace Functions
------------------------------------------------------------------------
-- Create a new workspace named after the WM_NAME property of the currently focused window, and yield that name
dynamicWorkspace :: X ()
dynamicWorkspace = do
    _ <- Utils.dynamicWorkspaceFromFocused
    return ()

-- Switch to the new workspace
dynamicWorkspaceAndSwitch :: X ()
dynamicWorkspaceAndSwitch = do
    focusedWorkspaceName <- Utils.dynamicWorkspaceFromFocused
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
myTerminal = "/usr/bin/kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . integrate' . stack . workspace . current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawn "~/.xmonad/startup-hook"


------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- TODO: Create dynamicPropertyChange hooks for each of my workspaces so that anything
-- with my CUSTOM_TYPE xprop gets moved to it's correct workspace. This is a measure to help prevent
-- have to list all that various and sundry applications I'd like to move in my xmonad config file
-- makeDynamicPropertyChange
myWorkspaceNames = ["main", "main", "comms", "media", "dev1", "dev2", "dev3" ,"games", "games2"]
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
myManageHook = manageSpawn <+> composeAll [ isDialog                       --> doCenterFloat
                                     -- , isFullscreen   --> doFullFloat
                                     , role      =? "pop-up"             --> doFloat
                                     , title     =? "Hearthstone"        --> doFullFloat
                                     , title     =? "HearthstoneOverlay" --> doFloat
                                     -- , className =? "obsidian"           --> doShift "9:notes"
                                     , title     =? "Hearthstone"        --> doShift "9:games"
                                     , className =? "Steam"              --> doShift "8:games"
                                     , className =? "Gamehub"            --> doShift "8:games"
                                     , className =? "Lutris"             --> doShift "8:games"
                                     , className =? "discord"            --> doShift "3:comms"
                                     , className =? "Element"            --> doShift "3:comms"
                                     , className =? "Ripcord"            --> doShift "3:comms"
                                     , className =? "Thunderbird"        --> doShift "3:comms"
                                     , className =? "mpvIdle"            --> doShift "4:media"
                                     , transience'
                                     , scratchpadManageHookDefault
                                     ]
    where
      role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- EVENTHOOK
------------------------------------------------------------------------
spotifyEventHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> defaultFloating) -- Spotify doesn't set their props before it maps itself, we have to wait for the change dynamically
--elementEventHook = dynamicPropertyChange "WM_CLASS" (className =? "Element" --> defaultFloating) -- Having trouble getting Element to shift on start
gameEventHook = dynamicPropertyChange "CUSTOM_TYPE" (stringProperty "CUSTOM_TYPE" =? "Game" --> doShift "7:games") -- I'd like to not have to explicitly list game stuff, and instead give them a custom xprop that we look for

myEventHooks = handleEventHook def <+> spotifyEventHook <+> gameEventHook <+> ewmhDesktopsEventHook


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
                      NS "spotify" "spotify --force-device-scale-factor=2.0" (className =? "Spotify") defaultFloating
                    , NS "terminal" (myTerminal ++ " --name scratchpad") (appName =? "scratchpad") defaultFloating
                    --, NS "spotify" "firefox --no-remote -P Spotify --class spotify-scratchpad --kiosk https://open.spotify.com" (className =? "spotify-scratchpad") defaultFloating
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
         , ("M-,"            , prevScreen                                   ) -- Switch focus to prev monitor

         , ("M-'", banishScreen LowerLeft                                   ) -- banish mouse

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
         , ("M-o"            , spawn "/home/david/scripts/open"             ) -- Open links based on my app opener (domain based)
         , ("M-."            , spawn "/home/david/scripts/emoji"            ) -- Poor mans emoji keyboard

         -- Password Manager
         , ("M-S-p"          , spawn "pmenu --type"                        ) -- Display password prompt with domain check
         , ("M-C-p"          , spawn "passmenu --type"                     ) -- Display password prompt
         , ("M-M1-p"         , spawn "passmenu"                            ) -- Display password prompt

         -- System Controls
         , ("M-C-d"          , spawn disableXscreensaver                    ) -- disable xscreensaver
         , ("M-C-l"          , spawn "xscreensaver-command -lock"           ) -- lock screen

         -- Media Controls
         , ("<XF86AudioMute>"         , spawn "/home/david/scripts/vol-ctl toggle")
         , ("<XF86AudioRaiseVolume>"  , spawn "/home/david/scripts/vol-ctl up")
         , ("<XF86AudioLowerVolume>"  , spawn "/home/david/scripts/vol-ctl down")
         -- TODO These should probably be more general, be able to handle stuff other than spotify. Not sure how I'll know what to send commands to...
         -- , ("<XF86AudioNext>"         , spawn "/home/david/scripts/spotify/next")
         -- , ("<XF86AudioPrev>"         , spawn "/home/david/scripts/spotify/previous")
         -- , ("<XF86AudioStop>"         , spawn "/home/david/scripts/spotify/stop")
         -- , ("<XF86AudioPlay>"         , spawn "/home/david/scripts/spotify/play-pause")
         --
         -- This is me attempting to be more general with media commands
         , ("<XF86AudioNext>"         , spawn "playerctl next")
         , ("<XF86AudioPrev>"         , spawn "playerctl previous")
         , ("<XF86AudioStop>"         , spawn "playerctl stop")
         , ("<XF86AudioPlay>"         , spawn "playerctl play-pause")

         -- Applications
         , ("<Print>"        , spawn "maim -s | xclip -selection clipboard -t image/png"                            ) -- Screenshot
         , ("M-<Print>"      , spawn "maim -i $((16#$(xwininfo | grep \"Window id\" | awk '{print $4}' | cut -c3-))) ~/Pictures/Screenshots/$(date +%s).png" ) -- Screenshot
         , ("M-C-<Print>"    , spawn "/home/david/scripts/pinta-ss" ) -- Screenshot
         , ("M-S-s"          , namedScratchpadAction myScratchPads "spotify"  ) -- Spawn a scratchpad with spotify
         , ("M-<F12>"        , namedScratchpadAction myScratchPads "terminal"  ) -- Spawn a scratchpad with terminal
         , ("M-w"            , spawn "firefox-developer-edition"                              ) -- launch browser
         , ("M-e"            , spawn "rox"                                  ) -- launch file manager
         , ("M-z"            , spawn "/home/david/scripts/obsidian.sh"      ) -- note-taking app
         , ("M-M1-r"         , spawn "/home/david/scripts/toggle-ruler"    ) -- Weird transparent overlay thing
         , ("M-M1-c"         , spawn "/home/david/scripts/turbo"         ) -- turbo clicker

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
                    , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                    , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                    , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                    , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                    , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                    , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                    , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                    , ppExtras  = [windowCount]                           -- # of windows current workspace
                    , ppOrder  = \(ws:_:t:ex) -> [ws]++ex++[t]
                    }
    , manageHook = myManageHook
    , handleEventHook  = myEventHooks
    , layoutHook  = myLayoutHook
    , workspaces = myWorkspaces
    } `additionalKeysP` myKeys
