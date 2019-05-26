import System.IO
import System.Exit (exitSuccess)

import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Minimize
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

main = do
    xmproc <- spawnPipe "xmobar /home/david/.xmobarrc"
    xmonad $ docks $ ewmh def {
      modMask           = mod4Mask -- use the Windows button as mod
    , terminal          = "termite"
    , logHook           = dynamicLogWithPP xmobarPP 
                                  { ppOutput = hPutStrLn xmproc 
                                  , ppTitle = xmobarColor "green" "" . shorten 50}
    , manageHook        = manageDocks <+> mconcat [ manageHook def
                                  , isFullscreen                  --> doFullFloat
                                  , className =? "MPlayer"         --> doFloat
                                  , className =? "mplayer2"        --> doFloat
                                  , className =? "mpv"             --> doFloat
                                  , className =? "Gimp"            --> doFloat
                                  , className =? "Vlc"             --> doFloat
                                  , className =? "plasma-desktop"  --> doFloat
                                  , className =? "plasmashell"     --> doFloat
                                  , className =? "zoom"            --> doFloat
                                  , role      =? "pop-up"          --> doFloat
                                  ]
    , handleEventHook   = handleEventHook def <+> fullscreenEventHook
    , layoutHook  = avoidStruts $ layoutHook defaultConfig
    , startupHook = ewmhDesktopsStartup
    } `additionalKeysP` [ ("M-b"          , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M-<Tab>"      , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab  ) -- classic alt-tab behaviour
         , ("M-<Right>"    , nextWS                                       ) -- go to next workspace
         , ("M-<Left>"     , prevWS                                       ) -- go to prev workspace
         , ("M-S-<Right>"  , shiftToNext                                  ) -- move client to next workspace
         , ("M-S-<Left>"   , shiftToPrev                                  ) -- move client to prev workspace
         , ("M-p"          , spawn "dmenu_run"                            ) -- app launcher
         , ("M-r"          , spawn "xmonad --recompile; xmonad --restart" ) -- restart xmonad
         , ("M-w"          , spawn "firefox"                              ) -- launch browser
         , ("M-e"          , spawn "dolphin"                              ) -- launch file manager
         , ("C-M-l"        , spawn "xscreensaver-command -lock"           ) -- lock screen
         , ("C-M-<Delete>" , spawn "shutdown -r now"                      ) -- reboot
         , ("C-M-<Insert>" , spawn "shutdown -h now"                      ) -- poweroff
         , ("M-S-q"        , io  exitSuccess                              )
         , ("M-q"          , restart "xmonad" True                        )
         ]

    where
      role = stringProperty "WM_WINDOW_ROLE"
