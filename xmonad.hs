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
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

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
                                  , className =? "Steam"	       --> doShift "7:games"
                                  , className =? "Lutris"          --> doShift "7:games"
                                  , className =? "discord"	       --> doShift "2:chat"
                                  , scratchpadManageHookDefault
                                  ]
    , handleEventHook   = handleEventHook def <+> fullscreenEventHook
    , layoutHook  = avoidStruts $ layoutHook defaultConfig
    , startupHook = ewmhDesktopsStartup
    , workspaces = ["1:main", "2:chat", "3", "4", "5", "6" ,"7:games", "8", "9","10"] 
    } `additionalKeysP` [ ("M-b"          , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M-<Tab>"       , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab  ) -- classic alt-tab behaviour
         , ("M-<Right>"     , nextWS                                       ) -- go to next workspace
         , ("M-<Left>"      , prevWS                                       ) -- go to prev workspace
         , ("M-S-<Right>"   , shiftToNext                                  ) -- move client to next workspace
         , ("M-S-<Left>"    , shiftToPrev                                  ) -- move client to prev workspace
         , ("M-<Page_Up>"   , spawn "pulseaudio-ctl up"                    ) -- Volume up 5%
         , ("M-<Page_Down>" , spawn "pulseaudio-ctl down"                  ) -- Volume down 5%
         , ("M-S-m"         , spawn "pulseaudio-ctl mute"                  ) -- Volume down 5%
         , ("<Print>"       , spawn "spectacle"                            ) -- Screenshot
         , ("M-S-s"         , namedScratchpadAction scratchpads "spotify"  ) -- Spawn a scratchpad with spotify
         , ("M-<F12>"       , namedScratchpadAction scratchpads "termite"  ) -- Spawn a scratchpad with termite
         , ("M-p"           , spawn "dmenu_run"                            ) -- app launcher
         , ("M-r"           , spawn "xmonad --recompile; xmonad --restart" ) -- restart xmonad
         , ("M-w"           , spawn "firefox"                              ) -- launch browser
         , ("M-e"           , spawn "dolphin"                              ) -- launch file manager
         , ("C-M-l"         , spawn "xscreensaver-command -lock"           ) -- lock screen
         , ("C-M-d"         , spawn disableXscreensaver                    ) -- disable xscreensaver
         , ("C-M-<Delete>"  , spawn "shutdown -r now"                      ) -- reboot
         , ("C-M-<Insert>"  , spawn "shutdown -h now"                      ) -- poweroff
         , ("M-S-q"         , io  exitSuccess                              )
         , ("M-q"           , restart "xmonad" True                        )
         ]

    where
      role = stringProperty "WM_WINDOW_ROLE"
      disableXscreensaver = "sed -i 's/mode:\\s*off/\\x0/g; s/mode:\\s*one/mode:\\t\\toff/g; s/\\x0/mode:\\t\\tone/g' ~/.xscreensaver"
      scratchpads = [
                      NS "spotify" "D_PRELOAD=LD_PRELOAD=libcrypto-compat.so.1.0.0:libssl-compat.so.1.0.0:libcurl-openssl-1.0.so:/usr/lib/spotifywm.so /usr/local/bin/spotify" (className =? "Spotify") defaultFloating
                    , NS "termite" "termite --name scratchpad" (resource =? "scratchpad") defaultFloating
                    ]
