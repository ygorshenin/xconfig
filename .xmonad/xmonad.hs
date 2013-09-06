import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

main = do
  xmobar <- spawnPipe "xmobar"
  xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
                         , layoutHook = avoidStruts $ layoutHook defaultConfig
                         , logHook = dynamicLogWithPP xmobarPP
                                         { ppOutput = hPutStrLn xmobar
                                         , ppTitle = xmobarColor "green" "" . shorten 50
                                         }
                         , modMask = mod4Mask
                         , terminal = "xterm"
                         } `additionalKeys`
    [ ((controlMask, xK_Shift_L),
      spawn "~/layout_switch.sh")
    , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]
