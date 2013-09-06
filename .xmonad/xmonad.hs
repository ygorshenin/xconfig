import Data.Char (toLower)
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

data VolumeCommand = Lower | Raise | Toggle
                   deriving (Show)

switchLayout :: MonadIO m => m ()
switchLayout = spawn "~/scripts/layout_switch.sh"

switchVolume :: MonadIO m => VolumeCommand -> m ()
switchVolume cmd = spawn $ "~/scripts/volume_switch.sh" ++ " " ++ arg
  where arg = map toLower $ show cmd

main = do
  xmobar <- spawnPipe "xmobar"
  let layout = onWorkspace "9" (noBorders $ Full) . avoidStruts $
               layoutHook defaultConfig
  xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
                         , layoutHook = layout
                         , logHook = dynamicLogWithPP xmobarPP
                             { ppOutput = hPutStrLn xmobar
                             , ppTitle = xmobarColor "green" "" . shorten 50
                             }
                         , modMask = mod4Mask
                         , terminal = "xterm"
                         } `additionalKeys`
    [ ((controlMask, xK_Shift_L), switchLayout)
    , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_g), spawn "google-chrome")
    , ((0, 0x1008ff11), switchVolume Lower)
    , ((0, 0x1008ff13), switchVolume Raise)
    , ((0, 0x1008ff12), switchVolume Toggle)
    ]
