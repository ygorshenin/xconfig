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

workWorkspaces    = ["0x1:code", "0x2:code", "0x3:web"]
genericWorkspaces = map (\d -> "0x" ++ (show d) ++ ":generic") [4..8]
mediaWorkspaces   = ["0x9:media"]
myWorkspaces = concat [workWorkspaces, genericWorkspaces, mediaWorkspaces]

main = do
  xmobar <- spawnPipe "xmobar"
  let layout = onWorkspaces mediaWorkspaces (noBorders $ Full) $
               avoidStruts .
               smartBorders $ layoutHook defaultConfig
  xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
                         , workspaces = myWorkspaces
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
    , ((mod4Mask .|. shiftMask, xK_p), spawn "palimpsest")
    , ((0, 0x1008ff11), switchVolume Lower)
    , ((0, 0x1008ff13), switchVolume Raise)
    , ((0, 0x1008ff12), switchVolume Toggle)
    ]
