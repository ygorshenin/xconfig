import Data.Char (toLower)
import System.IO
import Text.Printf
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

data VolumeCommand = Lower | Raise | Toggle
                   deriving (Show)

switchLayout :: MonadIO m => m ()
switchLayout = spawn "~/bin/layout-switch"

switchVolume :: MonadIO m => VolumeCommand -> m ()
switchVolume cmd = spawn $ "~/bin/volume-switch" ++ " " ++ arg
  where arg = map toLower $ show cmd


emacsWorkspace = "0x1:code"
shellWorkspace = "0x2:code"
webWorkspace   = "0x3:web"

codeWorkspaces    = [emacsWorkspace, shellWorkspace]
genericWorkspaces = map (printf "0x%d:generic") ([4 .. 8] :: [Int])
mediaWorkspaces   = ["0x9:media"]
myWorkspaces = concat [codeWorkspaces, [webWorkspace], genericWorkspaces, mediaWorkspaces]

onStartup = do
  setWMName "LG3D"  -- need for correct Java Swing applications work
  spawnOn emacsWorkspace "emacs"
  spawnOn shellWorkspace "xterm"
  spawnOn webWorkspace "google-chrome"

main = do
  xmobar <- spawnPipe "xmobar"
  spawn "setxkbmap -query | awk '/layout:.*/ { print $2 }' >/tmp/.layout"
  spawn "amixer get Master | awk -f ~/bin/amixer_status.awk >/tmp/.volume"
  let layout = onWorkspaces (codeWorkspaces ++ mediaWorkspaces) (noBorders $ Full) $
               avoidStruts .
               smartBorders $ layoutHook def
  xmonad $ def { manageHook  = manageSpawn <+> manageDocks <+> manageHook def
               , workspaces  = myWorkspaces
               , startupHook = onStartup
               , layoutHook  = layout
               , logHook     = dynamicLogWithPP xmobarPP
                               { ppOutput = hPutStrLn xmobar
                               , ppTitle = xmobarColor "green" "" . shorten 50
                               }
               , modMask     = mod4Mask
               , terminal    = "xterm"
               } `additionalKeys`
    [ ((controlMask, xK_Shift_L), switchLayout)
    , ((mod4Mask .|. shiftMask, xK_l), spawn "screenlock")
    , ((mod4Mask .|. shiftMask, xK_g), spawn "google-chrome-stable")
    , ((mod4Mask, xK_Print), spawn "scrot ~/Pictures/screen-%Y-%m-%d-%H-%M-%S.png -d 1")
    , ((mod4Mask .|. controlMask, xK_Print), spawn "scrot ~/Pictures/window-%Y-%m-%d-%H-%M-%S.png -u -d 1")
    , ((0, 0x1008ff11), switchVolume Lower)
    , ((0, 0x1008ff13), switchVolume Raise)
    , ((0, 0x1008ff12), switchVolume Toggle)
    ]
