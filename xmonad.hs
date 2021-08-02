import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition (insertPosition)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Below))

-- Utils
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Layout
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.MultiColumns
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Actions
import XMonad.Actions.CopyWindow (kill1)

-- Misc
import System.IO

-- Main
main :: IO ()
main = do
  -- Xmobar
  xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobarrc"
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ def
      -- Defaults
      { modMask             = wmModKey
      , terminal            = wmTerm
      , focusFollowsMouse   = mouseFocus
      , borderWidth         = wmBorderSize
      , normalBorderColor   = wmBorderNormal
      , focusedBorderColor  = wmBorderFocus
      , workspaces          = wmWorkspaces

      -- Hooks
      , handleEventHook     = fullscreenEventHook
      , manageHook          = insertPosition Below Newer <+> manageDocks <+> manageHook def
      , layoutHook          = wmLayoutHook
      , startupHook         = wmStartupHook
      , logHook = dynamicLogWithPP $ xmobarPP
         { ppOutput = \x -> hPutStrLn xmproc1 x
         , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
         , ppVisible = xmobarColor "#98be65" ""
         , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
         , ppHiddenNoWindows = xmobarColor "#c792ea" ""
         , ppTitle = xmobarColor "#c5c8c6" "" . shorten 60
         , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
         , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
         , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
         }
      } `additionalKeysP` wmKeys


-- Defaults
wmModKey :: KeyMask
wmModKey = mod4Mask

wmTerm :: String
wmTerm = "alacritty"

mouseFocus :: Bool
mouseFocus = False

wmBrowser :: String
wmBrowser = "firefox"


-- Appearance
wmBorderSize :: Dimension
wmBorderSize = 2

wmBorderNormal :: String
wmBorderNormal = "#5a6478"

wmBorderFocus :: String
wmBorderFocus = "#ff6c6b"

wmFont :: String
wmFont = "xft:Source Code Pro:regular:size=10:antialias=true:hinting=true"


--- UI Colours
bgColor :: String
bgColor = "#1d1f21"

fgColor :: String
fgColor = "#c5c8c6"

layoutColor :: String
layoutColor = "#AA3355"


-- Workspaces
wmWorkspaces = [" web ", " edit ", " dir ", " term ", " disc ", " mu ", " sys ", " vid ", " gfx "]
  where
    ws l =
      [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ ws ++ "  ^ca()"
      | (i, ws) <- zip [1 ..] l
      , let n = i
      ]


-- Layout Hook
wmLayoutHook = avoidStruts
  $ mkToggle (single REFLECTX)
  $ mkToggle (NBFULL ?? EOT)
  $ spacingRaw True (Border 0 0 0 0) True (Border 0 0 4 4) True $ gaps [(U,8), (D,8), (R,4), (L,4)]
  $ tiled ||| Grid ||| multiCol [1] 1 0.01 (-0.5)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2


-- Startup Hook
wmStartupHook = do
  spawnOnce "xrandr --output DisplayPort-0 --mode 1920x1080 --rate 144.00"
  spawnOnce "pulseaudio -D"


-- Keys
wmKeys :: [(String, X ())]
wmKeys =
  [ ("M-S-c", spawn "xmonad --recompile")
  , ("M-S-r", spawn "xmonad --restart")
  -- WM Util
  , ("M-d", spawn dmenu)
  , ("M-q", kill1)
  , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-S-f", sendMessage $ MT.Toggle REFLECTX)
  -- WM Programs
  , ("M-<Return>", spawn wmTerm)
  , ("M-S-<Return>", spawn "pcmanfm")
  , ("M-S-d", spawn "discord --no-sandbox")
  , ("M-S-e", spawn "emacs")
  , ("M-w", spawn wmBrowser)
  ]
  where
    fn = "Misc Termsyn.Icons:size=12.8"
    dmenu =
      "dmenu_run -i \
      \ -fn '"
        ++ fn
        ++ "' \
           \ -nf '"
        ++ fgColor
        ++ "' \
           \ -sf '"
        ++ fgColor
        ++ "' \
           \ -nb '"
        ++ bgColor
        ++ "' \
           \ -sb '"
        ++ layoutColor
        ++ "'"
