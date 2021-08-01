import Control.Monad
import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

-- Utils
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Scratchpad

-- Layout
import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Square
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.TwoPane

-- Actions
import XMonad.Actions.CopyWindow (kill1)

-- Misc
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Directory
import System.Exit ()
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import qualified XMonad.StackSet as W

-- Main
main :: IO ()
main = do
  -- Xmobar
  xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobarrc"
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ def
      -- Simple stuff
      { modMask             = modm
      , terminal            = term
      , focusFollowsMouse   = mouseFocus
      , borderWidth         = bdrSize
      , normalBorderColor   = bdrNormal
      , focusedBorderColor  = bdrFocus
      , workspaces          = workspaces'

      -- Lets hook up
      , handleEventHook     = eventHook
      , layoutHook          = layoutHook'
      -- , manageHook          = manageHook'
      , startupHook         = startupHook'
      , logHook = dynamicLogWithPP $ xmobarPP
         { ppOutput = \x -> hPutStrLn xmproc1 x
         , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
         , ppVisible = xmobarColor "#98be65" ""
         , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
         , ppHiddenNoWindows = xmobarColor "#c792ea" ""
         , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
         , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
         , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
         -- , ppExtras  = [windowCount]
         , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
         }
      } `additionalKeysP` myKeys


---- Simple stuff
modm          = mod4Mask
term          = "alacritty"
mouseFocus    = False
workspaces'   = myWorkspaces
keyboard      = myKeys
browser       = "firefox"


----- Appearance
bdrSize       = 0
bdrNormal     = fgColor
bdrFocus      = bgColor
font          = "xft:Source Code Pro:regular:size=10:antialias=true:hinting=true"
monitorSize   = 1980
monitor n     = show(round(monitorSize * n))
monitor' n    = round(monitorSize * n)


----- WHAT COLOR?
bgColor       = "#B79288"
fgColor       = "#F2EBEA"
layoutColor   = "#AA3355"
wsBgColor     = "#f1f3f4"
wsFgColor     = bgColor
barBgColor    = bgColor
barFgColor    = fgColor
hintColor     = layoutColor

---- Hooks
eventHook     = fullscreenEventHook
layoutHook'   = myLayoutHook
-- logHook'      = myLogHook
startupHook'  = myStartupHook


-- Workspaces
myWorkspaces = [" web ", " edit ", " dir ", " term ", " disc ", " mu ", " sys ", " vid ", " gfx "]
  where
    ws l =
      [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ ws ++ "  ^ca()"
      | (i, ws) <- zip [1 ..] l
      , let n = i
      ]


-- Layout Hook
myLayoutHook =
  avoidStruts
  $ mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ onWorkspace (w !! 0) termLayout
  $ onWorkspace (w !! 1) webLayout
  $ standardLayout
  where
    w = workspaces'
    termLayout =
      gaps [(L,1), (U,1), (R,1), (D,1)] $
      standardLayout
    webLayout = Tall (1) (3/100) (1/2)
    standardLayout =
      smartBorders $
      renamed [CutWordsLeft 1] $
      smartSpacingWithEdge 8 $ layoutHook defaultConfig


-- Scratchpad
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.3
    w = 1
    t = 1 - h
    l = 1 - w


-- Startup Hook
myStartupHook = do
  spawnOnce "xrandr --output DisplayPort-0 --mode 1920x1080 --rate 144.00"
  spawnOnce "pulseaudio -D"


-- Keys
myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-c", spawn "xmonad --recompile")
  , ("M-S-r", spawn "xmonad --restart")
  -- WM Util Binds
  , ("M-d", spawn dmenu)
  , ("M-q", kill1)
  , ("M-f", sendMessage $ Toggle FULL)
  -- WM Programs
  , ("M-<Return>", spawn term)
  , ("M-S-<Return>", spawn "pcmanfm")
  , ("M-S-d", spawn "discord --no-sandbox")
  , ("M-S-e", spawn "emacs")
  , ("M-w", spawn browser)
  ]
  where
    fn = "Misc Termsyn.Icons:size=12"
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
