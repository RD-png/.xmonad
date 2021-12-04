import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmhDesktopsEventHook, ewmhDesktopsLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition (insertPosition, Focus (Newer), Position (Below))

-- Utils
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Layout
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import XMonad.Layout.MultiColumns
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.CycleWS
-- import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.RefocusLast

-- Misc
import System.IO
import qualified XMonad.StackSet as W


-- Main
main :: IO ()
main = do
  -- Xmobar
  -- xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.config/xmonad/xmobarrc"
  xmproc1 <- spawnPipe "polybar -c $XDG_CONFIG_HOME/polybar.config main &> /dev/null &"
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ def
      -- Defaults
      {
        modMask             = wmModKey
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
      , logHook = ewmhDesktopsLogHook
      } `additionalKeys` wmKeys


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
wmBorderFocus = "#c792ea"

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
wmWorkspaces :: [[Char]]
wmWorkspaces = [" web ", " edit ", " dir ", " term ", " disc ", " mu ", " sys ", " vid ", " gfx "]


-- Layout Hook
wmLayoutHook = avoidStruts
  $ mkToggle (single REFLECTX)
  $ mkToggle (NBFULL ?? EOT)
  $ spacingRaw True (Border 0 0 0 0) True (Border 4 4 4 4) True $ gaps [(U,4), (D,4), (R,4), (L,4)]
  $ tiled ||| Grid ||| multiCol [1] 1 0.01 (-0.5)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2

-- Startup Hook
wmStartupHook :: X ()
wmStartupHook = do  
  spawnOnce "feh --bg-scale /root/.config/xmonad/xpm/street.jpg"


-- Keys
wmKeys :: [((KeyMask, KeySym), X ())]
wmKeys =
  [
    ((wmModKey .|. shiftMask, xK_c), spawn "xmonad --recompile")
  , ((wmModKey .|. shiftMask, xK_r), spawn "xmonad --restart")
  -- -- WM Util
  , ((wmModKey, xK_d), spawn dmenu)
  , ((wmModKey, xK_q), kill1)
  , ((wmModKey, xK_f), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  , ((wmModKey .|. shiftMask, xK_f), sendMessage $ MT.Toggle REFLECTX)
  -- WM Programs
  , ((wmModKey, xK_Return), spawn wmTerm)
  , ((wmModKey .|. shiftMask, xK_Return), spawn "pcmanfm")
  , ((wmModKey .|. shiftMask, xK_d), spawn "discord --no-sandbox")
  , ((wmModKey .|. shiftMask, xK_p), spawn "postman --no-sandbox")
  , ((wmModKey .|. shiftMask, xK_e), spawn "emacs")
  , ((wmModKey, xK_w), spawn wmBrowser)
  ]
  -- Single monitor setup
  ++[((wmModKey , k), bindOn
       [ ("", windows $ W.greedyView n), (n, toggleWS)]) | (n, k) <- zip wmWorkspaces ([xK_1..xK_9]++[xK_0])]
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
