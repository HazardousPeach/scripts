import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Layout.NoBorders
import XMonad.StackSet as W
import System.IO
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import Graphics.X11.Xinerama (getScreenInfo)
import Data.IORef

myWorkspaces = ["edit", "web", "term", "read", "chat"] ++ map show [6..9]
myManageHook = composeAll . concat $
               [ [ className =? c --> doFloat | c <- cFloats ]
               , [ title =? t --> doFloat | t <- tFloats ]
               , [ role =? r --> doFloat | r <- rFloats]
               , [ isFullscreen --> doFullFloat ]
               , [ className =? "Evince" -->doShift "read"]
               , [ className =? "HipChat" -->doShift "chat"]
               , [ className =? "ScudCloud" -->doShift "chat"]
               , [ className =? "Firefox" -->doShift "web"]]
  where cFloats = ["NES", "tilda"]
        tFloats = ["Firefox Preferences", "Downloads", "Add-ons", "Rename",
                   "Create", "tilda", "Emacs Anywhere"]
        rFloats = ["gimp-toolbox", "gimp-dock"]
        role = stringProperty "WM_WINDOW_ROLE"

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where h = 0.2
        w = 1
        t = 1 - h
        l = 1 - w
xdisplays :: X [Rectangle]
xdisplays = withDisplay $ io . getScreenInfo
main = do
     xmprocsRef <- (newIORef []) :: IO (IORef [Handle])
     -- xmproc <- spawnPipe "/usr/bin/xmobar"
     xmonad $ def
                    { borderWidth = 2
                    , terminal  = myTerminal
                    , XMonad.workspaces = myWorkspaces
                    , normalBorderColor = "#cccccc"
                    , focusedBorderColor = "#112255"
                    , focusFollowsMouse = False
                    , manageHook = myManageHook <+> manageDocks <+> manageScratchPad <+> manageHook def
                    , layoutHook = avoidStruts $ smartBorders $ layoutHook def
                    , handleEventHook = docksEventHook <+> fullscreenEventHook <+> handleEventHook def
                    , startupHook = do
                        n <- length <$> xdisplays
                        io $ writeIORef xmprocsRef =<< (sequence $ (replicate n (spawnPipe "/usr/bin/xmobar") :: [IO Handle]))

                    , logHook = dynamicLogWithPP xmobarPP
                      { ppOutput = \x ->
                          do
                            xmprocs <- readIORef xmprocsRef
                            fmap (const ()) $ sequence
                              $ fmap (flip hPutStrLn x) xmprocs
                      , ppTitle = xmobarColor "green" "" . shorten 50
                      , ppLayout = const "" -- to disable the layout info on xmobar
                      }
                    , modMask = mod4Mask}
                    `additionalKeysP`
                    [ ("M-e", spawn "emacsclient -c -a \"\"")
                    , ("M-i", spawn "$HOME/.emacs_anywhere/bin/run")
                    , ("M-f", spawn "tor-browser")
                    , ("M-c", spawn "hipchat")
                    , ("M-d", spawn "xrandr --output eDP1 --off && xrandr --output eDP1 --mode 1920x1080 && xrandr --output DP1 --off && xrandr --output HDMI1 --off && xmonad --restart")
                    , ("M-b", spawn "next-background")
                    , ("M-s", sendMessage ToggleStruts)
                    , ("M-<Space>", scratchPad)
                    , ("XF86AudioRaiseVolume", spawn "amixer -q sset Master 10%+")
                    , ("XF86AudioLowerVolume", spawn "amixer -q sset Master 10%-")
                    , ("M-S-<Space>", sendMessage NextLayout)]
   where
     myTerminal = "urxvt"
     scratchPad = scratchpadSpawnActionTerminal "urxvt"
