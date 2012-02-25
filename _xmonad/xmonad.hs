import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Config.Xfce
import XMonad.Actions.CopyWindow
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Hooks.FadeInactive

main = do
	xmonad $ xfceConfig
		{ modMask = mod4Mask
		, normalBorderColor = "#000"
		, focusedBorderColor = "#76CCC8"
		-- , logHook = myLogHook
		, manageHook = myManageHook 
		, layoutHook = smartBorders (myLayoutHook) -- Don't put borders on fullFloatWindows
		} `additionalKeys` [
		((mod4Mask, xK_a), windows copyToAll)
		,((mod4Mask .|. shiftMask, xK_a),  killAllOtherCopies)
		]
myLayoutHook = spacing 3 $ layoutHook xfceConfig
-- myLayoutHook = layoutHook xfceConfig

-- manageScratchPad :: ManageHook
-- manageScratchPad = scratchpadManageHook (W.RationalRect 0.4 0.5 0.6 0.3)
myLogHook = fadeInactiveLogHook 0xcccccccc

myManageHook = composeAll
-- Allows focusing other monitors without killing the fullscreen
 -- [ isFullscreen --> (doF W.focusDown <+> doFullFloat)]
 
-- Single monitor setups, or if the previous hook doesn't work
    [ isFullscreen --> doFullFloat
    -- skipped
	, className =? "Xfce4-appfinder"  --> doFloat
	, className =? "Xfce4-mixer"  --> doFloat
    , className =? "Xfrun4"           --> doFloat
    , resource =? "eog"           --> doFullFloat
    , resource =? "Xfce4-notfiyd"           --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    -- , resource  =? "sun-awt-X11-XWindowPeer"       --> doIgnore
	, title =? "Desktop" --> doIgnore
	, title =? "xfce4-panel" --> doIgnore
    ]-- , layoutHook = noBorders
