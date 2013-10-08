import XMonad
--import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
    [ className =? "Unity-2d-panel" --> doFloat
    ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig {
           manageHook = manageDocks <+> myManageHook
                         <+> manageHook defaultConfig,
           layoutHook = avoidStruts  $  layoutHook defaultConfig,
           logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50
                         }
           , modMask = mod4Mask    -- Rebind Mod to the Windows key
           } --`additionalKeys`
           --[ ((0, xK_F6), lowerVolume 4 >> return ()),
           --  ((0, xK_F7), raiseVolume 4 >> return ())
          -- ]

