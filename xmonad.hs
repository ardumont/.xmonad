import           Control.Monad              ()
import           Data.List                  (sort)
import qualified Data.Map                   as M
import           Data.Monoid
import           System.Directory           (getHomeDirectory)
import           System.Exit
import           System.FilePath            (combine)
import           System.IO
import           XMonad
import           XMonad.Actions.Promote     (promote)
import qualified XMonad.Actions.Search      as S
import qualified XMonad.Actions.Submap      as SM
import           XMonad.Actions.WindowGo    (runOrRaiseNext)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Layout.Monitor
import           XMonad.Layout.NoBorders    (smartBorders)
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher  (launchApp)
import           XMonad.Prompt.Pass         (passPrompt)
import           XMonad.Prompt.RunOrRaise   (runOrRaisePrompt)
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad       (xmonadPromptC)
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run            (spawnPipe)

-- | The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "urxvt"

myTerminalQuery :: Query Bool
myTerminalQuery = className =? "URxvt"

myPdfReader :: String
myPdfReader = "zathura"

myPdfReaderQuery :: String -> Query Bool
myPdfReaderQuery "evince" = className =? "Evince" <||> className =? ".evince-wrapped"
myPdfReaderQuery "apvlv" = appName =? "apvlv" <&&> className =? "Apvlv"
myPdfReaderQuery "zathura" = appName =? "zathura" <&&> className =? "Zathura"
myPdfReaderQuery _ = error "Undefined"

-- | My preferential browser
--
myBrowser :: String
myBrowser = "qutebrowser"

firefoxQuery :: Query Bool
firefoxQuery = appName =? "Navigator" <&&> (className =? "Firefox" <||>
                                            className =? "Tor Browser" <||>
                                            className =? "Iceweasel" <||>
                                            className =? "Firefox-esr")

myBrowserQuery :: Query Bool
myBrowserQuery = qutebrowserQuery

-- | VM query
--
vmQuery :: Query Bool
vmQuery = (appName =? "qemu-system-x86_64" <&&> className =? "qemu-system-x86_64") <||>
          (appName =? "Qt-subapplication" <&&> className =? "VirtualBox") <||>
          className =? ".emulator64-arm-wrapped"  <||>
          className =? "Android SDK Manager"

-- | My preferential emacs
--
myEmacsQuery :: Query Bool
myEmacsQuery = (appName =? "emacs" <||> appName =? "_emacs-wrapped") <&&> className =? "Emacs"

-- | Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- | Whether a mouse click select the focus or is just passed to the window
--
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- | Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth  = 2

-- | modMask lets you specify which modkey you want to use. mod4mask
-- is window key. I'm used to prefix key because of emacs, stumpwm,
-- xmonad-customed (here), conkeror, firefox with keysnail (rip),
-- qutebrowser-customed
--
myModMask :: KeyMask
myModMask = mod4Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- | Prefix key
--
prefixKey :: String
prefixKey = "C-;"

-- | A utility function to compute the final binding with the prefix key
--
prefix :: String -> String
prefix = ((prefixKey ++ " ") ++)

-- | Execute a command and show its result in a zenity window dialog
--
spawnZenityCmd :: String -> String -> X ()
spawnZenityCmd home =
  let zenityCmd :: String -> String
      zenityCmd cmd = zenity ++ " --info --text \"$(" ++ cmd ++ ")\""
        where zenity = nixProfilePath home ("zenity" :: String)
  in spawn . zenityCmd

-- | Complete the binary path for a nix profile
--
nixProfilePath :: String -> String -> String
nixProfilePath home cmd = combine home program
  where nixProfilePath = ".nix-profile/bin/"::String
        program = combine nixProfilePath cmd

-- | Run or raise with a default config folder from which finding the command
--
myRunOrRaise :: String -> String -> Query Bool -> X ()
myRunOrRaise home cmd = runOrRaiseNext $ nixProfilePath home cmd

-- | Spawn command adding the extra nix profile path needed
--
mySpawn :: String -> String -> X ()
mySpawn home cmd = spawn $ nixProfilePath home cmd

libreOfficeQuery :: Query Bool
libreOfficeQuery = (appName =? "libreofficedev" <||> appName =? "libreoffice") <&&>
                   (className =? "libreofficedev-writer" <||>
                    className =? "libreoffice-writer" <||>
                    className =? "libreofficedev-calc" <||>
                    className =? "libreoffice-calc" <||>
                    className =? "libreofficedev-draw" <||>
                    className =? "libreoffice-draw" <||>
                    className =? "libreoffice-startcenter")

conkerorQuery :: Query Bool
conkerorQuery = appName =? "Navigator" <&&> className =? "Conkeror"

qutebrowserQuery :: Query Bool
qutebrowserQuery = (appName =? "qutebrowser" <||> appName =? ".qutebrowser-wrapped") <&&> className =? "qutebrowser"

vlcQuery :: Query Bool
vlcQuery = appName =? "vlc" <&&> (className =? "vlc" <||> className =? "Vlc")

skypeQuery :: Query Bool
skypeQuery = appName =? "skype" <&&> className =? "Skype"

xephyrQuery :: Query Bool
xephyrQuery = appName =? ".Xephyr-wrapped" <&&> className =? "Xephyr"

-- | My keymap as (prefix keybindings, command description, command)
--
myKeymapWithDescription :: String -> XConfig Layout -> [(String, String, X ())]
myKeymapWithDescription home conf@(XConfig { terminal   = myTerm
                                           , layoutHook = myLayoutHook
                                           , workspaces = myWss}) =
  [ (prefix "C-g"       , "abort"                      , mySpawn home "xdotool key Escape")
  , (prefix "M1-c"      , "mouse-click-at-point"       , mySpawn home "xdotool click 1")
  , (prefix "M1-d"      , "xdotool-prompt"             , launchApp myXPConfig "xdotool")
  , (prefix "e"         , "emacs"                      , myRunOrRaise home "emacsclient --create-frame" myEmacsQuery)
  , (prefix "S-c"       , "lighttable"                 , myRunOrRaise home "light"                         (appName =? "lighttable" <&&> className =? "LightTable"))
  , (prefix "C-r"       , "simplescreenrecorder"       , myRunOrRaise home "simplescreenrecorder"          (appName =? "simplescreenrecorder" <&&> className =? "SimpleScreenRecorder"))
  , (prefix "M1-S-x"    , "mcomix"                     , myRunOrRaise home "mcomix"                        (appName =? "mcomix" <&&> className =? "MComix"))
  , (prefix prefixKey   , "promote"                    , promote)  -- window manipulation
  , (prefix "x"         , "terminal"                   , myRunOrRaise home myTerm                  myTerminalQuery)
  , (prefix "C-x"       , "xterm"                      , myRunOrRaise home "xterm"                 (appName =? "xterm" <&&> className =? "XTerm"))
  , (prefix "S-s"       , "sweethome-3d"               , myRunOrRaise home "sweethome3d"           (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-eteks-sweethome3d-SweetHome3D"))
  , (prefix "S-s"       , "signal"                     , myRunOrRaise home "signal-desktop"        (appName =? "signal" <&&> className =? "Signal"))
  , (prefix "S-t"       , "vlc"                        , myRunOrRaise home "vlc"                   vlcQuery)
  , (prefix "C-e"       , "pdf-reader"                 , myRunOrRaise home myPdfReader             (myPdfReaderQuery myPdfReader))
  , (prefix "C-i"       , "image-viewer"               , myRunOrRaise home "feh"                   (appName =? "feh" <&&> className =? "feh"))
  , (prefix "d"         , "pinta"                      , myRunOrRaise home "pinta"                 (appName =? "Pinta" <&&> className =? "Pinta"))
  , (prefix "C-a"       , "music-reader"               , myRunOrRaise home "audacious"             (appName =? "audacious" <&&> className =? "Audacious"))
  , (prefix "S-g"       , "gparted"                    , runOrRaiseNext "sudo gparted"             (appName =? "gpartedbin" <&&> className =? "GParted"))   -- expect this installed as main system
  , (prefix "C-S-x"     , "xosview"                    , myRunOrRaise home "xosview2"              (appName =? "xosview" <&&> className =? "XOsview2"))
  , (prefix "C-S-g"     , "dia"                        , myRunOrRaise home "dia"                   (appName =? "dia-normal" <&&> className =? "Dia-Normal"))
  -- , (prefix "b"         , "conkeror"                   , myRunOrRaise "conkeror"                   conkerorQuery)
  , (prefix "b"         , "qutebrowser"                , myRunOrRaise home "qutebrowser"           qutebrowserQuery)
  , (prefix "M1-t"      , "tuxguitar"                  , myRunOrRaise home "tuxguitar"             (appName =? "TuxGuitar" <&&> className =? "TuxGuitar"))
  , (prefix "o"         , "libre-office"               , myRunOrRaise home "libreoffice"           libreOfficeQuery)
  , (prefix "f"         , "browser"                    , myRunOrRaise home myBrowser               myBrowserQuery)
  , (prefix "C-S-e"     , "env"                        , spawnZenityCmd home "env")
  , (prefix "a"         , "date"                       , spawnZenityCmd home "date")
  , (prefix "S-k"       , "ssh-add-l"                  , spawnZenityCmd home "ssh-add -l")
  , (prefix "S-e"       , "cat-etc-environment"        , spawnZenityCmd home "cat /etc/environment")
  , (prefix "S-h"       , "cat-etc-hosts"              , spawnZenityCmd home "cat /etc/hosts")
  , (prefix "C-S-i"     , "sbin-ifconfig"              , spawnZenityCmd home "ifconfig")
  , (prefix "S-b"       , "acpi"                       , spawnZenityCmd home "acpi -b")
  , (prefix "^"         , "top"                        , spawnZenityCmd home "top -b -n 1 -c -d 1")
  , (prefix "C-s"       , "print-screen"               , spawn "screenshot=\"$HOME/Pictures/screenshots/$(date +%F_%H-%M-%S).png\" ; scrot -u $screenshot; notify-send -t 1000 \"$(basename $screenshot) done!\"")
  , (prefix "M1-s"      , "mouse-print-screen"         , spawn "~/bin/touchpad/toggle-touchpad-manual.sh 1; screenshot=\"$HOME/Pictures/screenshots/$(date +%F_%H-%M-%S).png\" ; scrot -s $screenshot; notify-send -t 1000 \"$(basename $screenshot) done!\"")
  , (prefix "C-t"       , "toggle-touchpad"            , spawn "~/bin/touchpad/toggle-touchpad.sh")
  , (prefix "C-S-s"     , "suspend"                    , spawn "systemctl suspend")
  , (prefix "C-S-h"     , "hibernate"                  , spawn "systemctl hibernate")
  , (prefix "C-b"       , "brightness-decrease"        , mySpawn home "xbacklight -dec 10")
  , (prefix "C-f"       , "brightness-increase"        , mySpawn home "xbacklight -inc 10")
  , (prefix "C-S-m"     , "brightness-half"            , mySpawn home "xbacklight -set 50")
  , (prefix "S-m"       , "brightness-max"             , mySpawn home "xbacklight -set 100")
  , (prefix "M1-f"      , "sound-increase"             , mySpawn home "amixer set Master 5%+")
  , (prefix "M1-b"      , "sound-decrease"             , mySpawn home "amixer set Master 5%-")
  , (prefix "M1-m"      , "sound-toggle"               , mySpawn home "amixer set Master toggle")
  , (prefix "M1-e"      , "pdf-reader-prompt"          , launchApp myXPConfig myPdfReader)
  , (prefix "s"         , "search-url"                 , search)
  , (prefix "r"         , "exec"                       , runOrRaisePrompt myXPConfig)
  , (prefix "g"         , "goto"                       , windowPromptGoto myXPConfig)
  , (prefix "M1-x"      , "meta-x"                     , xmonadPromptC keymapDescription myXPConfig)
  , (prefix "p"         , "pass-read"                  , passPrompt myXPConfig)
  , (prefix "c"         , "close-current-window"       , kill >> spawn "notify-send -t 1000 'window closed!'")
  , (prefix "<Space>"   , "rotate-layout"              , sendMessage NextLayout)
  , (prefix "C-<Space>" , "reset-layout"               , setLayout myLayoutHook)
  , (prefix "M1-r"      , "refresh"                    , refresh)
  , (prefix "M1-n"      , "window-goto"                , windowPromptGoto def)
  , (prefix "C-M1-b"    , "banish-mouse"               , spawn "~/bin/mouse/banish-mouse.sh")
  , (prefix "<Tab>"     , "window-move-focus-next"     , windows W.focusDown)
  , (prefix "j"         , "window-move-focus-next"     , windows W.focusDown)
  , (prefix "k"         , "window-move-focus-previous" , windows W.focusUp)
  , (prefix "m"         , "window-move-focus-master"   , windows W.focusMaster)
  , (prefix "<Return>"  , "window-swap-focus-master"   , windows W.swapMaster)
  , (prefix "C-j"       , "window-swap-focus-next"     , windows W.swapDown)
  , (prefix "C-k"       , "window-swap-focus-previous" , windows W.swapUp)
  , (prefix "M1-l"      , "master-shrink-area"         , sendMessage Shrink)
  , (prefix "M1-h"      , "master-expand-area"         , sendMessage Expand)
  , (prefix "t"         , "window-push-back-tiling"    , withFocused $ windows . W.sink)
  , (prefix "h"         , "window-inc-num-master-area" , sendMessage (IncMasterN 1))
  , (prefix "l"         , "window-dec-num-master-area" , sendMessage (IncMasterN (-1)))
  , (prefix "C-S-q"     , "xmonad-recompile"           , spawn "~/bin/xmonad-action recompile")
  , (prefix "S-q"       , "xmonad-restart"             , spawn "~/bin/xmonad-action restart")
  , (prefix "M1-q"      , "xmonad-quit"                , io exitSuccess)] ++
  -- M1-n   - Switch to workspace with id n
  -- S-n    - Move the client to workspace with id n
  [(prefix $ pk ++ k, desc ++ k , windows $ f i) | (i, k) <- zip myWss $ map show [1 .. length myWss]
                                                 , (f, pk, desc) <- [ (W.view  , "M1-", "workspace-keep-screen-switch-to-id-")
                                                                    , (W.shift, "S-" , "workspace-move-client-to-id-")]] ++
    -- S-M1-n - Switch to physical/Xinerama screen n
    -- C-M1-n - Move client to screen 1, 2, or 3
    --
  [(prefix $ pk ++ show screenNumber, metaXAction ++ show screenNumber, screenWorkspace screenNumber >>= flip whenJust (windows . fnAction))
    | screenNumber <- [1,0],
      (fnAction, pk, metaXAction) <- [ (W.view,  "S-M1-", "switch-to-physical-screen-")
                                    , (W.shift, "C-M1-", "move-client-to-screen-")]]
  where -- Permits the search through the system browser
        searchSite = S.promptSearchBrowser myXPConfig myBrowser
        search     = SM.submap . mkKeymap conf $
                     [ ("S-g", searchSite S.google)
                     , ("h"  , searchSite S.hoogle)
                     , ("S-h", searchSite S.hackage)
                     , ("a"  , searchSite S.amazon)
                     , ("i"  , searchSite S.imdb)
                     , ("S-i", searchSite S.images)
                     , ("d"  , searchSite S.deb)
                     , ("y"  , searchSite S.youtube)
                     , ("w"  , searchSite S.wikipedia)
                     , ("d"  , searchSite S.duckduckgo)
                     , ("s"  , searchSite S.stackage)
                     , ("S-w", searchSite S.wayback)
                     , ("g"  , searchSite $ S.searchEngine "github" "search?utf8=✓&q=")]
        -- Rework the keymap description to extract the command description and the associated actions
        keymapDescription = map (\ (keybinding, xmonadActionDesc, xmonadAction) -> (xmonadActionDesc ++ " - " ++ keybinding, xmonadAction)) fullKeymap
        fullKeymap = myKeymapWithDescription home conf

-- | key bindings
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf = mkKeymap conf keymap
                   where keymap = map (\ (keybinding, _, xmonadAction) -> (keybinding, xmonadAction)) fullKeymap
                         fullKeymap = myKeymapWithDescription home conf

-- | Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)]
    -- you may also bind events to the mouse scroll wheel (button4 and button5)

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- | My own layout
--
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

------------------------------------------------------------------------
-- Workspaces:

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

workspaceEmacs, workspaceTerminal, workspaceWeb, workspaceCode, workspaceIrc, workspaceIde, workspaceFloat, workspaceBooks, workspaceDb , workspaceVM, workspaceDevVM, workspaceRemote :: String
workspaceEmacs    = "01:emacs"
workspaceTerminal = "02:terminal"
workspaceWeb      = "03:web"
workspaceCode     = "04:code"
workspaceIrc      = "05:irc"
workspaceDb       = "06:db"
workspaceFloat    = "07:ide"
workspaceVM       = "08:vm"
workspaceDevVM    = "09:dev-vm"
workspaceBooks    = "10:books"
workspaceIde      = "11:ide"
workspaceRemote   = "12:remote"

myWorkspaces :: [String]
myWorkspaces = sort [ workspaceEmacs
                    , workspaceTerminal
                    , workspaceWeb
                    , workspaceCode
                    , workspaceIrc
                    , workspaceVM
                    , workspaceDevVM
                    , workspaceIde
                    , workspaceFloat
                    , workspaceBooks
                    , workspaceDb
                    , workspaceRemote
                    ]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'appName' (`resource`) are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ manageDocks
    , isFullscreen                                --> doFullFloat
    -- , vlcQuery                                    --> doShift workspaceFloat >> doFloat
    , className =? "Gimp"                         --> doShift workspaceFloat >> doFloat
    , xephyrQuery                                 --> doShift workspaceFloat >> doFloat
    , className =? "Zenity"                       --> doFloat
    , appName   =? "desktop_window"               --> doIgnore
    , appName   =? "kdesktop"                     --> doIgnore
    , myEmacsQuery                                --> doShift workspaceEmacs
    , myTerminalQuery                             --> doShift workspaceTerminal
    , myBrowserQuery                              --> doShift workspaceWeb
    , conkerorQuery                               --> doShift workspaceWeb
    , myPdfReaderQuery myPdfReader                --> doShift workspaceBooks
    , appName =? "sun-awt-X11-XFramePeer" <&&>
        className =? "jetbrains-idea-ce"          --> doShift workspaceIde
    , appName =? "sun-awt-X11-XFramePeer"         --> doShift workspaceDb
    , skypeQuery                                  --> doShift workspaceIrc
    , className =? "Pidgin"                       --> doShift workspaceIrc
    , vmQuery                                     --> doShift workspaceVM
    , className =? ".remmina-wrapped"             --> doShift workspaceRemote
    , appName =? "..key-mon-wrapped-wrapped" <&&>
        className =? "..key-mon-wrapped-wrapped"  --> doIgnore
    , manageMonitor screenKeyMonitor
    ]
  ------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()

-- | Font
--
myDefaultFont :: String
myDefaultFont = "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"

-- | Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#1e2320"
myFocusedBorderColor :: String
myFocusedBorderColor = "#5193f7"

-- | Default configuration for prompt
--
myXPConfig :: XPConfig
myXPConfig = def
              { font              = myDefaultFont
              , bgColor           = "#1e2320"
              , fgColor           = "#dddddd"
              , bgHLight          = "#5f5f5f"
              , fgHLight          = "#ffffff"
              , borderColor       = "#ffffff"
              , height            = 20
              , position          = Top
              , historySize       = 256
              , promptBorderWidth = 1}

-- | Make the screenkey window appear in front of other windows
--
screenKeyMonitor :: Monitor a
screenKeyMonitor = monitor
     { prop = ClassName "Screenkey"
     , rect = Rectangle 0 0 15 20 -- rectangle 20x20 in upper left corner
     , persistent = True
     , opacity = 0.8
     }

-- | Now run xmonad with all the defaults we set up.
main :: IO ()
main = do
  home <- getHomeDirectory
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig {
                  terminal           = myTerminal
                , focusFollowsMouse  = myFocusFollowsMouse
                , clickJustFocuses   = myClickJustFocuses
                , borderWidth        = myBorderWidth
                , modMask            = myModMask
                , workspaces         = myWorkspaces
                , normalBorderColor  = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys               = myKeys home
                , mouseBindings      = myMouseBindings
                , layoutHook         = smartBorders . avoidStruts $ myLayout
                , manageHook         = myManageHook
                , handleEventHook    = myEventHook
                -- Status bars and logging
                -- Perform an arbitrary action on each internal state change or X event.
                -- See the 'XMonad.Hooks.DynamicLog' extension for examples.
                --
                , logHook            = dynamicLogWithPP $ xmobarPP
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor "green" "" . shorten 50}
                , startupHook        = myStartupHook
            }
