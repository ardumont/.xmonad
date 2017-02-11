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
import           XMonad.Prompt.Pass         (passGeneratePrompt, passPrompt,
                                             passRemovePrompt)
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
--myPdfReader = "evince"
myPdfReader = "apvlv"

myPdfReaderQuery :: String -> Query Bool
myPdfReaderQuery "evince" = className =? "Evince" <||> className =? ".evince-wrapped"
myPdfReaderQuery "apvlv" = appName =? "apvlv" <&&> className =? "Apvlv"
myPdfReaderQuery _ = error "Undefined"

-- | My preferential browser
--
myBrowser :: String
myBrowser = "firefox"

myBrowserQuery :: Query Bool
myBrowserQuery = appName =? "Navigator" <&&> (className =? "Firefox" <||> className =? "Tor Browser" <||> className =? "Iceweasel")

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
myEmacsQuery = appName =? "emacs" <&&> className =? "Emacs"

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

-- | modMask lets you specify which modkey you want to use. mod4mask is window key
-- I'm used to prefix key because of emacs, stumpwm, conkeror and firefox with keysnail
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
spawnZenityCmd :: String -> X ()
spawnZenityCmd = spawn . zenityCmd
                 where zenityCmd :: String -> String
                       zenityCmd cmd = "zenity --info --text \"$(" ++ cmd ++ ")\""

-- | Display some data in a zenity window dialog
--
-- spawnZenityText :: String -> X ()
-- spawnZenityText = spawn . zenityText
--                   where zenityText :: String -> String
--                         zenityText s = "zenity --info --text '" ++ s ++ "'"

-- | Run or raise with a default config folder from which finding the command
--
myRunOrRaise :: String -> String -> Query Bool -> X ()
myRunOrRaise home cmd = runOrRaiseNext $ combine home cmd

libreOfficeQuery :: Query Bool
libreOfficeQuery = (appName =? "libreofficedev" <||> appName =? "libreoffice") <&&>
                   (className =? "libreofficedev-writer" <||>
                    className =? "libreoffice-writer" <||>
                    className =? "libreofficedev-calc" <||>
                    className =? "libreoffice-calc" <||>
                    className =? "libreofficedev-draw" <||>
                    className =? "libreoffice-draw")

conkerorQuery :: Query Bool
conkerorQuery = appName =? "Navigator" <&&> className =? "Conkeror"

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
  [ (prefix "C-g"       , "abort"                      , spawn "xdotool key Escape")
  , (prefix "M1-c"      , "mouse-click-at-point"       , spawn "xdotool click 1")
  , (prefix "M1-d"      , "xdotool-prompt"             , launchApp myXPConfig "xdotool")
  , (prefix "M1-w"      , "wifi-connect"               , spawn "~/.local/bin/hWifi")
  , (prefix "C-M1-w"    , "wifi-create-and-connect"    , launchApp myXPConfig "~/.local/bin/hWifi")
  , (prefix "e"         , "emacs"                      , myRunOrRaise home "bin/emacs/emacs.sh"                        myEmacsQuery)
  , (prefix "S-x"       , "xephyr"                     , myRunOrRaise home "bin/xephyr/xephyr-stumpwm.sh"              xephyrQuery)
  , (prefix "y"         , "yed"                        , myRunOrRaise home "bin/app/yed.sh"                            (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-install4j-runtime-launcher-Launcher"))
  , (prefix "S-c"       , "lighttable"                 , myRunOrRaise home "applications/LightTable/LightTable"        (className =? "ltbin"))
  , (prefix "C-r"       , "simplescreenrecorder"       , runOrRaiseNext "simplescreenrecorder"                         (appName =? "simplescreenrecorder" <&&> className =? "Simplescreenrecorder"))
  , (prefix "S-j"       , "visualvm"                   , myRunOrRaise home "applications/visualvm/bin/visualvm"        (className =? "java-lang-Thread"))
  , (prefix "S-l"       , "sqldeveloper"               , myRunOrRaise home "applications/sqldeveloper/sqldeveloper.sh" (appName =? "sun-awt-X11-XFramePeer"))
  , (prefix "S-u"       , "usb-creator-gtk"            , myRunOrRaise home "bin/admin/usb-creator.sh"                  (appName =? "usb-creator-gtk" <&&> className =? "Usb-creator-gtk"))
  , (prefix "C-m"       , "mail"                       , runOrRaiseNext "evolution"                                    (className =? ".evolution-wrapped"))
  --, (prefix "M1-r"      , "remmina"                    , runOrRaiseNext "remmina"                                      (className =? ".remmina-wrapped"))
  , (prefix "M1-S-w"    , "putty"                      , runOrRaiseNext "putty"                                        (appName =? "putty" <&&> className =? "Putty"))
  , (prefix "M1-S-x"    , "mcomix"                     , runOrRaiseNext "mcomix"                                       (appName =? "mcomix" <&&> className =? "MComix"))
  , (prefix prefixKey   , "promote"                    , promote)
  , (prefix "i"         , "ide"                        , runOrRaiseNext "idea-community"                               (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "jetbrains-idea-ce"))
  , (prefix "C-d"       , "db-gui"                     , runOrRaiseNext "mysql-workbench"          (appName =? "mysql-workbench-bin" <&&> className =? "Mysql-workbench-bin"))
  -- , (prefix "M1-S-d"    , "db-visualizer"              , runOrRaiseNext "dbvisguisteam"            (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-onseven-dbvis-DbVisualizerGUI"))
  , (prefix "M1-S-d"    , "pgdmin3"                    , runOrRaiseNext "pgadmin3"                 (appName =? "pgadmin3" <&&> className =? "Pgadmin3"))
  , (prefix "M1-g"      , "steam"                      , runOrRaiseNext "steam"                    (appName =? "Steam" <&&> className =? "Steam"))
  , (prefix "x"         , "terminal"                   , runOrRaiseNext myTerm                     myTerminalQuery)
  , (prefix "C-x"       , "xterm"                      , runOrRaiseNext "xterm"                    (appName =? "xterm" <&&> className =? "XTerm"))
  , (prefix "S-p"       , "chat"                       , runOrRaiseNext "pidgin"                   (className =? "Pidgin"))
  , (prefix "S-a"       , "android"                    , runOrRaiseNext "android"                  (className =? "Android SDK Manager"))
  , (prefix "S-d"       , "android-emulator"           , runOrRaiseNext "android"                  (className =? ".emulator64-arm-wrapped"))
  , (prefix "S-s"       , "sweethome-3d"               , runOrRaiseNext "sweethome3d"              (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-eteks-sweethome3d-SweetHome3D"))
  , (prefix "S-t"       , "vlc"                        , runOrRaiseNext "vlc"                      vlcQuery)
  , (prefix "C-e"       , "pdf-reader"                 , runOrRaiseNext myPdfReader                (myPdfReaderQuery myPdfReader))
  , (prefix "C-i"       , "image-viewer"               , runOrRaiseNext "eog"                      (className =? "Eog"))
  , (prefix "d"         , "pinta"                      , runOrRaiseNext "pinta"                    (className =? "Pinta"))
  , (prefix "S-i"       , "gimp"                       , runOrRaiseNext "gimp"                     (className =? "Gimp"))
  , (prefix "C-a"       , "music-reader"               , runOrRaiseNext "audacious"                (className =? "Audacious"))
  , (prefix "M1-j"      , "jconcole"                   , runOrRaiseNext "jconsole"                 (className =? "sun-tools-jconsole-JConsole"))
--  , (prefix "C-S-c"     , "arduino-ide"                , runOrRaiseNext "arduino"                  (className =? "processing-appBase"))
  , (prefix "C-w"       , "wireshark"                  , runOrRaiseNext "gksudo wireshark"         (className =? "wireshark"))
  , (prefix "n"         , "nautilus"                   , runOrRaiseNext "nautilus"                 (appName =? "nautilus" <&&> className =? "Nautilus"))
  , (prefix "S-n"       , "thunar"                     , runOrRaiseNext "thunar"                   (className =? "thunar"))
  , (prefix "C-M1-f"    , "filezilla"                  , runOrRaiseNext "filezilla"                (className =? "Filezilla"))
  , (prefix "C-v"       , "virtualbox"                 , runOrRaiseNext "VirtualBox"               vmQuery)
  , (prefix "u"         , "unetbootin"                 , runOrRaiseNext "unetbootin"               (className =? "unetbootin"))
  , (prefix "/"         , "transmission"               , runOrRaiseNext "transmission-gtk" $       (appName =? "transmission-gtk" <&&> className =? "Transmission-gtk") <||> (appName =? ".transmission-gtk-wrapped" <&&> className =? ".transmission-gtk-wrapped"))
  , (prefix "S-g"       , "gparted"                    , runOrRaiseNext "gksudo /usr/sbin/gparted" (className =? "gpartedbin"))
  , (prefix "S-f"       , "file-progression"           , runOrRaiseNext ""                         (className =? "file_progress"))
  , (prefix "C-S-x"     , "xosview"                    , runOrRaiseNext "xosview"                  (className =? "xosview"))
  , (prefix "C-S-g"     , "dia"                        , runOrRaiseNext "dia"                      (appName =? "dia-normal" <&&> className =? "Dia-Normal"))
  , (prefix "b"         , "conkeror"                   , runOrRaiseNext "conkeror"                 conkerorQuery)
  , (prefix "B"         , "baobab"                     , runOrRaiseNext "baobab"                   (appName =? "baobab" <&&> className =? "Baobab"))
  , (prefix "z"         , "gitk"                       , runOrRaiseNext "gitk"                     (className =? "gitk"))
  , (prefix "S-f"       , "fbreader"                   , runOrRaiseNext "fbreader"                 (className =? "fbreader"))
  , (prefix "M1-t"      , "tuxguitar"                  , runOrRaiseNext "tuxguitar"                (className =? "TuxGuitar"))
  , (prefix "C-M1-s"    , "skype"                      , runOrRaiseNext "skype"                    skypeQuery)
  , (prefix "o"         , "libre-office"               , runOrRaiseNext "libreoffice"              libreOfficeQuery)
  , (prefix "C-u"       , "soap-ui"                    , runOrRaiseNext "soapui"                   (appName =? "sun-awt-X11-XFramePeer" <&&> className =? "com-eviware-soapui-SoapUI"))
  , (prefix "f"         , "browser"                    , runOrRaiseNext myBrowser                  myBrowserQuery)
  , (prefix "C-c"       , "chromium"                   , runOrRaiseNext "chromium"                 (appName =? "Chromium" <&&> className =? "Chromium"))
  , (prefix "C-S-e"     , "env"                        , spawnZenityCmd "env")
  , (prefix "a"         , "date"                       , spawnZenityCmd "date")
  , (prefix "S-k"       , "ssh-add-l"                  , spawnZenityCmd "ssh-add -l")
  , (prefix "S-e"       , "cat-etc-environment"        , spawnZenityCmd "cat /etc/environment")
  , (prefix "S-h"       , "cat-etc-hosts"              , spawnZenityCmd "cat /etc/hosts")
  , (prefix "C-S-i"     , "sbin-ifconfig"              , spawnZenityCmd "/sbin/ifconfig")
  , (prefix "S-b"       , "acpi"                       , spawnZenityCmd "acpi -b")
  , (prefix "^"         , "top"                        , spawnZenityCmd "top -b -n 1 -c -d 1")
  , (prefix "C-s"       , "print-screen"               , spawn "screenshot=\"$HOME/Pictures/screenshots/$(date +%F_%H-%M-%S).png\" ; scrot -u $screenshot; notify-send -t 1000 \"$(basename $screenshot) done!\"")
  , (prefix "M1-s"      , "mouse-print-screen"         , spawn "~/bin/touchpad/toggle-touchpad-manual.sh 1; screenshot=\"$HOME/Pictures/screenshots/$(date +%F_%H-%M-%S).png\" ; scrot -s $screenshot; notify-send -t 1000 \"$(basename $screenshot) done!\"")
  , (prefix "C-t"       , "toggle-touchpad"            , spawn "~/bin/touchpad/toggle-touchpad.sh")
  , (prefix "C-S-s"     , "suspend"                    , spawn "systemctl suspend")
  , (prefix "C-S-h"     , "hibernate"                  , spawn "systemctl hibernate")
  , (prefix "S-a"       , "ssh-add"                    , spawn "~/bin/ssh/ssh-add.sh")
  , (prefix "C-b"       , "brightness-decrease"        , spawn "xbacklight -dec 10")
  , (prefix "C-f"       , "brightness-increase"        , spawn "xbacklight -inc 10")
  , (prefix "C-S-m"     , "brightness-half"            , spawn "xbacklight -set 50")
  , (prefix "S-m"       , "brightness-max"             , spawn "xbacklight -set 100")
  , (prefix "M1-f"      , "sound-increase"             , spawn "exec amixer set Master 10%+")
  , (prefix "M1-b"      , "sound-decrease"             , spawn "exec amixer set Master 10%-")
  , (prefix "M1-m"      , "sound-toggle"               , spawn "exec amixer set Master toggle")
  , (prefix "C-o"       , "wifi-off"                   , spawn "~/bin/wifi/wifi-off.sh")
  , (prefix "S-o"       , "wifi-on"                    , spawn "~/bin/wifi/wifi-on.sh")
  , (prefix "C-M1-l"    , "session-lock"               , spawn "~/bin/session/lock.sh")
  , (prefix "M1-e"      , "pdf-reader-prompt"          , launchApp myXPConfig myPdfReader)
  , (prefix "s"         , "search-url"                 , search)
  , (prefix "r"         , "exec"                       , runOrRaisePrompt myXPConfig)
  , (prefix "g"         , "goto"                       , windowPromptGoto myXPConfig)
  , (prefix "M1-x"      , "meta-x"                     , xmonadPromptC keymapDescription myXPConfig)
  , (prefix "p"         , "pass-read"                  , passPrompt myXPConfig)
  , (prefix "C-p"       , "pass-generate"              , passGeneratePrompt myXPConfig)
  , (prefix "C-S-p"     , "pass-generate"              , passRemovePrompt myXPConfig)
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
  , (prefix "C-S-q"     , "xmonad-recompile"           , spawn "stack exec xmonad -- --recompile && notify-send -t 1000 'XMonad recompiled!'")
  , (prefix "S-q"       , "xmonad-restart"             , spawn "stack exec xmonad -- --restart && notify-send -t 1000 'XMonad Restarted!'")
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
    , vlcQuery                                    --> doShift workspaceFloat >> doFloat
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
