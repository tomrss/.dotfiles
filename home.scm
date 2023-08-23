(use-modules (gnu home)
	         (gnu home services)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
 (packages
  (specifications->packages
   '(
     ;; editors
     "emacs-next-pgtk"

     ;; desktop
     "waybar"
     "fuzzel"
     "hicolor-icon-theme"
     "brightnessctl"
     "pavucontrol"
     "xdg-utils"
     "gnome-commander"

     ;; browsers
     "firefox"
     "qutebrowser"

     ;; media
     "qbittorrent"
     "mkvtoolnix"
     "mpv"
     "qview"
     "youtube-dl"
     "picard"
     "calibre"

     ;; fonts
     "font-jetbrains-mono"

     ;; networking
     "protonvpn-cli"
     "curl"
     "nmap"
     "netcat"
     "net-tools"

     ;; lib
     "glibc-locales"
     "ncurses"
     "libvterm"
     "qtwayland@5"

     ;; util
     "imagemagick"
     "ghostscript"
     "ntfs-3g"
     "tree"
     "ripgrep"
     "unzip"
     "zip"

     ;; toolchain
     "gcc-toolchain"
     "cmake"
     "make"
     "python"
     "python-pip"
     "python-virtualenv"
     "go"
     "gopls"
     )))

 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (aliases '(("cp" . "cp -i")
                             ("df" . "df -h")
                             ("egrep" . "egrep --colour=auto")
                             ("fgrep" . "fgrep --colour=auto")
                             ("free" . "free -m")
                             ("grep" . "grep --colour=auto")
                             ("l" . "ls -lh")
                             ("ll" . "ls -lah")
                             ("ls" . "ls --color=auto")
                             ("more" . "less")
                             ("np" . "nano -w PKGBUILD")
                             ("python" . "python3")))
                  (bashrc (list (local-file
                                 ".bashrc"
                                 "bashrc")))
                  (bash-profile (list (local-file
                                       ".bash_profile"
                                       "bash_profile")))))
        (simple-service 'home-env-var-service
		                home-environment-variables-service-type
		                `(("_JAVA_AWT_WM_NONREPARENTING" . "1")
                          ("QT_QPA_PLATFORM" . "wayland")
                          ("EMACS_THEME" . "doom-nord")))
        (service home-xdg-configuration-files-service-type
                 `(("sway" ,(local-file "sway" #:recursive? #t))
                   ("alacritty" ,(local-file "alacritty" #:recursive? #t))
                   ("waybar" ,(local-file "waybar" #:recursive? #t))
                   ("fuzzel" ,(local-file "fuzzel" #:recursive? #t))
                   ;; don't copy the whole folder because some files are to be written from qutebrowser
                   ;; TODO: move that kind of files to another folder from qutebrowser config
                   ("qutebrowser/themes" ,(local-file "qutebrowser/themes" #:recursive? #t))
                   ("qutebrowser/config.py" ,(local-file "qutebrowser/config.py"))
                   ("qutebrowser/quickmarks" ,(local-file "qutebrowser/quickmarks")))))))
