;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
	     (gnu home services)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages '(
                                       ;; editors
                                       "emacs-next-pgtk"

                                       ;; browsers
                                       "firefox"
                                       "qutebrowser"

                                       ;; desktop
                                       "waybar"
                                       "fuzzel"
                                       "hicolor-icon-theme"
                                       "pavucontrol"
                                       "xdg-utils"
                                       "gnome-commander"

                                       ;; media
                                       "qbittorrent"
                                       "mkvtoolnix"
                                       "mpv"
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
                                       "qtwayland@5.15.8"

                                       ;; util
                                       "imagemagick"
                                       "ghostscript"
                                       "ntfs-3g"
                                       "tree"
                                       "ripgrep"
                                       "unzip"
                                       "zip"

                                       ;; build
                                       "gcc-toolchain"
                                       "cmake"
                                       "make"

                                       ;; python
                                       "python"
                                       "python-pip"
                                       "python-virtualenv"

                                       ;; go
                                       "go"
                                       "gopls"
                                       )))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.

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
				          ("QTWEBENGINE_CHROMIUM_FLAGS" . "--disable-seccomp-filter-sandbox")
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
