;; TODO refactor with services
(define-module (tomrss home dergano)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (tomrss home services wm))

(home-environment
(packages
 (specifications->packages
  '(
    ;; editors
    "emacs-next-pgtk"

    ;; desktop
    "waybar"
    "fuzzel"
    "adwaita-icon-theme"
    "hicolor-icon-theme"
    "brightnessctl"
    "pavucontrol"
    "xdg-utils"
    "gnome-commander"
    "swaynotificationcenter"

    ;; browsers
    "firefox"
    "qutebrowser"

    ;; media
    "qbittorrent"
    "mkvtoolnix"
    "mpv"
    "qview"
    "youtube-dl"
    "calibre"

    ;; fonts
    "font-jetbrains-mono"

    ;; networking
    "openssh"
    "openvpn"
    "sshfs"
    "protonvpn-cli"
    "curl"
    "nmap"
    "netcat"
    "net-tools"
    "bind:utils" ;; dig, nslookup
    "openresolv"

    ;; lib
    "ncurses"
    "libvterm"
    "qtwayland"
    "libxcrypt"

    ;; util
    "imagemagick"
    "ghostscript"
    "ntfs-3g"
    "cifs-utils"
    "tree"
    "ripgrep"
    "unzip"
    "zip"
    "unrar-free"
    "lsof"
    "bc"
    "docker-compose"

    ;; toolchain
    "gcc-toolchain"
    "cmake"
    "make"
    "pkg-config"
    "ccls"
    "bear"
    "guile"
    "python"
    "python-pip"
    "python-virtualenv"
    "go"
    )))

(services
 (list (service home-bash-service-type
                (home-bash-configuration
                 (bashrc (list (local-file "../../.bashrc" "bashrc")))
                 (bash-profile (list (local-file "../../.bash_profile" "bash_profile")))))
       (service home-zsh-service-type
                (home-zsh-configuration
                 (xdg-flavor? #f)
                 (zshrc (list (local-file "../../.zshrc" "zshrc")))
                 (zprofile (list (local-file "../../.zprofile" "zprofile")))))
       (simple-service 'home-env-var-service
		               home-environment-variables-service-type
		               `(("SHELL" . ,(file-append zsh "/bin/zsh"))
                         ("QT_QPA_PLATFORM" . "xcb")
                         ("QUTE_QT_WRAPPER" . "PyQt6")
                         ("CALIBRE_USE_DARK_PALETTE" . "1")
                         ("PATH" . "$PATH:$HOME/go/bin")))
       (service home-xdg-configuration-files-service-type
                `(("alacritty" ,(local-file "../../alacritty" #:recursive? #t))
                  ("waybar" ,(local-file "../../waybar" #:recursive? #t))
                  ("fuzzel" ,(local-file "../../fuzzel" #:recursive? #t))
                  ("swaync" ,(local-file "../../swaync" #:recursive? #t))
                  ;; don't copy the whole folder because some files are to be written from qutebrowser
                  ;; TODO: move that kind of files to another folder from qutebrowser config
                  ("qutebrowser/themes" ,(local-file "../../qutebrowser/themes" #:recursive? #t))
                  ("qutebrowser/config.py" ,(local-file "../../qutebrowser/config.py"))
                  ("qutebrowser/quickmarks" ,(local-file "../../qutebrowser/quickmarks"))))
       (service home-sway-service-type
                (home-sway-configuration
                 (config-folder "/home/tomrss/.dotfiles/sway"))))))
