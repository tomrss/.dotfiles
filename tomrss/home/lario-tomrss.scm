(define-module (tomrss home lario-tomrss)
  #:use-module (tomrss packages jellyfin)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services shells))

(home-environment
 (packages 
  (list emacs-no-x
        ;; networking utils
        curl
        nmap
        netcat
        net-tools
        lsof
        openresolv
        openvpn
        ;; other nice utilities
        tree
        ripgrep
        zip
        unzip
        bc))

 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
		          (bashrc (list (local-file "../../.bashrc" "bashrc")))))
        (service home-dbus-service-type))))
