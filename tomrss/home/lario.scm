(define-module (tomrss home lario)
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
  #:use-module (gnu home services shells)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services syncthing))

(home-environment
 (packages 
   ;; kiosk with jellyfin client
   (list cage
         jellyfin-media-player
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
		  (bashrc       (list (local-file 
					"../../.bashrc"
					"bashrc")))
                  (bash-profile (list (plain-file 
					"start-jellyfin-kiosk"
					(string-append
					  "# If running from tty1 start jellyfin kiosk\n"
					  "[ \"$(tty)\" = \"/dev/tty1\" ] && "
					  "cage -d -- jellyfinmediaplayer --platform wayland"))))))
	(service home-dbus-service-type))))
