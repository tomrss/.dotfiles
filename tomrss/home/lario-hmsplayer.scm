(define-module (tomrss home lario-hmsplayer)
  #:use-module (tomrss packages jellyfin)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop))

;; kiosk with jellyfin client

(home-environment
 (packages 
  (list cage
        jellyfin-media-player))

 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              (list (plain-file 
					 "start-jellyfin-kiosk"
					 (string-append
					  "# If running from tty1 start jellyfin kiosk\n"
					  "[ \"$(tty)\" = \"/dev/tty1\" ] && "
					  "cage -d -- jellyfinmediaplayer --platform wayland"))))))
   (service home-dbus-service-type))))
