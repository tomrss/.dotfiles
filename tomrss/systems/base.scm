(define-module (tomrss systems base)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services avahi)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gnome)     ; libnotify
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (%base-system))

(define %user-uid 1000)

(define %udev-rules-folder "/home/tomrss/.dotfiles/udev")

;; TODO: use file->udev-rule to simplify this
(define (file-to-udev-rule-service file)
  (let ((filename (car file)))
    (udev-rules-service
     (string->symbol filename)
     (udev-rule filename
                (string-replace-substring
                 (call-with-input-file
                     (format #f "~a/~a" %udev-rules-folder filename)
                   get-string-all)
                 "%%UID%%"
                 (format #f "~d" %user-uid))))))

(define %udev-file-rules-services
  (map file-to-udev-rule-service
       (cdr (cdr (file-system-tree %udev-rules-folder)))))

(define (nonguix-key)
  (plain-file "nonguix.pub"
              "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define %base-system
  (operating-system
   ;;; Base system configuration
   (host-name "base")                  ; will be overwritten
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   ;;; Localization
   (locale "en_US.utf8")
   (timezone "Europe/Rome")
   (keyboard-layout (keyboard-layout "it" #:options '("caps:escape")))

   ;;; Users and groups
   (users (cons*
           (user-account
            (name "tomrss")
            (comment "Tommaso Rossi")
            (uid %user-uid)
            (group "users")
            (home-directory "/home/tomrss")
            (supplementary-groups '("wheel"
                                    "netdev"
                                    "docker"
                                    "audio"
                                    "tty"
                                    "input"
                                    "video")))
           %base-user-accounts))

   ;;; Base packages for minimal system functionality
   (packages (cons*
              brightnessctl
              pulseaudio
              git
              vim
              stow
              %base-packages))

   ;;; Shepherd services
   (services
    (append
     (list
      ;;; Networking
      ;;; https://guix.gnu.org/manual/devel/en/html_node/Networking-Setup.html
      ;;; https://guix.gnu.org/manual/devel/en/html_node/Networking-Services.html

      ;; needed by network manager
      (service wpa-supplicant-service-type)
      ;; network manager will take care of most of it. just add vpn plugin
      (service network-manager-service-type
               (network-manager-configuration
                (vpn-plugins
                 (list network-manager-openvpn))))
      ;; time sync
      (service ntp-service-type)
      ;; enable ssh server on port 2222
      (service openssh-service-type
               (openssh-configuration
                (permit-root-login #f)
                (openssh openssh-sans-x)
                (port-number 2222)))
      ;; cellular modems
      (service modem-manager-service-type)
      ;; discoverability on the local network
      (service avahi-service-type)

      ;;; Desktop services 
      ;;; https://guix.gnu.org/manual/en/html_node/Desktop-Services.html

      ;; TODO try seatd
      (service elogind-service-type)
      (service udisks-service-type)
      (service upower-service-type)
      (service dbus-root-service-type)
      fontconfig-file-system-service

      ;;; Power management services
      ;;; https://guix.gnu.org/manual/en/html_node/Power-Management-Services.html

      ;; power saving
      (service tlp-service-type
               (tlp-configuration
                (cpu-boost-on-ac? #t)))
      ;; control thermal state
      (service thermald-service-type)

      ;;; Miscellaneous services
      ;;; https://guix.gnu.org/manual/devel/en/html_node/Miscellaneous-Services.html

      ;; containerd is needed by docker
      (service containerd-service-type)
      (service docker-service-type)

      ;;; Window Manager
      ;;; https://guix.gnu.org/manual/devel/en/html_node/X-Window.html

      ;; Configure screenlocker here to avoid PAM issues
      ;; NOTE: I would like to put this in home config (sway is installed there)
      ;;  but it's not possibile
      (service screen-locker-service-type
               (screen-locker-configuration
                (name "swaylock")
                (program (file-append swaylock "/bin/swaylock"))
                (using-pam? #t)
                (using-setuid? #f))))

     ;;; Base services
     ;; https://guix.gnu.org/manual/en/html_node/Base-Services.html

     ;; udev rules
     (cons*
      (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
      %udev-file-rules-services)

     (modify-services %base-services
                      ;; configure nonguix substitutes in the guix daemon
                      (guix-service-type
                       config => (guix-configuration
                                  (inherit config)
                                  (substitute-urls
                                   (append (list "https://substitutes.nonguix.org")
                                           %default-substitute-urls))
                                  (authorized-keys
                                   (cons* (nonguix-key)
                                          %default-authorized-guix-keys)))))))

   ;;; Configure the bootloader
   
   ;; typical bootloader for efi systems, override it in case of
   ;; legacy bios systems or some stranger stuff
   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (timeout 2)
     (keyboard-layout keyboard-layout)))

   ;;; Stuff that NEEDS to be overridden

   ;; this will be overwritten
   (file-systems (cons*
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))))
