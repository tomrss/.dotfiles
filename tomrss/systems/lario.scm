;; TODO use base system
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnome)     ; libnotify
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(define (nonguix-key)
  (plain-file "nonguix.pub"
              "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define (auto-login-to-tty config tty user)
  "Auto login on specific TTY.

  From the Guix cookbook:
  https://guix.gnu.org/cookbook/en/html_node/Auto_002dLogin-to-a-Specific-TTY.html
  "
  (if (string=? tty (mingetty-configuration-tty config))
      (mingetty-configuration
       (inherit config)
       (auto-login user)
	   (login-pause? #t))
      config))

(operating-system
 ;;; Base system configuration
 (host-name "lario")
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
          (uid 1000)
          (group "users")
          (home-directory "/home/tomrss")
          (supplementary-groups '("wheel"
                                  "netdev"
                                  "docker"
                                  "audio"
                                  "tty"
                                  "input"
                                  "video")))
         (user-account
          (name "hms")
          (comment "Home Media Server")
          (uid 999)
          (group "applications")
          (home-directory "/home/hms")
          (supplementary-groups '("audio"
                                  "input"
                                  "video")))
         %base-user-accounts))

 (groups (cons*
          (user-group
           (name "applications")
           (id 999))
          %base-groups))

 ;;; Base packages for minimal system functionality
 (packages (cons*
            brightnessctl
            pulseaudio
            git
            vim
            gnu-make
            stow
            %base-packages))

 ;;; Shepherd services
 (services
  (cons*
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
   (service docker-service-type))

  ;;; Base services
  ;;; https://guix.gnu.org/manual/en/html_node/Base-Services.html

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
                                       %default-authorized-guix-keys))))
		           (mingetty-service-type 
		            config => (auto-login-to-tty
                               config "tty1" "tomrss"))))

 ;;; Configure the bootloader
 
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (timeout 1)
   (keyboard-layout keyboard-layout)))

 ;;; File systems (fstab)

 (swap-devices
  (list (swap-space
         (target (uuid "f62d8022-c3a7-4582-9d08-de83b0bfdb87")))))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "DD7A-B2D2" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "21399c6b-c6d5-41d1-b2e9-a0a3709cca8e"))
          (type "ext4"))
         (file-system
          (mount-point "/var/homemediaserver/data")
          (device (uuid "0dd23756-888f-4fa7-b1c3-ba27ba336873"))
          (type "ext4"))
         %base-file-systems)))
