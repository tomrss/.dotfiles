(use-modules (gnu)
             (gnu services desktop)
             (gnu services docker)
             (gnu services xorg)        ; just to exclude gdm service
             (gnu packages wm)
             (gnu packages glib)
             (gnu packages vim)
             (gnu packages certs)
             (gnu packages xdisorg)
             (gnu packages terminals)
	         (gnu packages pulseaudio)
             (gnu packages package-management)
             (gnu packages version-control)
	         (srfi srfi-1)
             (web client)
             (web response)
             (rnrs bytevectors)
             (guix packages)
	         (nongnu packages linux)
             (nongnu system linux-initrd))

(define %nonguix-public-key
  (utf8->string
   (read-response-body
    (http-request "https://substitutes.nonguix.org/signing-key.pub" #:streaming? #t))))

(define %no-login-desktop-services
  (remove (lambda (service)
    	    (eq? (service-kind service) gdm-service-type))
    	  %desktop-services))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (initrd microcode-initrd)
 
 (locale "en_US.utf8")
 (timezone "Europe/Rome")
 (keyboard-layout (keyboard-layout "it" #:options '("caps:escape")))
 (host-name "dergano")
 (users (cons* (user-account
                (name "tomrss")
                (comment "Tommaso Rossi")
                (group "users")
                (home-directory "/home/tomrss")
                (supplementary-groups '("wheel"
                                        "netdev"
                                        "audio"
                                        "tty"
                                        "input"
                                        "docker"
                                        "video")))
               %base-user-accounts))

 (packages
  (cons* nss-certs
         sway
         swaybg
         swayidle
         swaylock
	     pulseaudio
         alacritty
         git
         vim
         stow
         %base-packages))

 (services (cons*
            (service docker-service-type)
            (modify-services
             %no-login-desktop-services
             (guix-service-type
              config => (guix-configuration
                         (inherit config)
                         (substitute-urls
                          (append (list "https://substitutes.nonguix.org")
                                  %default-substitute-urls))
                         (authorized-keys
                          (cons* (plain-file "non-guix.pub"
                                             %nonguix-public-key)
                                 %default-authorized-guix-keys)))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (timeout 2)
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (swap-space
         (target (uuid "c97b2864-212b-4f1b-91eb-2986b8f3f07e")))))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid "6db3f317-0582-49fd-b109-ebe7f57314a6"))
         (target "cryptroot")
         (type luks-device-mapping))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/cryptroot")
          (type "ext4")
          (dependencies mapped-devices))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "2FAC-6752" 'fat32))
          (type "vfat"))
         %base-file-systems)))
