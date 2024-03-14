(use-modules (gnu)
             (gnu services desktop)
             (gnu services xorg)        ; just to exclude gdm service
             (gnu packages vim)
             (gnu packages certs)
             (gnu packages gnome)
             (gnu packages terminals)
             (gnu packages pulseaudio)
             (gnu packages package-management)
             (gnu packages version-control)
             (guix packages)
             (srfi srfi-1)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 string-fun)
             (ice-9 textual-ports)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(define %no-login-desktop-services
  (remove (lambda (service)
    	    (eq? (service-kind service) gdm-service-type))
    	  %desktop-services))

(define %user-uid 1000)

(define %udev-rules-folder "/home/tomrss/.dotfiles/udev")

(define (file-to-udev-rule-service file)
  (let ((filename (car file)))
    (udev-rules-service
     (string->symbol filename)
     (udev-rule filename
                (string-replace-substring
                 (call-with-input-file (format #f "~a/~a" %udev-rules-folder filename) get-string-all)
                 "%%UID%%"
                 (format #f "~d" %user-uid))))))

(define %udev-rules-services
  (map file-to-udev-rule-service
       (cdr (cdr (file-system-tree %udev-rules-folder)))))


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
                (uid %user-uid)
                (group "users")
                (home-directory "/home/tomrss")
                (supplementary-groups '("wheel"
                                        "netdev"
                                        "audio"
                                        "tty"
                                        "input"
                                        "video")))
               %base-user-accounts))

 (packages
  (cons* nss-certs
         pulseaudio
         alacritty
         git
         vim
         stow
         libnotify
         %base-packages))

 (services
  (append %udev-rules-services
          (modify-services
           %no-login-desktop-services
           (guix-service-type
            config => (guix-configuration
                       (inherit config)
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (cons* (local-file "/home/tomrss/.dotfiles/nonguix-signing-key.pub")
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
