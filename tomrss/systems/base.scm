(define-module (tomrss systems base)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg) ; just to exclude gdm service
  #:use-module (gnu services docker)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (base-system))

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
                 (call-with-input-file
                     (format #f "~a/~a" %udev-rules-folder filename)
                   get-string-all)
                 "%%UID%%"
                 (format #f "~d" %user-uid))))))

(define %udev-rules-services
  (map file-to-udev-rule-service
       (cdr (cdr (file-system-tree %udev-rules-folder)))))

(define base-system
  (operating-system
    (host-name "base")                  ; will be overwritten
    (kernel linux)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)
    (locale "en_US.utf8")
    (timezone "Europe/Rome")
    (keyboard-layout (keyboard-layout "it" #:options '("caps:escape")))
    (users (cons* (user-account
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

    (packages
     (cons* pulseaudio
            alacritty
            git
            vim
            stow
            libnotify
            %base-packages))

    (services
     (append
      (list
       (service openssh-service-type
                (openssh-configuration
                 (permit-root-login #f)
                 (openssh openssh-sans-x)
                 (port-number 2222)))
       (service containerd-service-type)
       (service docker-service-type))
      %udev-rules-services
      (modify-services %no-login-desktop-services
        (guix-service-type
         config => (guix-configuration
                    (inherit config)
                    (substitute-urls
                     (append (list "https://substitutes.nonguix.org")
                             %default-substitute-urls))
                    (authorized-keys
                     (cons* (plain-file "nonguix.pub"
                                        "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                            %default-authorized-guix-keys)))))))

    ;; this will be overwritten by legacy bios systems
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (timeout 2)
      (keyboard-layout keyboard-layout)))

    ;; this will be overwritten
    (file-systems (cons*
                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))))
