(define-module (tomrss systems lario)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (tomrss systems base))

(operating-system
  (inherit base-system)
  (host-name "lario")

  (bootloader
   (bootloader-configuration
    (bootloader grub-bootloader)
    (targets (list "/dev/sda"))
    (timeout 2)
    (keyboard-layout (operating-system-keyboard-layout base-system))))

  (mapped-devices
   (list (mapped-device
          (source (uuid "e82bd059-09e7-4abf-b080-938790f4fcca"))
          (target "root-partition")
          (type luks-device-mapping))))

  (file-systems
   (cons* (file-system
            (device (file-system-label "root-partition"))
            (mount-point "/")
            (type "ext4")
            (dependencies mapped-devices))
          %base-file-systems)))
