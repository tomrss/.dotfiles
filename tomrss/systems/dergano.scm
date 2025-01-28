(define-module (tomrss systems dergano)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (tomrss services base))

(operating-system
 (inherit %base-system)

 (host-name "dergano")

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
