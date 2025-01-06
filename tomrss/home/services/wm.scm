(define-module (tomrss home services wm)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)
  #:export (home-sway-service-type
            home-sway-configuration))

(define (home-sway-profile-service config)
  (list sway
        swayidle
        swaylock))

(define-configuration home-sway-configuration
  (config-folder
   (text-config config-folder)
   "Path of the folder where sway configuration files are saved."))

(define (home-sway-configuration-files config)
  (let ((config-folder (home-sway-configuration-config-folder config)))
    `(("sway" ,(local-file config-folder #:recursive? #t)))))

(define (home-sway-environment-variables config)
  '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))

(define home-sway-service-type
  (service-type (name 'home-desktop)
                (description "Sway window manager service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-sway-profile-service)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        home-sway-configuration-files)
                       (service-extension
                        home-environment-variables-service-type
                        home-sway-environment-variables)))
                (default-value #f)))
