(define-module (tomrss packages jellyfin)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system qt)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wget))

(define-public jellyfin-media-player
  (package
    (name "jellyfin-media-player")
    (version "v1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jellyfin/jellyfin-media-player")
             (commit "v1.11.1")))
       (file-name (git-file-name name version))
       (sha256 (base32 "09c13qvdil76mlfc372jss9qk578dfrb0nyayh4a8l9kcj8zij96"))))
    (inputs
     (list libtool harfbuzz freetype fontconfig libx11 libxrandr
           libvdpau libva mesa-headers egl-wayland yasm alsa-lib
           pulseaudio uchardet zlib fribidi gnutls sdl2
           qtwebengine-5 qtquickcontrols-5 qtbase-5 qtwayland-5
           qtx11extras curl wget unzip python mpv))
    (native-inputs
     (list ninja python-wrapper))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-GNinja")
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ninja"
                     "-j" (number->string (parallel-job-count)))))
         (replace 'install
           (lambda _
             (invoke "ninja" "install"))))))
    (home-page "https://github.com/jellyfin/jellyfin-media-player")
    (synopsis "Jellyfin Desktop Client")
    (description "Desktop client for Jellyfin, the free software media system.
It uses jellyfin-web with embedded MPV player.")
    (license license:gpl2)))
