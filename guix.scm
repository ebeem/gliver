(use-modules
  (guix build-system gnu)
  (guix packages)
  (guix download)
  (guix gexp)
  (guix git)
  (guix licenses)
  (gnu packages freedesktop)
  (gnu packages gtk)
  (gnu packages gnome)
  (gnu packages guile)
  (gnu packages pkg-config)
  (gnu packages window-management)
  (gnu packages xdisorg)
  (gnu packages zig-xyz))

(define-public gliver
  (package
   (name "gliver")
   (version "master")
   (source
    (git-checkout (url (dirname (current-filename)))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:make-flags '(list (string-append "PREFIX=" (assoc-ref %outputs "out")))
     #:phases
     #~(modify-phases %standard-phases
         (delete 'configure))))
   (inputs
    (list guile-3.0
          river
          wayland
          libxkbcommon
          librsvg
          guile-cairo
          pango
          pkg-config))
   (synopsis "A guile scheme wayland window manager based on river")
   (description "Gliver brings the keyboard-driven, manual-tiling workflow of StumpWM
to Wayland, using River as the compositor backend and GNU Guile Scheme
as the extension language. It attempts to be highly customizable while
relying entirely on the keyboard for input.")
   (home-page "https://codeberg.org/ebeem/gliver")
   (license gpl3+)))

gliver
