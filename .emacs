;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/

;;------------------------------------------------------------------------------
;; Package manager load and setup
(require 'cl)
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;;------------------------------------------------------------------------------
;; Install packages as necessary on startup. Credit largely to Emacs Prelude.
(defvar my-packages '(ace-jump-mode
                      auctex
                      change-inner
                      exec-path-from-shell
                      expand-region
                      fastnav
                      glsl-mode
                      gnugo
                      haskell-mode
                      idris-mode
                      ido-vertical-mode
                      lua-mode
                      magit
                      markdown-mode
                      monokai-theme
                      paredit
                      projectile
                      rainbow-delimiters
                      solarized-theme
                      ucs-utils
                      unicode-fonts
                      web-mode
                      zenburn-theme)
  "Packages to install at launch, when necessary.")

(defun my-packages-installed-p ()
  "Loop through my preferred packages and determine which are installed."
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-install-packages ()
  "Install any missing packages."
  (unless (my-packages-installed-p)
    (package-refresh-contents)
    (dolist (p my-packages)
     (unless (package-installed-p p)
        (package-install p)))))

(my-install-packages)

;;------------------------------------------------------------------------------
;; Fix broken $PATH on OS X with GUI
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq default-directory "/Users/abbottk/"))

;;------------------------------------------------------------------------------
;; GUI Settings
(if (or (not (display-graphic-p)) (not (eq system-type 'darwin)))
    (menu-bar-mode -1))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode -1)
(column-number-mode 1)
;;(set-fringe-mode 0)

;;------------------------------------------------------------------------------
;; Fonts, colors, aesthetics
(defun kima-size-font ()
  (interactive)
  (concat "PragmataPro" "-"
          (when window-system
            (if (> (x-display-pixel-width) 2000)
                "14"
              "12"))))

(let ((font-name (kima-size-font)))
  (add-to-list 'default-frame-alist (cons 'font font-name))
  (set-face-attribute 'default t :font font-name))

(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))
(setq-default line-spacing 0)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1080)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(defun gud-frame ()
  (interactive)
  (set-frame-width (selected-frame) 180))

(global-set-key (kbd "C-c g") 'gud-frame)

(defun reset-frame ()
  (interactive)
  (if (> (x-display-pixel-width) 1080)
      (set-frame-width (selected-frame) 120)
    (set-frame-width (selected-frame) 80)))

(global-set-key (kbd "C-c r") 'reset-frame)


;;------------------------------------------------------------------------------
;; Good behavior
(setq confirm-nonexistent-file-or-buffer nil
      mac-command-modifier 'super
      mac-option-modifier 'meta
      ns-function-modifier 'hyper
      make-backup-files nil
      ns-pop-up-frames nil
      ring-bell-function (lambda () (message "*I can't let you do that Dave*"))
      vc-follow-symlinks t
      scroll-conservatively 1
      require-final-newline t)

(setq-default dired-use-ls-dired nil
              fill-column 78
              indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)

;;------------------------------------------------------------------------------
;; Default registers
(set-register ?e '(file . "~/.emacs"))

;;------------------------------------------------------------------------------
;; Global keybindings
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-c %") 'replace-regexp)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-c w") 'wrap-region)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)

;;------------------------------------------------------------------------------
;; Ace jump mode
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

;;------------------------------------------------------------------------------
;; Auctex / LaTeX
(setq TeX-PDF-mode t
      TeX-auto-save nil
      TeX-parse-self nil
      LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))
      reftex-plug-into-AUCTeX t)

(setq-default TeX-master nil)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(when (eq system-type 'darwin)
  (setq TeX-view-program-list
        '(("PDF Viewer" "open %o")
          ("DVI Viewer" "open %o")
          ("HTML Viewer" "open %o"))
        TeX-view-program-selection
        '((output-pdf "PDF Viewer")
          (output-dvi "DVI Viewer")
          (output-html "HTML Viewer"))))

(customize-set-variable 'LaTeX-verbatim-environments
                        '("verbatim" "verbatim*" "program" "programc" "prog"))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (reftex-mode t)))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (turn-on-auto-fill)
                             (LaTeX-math-mode)
                             (turn-on-reftex)
                             (outline-minor-mode)))

(defun kima-bibtex-next-entry ()
  (interactive)
  (bibtex-end-of-entry)
  (search-forward-regexp "^@.*")
  (beginning-of-line))

(defun kima-bibtex-prev-entry ()
  (interactive)
  (bibtex-beginning-of-entry)
  (search-backward-regexp "^@.*")
  (beginning-of-line))

(defun kima-bibtex-bind-forward-back-keys ()
  "Change the keys for moving by paragraph to do something
sensible in bibtex files."
  (define-key bibtex-mode-map (kbd "M-}") 'kima-bibtex-next-entry)
  (define-key bibtex-mode-map (kbd "M-{") 'kima-bibtex-prev-entry))

(add-hook 'bibtex-mode-hook 'kima-bibtex-bind-forward-back-keys)

;;------------------------------------------------------------------------------
;; Spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        '((nil ,@default)
          "english" ,@default)))

;;------------------------------------------------------------------------------
;; Org
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "HOLD" "REVIEW" "|" "DONE")))
(setq org-pretty-entities 1)
(setq org-startup-truncated t)
(setq org-startup-indented 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;------------------------------------------------------------------------------
;; Projectile
(projectile-global-mode)

;;------------------------------------------------------------------------------
;; Ido
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t)

;;------------------------------------------------------------------------------
;; Commenting
(defun kima-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there
is no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun kima-test-web-mode ()
  "Handles web-modes stupid special commenting stuff"
  (interactive)
  (if (equal major-mode 'web-mode)
      (web-mode-comment-or-uncomment)
    (kima-comment-or-uncomment-region-or-line)))

(global-set-key (kbd "C-x C-;") 'kima-test-web-mode)

(defun comment-header-line ()
  "Insert a long comment line with hyphens to denote sections in code."
  (interactive)
  (if (and comment-start comment-end)
      (progn (end-of-line)
             (unless (eq (current-column) 0)
               (open-line-above))
             (let* ((cs-trim (s-trim comment-start))
                    (ce-trim (s-trim comment-end))
                    (nbeg (string-width cs-trim))
                    (nend (string-width ce-trim))
                    (numchars (- fill-column (+ nbeg nend))))
               (insert cs-trim)
               (insert-char ?- numchars)
               (insert ce-trim)))
    (message "Comment characters are not set")))  ; looking at you, web-mode
(global-set-key (kbd "C-c C-h") 'comment-header-line)

;;------------------------------------------------------------------------------
;; C/C++

;; TODO -- write some kind of magic function to automatically determine which
;; build system to use.  In the mean time, default to scons.
;; (defun kima-c-mode-hook ()
;;   (setq c-default-style "k&r"
;;         c-basic-offset 4)
;;   (local-set-key (kbd "C-c C-c") 'kima-compile-func)
;;   (local-set-key (kbd "C-c C-k") 'kima-compile-clean-func)
;;   (local-set-key (kbd "C-c C-l") 'scons-build)
;;   (local-set-key (kbd "C-c C-r") 'scons-run-exec))
;; (add-hook 'c-mode-common-hook 'kima-c-mode-hook)

;; (defun kima-compile-func ()
;;   (interactive)
;;   (compile (format "make -C %s" (file-name-directory (get-closest-pathname)))))

;; (defun kima-compile-clean-func ()
;;   (interactive)
;;   (compile (format "make -C %s clean"
;;                    (file-name-directory (get-closest-pathname)))))

;;------------------------------------------------------------------------------
;; Haskell
(add-hook 'haskell-mode-hook 'kima-haskell-hook)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(defun kima-haskell-hook ()
  (setq haskell-interactive-mode-hide-multi-line-errors nil
        haskell-tags-on-save t
        haskell-process-type 'auto)
  (turn-on-haskell-indentation)
  (turn-on-haskell-decl-scan)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-check))

;;------------------------------------------------------------------------------
;; Agda
(require 'ucs-utils)
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-hook 'agda2-mode-hook
          '(lambda ()
             (customize-set-variable
              'agda2-highlight-face-groups 'default-faces)
             (customize-set-variable
              'agda2-include-dirs '("." "/Users/abbottk/Library/Haskell/bin"))))

;;------------------------------------------------------------------------------
;; Idris
(require 'idris-mode)
(add-hook 'idris-mode-hook
          '(lambda ()
             (idris-define-loading-keys)
             (idris-define-docs-keys)
             (idris-define-editing-keys)
             (idris-define-general-keys)
             (turn-on-idris-simple-indent)
             (set-face-attribute 'idris-semantic-data-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)
             (set-face-attribute 'idris-semantic-type-face nil
                                 :foreground nil
                                 :inherit 'font-lock-string-face)
             (set-face-attribute 'idris-loaded-region-face nil
                                 :background nil)))

;;------------------------------------------------------------------------------
;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'geiser-repl-mode-hook 'paredit-mode)

;;------------------------------------------------------------------------------
;; Rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;; GDB
(setq gdb-many-windows t)

;;------------------------------------------------------------------------------
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(when (eq system-type 'darwin)
  (setq magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;;------------------------------------------------------------------------------
;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;------------------------------------------------------------------------------
;; FastNav
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)

;;------------------------------------------------------------------------------
;; Change Inner
(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;;------------------------------------------------------------------------------
;; Yasnippet
;; (setq yas-verbosity 0)
;; (yas-global-mode 1)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))

;;------------------------------------------------------------------------------
;; Whitespace and long lines
(setq whitespace-style '(face lines))
(setq whitespace-line-column 80)
(add-hook 'prog-mode-hook 'whitespace-mode)

;;------------------------------------------------------------------------------
;; Web mode
;; (require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; This is a true hack, and should be fixed
(defun kima-run-tidy ()
  (interactive)
  (shell-command "tidy-dir")
  (revert-buffer t t))

(defun kima-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (define-key web-mode-map (kbd "C-c C-c t") 'kima-run-tidy))
(add-hook 'web-mode-hook 'kima-web-mode-hook)

;;------------------------------------------------------------------------------
;; Eshell and other terminals
(add-hook 'eshell-mode-hook
          (lambda () (setq pcomplete-cycle-completions nil)))

(add-hook 'term-mode-hook
          (lambda () (setq term-buffer-maximum-size 10000)))

(setq explicit-shell-file-name "/usr/local/bin/bash")

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
     (split-window-sensibly (selected-window))
     (other-window 1)
     (ansi-term "/usr/local/bin/bash"))
    (switch-to-buffer-other-window "*ansi-term*")))

(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

(defalias 'access
  (lambda ()
    (cd "/.ssh:abbottk@access.engr.oregonstate.edu:~/")))
(defalias 'nome
  (lambda ()
    (cd "/.ssh:abbottk@nome.eecs.oregonstate.edu:~/")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

;;------------------------------------------------------------------------------
;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glvs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glfs\\'" . glsl-mode))

;;------------------------------------------------------------------------------
;; Tramp
(setq tramp-shell-prompt-pattern
      "^[^]#$%>\n]*[]#$%>]$? *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")

;;------------------------------------------------------------------------------
;; Proof General & Coq
(load-file "/Users/abbottk/.emacs.d/ProofGeneral-4.2/generic/proof-site.el")
(add-to-list 'load-path "/usr/local/opt/coq/lib/emacs/site-lisp")
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(add-hook 'coq-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'coq-mode-hook (lambda () (show-paren-mode -1)))

(custom-set-variables
 '(LaTeX-verbatim-environments
   (quote
    ("verbatim" "verbatim*" "program" "programc" "prog")) t)
 '(proof-electric-terminator-enable t))

;;------------------------------------------------------------------------------
;; Eww
(global-set-key (kbd "C-c b") 'eww)

;;------------------------------------------------------------------------------
;; Gnu-Go
(setq gnugo-xpms 'gnugo-imgen-create-xpms)
(add-hook 'gnugo-start-game-hook 'gnugo-image-display-mode)

;;------------------------------------------------------------------------------
;; Prolog
;; TODO: Start caring about Perl
(add-hook 'prolog-mode-hook 'kima-prolog-hook)
(setq prolog-system 'swi)
(defun kima-prolog-hook ()
  (setq prolog-indent-width 4)
  (define-key prolog-mode-map (kbd "C-c C-l") 'prolog-compile-file))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;------------------------------------------------------------------------------
;; Dash
(global-set-key (kbd "C-c d") 'dash-at-point)

;;------------------------------------------------------------------------------
;; Expand Region
(global-set-key (kbd "C-=") 'er/expand-region)

;;------------------------------------------------------------------------------
;; PostScript Print Properties
(setq ps-paper-type 'letter
     ps-font-size 8.0
     ps-print-header nil
     ps-portrait-mode t)

;;------------------------------------------------------------------------------
;; Misc things that should probably be in a different file
(defun reload-dot-emacs ()
  "Reload the default configuration file."
  (interactive)
  (load-file "~/.emacs")
  (message "Reloaded .emacs file..."))
(global-set-key (kbd "C-x C-r") 'reload-dot-emacs)

(defun open-with ()
  "Open current buffer with an external tool, such as a browser."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(global-set-key (kbd "C-c o") 'open-with)

;; TODO -- this currently only works for OSX
(defun open-file-externally (fname)
  "Interactively select a file to open with an external tool."
  (interactive (list (read-file-name "Open file: ")))
  (message "Path is %s" fname)
  (when fname
    (shell-command (concat
                    "open " fname))))

(defun open-line-below ()
  "Insert a line below regardless of point position.  Like Vim's 'o' command."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key [(shift return)] 'open-line-below)
(global-set-key (kbd "C-o") 'open-line-below)

(defun open-line-above ()
  "Open a new line above the current line, like Vim's 'O' command."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))
(global-set-key (kbd "C-S-o") 'open-line-above)

;;------------------------------------------------------------------------------
;; Load local files / packages
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "wrap")
(load-library "buildscript")
(load-library "tagutils")

;;------------------------------------------------------------------------------
(message "%s" "Good morning, Dave")
