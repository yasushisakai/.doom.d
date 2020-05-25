;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yasushi Sakai"
      user-mail-address (getenv "EMAIL"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "HackGen Console" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

;; (use-package! org-hugo
;;   :init
;;   (setq org-hugo-default-section-directory "memo"
;; ))

;; (use-package! org-re-reveal
;;   :config
;;   (setq
;;    org-re-reveal-mousewheel nil
;;    org-re-reveal-progress nil
;;    org-re-reveal-transition "fade"
;;    org-re-reveal-theme "simple"
;;    ))

;; this adds the backlinks section at the end
(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                                 (org-roam--get-title-or-slug (car it))))
       "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
    ""))

(defun my/org-export-preprocessor (backend)
  (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

(use-package! org-roam
  :config
  (setq org-roam-directory "~/Dropbox/org/")
  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
  )

(add-to-list 'org-roam-capture-templates
    '("n" "new note" plain (function org-roam--capture-get-point)
       "%?"
       :file-name "${slug}"
       :head "#+HUGO_BASE_DIR: ../../Documents/code/yasushisakai.com/
#+HUGO_SECTION: memo
#+HUGO_SLUG: ${slug}
#+HUGO_DRAFT: true
#+TITLE: ${title}
#+DATE: %<%Y-%m-%d %H:%M>
#+LATEX_HEADER: \\usepackage{CJKutf8}"
       :unnarrowed t))

(setq org-directory "~/Dropbox/org/")

(defun org-journal-file-header-func(time)
  "Custom function to create journal header."
  (concat "#+HUGO_BASE_DIR: ../../../Documents/code/yasushisakai.com/\n#+HUGO_SECTION: memo\n#+HUGO_DRAFT: true\n#+LATEX_HEADER: \\usepackage{CJKutf8}\n#+TITLE: " (format-time-string "%F") (print time)))

(use-package! org-journal
  :init
  (setq
   org-journal-dir "~/Dropbox/org/journal"
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-date-format "%Y-%m-%d"
   ;; org-journal-date-format ""
   org-journal-file-header 'org-journal-file-header-func
   ))

;; set defalut bib file
(setq reftex-default-bibliography "~/Dropbox/org/bib/main.bib")
(setq bibtex-completion-library-path "~/Dropbox/papers/")
(setq bibtex-completion-pdf-field "File")

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-pandoc-citeproc)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))

(use-package! org-ref
  :after (org ivy-bibtex helm-bibtex)
  :init
  (when (featurep! :completion ivy)
    (setq org-ref-completion-library 'org-ref-ivy-cite))
  (when (featurep! :completion helm)
    (setq org-ref-completion-library 'org-ref-helm-bibtex))
  (setq org-ref-notes-directory "~/Dropbox/org/bibnotes"
        org-ref-default-bibliography '("~/Dropbox/org/bib/main.bib")
        bibtex-completion-bibliography '("~/Dropbox/org/bib/main.bib")
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 3
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

;; use rust-analyzer
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))

;; display time
(setq display-time-mode 1)

(setq org-image-actual-width '(300))

;; tidal
;; (setq load-path (cons "~/tidal/" load-path))
;; (require 'tidal)
;; (setq tidal-interpreter "/Users/yasushi/.ghcup/bin/ghci")
