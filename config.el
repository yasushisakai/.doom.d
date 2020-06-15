;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yasushi Sakai"
      user-mail-address "ysshski@gmail.com")

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
(setq doom-font (font-spec :family "HackGen35 Console" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/yasushi/Dropbox/org")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-image-actual-width '(300))

(setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-roam
  :config
  (setq org-roam-directory "/Users/yasushi/Dropbox/org")
  (setq org-roam-capture-templates
  '(("n" "new note" plain (function org-roam--capture-get-point)
       "%?"
       :file-name "${slug}"
       :head "#+TITLE: ${title}
#+DATE: %<%Y-%m-%d %H:%M>
:DRAWER:
#+HUGO_BASE_DIR: ../../Documents/code/yasushisakai.com/
#+HUGO_SECTION: memo
#+HUGO_SLUG: ${slug}
#+HUGO_DRAFT: true
#+HUGO_LOCALE: ja
#+LATEX_HEADER: \\usepackage{CJKutf8}
:END:"
       :unnarrowed t)))
)

;; gets a list of backlinks
(defun org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                                 (org-roam--get-title-or-slug (car it))))
       "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
    ""))

;; hook function to add 'Backlinks' when exporting
(defun org-export-preprocessor-roam-append-backlinks (backend)
  (let ((links (org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

;; register hook
(add-hook 'org-export-before-processing-hook 'org-export-preprocessor-roam-append-backlinks)

(setq org-roam-db-location "/Users/yasushi/Utility/roam_db.sql")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-ref
  :init
  (setq reftex-default-bibliography '("/Users/yasushi/Dropbox/org/bib/main.bib"))
  (setq org-ref-notes-function 'org-ref-notes-function-many-files)
  (setq org-ref-bibliography-notes '(org-directory))
  (setq org-ref-default-bibliography '("/Users/yasushi/Dropbox/org/bib/main.bib")
        org-ref-pdf-directory "/Users/yasushi/Dropbox/papers/")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! helm-bibtex
  :commands (helm-bibtex)
  :init
  (setq bibtex-completion-bibliography '("/Users/yasushi/Dropbox/org/bib/main.bib")) ;; you can other files
  (setq bibtex-completion-notes-path org-directory)
  ;; tries to look for the File field that points to the location of pdfs
  (setq bibtex-completion-pdf-field "File")
  (map! :leader
        :prefix "r"
        :desc "Helm Bibtex" "h" #'helm-bibtex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam-bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; use org-noter to add notes
(use-package! org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions)))
  :config
  (setq orb-templates '(("r" "ref note" plain (function org-roam--capture-get-point)
       "%?"
       :file-name "${slug}"
       :head "#+TITLE: Notes on \"${title}\"
#+DATE: %<%Y-%m-%d %H:%M>
:DRAWER:
#+ROAM_KEY: ${ref}
#+HUGO_BASE_DIR: ../../Documents/code/yasushisakai.com/
#+HUGO_SECTION: memo
#+HUGO_SLUG: ${slug}
#+HUGO_DRAFT: true
#+LATEX_HEADER: \\usepackage{CJKutf8}
:END:

* Notes:
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-journal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-journal
  ;; :ensure t
  ;; :defer t
  :config
  (setq org-journal-dir "/Users/yasushi/Dropbox/org/journal")
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-file-type "monthly")
  (setq org-journal-date-format " log_%Y-%m-%d
:PROPERTIES:
:EXPORT_FILE_NAME: %Y-%m-%d
:EXPORT_DATE: %Y-%m-%d
:END:
")
  (setq org-journal-date-prefix "** TODO")
  (setq org-journal-time-format " %H:%M")
  (setq org-journal-time-prefix "***")
  (setq org-journal-file-header "#+TITLE: Journal: %Y-%m
#+DATE: %Y-%m-01 00:00
:DRAWER:
#+HUGO_BASE_DIR: ../../../Documents/code/yasushisakai.com/
#+HUGO_SECTION: journal
:END:

* %Y/%m

"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-anayzer-server-command '("~/.cargo/bin/rust-analyzer"))
