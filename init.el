;; -*- lexical-binding: t -*-
(setq lexical-binding t)

;; Copyright 2022 Curtis Klassen

;; This program is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this
;; program. If not, see <https://www.gnu.org/licenses/>.

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
	 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; utility functions
(defun my/find-file (get-file-func)
  (let ((f (funcall get-file-func)))
    (when (ace-window t)
      (find-file f))))

(defun my/map-chars (f str as)
  (apply
   'concat
   (cl-loop for c across str
	    collect (funcall
                     f
	             (cond ((eq as :char)
		            c)
		           ((eq as :string)
		            (char-to-string c))
		           (t
		            (error "'as' must be either ':char' or ':string'")))))))

;; completions
(use-package vertico
  :straight t
  :init (vertico-mode))

(defadvice vertico-insert (after vertico-insert-add-history activate)
  "Make 'vertico-insert' add to the minibuffer history."
  (unless (eq minibuffer-history-variable t)
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

(use-package savehist
  :init (savehist-mode)
  :hook ((save-buffer . savehist-save)))

(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

(defun orderless/literal (pattern _index _total)
  (cond ((s-prefix? "`" pattern)
         (cons 'orderless-literal (substring pattern 1)))))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-flex
                               orderless-prefixes))
  (orderless-style-dispatchers '(orderless/literal)))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
	 ("C-x b" . consult-buffer)))

(use-package embark
  :straight t
  :bind (("C-c e a" . embark-act)
         ("C-c e b" . embark-become))
  :custom (embark-quit-after-action nil))

(use-package embark-consult
  :straight t)

(use-package cape
  :straight t)

(use-package corfu
  :straight t
  :init (global-corfu-mode)
  :custom (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-delay 0.1))

;; doom-nord
(use-package doom-themes
  :straight t
  :config (load-theme 'doom-nord t))

;; lisp
(defun lisp/edit-definition ()
  "Edit the symbol at point in the window selected by 'ace-window'"
  (interactive)
  (let ((sym (sly-symbol-at-point)))
    (when (ace-window t)
      (sly-edit-definition sym))))

(use-package sly
  :straight t
  :init (setq sly-lisp-implementations '((ccl ("~/Downloads/ccl-dev/lx86cl64"))
					 (sbcl ("sbcl"))))
  (add-to-list 'sly-contribs 'sly-fancy)
  :custom-face (sly-mrepl-output-face ((t (:foreground "#B48EAD"))))
  :bind (:map lisp-mode-map
              ("C-M-." . lisp/edit-definition)))

;; paredit
(defun override-slime ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key)
    nil))

(defun paredit-space-macros (endp delimiter)
  (or endp
      (if (eq (char-syntax delimiter) ?\()
	  (not (looking-back "#."))
	t)))

(use-package paredit
  :straight t
  :hook ((prog-mode . enable-paredit-mode)
	 (slime-repl-mode . enable-paredit-mode)
	 (slime-repl-mode . override-slime)
	 (sly-mrepl-mode . enable-paredit-mode))
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates 'paredit-space-macros))

;; modeline
(use-package minions
  :straight t
  :config (minions-mode 1))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  (doom-modeline-minor-modes t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; format-all
(use-package format-all
  :straight t)

;; tramp
(use-package tramp
  :straight t
  :init (autoload 'tramp-register-crypt-file-name-handler "tramp-crypt"))

;; magit
(defun magit/find-file ()
  "Find the file at point in the window selected by 'ace-window'"
  (interactive)
  (my/find-file (lambda () (magit-file-at-point t))))

(use-package magit
  :straight t
  :bind (:map magit-mode-map
         ("C-F" . magit/find-file)))

(use-package forge
  :straight t)

;; keepass
(use-package keepass-mode
  :straight t)

;; vterm
(use-package vterm
  :straight t)

;; ement
(use-package plz
  :straight (:type git
		   :host github
		   :repo "alphapapa/plz.el"))

(use-package ement
  :straight (:type git
		   :host github
		   :repo "alphapapa/ement.el"))

;; pdf-tools
(use-package pdf-tools
  :straight (pdf-tools
	     :type git
	     :host github
	     :repo "flatwhatson/pdf-tools"
	     :branch "fix-macros")
  :config (pdf-loader-install)
  :bind (:map pdf-view-mode-map
	      ("C-s" . 'isearch-forward-word)))

;; all-the-icons-dired
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

;; ace-window
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))
  :custom (aw-dispatch-always t))

;; dired
(defun dired/find-file ()
  "Find the file at point in the window selected by 'ace-window'"
  (interactive)
  (my/find-file #'dired-get-file-for-visit))

(use-package dired
  :bind (:map dired-mode-map
         ("C-F" . dired/find-file))
  :hook ((dired-mode . dired-hide-details-mode))
  :custom (dired-auto-revert-buffer t))

;; nethack
(defvar nethack/keymap (make-keymap))

(define-minor-mode nethack/mode
  "Mode to define Nethack keybinds for Dvorak
Doesn't work unless 'OPTIONS=number_pad:1' is set in '~/.nethackrc'"
  nil
  " nethack"
  nethack/keymap)

(defun nethack/make-move-func (key)
  (lambda ()
    (interactive)
    (vterm-send key)))

(defun nethack/add-key (key-from dir)
  (cl-labels ((lookup-direction (direction)
				(cl-case direction
				  (:down-left "1")
				  (:down "2")
				  (:down-right "3")
				  (:left "4")
				  (:right "6")
				  (:up-left "7")
				  (:up "8")
				  (:up-right "9")
				  (t (error "Supplied direction is not valid")))))
    (define-key nethack/keymap
      (kbd key-from)
      (nethack/make-move-func (lookup-direction dir)))))

(defun nethack/play ()
  (interactive)
  (if (get-buffer "nethack")
      (switch-to-buffer "nethack")
    (progn (vterm "nethack")
	   (switch-to-buffer "nethack")
	   (my/map-chars 'vterm-send "nethack" :string)
	   (vterm-send-return)))
  (nethack/mode 1))

(mapcar (lambda (ls) (apply 'nethack/add-key ls))
	'(("M-h" :left)
	  ("M-t" :down)
	  ("M-n" :up)
	  ("M-s" :right)
	  ("M-g" :up-left)
	  ("M-c" :up-right)
	  ("M-b" :down-left)
	  ("M-m" :down-right)))

;; eshell
;; https://www.emacswiki.org/emacs/EshellPrompt
(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
with single characters starting from the left to try and get
the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun rjs-eshell-prompt-function ()
  (concat (shortened-path (eshell/pwd) 10)
          (if (= (user-uid) 0) " # " " $ ")))

;; https://emacs.stackexchange.com/questions/12503/how-to-clear-the-eshell
(defun my/eshell-run (cmd)
  "Runs the command 'cmd' in eshell"
  (with-current-buffer "*eshell*"
    (end-of-buffer)
    (eshell-kill-input)
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (eshell-bol)
    (yank)
    (message (format "Ran in Eshell: %s" cmd))))

(use-package eshell
  :custom (eshell-prompt-function 'rjs-eshell-prompt-function)
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  :bind (("C-c s <backspace>" . (lambda ()
                                  (interactive)
                                  (my/eshell-run "clear 1")))
         ("C-c s s" . (lambda ()
                        (interactive)
                        (switch-to-buffer "*eshell*")))))

;; Emojify
(use-package emojify
  :straight t
  :init (emojify-set-emoji-styles '(unicode)))

;; UI
(tool-bar-mode 0)
(menu-bar-mode 0)
(fringe-mode 0)
(scroll-bar-mode 0)

(window-divider-mode 1)
(blink-cursor-mode 1)
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

(add-to-list 'default-frame-alist '(font . "Rec Mono Semicasual-24"))
(add-to-list 'default-frame-alist '(alpha . (95 . 85)))

(setq display-line-numbers-type t)
(setq column-number-mode t)

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-string-face nil :slant 'italic)

;; other settings
(setq warning-minimum-level :emergency)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(global-auto-revert-mode 1)

(setq tab-width 4)

(setq-default fill-column 85)
(setq-default auto-fill-function nil)

(setq-default org-src-fontify-natively t)

(setq-default indent-tabs-mode nil)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(setq initial-scratch-message ";; Welcome to Emacs !!\n\n")
(setq org-startup-folded 'content)
(setq org-blank-before-new-entry nil)

(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

;; insert characters
(defvar special-chars (let ((chars '(("λ" . "lambda")
                                     ("…" . "ellipsis")
                                     ("—" . "emdash")
                                     ("™" . "tm")
                                     ("‽" . "interrobang")
                                     ("é" . "e : acute")
                                     ("ï" . "i : diaresis")))
                            (others '(("🅱️" . "Meme B"))))
                        (mapcar (lambda (char-cons)
                                  (format "'%s' - (%s)"
                                          (car char-cons)
                                          (cdr char-cons)))
                                (append chars others))))

(defun insert-special-char (arg)
  "Prompt for a special character and insert it; if called with a
prefix, the character will be inserted that many times. If the
prefix is negative (M--), the character will additionally be
capitalized"
  (interactive "p")
  (let ((char (substring (car (s-match "^'.*'"
                                       (completing-read "Character to insert: "
                                                        special-chars)))
                         1
                         -1)))
    (dotimes (_i (abs arg))
      (insert (if (< arg 0)
		  (upcase char)
		char)))))

(global-set-key (kbd "C-c 8 i") 'insert-special-char)

;; other keybinds
(global-set-key (kbd "C-x M-;") 'comment-line)
(global-set-key (kbd "C-M-f") 'forward-to-word)
(global-set-key (kbd "C-c j") 'just-one-space)
