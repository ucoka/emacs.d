;;;
; init.el
;
; Last Update: 2023/08/15 17:07:21
;; This file is saved as iso-2022-7bit
;;;;
;;; Code:

;---- 0. load-path ----
(let ((default-directory (expand-file-name "~/.emacs.d/local-lisp")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	(normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'load-path "~/.emacs.private.d")

;(setenv "PATH" (concat "c:/msys64/mingw64/bin;c:/msys64/usr/bin;" (getenv "PATH")))

(if (and (eq system-type 'gnu/linux) (string-match "Ubuntu" (shell-command-to-string "lsb_release -d")))
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/global")
    )

; ---- 1. startup frame, mode line ----

;; frame-setup
(setq ls-lisp-dirs-first t) ;; display directories first with dired
(tool-bar-mode -1) ; Show/hide toolbar with icons (t[default]/nil)
(auto-image-file-mode) ; display image files; use find-file to see images
(menu-bar-mode -1) ;show/hide menu bar (t[default]/nil)

(setq-default cursor-in-non-selected-windows nil) ; show/not show cursor in other windows

(let ((my-background-color))
  (if (featurep 'w32) ; NTEmacs
      (setq my-background-color "dark slate gray")
    (setq my-background-color "gray15"))

  (setq default-frame-alist
        (append
         (list
          '(foreground-color . "peach puff") ;text-color
          `(background-color . ,my-background-color) ;background-color
          '(top . 0) ;frame top-left position Y-coordinate
          '(left . 0) ;frame top-left position X coordinate
          '(vertical-scroll-bars . nil) ;scroll-bars left/right/hide (left[default]/right/nil)
          '(cursor-color . "white") ;cursor color (can be set for box/bar)
          '(line-spacing . 0)
          )
         default-frame-alist)))
;;

; determine the default width and height of the frame from the screen resolution
(let (my-width my-height)
  (if (featurep 'w32) ; NTEmacs
      (cond ((and (>= (x-display-pixel-width) 2560) (>= (x-display-pixel-height) 1440))
             (setq my-width 316)
             (setq my-height 76))
            ((and (eq (x-display-pixel-width) 1680) (eq (x-display-pixel-height) 1050))
             (setq my-width 205)
             (setq my-height 54))
            (t
             (setq my-width 165)
             (setq my-height 57))
            )
    (cond ((and (>= (x-display-pixel-width) 2560) (>= (x-display-pixel-height) 1440))
           (setq my-width 210)
           (setq my-height 56))
          ((and (eq (x-display-pixel-width) 1680) (eq (x-display-pixel-height) 1050))
           (setq my-width 210)
           (setq my-height 56))
          (t
           (setq my-width 210)
           (setq my-height 56))
          ))

  (setq default-frame-alist
        (append `((width . ,my-width) (height . ,my-height)) default-frame-alist)))

; Fixed a bug that the size of the initial display frame was not set to the default-frame-alist setting size. (NTEmacs23.2)
(add-hook 'window-setup-hook
          (lambda ()
            (modify-frame-parameters (selected-frame) default-frame-alist)))

;;mode-line setting
(modify-face 'mode-line "LightGoldenrod1" "DeepSkyBlue4" nil nil nil nil) ;;color
(line-number-mode t) ;show how many lines the cursor is on (default)
(column-number-mode 1)
(setq display-time-24hr-format t) ;in 24-hour notation,
(display-time) ;display time in mode line
;

; ---- 2. Japanese language environment ----
(set-language-environment "Japanese")

(unless (featurep 'meadow)
  (set-keyboard-coding-system 'japanese-shift-jis)

  (dolist (coding-system '(sjis euc-jp iso-2022-jp iso-2022-7bit shift_jis-2004 euc-jis-2004 iso-2022-jp-2004))
    (coding-system-put coding-system :decode-translation-table 'japanese-ucs-jis-to-cp932-map)
    (coding-system-put coding-system :encode-translation-table 'japanese-ucs-cp932-to-jis-map))

  ;; encode-translation-table setting
  (coding-system-put 'euc-jp      :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'iso-2022-jp :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'cp932       :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  ;; Set charset and coding-system priority
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201 'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)
)

(set-locale-environment "C") ; in order to set the week of a day in English like "Sunday"

; ---- 3. Windows IME ----
(setq quail-japanese-use-double-n t)

; ---- 4. font ----
(if (featurep 'w32) ; NTEmacs
    (progn
      (progn
        (add-to-list 'default-frame-alist '(font . "Consolas-11"))
        (add-to-list 'default-frame-alist '(line-spacing . 0))
        (add-to-list 'default-frame-alist '(letter-spacing . 0.1)  )
        (add-to-list 'default-frame-alist '(cursor-type . box)     )
        (add-to-list 'default-frame-alist '(cursor-color . "white"))
        (add-to-list 'default-frame-alist '(cursor-height . 2)     )
        (add-to-list 'default-frame-alist '(default-text-properties . '(line-height 1.2)))
        (add-to-list 'default-frame-alist '(font-lock-global-modes . '(not text-mode)))
      )))

;---- 5. key-set ---
;;<function-key>
;[f1] defaults to help-command

(define-key global-map "\C-c\C-r" 'replace-string) ; (custom)
(define-key global-map "\C-c\C-s" 'shell-command) ; (custom)
(global-set-key "\C-c;r" 'revert-buffer) ; (custom) update-file
(global-set-key "\C-cu" 'revert-buffer) ; (custom) update file
(define-key global-map "\C-x\C-b" 'buffer-menu) ; (custom)

(define-key global-map [?\C-,] 'scroll-down)       ; (custom) scroll-down
(define-key global-map "\M-v" 'scroll-down)        ; (custom) scroll-down


;; C-x 2,C-x 3 when window is split by C-x 2,C-x 3
(define-key global-map [?\C-\;] 'other-window) ;Move between divided windows (forward direction)
(define-key global-map [?\C-:]
  #'(lambda (arg) (interactive "p") (other-window (- arg)))) ;move between divided windows (negative direction))

(define-key global-map [C-S-tab] 'other-frame) ;(custom)
(define-key global-map [C-tab] 'other-frame) ;(custom)

(define-key global-map [C-backspace] 'backward-kill-word) ; (custom)
(define-key global-map "\C-xk" 'kill-current-buffer) ; (default)
(define-key global-map [C-delete] 'kill-current-buffer) ; (my assign / my function)

(define-key global-map [C-S-delete] 'my-dired-kill-dired-buffers) ;delete all directory entries from buffers
(define-key global-map "\C-c;d" 'my-dired-kill-dired-buffers) ;delete all directory entries from buffer
;
(define-key global-map "\C-t" 'scroll-down)          ; (custom)
;
(define-key global-map "\M-g" 'goto-line)
;
(define-key global-map "\C-h" 'backward-delete-char-untabify)    ; (custom)

(define-key global-map "\C-c\C-f" 'up-list) ;(custom) move to corresponding parenthesis (down)
(define-key global-map "\C-c\C-o" 'backward-up-list) ;(custom) move to the corresponding parenthesis (upward)

(define-key global-map "\M-?" 'help-command) ;(custom) help-command


;;;mode-switching
(define-key global-map "\C-cmh" 'hide-ifdef-mode) ; hide-ifdef-mode
(define-key global-map "\C-cmx" 'hexl-mode)       ; hexl-mode
(define-key global-map "\C-cmg" 'gtags-mode)      ; gtags-mode


;; [global unset key]
(global-unset-key "\e\e") ; Disable ESC ESC
(global-unset-key "\C-x\C-n") ; Disable narrow-to-region (if default)
(global-unset-key "\C-x\C-u") ; Prevent upcase-region (by default)
;;

;;;==== 6. Set display color (specifying font-lock-mode) ====
(global-set-key "\C-c;l" 'global-font-lock-mode)

(define-key global-map "\C-l" 'update-buffer-display) ;recenter(default)

(global-font-lock-mode t)

(setq font-lock-support-mode
      '(
        (t        . jit-lock-mode)))

;
;; Some new Colors for Font-lock.
(setq font-lock-mode-maximum-decoration t)
(require 'font-lock)

(setq font-lock-use-default-fonts nil)
(setq font-lock-use-default-colors nil)

; comment
(set-face-foreground 'font-lock-comment-face "orange")
(set-face-bold-p 'font-lock-comment-face nil)

; string
(set-face-foreground 'font-lock-string-face "SkyBlue")
(set-face-bold-p 'font-lock-string-face nil)

; function-name
(set-face-foreground 'font-lock-function-name-face "DodgerBlue")
(set-face-bold-p 'font-lock-function-name-face t)

; keyword
(set-face-foreground 'font-lock-keyword-face "SteelBlue")
(set-face-bold-p 'font-lock-keyword-face nil)

; type-name
(set-face-foreground 'font-lock-type-face "Green")
(set-face-bold-p 'font-lock-type-face t)

; variable-name
(set-face-foreground 'font-lock-variable-name-face "Yellow")
(set-face-bold-p 'font-lock-variable-name-face nil)

; warning ??
(set-face-foreground 'font-lock-warning-face "Red")
(set-face-bold-p 'font-lock-warning-face nil)

; const (case labels, etc.)
(set-face-foreground 'font-lock-constant-face "Red")
(set-face-bold-p 'font-lock-constant-face nil)

; (include,ifdef etc.)
(set-face-foreground 'font-lock-builtin-face "magenta")
(set-face-bold-p 'font-lock-builtin-face t)

;(add-hook 'font-lock-mode-hook
;	  '(lambda ()
;	     (local-set-key "\C-l" 'font-lock-fontify-buffer)
;	     ))



;;; Highlight "tabs" and "full-width spaces" and "spaces and tabs before line breaks".
(defface my-face-b-1 '((t (:background "#375757"))) nil) ; "full-width spaces"
(defface my-face-b-2 '((t (:background "#375757"))) nil) ; "tabs"
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil) ; "space or tab before newline"

(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
;;;;

;;;;; Automatic insertion of last-modified date
;;;; If within 8 lines (default, but can be changed) from the beginning of the file, you can write Time-stamp: <> or
;;;; Time-stamp: " ", the date will be automatically inserted when saving the file.
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
          (cons 'time-stamp write-file-hooks)))
(setq time-stamp-active t)
(setq time-stamp-line-limit 20) ;Number of lines for which time-stamp rewrite is active
;
(setq time-stamp-start "Last Update:")
;
;;Time-stamp format
(setq time-stamp-format " %Y/%02m/%02d %02H:%02M:%02S")
(setq time-stamp-end "$")
;;


;---- dired ----
;;; display the full path or buffer name of the currently open file in the title bar
(defvar dired-mode-p nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (make-local-variable 'dired-mode-p)
	    (setq dired-mode-p t)))
(setq frame-title-format-orig frame-title-format)
(setq frame-title-format '((buffer-file-name "%f" (dired-mode-p default-directory mode-line-buffer-identification))))

;;; wdired
(when (locate-library "wdired")
  (require 'wdired)
  (define-key global-map "\C-cmd" 'wdired-change-to-wdired-mode)
)
;

;; bubble-buffer ()
; Buffer Switcher, which switches buffers sequentially.
;
(defvar LIMIT 1)
(defvar time 0)
(defvar mylist nil)
;
(defun time-now ()
   (car (cdr (current-time))))
;
(defun bubble-buffer ()
  (interactive)

  (if (or (> (- (time-now) time) LIMIT) (null mylist))
      (progn (setq mylist (copy-alist (buffer-list)))
	     (delq (get-buffer " *Minibuf-0*") mylist)
	     (delq (get-buffer " *Minibuf-1*") mylist)))

  (bury-buffer (car mylist))
  (setq mylist (cdr mylist))
  (setq newtop (car mylist))
  (switch-to-buffer (car mylist))
  (setq rest (cdr (copy-alist mylist)))
  (while rest
    (bury-buffer (car rest))
    (setq rest (cdr rest)))
  (setq time (time-now)))
;
;;


;---- kill-buffer (omit confirmation) ----
(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))
;

;---- default mode for new buffers ----
(setq default-major-mode 'text-mode)
(setq text-mode-hook
      '(lambda ()
	 (auto-fill-mode 0)
	 ))

;;;

;---Scroll settings ---
; Number of lines to scroll when the cursor moves up and down out of the window.
; 0 means always center (scroll-step is an old variable, not used now). It is not used now.)
(setq scroll-conservatively 1)
(setq scroll-margin 4)

;
;;;

;--- GNU GLOBAL(gtags) gtags.el ---
;new setting
(when (locate-library "gtags")
  (require 'gtags)

  (global-set-key "\M-t" 'gtags-find-tag) ;To the function definition source
  (global-set-key "\M-r" 'gtags-find-rtag) ;To function reference
  (global-set-key "\M-s" 'gtags-find-symbol) ;To variable definition source/reference
  (global-set-key "\M-f" 'gtags-find-file)
  (global-set-key "\M-p" 'gtags-find-pattern)
  (global-set-key [?\C-,] 'gtags-pop-stack)    ;Return to previous buffer

  (setq gtags-mode-hook
        '(lambda ()
           (setq gtags-select-buffer-single t)
;           (setq gtags-folllows-case-fold-search t)
           ))

  (add-hook 'gtags-select-mode-hook
            '(lambda ()
               (setq hl-line-face 'underline)
               (hl-line-mode 1)
               ))
)

;---- gtags_ex ----
(when (locate-library "gtags_ex")
  (require 'gtags_ex)
  (global-set-key "\C-cgu" 'gtags_ex-update)
  (defadvice save-buffer (after after-save-buffer ())
    "after save-buffer process"
    (progn
      (gtags_ex-update-on-background))
    )
  (ad-activate 'save-buffer)
  )

;---- hexl-mode ----
(setq auto-mode-alist
      (append '(
                ("\\.bin$" . hexl-mode) ; .bin file : hexl-mode
                ("\\.prg$" . hexl-mode) ; .prg file : hexl-mode
                ) auto-mode-alist))


;--- c-mode ----
(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(setq auto-mode-alist
      (append '(
                ("\\.h$" . c-mode) ; .h file : c-mode
                ("\\.cmn$" . c-mode) ; .cmn file : c-mode
                ) auto-mode-alist))
;
(defconst my-style
  '(
	(c-offsets-alist . (
                        (comment-intro . 4)
                        (case-label . *)           ; or 2
                        (statement-case-intro . *) ; or 2
                        ;(access-label . 0)
                        ))
    )
  "My Style")
;
(add-hook
 'c-mode-hook
 '(lambda()
    (c-set-style "k&r") ;style for "Programming Language C (aka K&R)
    (when (locate-library "gtags")
      (gtags-mode 1) ;gtags-mode on when c-mode
      )
    (hide-ifdef-mode 1) ;in c-mode, hide-ifdef-mode is on
    (hs-minor-mode 1)
;
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)

;    (semantic-mode 1)
))
;;;

(when (locate-library "electric")
  (setq electric-indent-mode nil)
  )

;------ Using the shell from Emacs -----
(global-set-key "\C-cR" 'rename-buffer)
;
;;; If you use Cygwin's bash
(if (featurep 'w32) ; NTEmacs
    (progn
      (setq explicit-shell-file-name "c:/msys64/usr/bin/bash.exe") ;; For shell mode
      (setq shell-file-name "c:/msys64/usr/bin/sh.exe") ; for shell commands
      (setq shell-command-switch "-c")
))
;

;;; argument-editing configuration
(when (locate-library "mw32script")
  (require 'mw32script)
  (mw32script-init)
)

;
;;; coding-system configuration
(add-hook 'shell-mode-hook
          (lambda ()
;       (set-buffer-process-coding-system 'undecided-dos 'sjis-unix) ; which was OK in Meadow 1.15
;       (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)   ; Meadow 2.XX and later must do this
;       (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix) ; cygwin 1.7 support
;       (set-buffer-process-coding-system 'sjis-unix 'utf-8-unix) ; cygwin 1.7 support
;       (set-buffer-process-coding-system 'utf-8-unix 'sjis-unix) ; cygwin 1.7 support
        )
      )

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; ^Take M
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;
;;; completion in shell-mode (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
;

;---- shell-command (M-!) ----
(when (locate-library "shell-command")
  (require 'shell-command) ; M-! completion with shell-command in (shell-command.el)
)

; ---- Backup ----
;
(setq make-backup-files nil) ;create backup files or
(setq backup-inhibited t)
;
(defconst my-backup-save-dir "~/trash")

(if (file-exists-p my-backup-save-dir)
 (define-key global-map "\C-x\C-s" 'my-backup-save2) ;Create backups in a specific directory.
)

;;
;; my-backup-save-dir/%y%m%d/ filename. Create backup at %H%M%S.
;;
; (ex) ~/trash/031020/test.txt.143300 (save test.txt at 2003/10/20 14:33:00)
;;
(defun my-backup-save ()
(interactive)
(let( (my-backup-save-path (directory-file-name (expand-file-name (concat my-backup-save-dir "/" (format-time-string "%y%m%d" (current-time))))))
      )
  (unless (file-exists-p my-backup-save-path) ;create only if it does not exist
      (make-directory my-backup-save-path)
      )
  (if (buffer-modified-p (current-buffer))
      (append-to-file 1 (1+ (buffer-size))
		      (expand-file-name
		       (concat (car (split-string (buffer-name) "<")) "." (format-time-string "%H%M%S" (current-time)))
		       my-backup-save-path)))
  (save-buffer)
    )
)
;;

;;
;; Create backup at my-backup-save-dir/%Y/%m/%d/%H%M%S. filename.
;;
; (ex) ~/trash/2003/10/20/143300.test.txt (save test.txt at 2003/10/20 14:33:00)
;;
(defun my-backup-save2 ()
(interactive)
(let(
     (my-backup-save-path-year  (directory-file-name (expand-file-name (concat my-backup-save-dir "/" (format-time-string "%Y" (current-time)) ))))
     (my-backup-save-path-month (directory-file-name (expand-file-name (concat my-backup-save-dir "/" (format-time-string "%Y" (current-time)) "/" (format-time-string "%m" (current-time)) ))))
     (my-backup-save-path-day   (directory-file-name (expand-file-name (concat my-backup-save-dir "/" (format-time-string "%Y" (current-time)) "/" (format-time-string "%m" (current-time)) "/" (format-time-string "%d" (current-time))))))
      )
  (unless (file-exists-p my-backup-save-path-year) ;create only if it does not exist
      (make-directory my-backup-save-path-year)
      )

  (unless (file-exists-p my-backup-save-path-month) ;create only if it does not exist
      (make-directory my-backup-save-path-month)
      )

  (unless (file-exists-p my-backup-save-path-day) ;create only if it does not exist
      (make-directory my-backup-save-path-day)
      )

  (if (buffer-modified-p (current-buffer))
      (append-to-file 1 (1+ (buffer-size))
		      (expand-file-name
		       (concat (format-time-string "%H%M%S" (current-time)) "." (car (split-string (buffer-name) "<")))
		       my-backup-save-path-day)))
  (save-buffer)
;  (gtags_ex-update-on-background)
    )
)
;;


;---- kill/yank ----
;;Do not put multiple strings with the same content in a kill-ring.
;;(If the content is already in kill-ring when you do kill-ring-save etc., the content will be merged into one string at the top of kill-ring,
;;kill-ring-save, etc., if the content is already in the kill-ring, it will be combined into one string at the top of the kill-ring).
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))
;
;;browse-kill-ring.el
(when (locate-library "browse-kill-ring")
  (require 'browse-kill-ring)
  (global-set-key (kbd "C-c y") 'browse-kill-ring)
)
;
(global-set-key "\M-y" 'yank-pop-forward)
(global-set-key "\C-\M-y" 'yank-pop-backward)

;---- isearch ----
;While isearch is running, press C-k to edit the string in the minibuffer
;C-k to edit the string in the minibuffer.
;This allows you to use Japanese characters as well.
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;;highlight all matched strings in i-search (requires ishl.el)
(and window-system
     (not (boundp 'isearch-lazy-highlight-search))
     (locate-library "ishl")
     (autoload 'ishl-mode "ishl" nil t)
     (setq ishl-initial-delay 0.25
           ishl-delay 0.05)
     (add-hook 'isearch-mode-hook
               #'(lambda () (ishl-mode 1))))


;---- bookmark ----
(setq bookmark-save-flag 1)
(let ((my-bookmark-file "~/.emacs.private.d/.emacs.bmk"))
       (when (file-exists-p my-bookmark-file)
         (setq bookmark-default-file my-bookmark-file)))

;---- other ----
(transient-mark-mode 1) ;highlight-region
(show-paren-mode 1) ;highlight corresponding parentheses
(setq next-line-add-newlines nil) ;Disallow adding new lines with newline at the end of buffer
(setq kill-whole-line t) ;kill-line(C-k) to include newlines at the end of a line
(setq visible-bell t) ;Flash the screen instead of sounding (visible bell)
(setq-default auto-save-interval 1000) ;Number of keyboard input characters between auto-saves (default:300)
(setq truncate-partial-width-windows nil) ;Wrap off-screen text when splitting window

; display part of the path of the open file to distinguish between buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;

;;
; display line numbers on the left side
;(require 'wb-line-number)
;(wb-line-number-toggle)

;---- redo ----
;(when (locate-library "redo")
;  (require 'redo)
;  (define-key global-map [?\C-.] 'redo)
;)
;;
(define-key global-map [?\C-.] 'undo-redo)

;;;; When region is active, use Backspace to remove it
(when transient-mark-mode
  (defadvice backward-delete-char-untabify
    (around ys:backward-delete-region activate)
    (if mark-active
        (delete-region (region-beginning) (region-end))
      ad-do-it)))
;;

;---- abbrev (abbreviation expansion) ----
;(static)
(cond
 ((file-exists-p "~/.abbrev_defs") ; only if .abbrev_defs exists in home directory
  (setq abbrev-file-name "~/.abbrev_defs")
  (read-abbrev-file)
    )
 )
(setq save-abbrevs t) ; Set to save .abbrev_defs when exiting Emacs
;
;(dynamic)

(define-key global-map "\C-c\C-j" 'dabbrev-expand)  ; (custom)

(define-key global-map "\C-j" 'dabbrev-expand)  ; (custom)

(setq dabbrev-abbrev-char-regexp "\\w\\|\\s_") ;Use Japanese
(setq dabbrev-case-replace nil) ; Set case
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auto Lookup
;;;
;; Programs used to obtain translations
(setq auto-lookup-backend 'sdic)
; (setq auto-lookup-backend 'sdic)

;
;; auto-lookup-mode, global-autolookup-mode
(autoload 'auto-lookup-mode "autolookup"
          "Minor mode to automatically display translations of words under point" t)
(autoload 'global-auto-lookup-mode "autolookup"
          "Activate a minor mode on all buffers that automatically displays the translation of the word below the point.
Activate on all buffers." t)
;
;; Time (in seconds) before displaying the translation
(setq auto-lookup-interval 1)
;
(global-set-key "\C-cml" 'auto-lookup-mode)
;;; Auto Lookup configuration ends here


;----ediff----
(global-set-key "\C-cdd" 'ediff)
(global-set-key "\C-cdr" 'ediff-revision)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; don't show toolbar in a separate frame
(setq-default ediff-auto-refine-limit 10000) ; maximum bytes to highlight changes
(setq ediff-split-window-function 'split-window-horizontally)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
 '(ediff-fine-diff-A ((t (:foreground "Navy" :background "sky blue"))))
 '(ediff-fine-diff-B ((t (:foreground "Black" :background "cyan")))))



;;

;;---- grep-find ----
(setq grep-find-command "/bin/find . -name \"*.c\" -o -name \"*.h\" -o -name \"*.cmn\" -o -name \"*.mar\" -o -name \"*.i\" -o -name \"*.inc\" -o -name \"*.sys\" -o -name \"*.mod\" -o -name \"*.map\" -o -name \"*.conf\" -o -name \"*.src\" -o -name \"*.el\" | xargs grep -n ")
;;
(global-set-key "\C-cgg" 'grep-find)


;; Save settings and exit. Load on startup.
;(desktop-load-default)
;(desktop-read)
;; The first time you do this, you need to save the state explicitly with M-x desktop-save.
;;

;---- disable function ----
(put 'narrow-to-region 'disabled nil) ;disable narrowing (only allow access to region selections)

;---- function redefined ----
;previous-line (arg)
;defun warning when moving the cursor up at the beginning of a buffer
(defun previous-line (arg)
 (interactive "p")
 (condition-case nil
  (line-move (- arg))
  (beginning-of-buffer)))
;
;;

; ---- custom function ----
(define-key global-map "\C-x7" 'insert-date)       ; (custom)
;
(defun insert-date ()
  "Insert the date."
  (interactive)
  (shell-command "date '+%y%m%d(%k:%M)'" t)
  (end-of-line)
  (delete-char 1)
)
;

;; my-dired-kill-dired-buffers ()
; kill all Dired-mode buffers in buffer-list
;
(defun my-dired-kill-dired-buffers ()
  "Remove dired-mode buffers."
  (interactive)
  (let ((bl (buffer-list)) (kill-list '()))
    (setq temp-buffer (current-buffer))
    (while bl
      (set-buffer (car bl))

      (if (and (stringp mode-name) (string-match "Dired by \\(name\\|date\\)" mode-name))
	  (add-to-list 'kill-list (car bl)))

      (setq bl (cdr bl)))
    (while kill-list
      (kill-buffer (car kill-list))
      (setq kill-list (cdr kill-list))))
  )
;
;;


;;;;;
; Start shell. Appending date and time to buffer name at startup allows multiple startups.
; 11/01/2002 8:11:36 PM
(defun newshell ()
  ""
  (interactive)
  (shell)
  (rename-buffer (concat "*shell" (format-time-string "%y%m%d-%H%M%S" (current-time)) "*" ) )
  )
;;;

;; Start/switch shell
(global-set-key "\C-cs" 'char_select_shell)
(defun char_select_shell (arg)
  ""
  (interactive "c")
  (let ((my-shell-name))
    (setq my-shell-name (concat "*shell(" (char-to-string arg) ")*"))
    (if (eq (get-buffer my-shell-name) nil) ;if shell does not exist, start and name it
    (progn
      (shell)
      (rename-buffer my-shell-name)
      )
      (switch-to-buffer (get-buffer my-shell-name)) ;if it exists, just switch
      )
    )
  )

(defun char_select_eshell (arg)
  ""
  (interactive "c")
  (let ((my-shell-name))
    (setq my-shell-name (concat "*eshell(" (char-to-string arg) ")*"))
    (if (eq (get-buffer my-shell-name) nil) ;if shell does not exist, start and name it
    (progn
      (eshell)
      (rename-buffer my-shell-name)
      )
      (switch-to-buffer (get-buffer my-shell-name)) ;if it exists, just switch
      )
    )
  )

;--- Window-related ---
(define-key global-map [S-left]  'shrink-window-horizontally) ;Resize split window (shift + <-)
(define-key global-map [S-right] 'enlarge-window-horizontally) ;resize split window (shift +->)
(define-key global-map [S-up] 'shrink-window) ;resize divided window (shift + up)
(define-key global-map [S-down] 'enlarge-window) ;resize divided window (shift + down)
;

;;; recenter + font-lock-fontify-buffer (not used)
(defun my-recenter-and-fontify-buffer ()
  (interactive)
  (recenter)
  (font-lock-fontify-buffer))
;;



; ---- personal settings ----

;--- remove "Encoded-kbd" from mode-line ----
(let ((elem (assq 'encoded-kbd-mode minor-mode-alist)))
  (when elem
    (setcar (cdr elem) "")))

;;reverse the upcase-word and downcase-word arguments +/-.
(define-key global-map "\M-u" 'my-upcase-word) ;Convert the word before the cursor to upper case
(define-key global-map "\M-l" 'my-downcase-word) ;Convert word before cursor to lowercase
;M-- M-u to convert word behind cursor to upper case
;M-- M-l to lowercase the word behind the cursor
(defun my-upcase-word (arg)
  (interactive "p")
  (upcase-word (- arg))
)
;
(defun my-downcase-word (arg)
  (interactive "p")
  (downcase-word (- arg))
)
;;

;=== occur ===
(when (locate-library "color-occur")
  (load "color-occur")
)

;=== w3m ===
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
(autoload 'w3m-weather "w3m-weather" "Display weather report." t)
(autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)

(add-hook 'w3m-mode-hook
	  '(lambda ()
	     (set-face-foreground 'w3m-anchor-face "DodgerBlue")
	     (set-face-foreground 'w3m-arrived-anchor-face "SteelBlue")
	     ))
;;;
(global-set-key "\C-cmw" 'w3m)


;;=== speedbar ===
(define-key global-map "\C-cbs" 'speedbar)
(define-key global-map "\C-cbb" 'speedbar-get-focus)
(define-key global-map "\C-cba" 'speedbar-toggle-show-all-files)

(add-hook 'speedbar-load-hook (lambda ()
                                (require 'semantic/sb)
                                (setq speedbar-update-flag nil)
                                (speedbar-disable-update)
                                (define-key global-map "\C-c:" 'speedbar-get-focus)
;                                (define-key global-map "\C-cg" 'speedbar-update-contents)
                                ))

;;=== mew ===
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

(setq mew-auto-get nil) ;; Don't fetch mail on startup

(setq mew-icon-directory "c:/usr/mew-3.3/etc")

  ;; Optional setup (Read Mail menu for Emacs 21):
  (if (boundp 'read-mail-command)
      (setq read-mail-command 'mew))

  ;; Optional setup (e.g. C-xm for sending a message):
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))

(setq mew-user "username")
(setq mew-mail-domain "example.co.jp")
(setq mew-smtp-server "smtp.example.co.jp")  ;; if not localhost
(setq mew-pop-user "user")                   ;; (user-login-name)
(setq mew-pop-server "pop3.example.co.jp")   ;; if not localhost
(setq mew-pop-auth 'pass)

(setq mew-use-cached-passwd t) ;;temporarily store passwords
(setq mew-passwd-reset-timer nil)
(setq mew-use-timer t) ; Password storage time

(setq mew-pop-delete nil) ;;;; Leave mail on server


;;; === mhc ====
;Mew (since 1.94) user: ;Mew (since 1.94) user: ;Mew (since 1.94) user
(setq mhc-mailer-package 'mew)
(autoload 'mhc-mode "mhc" nil t)
(add-hook 'mew-summary-mode-hook 'mhc-mode)
(add-hook 'mew-virtual-mode-hook 'mhc-mode)
(add-hook 'mew-message-hook      'mhc-misc-hdr-decode)

(setq mhc-base-folder "+schedule")


;; === recentf ===
(when (locate-library "recentf")
  (require 'recentf)
  (let ((my-recentf-file "~/.emacs.private.d/.recentf"))
    (when (file-exists-p my-recentf-file)
      (setq recentf-save-file my-recentf-file)))
  (setq recentf-auto-cleanup 'never)
  (if (featurep 'w32) ; NTEmacs
      (progn
        (setq recentf-save-file-coding-system 'japanese-shift-jis)
        ))
  (recentf-mode 1)
  (setq recentf-max-menu-items 50) ;Number to display in the menu bar
  (setq recentf-max-saved-items 70) ;maximum number of saved items
  (define-key global-map "\C-crf" 'recentf-open-files)
  (define-key global-map "\C-cre" 'recentf-edit-list)
  (setq recentf-exclude '(".*\.cache"))
)

;; === hide-ifdef ===
(add-hook 'hide-ifdef-mode-hook
	  (function (lambda ()
		      (define-key hide-ifdef-mode-submap "@" 'hide-ifdef-block)
		      (define-key hide-ifdef-mode-submap "a" 'show-ifdef-block)

		      (define-key hide-ifdef-mode-submap "f" 'forward-ifdef )
		      (define-key hide-ifdef-mode-submap "b" 'backward-ifdef)
		      (define-key hide-ifdef-mode-submap "n" 'next-ifdef    )
		      (define-key hide-ifdef-mode-submap "p" 'previous-ifdef)
)))


(setq hide-ifdef-mode-hook
      (lambda ()
        (if (not hide-ifdef-define-alist)
            (setq hide-ifdef-define-alist
                  '((default1 CONFIG_BOARD1 \1)
                    (default2 CONFIG_BOARD2 \1)
                    )))
        (hide-ifdef-use-define-alist 'default1)
        ))

(setq hide-ifdef-initially t)
(setq hide-ifdef-lines nil) ; if t, do not show #if, #ifdef, #ifndef, #else, #endif, etc. lines
(setq hide-ifdef-shadow t)

;; === hideshow.el ===
(setq hs-hide-comments-when-hiding-all nil)

(when (locate-library "filecache")
  (require 'filecache)
)
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (local-set-key "\C-c\C-i" 'file-cache-minibuffer-complete)))

(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)

;;;--- w32-symlinks (06.05.10)---
;---Windows links can be used to navigate directories.
;
(when (locate-library "w32-symlinks")
  (require 'w32-symlinks)
  (setq w32-symlinks-shortcut-target t))


;;; ---- toggle tab width (06.05.10) ---
(define-key global-map "\C-ct4"
  #'(lambda (arg) (interactive "p") (setq tab-width 4)))

(define-key global-map "\C-ct8"
  #'(lambda (arg) (interactive "p") (setq tab-width 8)))

;---- vc-svn ----
(when (locate-library "vc-svn")
  (require 'vc-svn)
  )

;---- AUCTex ----
(when (locate-library "tex-site")
  (require 'tex-site)
  (require 'tex)

  (setq TeX-default-mode 'japanese-Latex-mode)
  (setq japanese-LaTeX-default-style "jsarticle")
  (setq japanese-LaTeX-command-default "pLaTeX")
  (setq kinsoku-limit 10)
  (setq LaTeX-indent-level 4)
  (setq LaTeX-item-indent 2)
                                        ;(add-to-list 'TeX-output-view-style
                                        ;             '("^dvi$" "." "dvipdfmx %dS %d && open %s.pdf"))
  (add-to-list 'TeX-output-view-style
               '("^dvi$" "." "dvipdfmx.exe %dS %d && \"C:/Program Files/Adobe/Reader 9.0/Reader/AcroRd32.exe\" %s.pdf" ))

;  (setq TeX-view-program-selection '((output-dvi "DVIviewer")))
)

;---- org-mode ----
(let ((my-org-mode-init-file "~/.emacs.private.d/init-org.el"))
  (when (file-exists-p my-org-mode-init-file)
    (load-file my-org-mode-init-file)))

;---- tramp ----
;(when (locate-library "tramp")
;  (require 'tramp)
;  )
;(setq ange-ftp-ftp-program-name "c:/usr/ftp.exe")

;(setq tramp-debug-to-file t)
;(customize-set-variable 'tramp-default-method "ftp")
;(setq remote-file-name-inhibit-locks t)
;(setq tramp-default-method "ftp")
;(when (eq window-system 'w32)
;;  (setq tramp-default-method "pscp")
;;  (setq tramp-default-method "ftp")
;;  (setq tramp-use-ssh-controlmaster-options nil)
;)

(defun explorer-current-directory ()
  "explorer current diretory"
  (interactive)
  (let (now-command-opt-str)
    (setq now-command-opt-str (concat (mapcar '(lambda (x) (if(= x ?/) ?\\ x)) (string-to-list default-directory))))
    (start-process "*explorer*" nil "explorer.exe" now-command-opt-str)
    )
  )
(global-set-key "\C-ce" 'explorer-current-directory)

;---- ECB(Emacs Code Browser) ----
(when (locate-library "ecb")
  (require 'ecb)
;  (custom-set-variables '(ecb-options-version "2.40"))
  )

;---- package ----
(when (locate-library "package")
  (require 'package)
;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;   (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   (setq package-check-signature nil)
   (package-initialize)
)

;---- auto-complete ----
(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  )

;---- about buffer ----
(defun update-buffer-display ()
  "update buffer display"
  (interactive)
  (font-lock-fontify-buffer)
)

;---- calfw ----
(when (locate-library "calfw")
  (require 'calfw)
  )

;---- c-doc ----
(when (locate-library "c-doc")
  (require 'c-doc)
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (local-set-key "\C-ci" 'c-doc-insert)))
  )

;---- plantuml ----
(when (locate-library "plantuml-mode")
  (require 'plantuml-mode)
  (setq plantuml-jar-path "C:\\usr\\gnu\\bin\\plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq auto-mode-alist
        (append '(
                  ("\\.pu$" . plantuml-mode)
                  ("\\.puml$" . plantuml-mode)
                  ) auto-mode-alist))
  )

;;browse-kill-ring.el (redefined above but for some reason it is disabled)
(when (locate-library "browse-kill-ring")
  (require 'browse-kill-ring)
  (global-set-key (kbd "C-c y") 'browse-kill-ring)
)

(when (locate-library "flycheck")
  (require 'flycheck)
  (global-flycheck-mode)
;  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
;  (setq flycheck-idle-change-delay 3)

;  (flycheck-define-checker my-gcc
;    "A C/C++ checker using g++."
;    :command ("gcc" "-c" "-I../../inc" "-I." "-Ic:/msys64/mingw32/i686-w64-mingw32/include""-O1" "-Wall" source "-o" temporary-file-name)
;    :error-patterns  ((error line-start
;                             (file-name) ":" line ":" column ":" " Error: " (message)
;                             line-end)
;                      (error line-start
;                             (file-name) ":" line ":" column ":" " Fatal Error: " (message)
;                             line-end)
;                      (warning line-start
;                               (file-name) ":" line ":" column ":" " Warning: " (message)
;                               line-end))
;    :modes (c-mode c++-mode))
;
;  (add-hook 'c-mode-common-hook
;            '(lambda()
;               (flycheck-select-checker 'my-gcc)))

  (when (locate-library "flycheck-pos-tip")
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))
    )

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00001000
              #b00011100
              #b00111110
              #b01111111
              #b00111110
              #b00011100
              #b00001000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (define-fringe-bitmap 'my-flycheck-fringe-indicator2
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b01000001
              #b01100011
              #b00110110
              #b00011100
              #b00011100
              #b00011100
              #b00110110
              #b01100011
              #b01000001
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    )

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator2
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  (set-face-foreground 'flycheck-fringe-error "red")
  (set-face-foreground 'flycheck-fringe-warning "magenta")
  (set-face-foreground 'flycheck-fringe-info "lightblue")
)

(when (locate-library "fringe-helper")
  (require 'fringe-helper)
)

(when (locate-library "git-gutter")
  (require 'git-gutter)
  (global-git-gutter-mode t)
)
(when (locate-library "git-gutter-fringe")
  (require 'git-gutter-fringe)
)

(when (locate-library "counsel")
  (require 'counsel)
  (counsel-mode 1)
)

(when (locate-library "swiper")
  (require 'swiper)
  (global-set-key (kbd "C-c a") 'swiper)
)

;---- bazel-mode ----
(when (locate-library "bazel-mode")
  (require 'bazel-mode)
;  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))

  (setq auto-mode-alist
        (append '(
;                  ("\\BUILD.*" . bazel-mode)
                  ("\\.BUILD$" . bazel-mode)
                  ("\\.bazelrc$" . bazel-mode)
                  ) auto-mode-alist))

)

;---- emojify ----
(when (locate-library "emojify")
  (require 'emojify)
  (global-emojify-mode)
  )

;---- magit ----
 (when (locate-library "magit")
   (require 'magit)
   (define-key global-map "\M-o" 'magit)
   (define-key magit-mode-map (kbd "q") (lambda() (interactive) (magit-mode-bury-buffer t)))
 )

;;---- private settings ----
(let ((my-private-init-file "~/.emacs.private.d/init-private-setting.el"))
  (when (file-exists-p my-private-init-file)
    (load-file my-private-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable everything if there is a mistake in this file
(put 'eval-expression 'disabled t)
(put 'eval-expression 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-publish-use-timestamps-flag nil)
 '(org2blog/wp-show-post-in-browser 'show)
 '(package-selected-packages
   '(docker counsel-tramp kconfig-mode go-mode org2blog modus-themes helm-ag emojify ox-zenn dockerfile-mode markdown-mode yaml-mode plantuml-mode flycheck-plantuml flycheck git-commit git-gutter google-maps helm helm-core irony magit-popup popup pos-tip powerline rich-minority smart-mode-line swiper with-editor rust-mode bazel-mode counsel-gtags counsel flx swiper-helm flycheck-pos-tip smart-mode-line-powerline-theme spaceline git-gutter-fringe git-gutter-fringe+ fringe-helper org-plus-contrib org o-blog markdown-mode+ magit js-doc irony-eldoc htmlize git-gutter+ flycheck-irony ecb cp5022x color-identifiers-mode calfw browse-kill-ring auto-complete auctex))
 '(tramp-connection-timeout 10))
