(setq GNU (not (string-match "XEmacs\\|Lucid" (emacs-version))))
(unless (boundp 'running-xemacs)
  (defvar running-xemacs nil))
;(if GNU
;    (do-emacs-thing)
;  (do-xemacs-thing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-screen-alist '((width . 200) (height . 155)))

(setq c-default-style "k&r")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(setq line-number-mode t)
(setq column-number-mode t)
(set-default 'truncate-lines t)

(if GNU
    (delete-selection-mode 1)
  (pending-delete-mode 1))

;; set default coding to unix
;(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; turn on font-lock mode
;(when (fboundp 'global-font-lock-mode)
;  (global-font-lock-mode t))
;(when (fboundp 'font-lock-mode)
;  (font-lock-mode t))
;(add-hook 'c-mode-hook          'turn-on-font-lock)
;;; check if this works in emacs as well (works in xemacs)
;(require 'font-lock)
;(setq font-lock-auto-fontify t)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; ========== Line by line scrolling ==========
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

(global-set-key [f3] 'grep)
(global-set-key [f9] 'compile)
(global-set-key (kbd "<C-f5>") '(lambda() "copy to register 5" (interactive) (copy-to-register '5 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '5 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f5>") '(lambda() "insert register 5" (interactive) (insert-register '5 nil)))
(global-set-key (kbd "<C-f6>") '(lambda() "copy to register 6" (interactive) (copy-to-register '6 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '6 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f6>") '(lambda() "insert register 6" (interactive) (insert-register '6 nil)))
(global-set-key (kbd "<C-f7>") '(lambda() "copy to register 7" (interactive) (copy-to-register '7 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '7 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f7>") '(lambda() "insert register 7" (interactive) (insert-register '7 nil)))
(global-set-key (kbd "<C-f8>") '(lambda() "copy to register 8" (interactive) (copy-to-register '8 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '8 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f8>") '(lambda() "insert register 8" (interactive) (insert-register '8 nil)))
(if (string-equal system-type "darwin")
    ;; copy ctrl-insert
    (global-set-key (kbd "<C-help>") 'clipboard-kill-ring-save)
    ;; paste shift-insert ... shift does not register with insert and help just starts...help
    (global-set-key (kbd "<S-help>") 'clipboard-yank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert to point, duplicate line, select word, delete line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun val-set-ins-point ()
  "Set insertion point; use with val-cp-to-ins-point"
  (interactive)
  (setq inspoint (point))
  (setq insbuffer (current-buffer))
  (message "Insert buf: %s, point: %d" (buffer-name) inspoint))

(defun val-cp-to-ins-point (start end)
  "Copy the region to insertion point."
  (interactive "r")
  (setq srcbuffer (current-buffer))
  (let (cpbuffer (current-buffer))
    (switch-to-buffer insbuffer)
    (goto-char inspoint)
    (message "Copy to dest")
    (insert-buffer-substring srcbuffer start end)))

(defun val-dup-line ()
  "Duplicate current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((startpos (point)))
      (end-of-line)
      (let ((endpos (point)))
	(insert "\n")
	(insert-buffer-substring (current-buffer) startpos endpos)))))

(defun val-sel-word ()
  "Select curent word."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word)
  (exchange-point-and-mark)
    ;(copy-region-as-kill (point) (mark))
  (message "select word %s %s" (mark) (point)))

;; ===== Function to delete a line =====
;; First define a variable which will store the previous column position
(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

(defun move-cursor-previous-pane()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<return>") 'val-dup-line)
(global-set-key "\C-cd" 'val-dup-line)
(global-set-key (kbd "S-<return>") 'val-sel-word)
(global-set-key "\C-cs" 'val-sel-word)
(global-set-key "\C-ci" 'val-set-ins-point)
(global-set-key "\C-cc" 'val-cp-to-ins-point)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key (kbd "S-<delete>") 'nuke-line)
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key (kbd "S-<tab>") 'tab-to-tab-stop)
(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'move-cursor-previous-pane)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind windows keyboard keys while running on Mac
(if GNU
    (progn
     (global-set-key [S-kp-delete] 'nuke-line)
     ;;(global-set-key [C-H-help] 'ns-copy-including-secondary)
     (global-set-key [C-H-help] 'kill-ring-save)
     (global-set-key [S-H-help] 'yank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special setings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add region to search ring to start a search with region quickly
(if GNU
    ;; gnu emacs has fewer args to buffer-substring, and needs to explicitly deactivate mark
    (add-hook 'isearch-mode-hook '(lambda() "add region to search-ring" (interactive)
                                    (if (region-active-p)
                                        (setq search-ring (cons (buffer-substring (region-beginning) (region-end)) search-ring)
                                              deactivate-mark t))))
  (add-hook 'isearch-mode-hook '(lambda() "add region to search-ring" (interactive)
                                  (if (region-active-p)
                                      (setq search-ring (cons (buffer-substring (region-beginning) (region-end) nil) search-ring))))))

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 1))

;Bind the functions to the /-key and the *-key (on the numeric keypad) with:
(global-set-key (kbd "C-<prior>") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-<next>") 'scroll-up-keep-cursor)

; set correct path to find/grep on windows with cygwin
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "c:\\cygwin\\bin;" (getenv "PATH")))
  (setq find-program "c:\\cygwin\\bin\\find.exe")
  ; try to improve slow performance on windows.
  (setq w32-get-true-file-attributes nil))

(unless (string-equal system-type "darwin")
	(setq grep-command "find ./ -type f | grep -v \"\\.svn/\\|test/\\|\\.dep\\|\\.obj\\|\\.exe\\|\\.git/\\|/doc/\\|/vc/\\|/doxy\\|~$\\|#$\" | xargs grep -n ")
	;(setq grep-command "find ./ | grep -v \"\\.svn/\\|.git/\\|/doc/\\|/vc/\\|/doxy\\|~$\\|#$\" | xargs grep -n ")
)

;prevent emacs from spliting windows by itself
(setq split-height-threshold nil
      split-width-threshold nil)

(if (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta
          exec-path (append exec-path '("/usr/local/bin"))
          ggtags-executable-directory "/usr/local/bin"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  ;(set-face-background 'cperl-array-face "wheat")
  ;(set-face-background 'cperl-hash-face "wheat")
)

;;;;;;;;;;;;;;;;;;;; BASH ;;;;;;;;;;;;;;;;;;;;

(defun valg-setup-sh-mode ()
  "My own personal preferences for `sh-mode'.

This is a custom function that sets up the parameters I usually
refer for `sh-mode'.  It is automatically added to
`sh-mode-hook', but is can also be called interactively."
  (interactive)
  (setq sh-indent-for-do 0
        sh-indent-after-do '+
        sh-indent-comment t))
(add-hook 'sh-mode-hook 'valg-setup-sh-mode)

;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;

(if (file-readable-p "C:\\Python27\\python.exe")
    ;; (setq py-python-command "c:\\cygwin\\bin\\python2.6.exe")
    (setq py-python-command "C:\\Python27\\python.exe"))


;;;;;;;;;;;;;;;;;;;; JASON ;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (setq-default ispell-program-name "c:\\Program Files\\Aspell\\bin\\aspell.exe")

;;(if (file-readable-p "~/.xemacs/sr-speedbar.el")
;;  (load-file "~/.xemacs/sr-speedbar.el"))

;; (if (file-readable-p "~/.emacs.d/sr-speedbar.el")
;;   (load-file "~/.emacs.d/sr-speedbar.el"))

;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;;; ********************
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
(cond (running-xemacs
       (require 'func-menu)
;;       (define-key global-map 'f12 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)

       ;; The Hyperbole information manager package uses (shift button2) and
       ;; (shift button3) to provide context-sensitive mouse keys.  If you
       ;; use this next binding, it will conflict with Hyperbole's setup.
       ;; Choose another mouse key if you use Hyperbole.
       (define-key global-map '(shift button3) 'mouse-function-menu)

       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p t
             fume-menubar-menu-location nil
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)
))

; (if (file-readable-p "~/.emacs.d/selective_undo_xmas.el")
;     (load-file "~/.emacs.d/selective_undo_xmas.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; macros
; (defalias 'logerr
;   (read-kbd-macro "TAB LOG_ER(S-SPC LEVEL, SPC \"Err: SPC \" SPC);"))
; (defalias 'logwar (read-kbd-macro
; "TAB LOG_EA 2*<backspace> WA(SPC level 5*<backspace> LEVEL. <backspace> , SPC \"wA 2*<backspace> w <backspace> War: SPC \" SPC);"))
; (defalias 'loginf (read-kbd-macro
; "TAB lo 2*<backspace> K <backspace> LOG_IN* <backspace> (SPC LEVEL, SPC : <backspace> \"ING 2*<backspace> nf: SPC \" SPC _ <backspace>);"))
; (defalias 'logdbg (read-kbd-macro
; "TAB LOG_IN(SPC LEVEL10, SPC \"DEBUG: SPC \" SPC _ <backspace>);"))
;
; (defalias 'header-shell (read-kbd-macro
; ;(concat "#!/bin/bash RET #/// SPC \\file SPC RET #/// SPC \\brief RET # RET # SPC Created SPC by SPC Valeriu SPC Goldberger SPC on SPC "
; (concat "#!/bin/bash RET  RET #/// SPC \\file SPC  SPC "
;         (buffer-file-name nil)
;         "RET #/// SPC \\brief RET # RET # SPC Created SPC by SPC Valeriu SPC Goldberger SPC on SPC "
;         (format-time-string "%D SPC %R" (current-time))
;         " RET # RET")))

(defun logerr ()
  "insert C LOG_ERR call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_ER(LEVEL, \"Err: \");\n"))
(defun logwar ()
  "insert C LOG_WAR call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_WA(LEVEL, \"War: \");\n"))
(defun loginf ()
  "insert C LOG_INF call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_IN(LEVEL, \"Inf: \");\n"))
(defun logdbg ()
  "insert C LOG_INF DEBUG call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_IN(LEVEL10, \"DEBUG: \");\n"))
(defun header-shell ()
  "insert C LOG_ERR call"
  (interactive)
  (indent-according-to-mode)
  (insert
   (concat "#!/bin/bash

#/// \\file  " (buffer-name) "
#/// \\brief
#
# Created by Valeriu Goldberger on "
           (format-time-string "%D SPC %R" (current-time))
           "\n#\n"
)))

(defun vg-setter-c (fctName varName varType)
  "…"
  (interactive "sEnter function's name: \nsEnter variable name: \nsEnter variable type(int|char):")
  (message "Fct name: %s, Var name is: %s, Typeis %s" fctName varName varType)
  (insert "void " fctName "(")
  (if (string= varType "int")
      (insert "int " varName)
    (insert "const char *"))
  (insert " x) {\n    ")
  (if (string= varType "int")
      (insert varName " = x;\n")
      (insert "strncpy(" varName ", x, sizeof(" varName ");\n"))
  (insert "}\n"))

; macro to apply google style over ValG style.
(fset 'vg-apply-google-style
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item (quote ([134217788 201326629 40 32 43 return 40 return 33 134217788 201326629 32 43 41 return 41 return 33 134217788 134217765 32 105 102 40 return 32 105 102 32 40 return 33 134217788 201326629 32 119 104 105 108 101 40 return 32 119 104 105 108 101 32 40 return 33 134217788 201326629 32 102 111 114 40 return 32 102 111 114 32 40 return 33 134217788 201326629 32 115 119 105 116 99 104 40 return 32 115 119 105 116 99 104 32 40 return 33 134217788 201326629 92 40 94 32 42 91 94 47 32 93 46 42 92 41 17 10 32 42 123 17 10 return 92 49 32 123 17 10 return 33 134217788 201326629 32 43 36 return return 33 134217788] 0 "%d")) arg)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
   (quote
    ("a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "a30d5f217d1a697f6d355817ac344d906bb0aae3e888d7abaa7595d5a4b7e2e3" "faee9b5542e25faa94e5242147429e25822662d55013d9d797c8abed3c2cb58d" "4e1a057fe9a9981e886bd0673d131da904e3306c884715b8bee333ef95303e39" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "e2338a4a05a413b6dec0b67d83a743be86790a657b27b3e5d160340a804c5da1" "859c7db4f1f3d28b0d7cbf58b7f402547eb9195859e010ff0a229f89b979edaf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#343d46")
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(inhibit-startup-screen t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t))
;;; (custom-set-faces
;;;  ;; custom-set-faces was added by Custom.
;;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;;  ;; Your init file should contain only one such instance.
;;;  ;; If there is more than one, they won't work right.
;;; '(default ((t (:inherit nil :stipple nil :background "white smoke" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Courier New")))))

(setq mouse-wheel-progressive-speed nil)
;;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

;; VG: use MELPA repository
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-el.html
;; Set my favorite color theme
(unless (string-equal system-type "darwin")
  (dolist (package `(color-theme))
    (require 'color-theme)
    (color-theme-initialize)
    (color-theme-xemacs)
    ;(color-theme-ramangalahy)
))
(put 'upcase-region 'disabled nil)

;; to build gtags DB, run gtags
;; load, use GNU global; gtags-pop-stack's shorcut doesn't work in
;; gtags selection buffer
;(if (string-equal system-type "darwin")
;    (setq load-path (cons "/usr/local/bin" load-path))
(unless (string-equal system-type "darwin")
    (setq load-path (cons "~/../bin/global/share/gtags" load-path)))
(autoload 'gtags-mode "gtags" "" t)

(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
    (let ((olddir default-directory)
          (topdir (read-directory-name
                    "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "gtags && echo 'created tagfile'")
      (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(add-hook 'gtags-mode-hook
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-*") 'gtags-pop-stack)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

(add-hook 'gtags-select-mode-hook
  (lambda()
    (local-set-key (kbd "M-*") 'gtags-pop-stack)))   ; find a tag, also M-.

;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (require 'gtags)
;;     (gtags-mode t)
;;     (djcb-gtags-create-or-update)))

;;; http://www.gnu.org/software/global/globaldoc_toc.html#Applications
;;; set gtags for c-mode
;; (setq c-mode-hook
;;          '(lambda ()
;;              (gtags-mode 1)
;;))

;; installed package auto-complete
; start auto-complete with emacs
(require 'auto-complete)
; perform default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; installed package yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; installed iedit
;; with iedit-mode you can modify all ocurrence of a variable at the same time
;; Fix iedit bug in Mac
; (define-key global-map (kbd "C-c ;") 'iedit-mode)

;; installed flymake-google-cpplint-load
;; copied cpplint
; start flymake-google-cpplint-load
; let's define a function for flymake initialization
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
    ;; '(flymake-google-cpplint-verbose "30")
   '(flymake-google-cpplint-linelength "120"))
    ;; '(flymake-google-cpplint-command "c:/Python27/Scripts/cpplint.bat"))
  (if (file-readable-p "c:/Python27/Scripts/cpplint.bat")
      (custom-set-variables
       '(flymake-google-cpplint-command "c:/Python27/Scripts/cpplint.bat")))
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;; installed flymake-cursor - to disaply suggestions from flymake-google-cpplint

;;; ; turn on Semantic
;;; (semantic-mode 1)
;;; ; let's define a function which adds semantic as a suggestion backend to auto complete
;;; ; and hook this function to c-mode-common-hook
;;; (defun my:add-semantic-to-autocomplete()
;;;   (add-to-list 'ac-sources 'ac-source-semantic)
;;;)
;;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; turn on ede mode
(global-ede-mode 1)

;; http://cedet.sourceforge.net/intellisense.shtml
;; Start inline completion: M-x semantic-complete-analyze-inline    C-c , SPACE
;; Speedbar completion mode: M-x semantic-speedbar-analysis

;; ; create a project for our program.
;; (ede-cpp-root-project "my project" :file "~/demos/my_program/src/main.cpp"
;; 		      :include-path '("/../my_inc"))
;; ; you can use system-include-path for setting up the system header file locations.
;; ; turn on automatic reparsing of open buffers in semantic
;; (global-semantic-idle-scheduler-mode 1)

;; slow emacs... looking for solutions
(setq vc-handled-backends nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;; go Modes
;; http://arenzana.org/2015/Emacs-for-Go/

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(if (string-equal system-type "darwin")
    ((setenv "GOPATH" "/Users/valeriug/dev/go")
     (add-to-list 'exec-path "/Users/valeriug/dev/go/bin"))
  (when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    (setenv "GOPATH" "C:\\valeriu\\go")
    (add-to-list 'exec-path "C:\\valeriu\\go\\bin")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defvar my-packages
  '(;;;; Go shit
    go-mode
    go-eldoc
    go-autocomplete

        ;;;;;; Markdown
    ;; markdown-mode

        ;;;;;; Javascript
    json-mode
        ;;;;;; Env
    ;; project-explorer
    ;; smooth-scroll
    ;; buffer-move
    ;; window-number
    )
  "My packages!")

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
    
;;Load Go-specific language syntax
(defun go-mode-setup ()
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'go-mode-setup)

;;Format before saving
(defun go-mode-setup ()
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Goimports
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Godef, shows function definition when calling godef-jump
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  ;; (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; ;;Project Explorer
;; (require 'project-explorer)
;; (global-set-key (kbd "M-e") 'project-explorer-toggle)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; add support for web mode for .tmpl files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode))
