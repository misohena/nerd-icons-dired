;;; nerd-icons-dired.el --- Shows icons for each file in dired mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (nerd-icons "0.0.1"))
;; URL: https://github.com/rainstormstudio/nerd-icons-dired
;; Keywords: files, icons, dired

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, simply install and add this to your init.el
;; (require 'nerd-icons-dired)
;; (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

;; or use use-package:
;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; This package is inspired by
;; - `all-the-icons-dired': https://github.com/jtbm37/all-the-icons-dired

;;; Code:

(require 'dired)
(require 'nerd-icons)

(defface nerd-icons-dired-dir-face
  '((t nil))
  "Face for the directory icon."
  :group 'nerd-icons-faces)

(defcustom nerd-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the Dired buffer."
  :group 'nerd-icons
  :type 'number)

(defcustom nerd-icons-dired-max-lines 1000
  "The maximum number of lines in the buffer in which icons will be displayed.
Performance can be improved by hiding icons when there are a large
number of files."
  :group 'nerd-icons
  :type '(choice (integer)
                 (const :tag "No limit" nil)))

(defcustom nerd-icons-dired-string-after-icon
  " " ;; (propertize " " 'font-lock-face '(:height 0.5))
  "String inserted between the icon and the filename in `nerd-icons-dired'."
  :group 'nerd-icons
  :type `(choice (const :tag "Tab" "\t")
                 (const :tag "Space" " ")
                 (const :tag "Half Space"
                        ,(propertize " " 'font-lock-face '(:height 0.5)))
                 (string :tag "Any String")))

(defvar nerd-icons-dired-mode)

(defun nerd-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'nerd-icons-dired-overlay t)
    (overlay-put ov 'after-string
                 ;; Workaround for the issue where overlapping faces
                 ;; are not applied
                 ;; https://github.com/rainstormstudio/nerd-icons-dired/issues/1
                 (propertize string 'display string))
    ;; Make sure to delete the icon when a file line is deleted.
    (overlay-put ov 'evaporate t)))

(defun nerd-icons-dired--overlays-in (beg end)
  "Get all nerd-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'nerd-icons-dired-overlay))
   (overlays-in beg end)))

(defun nerd-icons-dired--overlays-at (pos)
  "Get nerd-icons-dired overlays at POS."
  (apply #'nerd-icons-dired--overlays-in `(,pos ,pos)))

(defun nerd-icons-dired--remove-all-overlays-from-whole-buffer ()
  "Remove all `nerd-icons-dired' overlays from the whole buffer."
  (save-restriction
    (widen)
    (nerd-icons-dired--remove-all-overlays)))

(defun nerd-icons-dired--remove-all-overlays ()
  "Remove all `nerd-icons-dired' overlays within the narrowed region."
  (mapc #'delete-overlay
        (nerd-icons-dired--overlays-in (point-min) (point-max))))

(defun nerd-icons-dired--refresh ()
  (if (and nerd-icons-dired-max-lines
           (> (line-number-at-pos (point-max) t) nerd-icons-dired-max-lines))
      ;; If there are many files, it will be very slow, so disable icons.
      (nerd-icons-dired--remove-all-overlays-from-whole-buffer)
    (nerd-icons-dired--refresh--internal)))

(defun nerd-icons-dired--refresh--internal ()
  "Display the icons of files within the narrowed region of the Dired buffer."
  (nerd-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (when (dired-move-to-filename nil)
          (let ((file (dired-get-filename nil 'noerror))) ;; Full path
            (when file
              (nerd-icons-dired--add-overlay
               (point)
               (concat (nerd-icons-dired--icon file)
                       nerd-icons-dired-string-after-icon)))))
        (forward-line 1)))))

(defun nerd-icons-dired--icon (file)
  (cond
   ;; . or ..
   ((member (file-name-nondirectory file) '("." ".."))
    (nerd-icons-codicon
     "nf-cod-blank"
     :face 'nerd-icons-dired-dir-face
     :v-adjust nerd-icons-dired-v-adjust))
   ;; Directory
   ;; (Note: Do not call `file-directory-p' as it may trigger remote access.)
   ((save-excursion (forward-line 0)
                    (looking-at-p dired-re-dir))
    (if (file-remote-p file)
        ;; Avoid file-*-p functions
        (nerd-icons-sucicon
         "nf-custom-folder_oct"
         :face 'nerd-icons-dired-dir-face
         :v-adjust nerd-icons-dired-v-adjust)
      (nerd-icons-icon-for-dir
       file
       :face 'nerd-icons-dired-dir-face
       :v-adjust nerd-icons-dired-v-adjust)))
   ;; File
   (t
    (nerd-icons-icon-for-file
     file :v-adjust nerd-icons-dired-v-adjust))))

(defun nerd-icons-dired--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((result (apply fn args))) ;; Save the result of the advised function
    (when nerd-icons-dired-mode
      (nerd-icons-dired--refresh))
    result)) ;; Return the result

(defun nerd-icons-dired--after-readin-hook ()
  (when nerd-icons-dired-mode
    (nerd-icons-dired--refresh)))

(defun nerd-icons-dired--setup ()
  "Setup `nerd-icons-dired'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (add-hook 'dired-after-readin-hook #'nerd-icons-dired--after-readin-hook nil t)
    (nerd-icons-dired--refresh)))

(defun nerd-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (kill-local-variable 'tab-width)
  (remove-hook 'dired-after-readin-hook #'nerd-icons-dired--after-readin-hook t)
  (nerd-icons-dired--remove-all-overlays-from-whole-buffer))

;;;###autoload
(define-minor-mode nerd-icons-dired-mode
  "Display nerd-icons icon for each files in a Dired buffer."
  :lighter " nerd-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if nerd-icons-dired-mode
        (nerd-icons-dired--setup)
      (nerd-icons-dired--teardown))))

(provide 'nerd-icons-dired)
;;; nerd-icons-dired.el ends here
