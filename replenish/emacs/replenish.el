;;; replenish.el --- Derived mode to interact with replenish server.

;; Copyright (c) 2014 8c6794b6. All rights reserved.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: 8c6794b6 <8c6794b6@gmail.com>
;; Version: 20140821.1
;; Package-Requires: ((haskell-mode "20140805.942") (shm "20140714.341"))
;; Keywords: haskell repl

;;; Commentary:
;;; Derived mode to interact with replenish server.

;;; Code:

(require 'haskell-mode)
(require 'shm)

(defgroup replenish nil
  "Minor mode to interact with replenish server."
  :group 'haskell)

(defcustom replenish-default-host "localhost"
  "Default host of replenish server to connect."
  :group 'replenish
  :type 'string)

(defcustom replenish-default-port 9237
  "Default port of replenish server to connect."
  :group 'replenish
  :type 'integer)

(make-variable-buffer-local
 (defvar replenish-con nil
   "Connection to REPL server."))

(defvar replenish-mode-hook nil
  "Hook for replenish-mode.")

(defvar replenish-host-history nil
  "History of host used for connection.")

(defvar replenish-port-history nil
  "History of port used for connection.")

(defvar replenish-input-history nil
  "History for sent input.")

(defun replenish-connect ()
  "Show prompt for connecting to server."
  (interactive)
  (if (and (not (equal nil replenish-con))
           (process-live-p replenish-con))
      (message "Connection exists.")
    (let ((host (read-string
                 (concat "Host ("  replenish-default-host "): ")
                 nil 'replenish-host-history replenish-default-host nil))
          (port (read-string
                 (concat "Port ("
                         (number-to-string replenish-default-port)
                         "): ")
                 nil 'replenish-port-history
                 (number-to-string replenish-default-port)
                 nil)))
      (setq replenish-con
            (make-network-process
             :name "replenish"
             :host host
             :service port
             :nowait t
             :filter 'replenish-filter
             :sentinel 'replenish-sentinel)))))

(defun replenish-disconnect ()
  "Disconnect from replenish server."
  (interactive)
  (delete-process replenish-con))

(defun replenish-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (message "%s" (s-replace "" "‘" (s-replace "" "’" msg))))

(defun replenish-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond ((string= "open\n" msg)
         (message "replenish: Connected to server."))
        ((string-prefix-p "failed" msg)
         (message "replenish: Failed connecting to server."))
        (t (message "replenish: %s"
                    (replace-regexp-in-string "\n" " " msg)))))

(defun replenish-goto-top-level-node ()
  "Go to current top level node."
  (interactive)
  (let ((orig-node (shm-current-node))
        (next-node (progn
                     (shm/goto-parent)
                     (shm-current-node))))
    (if (equal orig-node next-node)
        (shm-current-node)
      (replenish-goto-top-level-node))))

(defun replenish-wrap-multiple-line (str)
  "Wrap STR as multiple line message."
  str)

(defun replenish-send-block ()
  "Send current top level node or selected region."
  (interactive)
  (if (region-active-p)
      (replenish-send-region)
    (replenish-send-current-top-level)))

(defun replenish-send-current-top-level ()
  "Send current top level node of haskell code."
  (interactive)
  (let* ((current (save-excursion
                    (replenish-goto-top-level-node)))
         (str (buffer-substring-no-properties
               (shm-node-start current)
               (shm-node-end current))))
    (process-send-string
     replenish-con
     (replenish-wrap-multiple-line str))))

(defun replenish-send-region ()
  "Send current region."
  (interactive)
  (process-send-string
   replenish-con
   (replenish-wrap-multiple-line
    (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun replenish-send-line ()
  "Send current line."
  (interactive)
  (process-send-string
   replenish-con
   (s-replace "\n" "" (thing-at-point 'line))))

(defun replenish-send-input ()
  "Show prompt for input and send."
  (interactive)
  (process-send-string
   replenish-con
   (read-string "Eval: " nil 'replenish-input-history "" nil)))

(defun replenish-info-at-point ()
  "Send for getting info with `thing-at-point'."
  (interactive)
  (process-send-string
   replenish-con
   (concat ":dump_info " (thing-at-point 'symbol))))

(defun replenish-load-file ()
  "Load given file."
  (interactive)
  (process-send-string
   replenish-con
   (concat ":load " (read-file-name "Load: "))))

(defun replenish-dump-env ()
  "Dump server environment."
  (interactive)
  (process-send-string replenish-con ":dump_hsc_env"))

(defvar replenish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'replenish-connect)
    (define-key map (kbd "C-c M-k") 'replenish-disconnect)
    (define-key map (kbd "C-c C-c") 'replenish-send-line)
    (define-key map (kbd "C-M-x") 'replenish-send-block)
    (define-key map (kbd "C-c :") 'replenish-send-input)
    (define-key map (kbd "C-c TAB") 'replenish-info-at-point)
    (define-key map (kbd "C-c C-t") 'replenish-info-at-point)
    (define-key map (kbd "C-c C-l") 'replenish-load-file)
    (define-key map (kbd "C-c C-b") 'replenish-dump-env)
    (define-key map (kbd "C-c C-z") 'replenish-info-at-point)
    map)
  "Keymap for replenish mode.
Most part of the keymaps are inherited from `haskell-mode'.")

;;;###autoload
(define-minor-mode replenish-mode
  "Minor mode to interact with replenish server.

When interactivly turning on, alternative function `replenish'
does toggling the minor mode and connecting to server.

\\{replenish-mode-map}"
  :lighter " Replenish"
  :keymap replenish-mode-map)

;;;###autoload
(defun replenish ()
  "Turn on `replenish-mode' and connect to server."
  (interactive)
  (replenish-mode 1)
  (replenish-connect))

(provide 'replenish)

;;; replenish.el ends here
