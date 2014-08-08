;;; replenish.el --- Minor mode to interact with replenish server.

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
;; Version: 20140808.342
;; Package-Requires: ((shm "20140714.341"))
;; Keywords: haskell repl

;;; Commentary:
;;; Minor mode to interact with replenish server.

;;; Code:

(require 'shm)

(defgroup replenish nil
  "Interacting with replenish server."
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

(defvar replenish-host-history nil
  "History of host used for connection.")

(defvar replenish-port-history nil
  "History of port used for connection.")

(defun replenish-connect ()
  "Show prompt for connecting to server."
  (interactive)
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
           :sentinel 'replenish-sentinel))))

(defun replenish-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (message "=> %s" msg))

(defun replenish-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond ((string= "open\n" msg)
         (message "Connected to server."))
        ((string-prefix-p "failed" msg)
         (message "Failed to connect to server."))
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
  (concat ":{\n" str "\n:}\n"))

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
  "Send current line to REPL."
  (interactive)
  (process-send-string
   replenish-con
   (concat (thing-at-point 'line) "\n")))

;;;###autoload
(defvar replenish-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'replenish-connect)
    (define-key map (kbd "C-c C-c") 'replenish-send-line)
    (define-key map (kbd "C-M-x") 'replenish-send-block)
    map))

;;;###autoload
(define-minor-mode replenish-mode
  "Minor mode to interact with replenish server."
  :lighter " Replenish"
  :keymap replenish-map)


(provide 'replenish)

;;; replenish.el ends here
