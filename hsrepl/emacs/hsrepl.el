;; hsrepl.el -- Minor mode to interact with hsrepl

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

;;; Commentary:
;;; Set of function to interact with hs-repl-server.

;;; Code:

(require 'shm)

(defgroup hsrepl nil
  "Minor mode for interacting with hsrepl-server."
  :group 'haskell)

(defvar hsrepl-host-history nil
  "History of host used for connection.")

(defvar hsrepl-port-history nil
  "History of port used for connection.")

(defcustom hsrepl-default-host "localhost"
  "Default host of hsrepl-server to connect."
  :group 'hsrepl
  :type 'string)

(defcustom hsrepl-default-port 9237
  "Default port of hsrepl-server to connect."
  :group 'hsrepl
  :type 'integer)

(make-variable-buffer-local
 (defvar hsrepl-con nil
   "Connection to REPL server."))

(defun hsrepl-connect ()
  "Show prompt for connecting to server."
  (interactive)
  (let ((host (read-string
               (concat "Host ("  hsrepl-default-host "): ")
               nil 'hsrepl-host-history hsrepl-default-host nil))
        (port (read-string
               (concat "Port ("
                       (number-to-string hsrepl-default-port)
                       "): ")
               nil 'hsrepl-port-history
               (number-to-string hsrepl-default-port)
               nil)))
    (setq hsrepl-con
          (make-network-process
           :name "hsrepl"
           :host host
           :service port
           :nowait t
           :filter 'hsrepl-filter
           :sentinel 'hsrepl-sentinel))))

(defun hsrepl-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (message "=> %s" msg))

(defun hsrepl-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond ((string= "open\n" msg)
         (message "Connected to server."))
        ((string-prefix-p "failed" msg)
         (message "Failed to connect to server."))
        (t (message "hsrepl: %s"
                    (replace-regexp-in-string "\n" " " msg)))))

(defun hsrepl-goto-top-level-node ()
  "Go to current top level node."
  (interactive)
  (let ((orig-node (shm-current-node))
        (next-node (progn
                     (shm/goto-parent)
                     (shm-current-node))))
    (if (equal orig-node next-node)
        (shm-current-node)
      (hsrepl-goto-top-level-node))))

(defun hsrepl-wrap-multiple-line (str)
  "Wrap STR as multiple line message."
  (concat ":{\n" str "\n:}\n"))

(defun hsrepl-send-block ()
  "Send current top level node or selected region."
  (interactive)
  (if (region-active-p)
      (hsrepl-send-region)
    (hsrepl-send-current-top-level)))

(defun hsrepl-send-current-top-level ()
  "Send current top level node of haskell code."
  (interactive)
  (let* ((current (save-excursion
                    (hsrepl-goto-top-level-node)))
         (str (buffer-substring-no-properties
               (shm-node-start current)
               (shm-node-end current))))
    (process-send-string
     hsrepl-con
     (hsrepl-wrap-multiple-line str))))

(defun hsrepl-send-region ()
  "Send current region."
  (interactive)
  (process-send-string
   hsrepl-con
   (hsrepl-wrap-multiple-line
    (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun hsrepl-send-line ()
  "Send current line to REPL."
  (interactive)
  (process-send-string
   hsrepl-con
   (concat (thing-at-point 'line) "\n")))

(defvar hsrepl-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'hsrepl-connect)
    (define-key map (kbd "C-c C-c") 'hsrepl-send-line)
    (define-key map (kbd "C-M-x") 'hsrepl-send-block)
    map))

;;;###autoload
(define-minor-mode hsrepl-mode
  "hsrepl"
  :lighter " Hsrepl"
  :keymap hsrepl-map)

(provide 'hsrepl)

;;; hsrepl.el ends here
