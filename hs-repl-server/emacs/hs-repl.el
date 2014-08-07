;; hs-repl.el -- Simple Haskell REPL server

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
(require 'pulse)

(defgroup 'hs-repl
  "Minor mode for interacting with hs-repl-server."
  :group 'haskell)

(defvar hs-repl-host-history nil
  "History of host used for connection.")

(defvar hs-repl-port-history nil
  "History of port used for connection.")

(defcustom hs-repl-default-host "localhost"
  "Default host of hs-repl-server to connect."
  :group 'hs-repl
  :type 'string)

(defcustom hs-repl-default-port 9237
  "Default port of hs-repl-server to connect."
  :group 'hs-repl
  :type 'integer)

(make-variable-buffer-local
 (defvar hs-repl-con nil
   "Connection to REPL server."))

(defun hs-repl-connect ()
  "Show prompt to connect with defaults."
  (interactive)
  (let ((host (read-string
               (concat "Host ("  hs-repl-default-host "): ")
               nil 'hs-repl-host-history hs-repl-default-host nil))
        (port (read-string
               (concat "Port ("
                       (number-to-string hs-repl-default-port)
                       "): ")
               nil 'hs-repl-port-history
               (number-to-string hs-repl-default-port)
               nil)))
    (setq hs-repl-con
          (make-network-process
           :name "hs-repl"
           :host host
           :service port
           :nowait t
           :filter 'hs-repl-filter
           :sentinel 'hs-repl-sentinel))))

(defun hs-repl-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (message "=> %s" msg))

(defun hs-repl-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond ((string= "open\n" msg)
         (message "Connected to server."))
        ((string-prefix-p "failed" msg)
         (message "Failed to connect to server."))
        (t (message "hs-repl: %s"
                    (replace-regexp-in-string "\n" " " msg)))))

(defun hs-repl-goto-top-level-node ()
  "Go to current top level node."
  (interactive)
  (let ((orig-node (shm-current-node))
        (next-node (progn
                     (shm/goto-parent)
                     (shm-current-node))))
    (if (equal orig-node next-node)
        (shm-current-node)
      (hs-repl-goto-top-level-node))))

(defun hs-repl-wrap-multiple-line (str)
  "Wrap STR as multiple line message."
  (concat ":{\n" str "\n:}\n"))

(defun hs-repl-send-block ()
  "Send current top level node or selected region."
  (interactive)
  (if (region-active-p)
      (hs-repl-send-region)
    (hs-repl-send-current-top-level)))

(defun hs-repl-send-current-top-level ()
  "Send current top level node of haskell code."
  (interactive)
  (let* ((current (save-excursion
                    (hs-repl-goto-top-level-node)))
         (str (buffer-substring-no-properties
               (shm-node-start current)
               (shm-node-end current))))
    (process-send-string
     hs-repl-con
     (hs-repl-wrap-multiple-line str))))

(defun hs-repl-send-region ()
  "Send current region."
  (interactive)
  (process-send-string
   hs-repl-con
   (hs-repl-wrap-multiple-line
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))))

(defun hs-repl-send-line ()
  "Send current line to REPL."
  (interactive)
  (process-send-string
   hs-repl-con
   (concat (thing-at-point 'line) "\n")))

(defvar hs-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'hs-repl-connect)
    (define-key map (kbd "C-c C-c") 'hs-repl-send-line)
    (define-key map (kbd "C-M-x") 'hs-repl-send-block)
    map))

;;;###autoload
(define-minor-mode hs-repl-mode
  "hs-repl"
  :lighter " HsREPL"
  :keymap hs-repl-map)

(provide 'hs-repl)

;;; hs-repl.el ends here
