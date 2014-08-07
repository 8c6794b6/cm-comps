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

(make-variable-buffer-local
 (defvar hs-repl-con nil
   "Connection to REPL server."))

(defvar hs-repl-host-history nil
  "History of host used for connection.")

(defvar hs-repl-port-history nil
  "History of port used for connection.")

(defun hs-repl-goto-top-level-node ()
  "Go to current top level node."
  (let ((orig-node (shm-current-node))
        (next-node (progn
                     (shm/goto-parent)
                     (shm-current-node))))
    (unless (equal orig-node next-node)
      (hs-repl-goto-top-level-node))))

(defun hs-repl-blink-node ()
  "Temporary highlight current top level node with pulse."
  (interactive)
  (pulse-momentary-highlight-region
   (save-excursion (hs-repl-goto-top-level-node)
                   (shm-node-start (shm-current-node)))
   (save-excursion (hs-repl-goto-top-level-node)
                   (shm-node-end (shm-current-node))))
  'shm-current-face)

(defun hs-repl-connect (host port)
  "Get connection with HOST and PORT."
  (interactive "sHost: \nnPort: ")
  (setq hs-repl-con
        (make-network-process
         :name "hs-repl"
         :host host
         :service port
         :nowait t
         :filter 'hs-repl-filter
         :sentinel 'hs-repl-sentinel)))

(defun hs-repl-do-connect ()
  "Show prompt to connect with defaults."
  (interactive)
  (let ((host (read-string
               "Host (localhost): "
               nil 'hs-repl-host-history "localhost" nil))
        (port (read-string
               "Port (9237): "
               nil 'hs-repl-port-history "9237" nil)))
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

(defun hs-repl-send-block ()
  "Send multiple-line block to REPL."
  (interactive)
  (save-excursion
    (hs-repl-goto-top-level-node)
    (let* ((current (shm-current-node))
           (str (buffer-substring-no-properties
                 (shm-node-start current)
                 (shm-node-end current))))
      (process-send-string
       hs-repl-con
       (concat ":{\n" str "\n:}\n"))))
  (hs-repl-blink-node))

(defun hs-repl-send-line ()
  "Send current line to REPL."
  (interactive)
  (process-send-string
   hs-repl-con
   (concat (thing-at-point 'line) "\n")))

(defvar hs-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'hs-repl-do-connect)
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
