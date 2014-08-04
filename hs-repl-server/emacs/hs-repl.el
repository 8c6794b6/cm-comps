;; hs-repl.el -- Simple Haskell REPL server

;;; Commentary:
;;; Set of function to interact with hs-repl-server.

;;; Code:

(defvar hs-repl-con
  nil
  "Connection to REPL server.")

(defun hs-repl-connect (host port)
  "Get connection with HOST and PORT."
  (interactive "sHost: \nnPort: ")
  (setq hs-repl-con
        (make-network-process
         :name "hs-repl"
         :host host
         :service port
         :nowait t
         :filter 'hs-repl-filter)))

(defun hs-repl-filter (process contents)
  "Filter to read from PROCESS and display the CONTENTS."
  (message contents))

(defun hs-repl-send-block ()
  "Send block to REPL."
  (interactive)
  (let ((str (buffer-substring-no-properties
              (region-beginning)
              (region-end))))
    (process-send-string hs-repl-con (concat str "\n"))))

(defvar hs-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-j") 'hs-repl-connect)
    (define-key map (kbd "C-M-x") 'hs-repl-send-block)
    map))

;;;###autoload
(define-minor-mode hs-repl-mode
  "hs-repl"
  :lighter " HsR"
  :keymap hs-repl-map)

(provide 'hs-repl)

;;; hs-repl.el ends here
