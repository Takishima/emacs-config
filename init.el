;;; init.el --- Entry point -*- lexical-binding: t -*-

;;; Commentary:

;; Symlinked as ~/.emacs.d/init.el, or read directly by
;; `emacs --init-directory <checkout>'.

;;; Code:

(load (expand-file-name ".emacs" (file-name-directory
                                  (file-truename (or load-file-name buffer-file-name))))
      nil 'nomessage)

;;; init.el ends here
