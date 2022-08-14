;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun eaf-open-2048 ()
  "Open EAF demo screen to verify that EAF is working properly."
  (interactive)
  (eaf-open "eaf-2048" "2048" 3))

(setq eaf-demo-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("2048" . eaf-demo-module-path))

(provide 'eaf-2048)
