;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun eaf-open-2048 ()
  "Open EAF 2048 with a grid size between 1 to 99"
  (interactive)
  (let ((gridsize (completing-read
                   "choose or input a grid size, default 4: "
                   '("4" "3" "5" "6")
                   nil
                   nil)))
    (eaf-open "eaf-2048" "2048" gridsize)))


;;;###autoload
(defun eaf-open-2048-ai ()
  "Watch  AI's 2048 performance"
  (interactive)
  (eaf-open "eaf-2048" "2048" 0))

(setq eaf-demo-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("2048" . eaf-demo-module-path))

(provide 'eaf-2048)
