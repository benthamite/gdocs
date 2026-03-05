((org-mode . ((eval . (add-hook 'after-save-hook
                                (lambda ()
                                  (require 'ox-texinfo)
                                  (let* ((inhibit-message t)
                                         (texi (org-texinfo-export-to-texinfo))
                                         (info (concat (file-name-sans-extension texi) ".info")))
                                    (when (executable-find "makeinfo")
                                      (call-process "makeinfo" nil nil nil
                                                    texi "-o" info))))
                                nil t)))))
