
;;muse mode
(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)  ;; and so on
(require 'muse-wiki)
(require 'muse-journal)
(add-hook 'muse-mode-hook 'abbrev-mode)

(setq muse-project-alist
      '(("nakkaya.com" 
	 ("~/Projects/nakkaya.com/muse/" :default "index")
         (:base "my-page-html" :path "~/Projects/nakkaya.com/html/"))
	("wiki" ("~/Projects/wiki/" :default "index"))))

(muse-derive-style "my-page-html" "html"
                   :header "~/Projects/nakkaya.com/muse/header.tmpl"
                   :footer "~/Projects/nakkaya.com/muse/footer.tmpl")
