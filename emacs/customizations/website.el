(require 'org)
;; (require 'ox-rss)

;; pulled from http://nicolas-petton.fr/blog/blogging-with-org-mode.html

(defvar worace-website-html-head
  "<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
 <link rel='stylesheet' href='css/site.css' type='text/css'/>")


(defvar worace-website-html-blog-head
"<link href='http://fonts.googleapis.com/css?family=Libre+Baskerville:400,400italic' rel='stylesheet' type='text/css'>
<link rel='stylesheet' href='../css/site.css' type='text/css'/>")

(defvar worace-website-html-preamble
  "<div class='nav'>
<ul>
<li><a href='/'>Home</a></li>
<li><a href='/blog/index.html'>Blog</a></li>
<li><a href='/contact.html'>Contact</a></li>
</ul>
</div>")

(defvar worace-website-html-postamble
  "<div class='footer'>
Copyright 2016 %a.<br>
Last updated %C. <br>
Built with %c.
</div>")

(setq org-publish-project-alist
      `(("org"
         :base-directory "~/worace.github.io/"
         :base-extension "org"
         :publishing-directory "~/Public/worace.github.io/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,worace-website-html-head
         :html-preamble ,worace-website-html-preamble
         :html-postamble ,worace-website-html-postamble)

        ("blog"
         :base-directory "~/worace.github.io/blog"
         :base-extension "org"
         :publishing-directory "~/Public/worace.github.io/blog/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,worace-website-html-blog-head
         :html-preamble ,worace-website-html-preamble
         :html-postamble ,worace-website-html-postamble)

        ("images"
         :base-directory "~/worace.github.io/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/Public/worace.github.io/images/"
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "~/worace.github.io/css/"
         :base-extension "css"
         :publishing-directory "~/Public/worace.github.io/css/"
         :publishing-function org-publish-attachment)

        ("website" :components ("org" "blog" "images" "css"))))

(defun blog-publish ()
  (interactive)
  (org-publish-all)
  (let ((prev-path (pwd)))
    (cd "~/Public/worace.github.io")
    (shell-command "git add -A && git commit -m 'Automated blog output commit.'")
    (shell-command "git push origin master")
    (cd "~/worace.github.io")))

(defun blog-preview ()
  (interactive)
  (org-publish-all)
  (browse-url "file:///Users/worace/Public/worace.github.io/index.html"))
