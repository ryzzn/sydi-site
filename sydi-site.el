;;; sydi-site.el --- website generator base on Emacs and org mode
;;
;; Filename: sydi-site.el
;; Description:
;; Author: Shi Yudi
;; Maintainer: Shi Yudi
;; Created: 2014-07-18T18:51:07+0800
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 782
;; URL: https://github.com/ryzzn/sydi-site
;; Doc URL: https://github.com/ryzzn/sydi-site
;; Keywords: sydi, Emacs, org mode, website
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 3-Jan-2015    Shi Yudi
;;    init version
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Code:

(require 'cl)

(defgroup sydi-site nil
  "sydi html generator"
  :group 'Applications
  :prefix "sydi-")

(defcustom sydi-base-directory "~/sydi.org/origin/"
  "Base org files directory."
  :group 'sydi-site
  :type 'directory)

(defcustom sydi-article-directory "posts"
  "Articles directory relating to `sydi-base-directory'."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-publish-directory "~/sydi.org/publish/"
  "Directory where to export org file to."
  :group 'sydi-site
  :type 'directory)

(defcustom sydi-base-code-directory "~/sydi.org/publish/code/"
  "Directory where to put code files."
  :group 'sydi-site
  :type 'directory)

(defcustom sydi-base-images-directory "~/sydi.org/publish/assets/images/"
  "Directory where to put images."
  :group 'sydi-site
  :type 'directory)

(defcustom sydi-site-url "http://sydi.org/"
  "Web site home URL."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-google-id "112098239943590093765"
  "Google id that will generate a link to your gplus page."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-site-name "MiScratch"
  "Your site name."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-google-tracker "<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-34738984-1', 'auto');
  ga('send', 'pageview');

</script>"
  "Google tracker script.
Change it's id to your Google tracker id that you can tracker
your visitors behaviors in Google Analytics."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-atom-exclude-file-list '("douban\\.org$" "^personal" "index\\.org$")
  "Exclude files which wouldn't export to atom file.
It's a list of regexp, `sydi-site' will check each file name for
each of regexp, it'll not export it if any regexp matches."
  :group 'sydi-site
  :type '(set regexp))

(defcustom sydi-atom-max-export-files-num 10
  "Max files to be export to atom file at most.
Don't set too big, 10 maybe a appropriate value, 20 is the top."
  :group 'sydi-site
  :type 'integer)

(defcustom sydi-exclude-pattern ".*my-wife.*\.org\\|sitemap.org"
  "Regexp pattern that org files wound be ignored to export."
  :group 'sydi-site
  :type 'regexp)

(defcustom sydi-include-pattern ""
  "Include files pattern to export."
  :group 'sydi-site
  :type 'regexp)

(defcustom sydi-auto-sitemap t
  "Whether Generate sitemap."
  :group 'sydi-site
  :type 'boolean)

(defcustom sydi-atom-file "dynamic/atom.xml"
  "Which file to store atom xml."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-sitemap-file "dynamic/sitemap.org"
  "Sitemap file path."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-recent-file "dynamic/recent-post.div"
  "Which file to store recent posts."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-recent-count 20
  "How many posts should display in recent tab."
  :group 'sydi-site
  :type 'integer)

(defcustom sydi-sidebar-file "components/sidebar.div.part"
  "Where the sidebar component file path is."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-footer-file "components/footer.div.part"
  "Where the footer component file path is."
  :group 'sydi-site
  :type 'string)

(defcustom sydi-remotes '((:user "ryan" :host "sydi.org" :directory "/var/www"))
  "A list of directories where your is located on.

Example:
((:user \"ryan\" :host \"sydi.org\" :directory \"/var/www/\"'))"
  :group 'sydi-site
  :type 'list)

(require 'ox)

(defvar sydi-comment-box-p t "Should add commnet box for this page.")
(defvar sydi-homepage-p nil "Indicate whether the page is home page.")
(defvar sydi-single-p t "Indicate whether a single post page or not.")

(add-to-list 'org-export-options-alist '(:comment-box nil "comment-box" t sydi-comment-box-p))
(add-to-list 'org-export-options-alist '(:homepage nil "homepage" nil sydi-homepage-p))
(add-to-list 'org-export-options-alist '(:single nil "single" t sydi-single-p))
(add-to-list 'org-export-options-alist '(:js-style nil nil nil))

;; support other languages for babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (sh . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (dot . t)
   (js . t)))


(eval-after-load 'ox-html
  '(progn
;;; add a horizontal line before footnotes
     (setq org-html-htmlize-output-type "css")
     (setq org-html-htmlize-font-prefix "")
     (setq org-export-allow-bind-keywords t)
     (setq org-html-head-include-scripts nil) ; 不加载默认js
     (setq org-html-head-include-default-style nil) ; 不加载默认css
     (setq org-html-link-home sydi-site-url)
     (setq org-export-with-section-numbers nil)
     (setq org-html-link-use-abs-url t)
     (setq org-html-preamble (lambda (plist) "<g:plusone></g:plusone>"))
     (setq org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<span id=\"text-footnotes\">
%s
</span>
</div>")))

;;;###autoload
(defun sydi-sync-server ()
  "This function run after all org files have been translated to HTML format.
It'll write a update the recent file by walking through the project."
  (sydi-write-recent-file))

(setq org-export-filter-final-output-functions '(sydi-final-export))

(defun sydi-final-export (contents backend info)
  "Filter to indent the HTML and convert HTML entities."
  (if (eq backend 'html)
      (sydi-final-html-export-filter contents info)))

;;;###autoload
;;; The hook is run after org-html export html done and
;;; still stay on the output html file.
(defun sydi-final-html-export-filter (contents info)
  "The sydi final export filter."
  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath t)
      (buffer-string)))
  (let ((content-no-script)
        (script "")
        (google-tracker sydi-google-tracker))
    ;; extract javascript
    (if (not (string-match "<script[^>]*>\\(.\\|\n\\)*</script>" contents))
        (setq content-no-script contents)
      (setq script (match-string-no-properties 0 contents))
      (setq content-no-script (concat (substring contents 0 (match-beginning 0))
                                        (substring contents (match-end 0)))))
    ;; format html contents
    (let* ((title (org-element-interpret-data (plist-get info :title)))
           (email (plist-get info :email))
           (author (org-element-interpret-data (plist-get info :author)))
           (body-only (plist-get info :body-only))
           (date (org-element-interpret-data (plist-get info :date)))
           (language    (plist-get info :language))
           (keywords    (plist-get info :keywords))
           (description (plist-get info :description))
           (style (plist-get info :style))
           (js-style (plist-get info :js-style))
           (charset (and org-html-coding-system
                         (fboundp 'coding-system-get)
                         (coding-system-get org-html-coding-system 'mime-charset)))
           (sidebar-html (if (plist-get info :base-directory)
                             (get-string-from-file
                              (concat (plist-get info :base-directory) sydi-sidebar-file))
                           ""))
           (footer-html (if (plist-get info :base-directory)
                            (get-string-from-file
                             (concat (plist-get info :base-directory) sydi-footer-file))
                          ""))
           (comment-box (if (plist-get info :comment-box)
                            "<div class=\"comments ds-thread\"></div>" ""))
           (head (concat (plist-get info :html-head) "\n" (plist-get info :html-head-extra)))
           (header (if (plist-get info :homepage) ""
                     (format "<div id=\"header\">
                                <div id=\"header-menu\"></div>
                                <div id=\"page-title\"><div id=\"page-title-text\">%s</div></div>
                                <div id=\"breadcrumbs\"></div>
                              </div>"
                             title))))
      (if body-only
        (format "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"title\" content=\"%s\"/>
<meta name=\"generator\" content=\"Org-mode modified by ryzn\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
%s
</head>
<body>
%s
<div id=\"wrapper\">
  <div id=\"sidebar\">%s</div>
  <div id=\"main\">
    %s
    <div id=\"content\">
      <div id=\"page-content\">
        <div class=\"post single\">
          <ul class=\"meta\"></ul>
            <div class=\"the-content\">
            <!-- content -->
            %s
            <!-- comment box -->
            %s
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
<!-- ENS WRAPPER -->
<div id=\"footer\">%s</div>
%s
%s
</body></html>"
                      language
                      language
                      title
                      charset
                      title
                      date
                      author
                      description
                      keywords
                      style
                      head
                      google-tracker
                      sidebar-html
                      header
                      content-no-script
                      comment-box
                      footer-html
                      script
                      js-style
                      ;; sydi-google-id
                      ;; author
                      ;; date
                      ;; sydi-site-name
                      )))))

(defun sydi-directory-file-compare (afile bfile)
  "Compare two file of `AFILE' and `BFILE' by their directory path."
  (let ((adir (file-name-directory afile))
        (bdir (file-name-directory bfile)))
    (cond
     ((string-prefix-p adir bdir) t)
     ((string-prefix-p bdir adir) nil)
     (t (string< afile bfile)))))

(defun sydi-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-as-directory (concat dir "/" sydi-article-directory)))
         (base-article-dir localdir)
         (oldlocal localdir)
         (indent-str "")
         (exclude-regexp (plist-get project-plist :exclude))
         (files (sort (nreverse (sydi-get-sorted-org-files localdir))
                      'sydi-directory-file-compare))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-title (or (plist-get project-plist :sitemap-title)
                            (concat "Sitemap for project " (car project))))
         (visiting (find-buffer-visiting sitemap-filename))
         (ifn (file-name-nondirectory sitemap-filename))
         (file)
         (sitemap-buffer))
    (with-current-buffer (setq sitemap-buffer
                               (or visiting (find-file sitemap-filename)))
      (message "Generating flat-style sitemap for %s" sitemap-title)
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (insert "#+BEGIN_HTML\n<div class=\"panes\">\n#+END_HTML\n")
      (insert "* Root \n")
      (dolist (file files)
        (let ((fn (file-name-nondirectory file))
              (link (file-relative-name file dir))
              (date (format-time-string "%Y-%m-%d" (sydi-get-org-file-date file)))
              (localdir (file-name-directory file)))
          (unless (string= "./" (file-relative-name oldlocal localdir))
            (let ((tag-name (file-relative-name localdir base-article-dir)))
              (insert (concat "* " tag-name "\n"))))
          (setq oldlocal localdir)

          ;; This is common to 'flat and 'tree
          (let ((entry
                 (org-publish-format-file-entry "%t" file project-plist))
                (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
            (cond ((string-match-p regexp entry)
                   (string-match regexp entry)
                   (insert (concat indent-str " + " (match-string 1 entry)
                                   "[[file:" link "]["
                                   (match-string 2 entry)
                                   "]]@@html:<span class=\"page-item-date\">@@"
                                   date " @@html:</span>@@" (match-string 3 entry) "\n")))
                  (t
                   (insert (concat indent-str " + [[file:" link "]["
                                   entry
                                   "]]@@html:<span class=\"page-item-date\">@@"
                                   date " @@html:</span>@@\n")))))))
      (insert "\n#+BEGIN_HTML\n</div>\n#+END_HTML\n")
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

;;;###autoload
(defun set-org-publish-project-alist ()
  (setq org-publish-project-alist
        `(("sydi"
           :components ("sydi-pages" "sydi-static"))
          ("sydi-static"
           :base-directory ,sydi-base-directory
           :base-extension "xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html\\|div\\|pl\\|template\\|txt\\|woff\\|eot\\|sh"
           :publishing-directory ,sydi-publish-directory
           :recursive t
           :publishing-function org-publish-attachment)
          ("sydi-pages"
           :base-directory ,sydi-base-directory
           :base-extension "org"
           :exclude ,sydi-exclude-pattern
           :include ,sydi-include-pattern
           :publishing-directory ,sydi-publish-directory
           :html-extension "html"
           :recursive t
           :makeindex nil
           :auto-sitemap ,sydi-auto-sitemap
           :sitemap-ignore-case t
           :sitemap-filename ,sydi-sitemap-file
           :sitemap-function sydi-publish-org-sitemap
           :htmlized-source t
           :with-toc nil
           :auto-preamble t
           :sitemap-title "站点地图 for 本网站"
           :author "施宇迪"
           :email "a@sydi.org"
           :language "zh-CN"
           :style "
<link rel=\"stylesheet\" href=\"/assets/css/style.css\" />
<link href=\"/assets/images/logo.png\" rel=\"icon\" type=\"image/x-icon\" />
<link href=\"/dynamic/atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"sydi.org atom\" />
"
           :js-style "<script type=\"text/javascript\" src=\"/assets/javascripts/jquery.min.js\"></script>
<script type=\"text/javascript\" src=\"/assets/javascripts/site.js\"></script>
"
           :publishing-function (org-html-publish-to-html)
           :body-only t
           :completion-function (sydi-sync-server)))))

(defun sydi-publish (&optional proj)
  "Publish Worg in htmlized pages."
  (interactive)
  (let ((org-format-latex-signal-error nil)
        (org-startup-folded nil)
        (org-export-date-timestamp-format "%Y-%m-%d")
        (make-backup-files nil)
        (proj (or proj "sydi")))
    (set-org-publish-project-alist)
    (message "Emacs %s" emacs-version)
    (org-version)
    (if sydi-auto-sitemap (sydi-generate-sitemap))
    (org-publish-project proj)))

(defun sydi-publish-current ()
  "Publish current org file"
  (interactive)
  (let ((sydi-exclude-pattern ".*")
        (sydi-include-pattern (list (file-relative-name buffer-file-name sydi-base-directory)))
        (sydi-auto-sitemap nil))
    (sydi-publish "sydi-pages")))

;; external browser should be chromium
(setq browse-url-generic-program
      (executable-find "chromium"))

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t))))))
    ad-do-it))

(defun sydi-generate-atom ()
  (interactive)
  (generate-atom sydi-base-directory (concat sydi-base-directory sydi-atom-file)))

;;;###autoload
(defun generate-atom (root-dir atom-file)
  "generate a atom style page"
  (defun web-time-string (&optional TIME)
    (concat (format-time-string "%Y-%m-%dT%T" TIME)
            ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
             (format-time-string "%z"))))
  (save-excursion
    (let* ((org-export-allow-BIND t)
           (org-files (subseq (sydi-get-sorted-org-files root-dir) 0 sydi-atom-max-export-files-num))
           (atom-filename atom-file)
           (visiting (find-buffer-visiting atom-filename))
           (atom-buffer (or visiting (find-file atom-filename)))
           (title sydi-site-name)
           (subtitle "About Linux, Distributed System, Data Base, High Performance System")
           (self-link "http://sydi.org/atom.xml")
           (link "http://sydi.org/")
           (id "http://sydi.org")
           (author "施宇迪")
           (email "a@sydi.org")
           (org-export-with-toc nil))
      (with-current-buffer atom-buffer
        (kill-region (point-min) (point-max))
        (insert (format
                 "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>%s</title>
  <subtitle>%s</subtitle>
  <link href=\"%s\" rel=\"self\"/>
  <link href=\"%s\"/>
  <updated>%s</updated>
  <id>%s</id>
  <author>
    <name><![CDATA[%s]]></name><email>%s</email>
  </author>
  <generator uri=\"%s\">ryan's orgmode generator</generator>"
                 title subtitle self-link link
                 (web-time-string)
                 id author email sydi-site-url))
        (dolist (file org-files)
          (message file)
          (let ((org-file-buffer (find-file file)))
            (set-buffer org-file-buffer)
            (let* ((plist (org-export--get-inbuffer-options))
                   (title (or
                           (plist-get plist :title)
                           "UNTITLED"))
                   (date (web-time-string
                          (org-time-string-to-time
                           (let ((inbuf-date (plist-get plist :date)))
                             (if inbuf-date
                                 (substring-no-properties (car inbuf-date))
                               (format-time-string
                                (org-time-stamp-format)
                                (cons 0 0))
                               )))))
                   (url (concat
                         (replace-regexp-in-string
                          (file-truename sydi-base-directory)
                          sydi-site-url
                          (file-name-sans-extension (buffer-file-name)))
                         ".html"))
                   (entry (format
                           "<entry>
  <title>%s</title>
  <link href=\"%s\" />
  <updated>%s</updated>
  <id>%s</id>
  <content type=\"html\"><![CDATA[%s]]></content></entry>"
                           title
                           url
                           date
                           url
                           (org-export-as 'html nil nil t)
                           )))
              (kill-buffer org-file-buffer)
              (set-buffer atom-buffer)
              (insert entry))))
        (insert "</feed>")
        (save-buffer)
        (kill-buffer)))))

(defun sydi-get-org-file-date (file &optional other)
  "Return org file date in #+date header line using `current-time' format.

If #+date keyword is not set and `other' equals to \"modify\", return the file system's modification time instead, if `other' equals to \"change\" return the file system's last change time instead, if `other' equals to \"access\" return the file systems's access time instead, otherwise return 0 as 1970-01-01 00:00:00, the minimal time.
"
  (let ((visiting (find-buffer-visiting file)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file-noselect file nil t)))
      (let* ((plist (org-export--get-inbuffer-options))
             (date (plist-get plist :date)))
        (unless visiting
          (kill-buffer (current-buffer)))
        (if date
            (org-time-string-to-time (substring-no-properties (car date)))
          (when (file-exists-p file)
            (cond ((equal other "access") (nth 4 (file-attributes file)))
                  ((equal other "modify") (nth 5 (file-attributes file)))
                  ((equal other "change") (nth 6 (file-attributes file)))
                  (t '(0 0)))))))))

(defun sydi-get-org-file-date-str (file)
  "Return org file date"
  (let ((visiting (find-buffer-visiting file)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file-noselect file nil t)))
      (plist-get (org-infile-export-plist) :date))))

(require 'find-lisp)
(defun sydi-get-sorted-org-files-quick (root-dir)
  "get sorted org files quickly, with cache"
  (let ((org-files (remove-if
                    (lambda (ele) (member-if
                                   (lambda (match-reg) (string-match-p match-reg (file-name-nondirectory ele)))
                                   sydi-atom-exclude-file-list))
                    (find-lisp-find-files root-dir "\\.org$"))))
    (dolist (file sydi-sorted-org-files)
      (delete file org-files))
    org-files))

(defun sydi-get-sorted-org-files (root-dir)
  "return a sorted org files list"
  (let* ((org-files (sydi-get-valid-org-files root-dir))
         (org-alist (mapcar (lambda (file) (cons file (sydi-get-org-file-date file))) org-files))
         (sorted-files (mapcar 'car
                               (sort org-alist
                                     (lambda (a b)
                                       (let* ((adate (sydi-get-org-file-date (car a)))
                                              (bdate (sydi-get-org-file-date (car b)))
                                              (A (+ (lsh (car adate) 16) (cadr adate)))
                                              (B (+ (lsh (car bdate) 16) (cadr bdate))))
                                         (>= A B)))))))
    sorted-files))

;; (customize-save-variable 'sydi-sorted-org-files (sydi-get-sorted-org-files "~/sydi.org/org"))

(defun sydi-get-valid-org-files (root-dir)
  (let* ((all-org-files (find-lisp-find-files root-dir "\\.org$"))
         (exclude-regexes sydi-atom-exclude-file-list)
         (exclude-file-p (lambda (org-file)
                           (member-if
                            (lambda (regex)
                              (string-match-p regex (file-name-nondirectory org-file)))
                            exclude-regexes))))
    (remove-if exclude-file-p all-org-files)))

(defun sydi-get-all-tags ()
  "return: ((tag1 meta1 meta2 ...) (tag2 meta1 ...) ...)"
  (let ((all-meta (sydi-get-all-meta))
        (result))
    (dolist (meta all-meta result)
      (dolist (tag (split-string (or (plist-get meta :keywords) "")) result)
        (let* ((tag (downcase tag))
              (item (assoc tag result)))
          (if item
              (progn
                (setcdr item (cons meta (cdr item)))
                (message "%s: %s" tag (plist-get meta :title)))
            (add-to-list 'result (list tag meta))))))))

(defun sydi-format-tag-cloud ()
  (let ((html "<div id=\"tag-cloud\">"))
    (dolist (item (sydi-get-all-tags) html)
      (setq html
            (concat html
                    (format "<a href=\"#\" rel=\"%d\">%s</a>\n"
                            (length (cdr item))
                            (car item) ))))
    (setq html (concat html "</div>"))))

;; (append-to-file (sydi-format-tag-cloud) nil "~/tagcloud.html")

(replace-regexp-in-string "^\\(.*\\)\\.org$" "/\\1.html" "xxxx.org")

(defun sydi-write-recent-file ()
  (let* ((meta-list (sydi-get-all-date-sorted-meta))
         (lastest-meta-list (butlast meta-list (- (length meta-list) sydi-recent-count)))
         (recent-file (concat sydi-base-directory sydi-recent-file))
         (content))
    (dolist (plist lastest-meta-list content)
      (let ((path (replace-regexp-in-string
                   "^\\(.*\\).org$" "/\\1.html"
                   (file-relative-name
                    (plist-get plist :file) sydi-base-directory)))
            (title (plist-get plist :title)))
        (setq content
              (concat
               content (format
                        "<li><a href=\"%s\">%s</a></li>\n" path title)))))
    (if (file-exists-p recent-file)
        (delete-file recent-file))
    (append-to-file content nil recent-file)))

(defun sydi-get-all-date-sorted-meta ()
  "return: (meta1 meta2 ...)
All meta are sorted by it's date property."
  (defun comp-date (adate bdate)
    (let ((A (+ (lsh (car adate) 16) (cadr adate)))
          (B (+ (lsh (car bdate) 16) (cadr bdate))))
      (>= A B)))
  (defun cons-date (strdate)
    (if strdate
        (org-time-string-to-time strdate)
      '(0 0)))
  (defun comp-org-date (aplist bplist)
    (let ((adate (cons-date (plist-get aplist :date)))
          (bdate (cons-date (plist-get bplist :date))))
      (comp-date adate bdate)))
  (sydi-get-all-sorted-meta 'comp-org-date))

(defun sydi-get-all-sorted-meta (pred)
  "use `pred' sort meta-list"
  (sort (sydi-get-all-meta) pred))

(defun sydi-get-all-meta ()
  "Get all org files' meta by scan their file directly."
  (mapcar 'sydi-get-org-meta (sydi-get-valid-org-files sydi-base-directory)))

(defun sydi-get-org-meta (org-file)
  (defun sydi-interpret (plist prop)
    (plist-put plist prop
               (let ((v (plist-get plist prop)))
                 (cond ((consp v)
                        (decode-coding-string
                         (substring-no-properties
                          (org-element-interpret-data v))
                         'utf-8))
                       ((stringp v)
                        (decode-coding-string v 'utf-8))
                       (t v)))))
  (let ((visiting (find-buffer-visiting org-file)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file-noselect org-file nil t)))
      (let ((plist (org-export--get-inbuffer-options)))
        (unless visiting
          (kill-buffer (current-buffer)))
        (dotimes (idx (length plist))
          (if (= 0 (% idx 2))
              (let ((prop (nth idx plist)))
                (sydi-interpret plist prop))))
        (plist-put plist :file org-file)))))

;;; for sitemap.xml
(defun sydi-all-urls (root-dir prefix)
  "Return all org files under `ROOT-DIR' in which replace prefix as `PREFIX' recursively."
  (let ((all-org-files (sydi-get-valid-org-files root-dir)))
    (mapcar
     (lambda (file)
       (concat prefix
               (file-name-sans-extension (file-relative-name file root-dir))
               ".html"))
     all-org-files)))

(defun sydi-generate-sitemap-ex (sitemap-filename root-dir prefix)
  (with-temp-buffer
    (kill-region (point-min) (point-max))
    (insert
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
"
      (mapconcat (lambda (url) (format "<url><loc>%s</loc><priority>0.5000</priority></url>" url))
                 (sydi-all-urls root-dir prefix) "\n")
      "</urlset>"))
    (write-file (concat root-dir sitemap-filename))
    (kill-buffer)))

(defun sydi-generate-sitemap ()
  (interactive)
  (sydi-generate-sitemap-ex "dynamic/sitemap.xml" sydi-base-directory "http://sydi.org/"))

(defun sydi-get-remote-directory (remote)
  "Get remote directory according to configuration of
`sydi-remotes'."
  (format "%s@%s:%s" (plist-get remote :user)
          (plist-get remote :host)
          (plist-get remote :directory)))

(defun sydi-remote-sync ()
  "Get remote sync command."
  (dolist (remote sydi-remotes)
    (let* ((local-directory sydi-publish-directory)
           (remote-directory (sydi-get-remote-directory remote))
           (command (format
                     "rsync -avz --delete --progress %s %s"
                     local-directory
                     remote-directory)))
      (message command)
      (call-process-shell-command command))))

(provide 'sydi-site)

;;; sydi-site.el ends here
