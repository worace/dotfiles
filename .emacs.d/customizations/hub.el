;;; hub.el --- hub helps you win at git with emacs.
;;
;; Copyright © 2014 Travis Jeffery
;;
;; Author: Travis Jeffery <tj@travisjeffery.com>
;; URL: https://github.com/travisjeffery/hub.el
;; Version: 1.0.0
;; Keywords: github git

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'gh-pulls)
(require 'magit)
(require 'helm)

;; Interactive

(defun hub-list-pull-requests ()
  "View an actionable list of pull requests on GitHub."
  (interactive)
  (let ((host (hub--host)))
    (if (not (string= host "github.com"))
        (hub--open-issue-directly host)
      (helm :sources '(hub--pulls-source)
            :buffer  "*open github*"))))

(defun hub-open-pull-request ()
  "Open a pull request on GitHub."
  (interactive)
  (let ((magit-use-emacsclient-p nil)
        (magit-git-executable (hub--executable))
        (magit-git-standard-options '()))
    (magit-commit-internal "pull-request" nil)))

(defun hub-fork ()
  "Make a fork of a remote repository on GitHub and add as remote."
  (interactive)
  (hub--command-one-line "fork"))

(defun hub-create (name)
  "Create this repository on GitHub and add GitHub as origin."
  (interactive "sName: ")
  (hub--command-one-line "create"))

(defun hub-browse ()
  "Open a GitHub page in the default browser."
  (interactive)
  (hub--command-one-line "browse"))

(defun hub-compare ()
  "Open a GitHub compare view page in the system's default web browser."
  (interactive)
  (hub--command-one-line "compare"))

(defun hub-checkout (url)
  "Checkout the branch of a Pull Request."
  (interactive "sURL: ")
  (hub--command-one-line (format "checkout %s" url)))

(defun hub-merge (url)
  "Merge the branch of a Pull Request."
  (interactive "sURL: ")
  (hub--command-one-line (format "merge %s" url)))

;; Helpers

(defcustom hub-api
  (gh-pulls-api "api" :sync t :cache nil :num-retries 1)
  "Github API instance. This is a `gh-pulls'"
  :group 'hub)

(defun hub--command-one-line (cmd)
  (with-temp-buffer
    (let ((ret (call-process-shell-command (format "%s %s" (hub--executable) cmd) nil t)))
      (when (zerop ret)
        (goto-char (point-min))
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position))))))

(defun hub--shell-command (cmd)
  (shell-command (format "%s %s" (hub--executable) cmd)))

(defun hub--remote-url ()
  (let ((cmd "config --get remote.origin.url"))
    (or (hub--command-one-line cmd)
        (error "%s" cmd))))

(defun hub--extract-user-host (url)
  (if (string-match "[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'" url)
      (values (match-string 1 url) (match-string 2 url))
    (error "Match %s" url)))

(defun hub--collect-pullreqs ()
  (let ((remote-url (hub--remote-url)))
    (cl-multiple-value-bind (user repo) (hub--extract-user-host remote-url)
      (let ((issues (gh-pulls-list hub-api user repo)))
        (if (null issues)
            (error "This repo has no issues!")
          (sort (oref issues data)
                (lambda (a b) (> (oref a number) (oref b number)))))))))

(defun hub--from-issues-real-to-display (issue)
  (with-slots (number title state) issue
    (format "#%-4d [%s] %s" number state title)))

(defun hub--convert-issue-api-url (url)
  (replace-regexp-in-string
   "api\\." ""
   (replace-regexp-in-string "/repos" "" url)))

(defun hub--open-issue-url (candidate)
  (dolist (issue (helm-marked-candidates))
    (browse-url (oref issue html-url)
                (hub--convert-issue-api-url (oref issue url)))))

(defun hub--construct-issue-url (host remote-url issue-id)
  (cl-multiple-value-bind (user repo) (hub--extract-user-host remote-url)
    (format "https://%s/%s/%s/issues/%s"
            host user repo issue-id)))

(defun hub--pulls-view-common (url)
  (require 'diff-mode)
  (with-current-buffer (get-buffer-create "*hub-diff*")
    (view-mode -1)
    (erase-buffer)
    (unless (zerop (call-process-shell-command (concat "curl -s " url) nil t))
      (error "Can't download %s" url))
    (goto-char (point-min))
    (diff-mode)
    (view-mode)
    (pop-to-buffer (current-buffer))))

(defun hub--pulls-view-diff (candidate)
  (hub--pulls-view-common (oref candidate diff-url)))

(defun hub--pulls-view-patch (candidate)
  (hub--pulls-view-common (oref candidate patch-url)))

(defun hub--checkout-pull (candidate)
  (hub--command-one-line (format "checkout %s" (oref candidate html-url))))

(defun hub--merge-pull (candidate)
  (hub--command-one-line (format "merge %s" (oref candidate html-url))))

(defun hub--executable ()
  (cond
   ((executable-find "gh") "gh")
   ((executable-find "hub") "hub")
   (t (error "Can't find executable `gh' or `hub'"))))

(defvar hub--pulls-source
  '((name . "Open GitHub From Issues")
    (candidates . hub--collect-pullreqs)
    (volatile)
    (real-to-display . hub--from-issues-real-to-display)
    (action . (("Open issue page with browser" . hub--open-issue-url)
               ("View Diff" . hub--pulls-view-diff)
               ("View Patch" . hub--pulls-view-patch)
               ("Checkout" . hub--checkout-pull)
               ("Merge" . hub--merge-pull)))))

(defun hub--open-issue-directly (host)
  (let ((remote-url (hub--remote-url))
        (issue-id (read-number "Issue ID:")))
    (browse-url (hub--construct-issue-url host remote-url issue-id))))

(defun hub--host ()
  (or (hub--command-one-line "config --get hub.host")
      "github.com"))

(provide 'hub)
;;; hub.el ends here
