;;; jupyters-manager.el --- Opening jupyter files from within dired; managing running server sessions (opening/closing servers)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(cl-defstruct jupyter-notebook-list-item url pre-question-mark-url-part port past-question-mark-url-part base-dir)

(defun jm-dired-find-jupyter-servers ()
  "Returns tuples of (server url, token, server base dir)."
  (let* ((str-output (shell-command-to-string "jupyter notebook list"))
         urls-base-dirs)
    ;; compile a list of urls and base-dirs
    (setq urls-base-dirs (remove nil
                                 (mapcar (lambda (str)
                                           (when (string-match ;; "\\(^http.+\\)\s+::\s+\\(.+\\)$"
                                                  "\\(?1:\\(?2:^http.+:\\(?3:[0-9]+\\).+\\)\\(?4:\\?.+\\)\\)\s+::\s+\\(?5:.+\\)$"
                                                  ;; "\\(?1:\\(?2:^http.+\\)\\(?3:\\?.+\\)\\)\s+::\s+\\(?4:.+\\)$"
                                                               str)
                                             (make-jupyter-notebook-list-item
                                              :url
                                              (match-string 1 str)
                                              :pre-question-mark-url-part
                                              (match-string 2 str)
                                              :port
                                              (match-string 3 str)
                                              :past-question-mark-url-part
                                              (match-string 4 str)
                                              :base-dir
                                              (match-string 5 str))))
                                         (split-string str-output "\n"))))))

(defun file-exists-somewhere-within-folder-p (file-path root-path)
  "check if file-path is in a subdirectory under root-path and not somewhere else."
  (let* ((rel-dir-path (file-relative-name file-path root-path)))
    (if (or (not (file-exists-p root-path))
            (not (file-exists-p file-path))
            (string-match-p (regexp-quote "..") rel-dir-path))
        nil
      rel-dir-path)))


(defconst new-server-candidate (list "new server" "new server"))

(defun jm-my-source-open-with-server (filepath)
  (if (equal (length (helm-marked-candidates)) 1)
      (if (equal (car new-server-candidate) (car (car (helm-marked-candidates))))
          (progn
            (message "Open in new server selected.")
            (jm-open-file-with-new-server filepath))
        (jm-open-file-with-running-server filepath
                                          (car (car (helm-marked-candidates)))))
    (message "Please select only one server for this action.")))

(defun jm-source-shutdown-servers (filepath)
  (remove (car new-server-candidate)
          (helm-marked-candidates))
  (jm-shutdown-these-servers filepath))

(defun jm-dired-open-notebook (&optional filepath)
  "This will open a new jupyter server. "
  (unless filepath
    (setq filepath (cond
                    ((and (eq major-mode 'dired-mode)
                          (dired-get-filename nil t))
                     (dired-get-filename nil t))
                    (t nil ;; (format-time-string "untitled_%Y-%m-%d_%H-%M-%S.ipynb")
                       ))))
    ;; compile a list of jupyter notebook servers
  ;; with a base-dir that is an (nth-order) parent
  ;; of the file's directory is already running
  (let* ((list-of-jupyter-servers (jm-dired-find-jupyter-servers))
         (helm-source-candidates (remove nil
                                         (mapcar (lambda (jupyter-server)
                                                   (when (file-exists-somewhere-within-folder-p filepath
                                                                                                (jupyter-notebook-list-item-base-dir jupyter-server))
                                                     jupyter-server))
                                                 list-of-jupyter-servers))))
    (helm :sources (helm-build-sync-source "select possible parent jupyter server"
                     :header-name (lambda (_)
                                    (format "header name"))
                     :candidates
                     (lambda ()
                       (append (mapcar (lambda (candidate)
                                         (list ;; (prin1-to-string candidate)
                                          (concat "running on port "
                                                  (jupyter-notebook-list-item-port candidate)
                                                  ", base dir: "
                                                  (jupyter-notebook-list-item-base-dir candidate))
                                          candidate))
                                       helm-source-candidates)
                               (list new-server-candidate)))
                     :action (helm-make-actions "Open the file with this server. "
                                                (lambda (_)
                                                  (jm-my-source-open-with-server filepath))
                                                "Shutdown these servers"
                                                (lambda (_)
                                                  (jm-source-shutdown-servers filepath)))))))


(defun jm-open-file-with-running-server (filepath &optional server-candidate)
  (let* ((compiled-url (concat (jupyter-notebook-list-item-pre-question-mark-url-part
                                server-candidate)
                               "notebooks/"
                               (file-relative-name filepath
                                                   (jupyter-notebook-list-item-base-dir server-candidate))
                               (jupyter-notebook-list-item-past-question-mark-url-part
                                server-candidate))))
    (browse-url-default-browser compiled-url)))

(defun jm-open-file-with-new-server (&optional filepath)
  ;; no servers is yet running for this file, so create new server
  (if filepath
      (call-process "jupyter-notebook" nil 0 nil
                    filepath)
    (let* ((default-directory (jm-ask-to-launch-jupyter-in-dir default-directory
                                                               "launch jupyter-notebook from this directory: ")))
      (call-process "jupyter-notebook" nil 0 nil))))

(defun jm-ask-to-launch-jupyter-in-dir (base-directory message)
  "Only if it's not already a directory."
  (let* ((directory (helm-read-file-name message
                                         :initial-input base-directory)))
    (unless (file-exists-p directory)
      (if (y-or-n-p (concat "create new directory " directory
                            " ?"))
          (make-directory directory 'parents))
      (message "doing nothing"))
    directory))

(defun jm-shutdown-these-servers (server-candidates)
  (message "stopping server")
  (mapcar (lambda (server-candidates)
            (setq server-candidates (car server-candidates))
            ;; TODO shutdown the notebook at port
            (call-process "jupyter"
                          nil
                          0
                          nil
                          "notebook"
                          "stop"
                          (jupyter-notebook-list-item-port server-candidates)))
          (helm-marked-candidates)))


(defun jm-new-server ()
  "launch new jupyter server from within emacs"
  (interactive)
  (jm-open-file-with-new-server nil))

(provide 'jupyters-manager)
;;; jupyters-manager.el ends here
