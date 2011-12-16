;; Author Dmitry Bushenko (d.bushenko@gmail.com)
;; License BSD
;;
;; The module provides the functions bound to the keys:
;; 1) C-<F9> opens a project file.
;; 2) <F2> generates the tags for the project and visits the tags table.
;; 3) <F7> quickly finds a file in the project.
;; 
;; Install
;; 1) Copy the file my-project.el to $HOME/.emacs.d
;; 2) Add the following to your .emacs: (load-file "~/.emacs.d/my-proj.el")
;;
;; Usage
;; Create 'project.el' in the project's root directory like this:
;; (defproject "My project"               ; Name of the project
;;   :root-dir "/root/dir"                ; Root directory of the project
;;   :sources "*.java"                    ; The file extension for etags
;;   :find-file '("*.java *.html *.css")  ; The file extension (one or more) for finding files. May be a string or a list of strings.
;;   :exclude "(SNAPSHOT)|(generated)")   ; The grep regexp for files and directories to exclude from search.

(defun collect-pairs (args)
  "Makes a list of pairs from a flat list. Provide it '(1 2 3 4 5 6)
   and you'll get '((1 2) (3 4) (5 6))"
  (let ((len (length args))
	pairs)
    (if (oddp len)
	(error "The number of arguments to 'defproject' should be even!"))
    (loop for i from 0 to len
	  do
	  (progn
	    (if (= i (1- len))
		(return))
	    (if (evenp i)
		(setq pairs (cons (list (nth i args) (nth (1+ i) args)) pairs)))))
    pairs))

(defun get-property (proj property)
  "Returns the value of the specified property.
Example: (get-property '((:a 1) (:b 2)) :b) -> 2"
  (let (result)
    (dolist (item proj)
      (if (equal (first item) property)
	  (setq result (second item))))
    result))

(defun get-sources (proj)
  (get-property proj :sources))

(defun get-root-dir (proj)
  (get-property proj :root-dir))

(defun get-find-file (proj)
  (get-property proj :find-file))

(defun get-exclude (proj)
  (get-property proj :exclude))

(defun defproject (name &rest args)
  "Use this function to describe your project. See the example at the top of the file.
Defines a global variable 'my-project' which contains the project properties."
  (let ((pairs (collect-pairs args)))
    (setq my-project pairs)))

(defun construct-find-file-string (params root-dir)
  "Constructs a shell command to find project files."
  (if (stringp params)
      (concat "cd " root-dir ";find . -name \"" params "\"") ;
    (let ((result (concat " -name \"" (first params) "\"")))
      (dolist (item (rest params))
	(setq result (concat result " -o -name \"" item "\"")))
      (concat "cd " root-dir ";find . " result))))

(defun construct-grep-string (params)
  "Constructs a shell command to exclude files from search results."
  (if (or (equal params nil) (string= params ""))
      ""
    (concat "|grep -i -v -E \"" params "\"")))

(defun my-find-file ()
  "Quick find file like in TextMate."
  (interactive)
  (let* ((root-dir (get-root-dir my-project))
	 (file-type (get-find-file my-project))
	 (files (split-string (shell-command-to-string
			       (concat (construct-find-file-string file-type root-dir)
				       (construct-grep-string (get-exclude my-project))))))
	 (selected (ido-completing-read "Select: " files)) )
    (message (concat "You've entered: " selected))
    (find-file (concat root-dir selected))))

(defun my-run-etags ()
  "Creates and visits the tags table for the project sources."
  (interactive)
  (let ((root-dir (get-root-dir my-project))
	(file-type (get-sources my-project)))
    (shell-command-to-string
     (concat (construct-find-file-string file-type root-dir) "  -print | xargs etags"))
    (visit-tags-table (concat root-dir "TAGS"))))

(defun my-project-load ()
  "Loads the specified project file."
  (interactive)
  (ido-find-file)
  (let ((file (buffer-file-name (current-buffer))))
    (load-file file))
  (kill-buffer (current-buffer))
  (message "The project is loaded!"))

;; Set the keys
;;
(global-set-key (kbd "<f2>") 'my-run-etags)
(global-set-key (kbd "<f7>") 'my-find-file)
(global-set-key (kbd "C-<f9>") 'my-project-load)
