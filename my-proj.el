;; Author Dmitry Bushenko (d.bushenko@gmail.com)
;; License BSD
;;
;; The module provides the functions bound to the keys:
;; 1) C-<F9> opens a project file. If loading fails, just open the project file, set pointer
;;    after the last bracket and evaluate it (C-x C-e).
;; 2) <F2> generates the tags for the project and visits the tags table.
;; 3) <F7> quickly finds a file in the project.
;; 4) C-<F7> grep on project sources.
;; 
;; Install
;; 1) Copy the file my-project.el to $HOME/.emacs.d
;; 2) Add the following to your .emacs: (load-file "~/.emacs.d/my-proj.el")
;;
;; Usage
;; Create 'project.el' in the project's root directory like this:
;; (defproject "My project"               ; Name of the project
;;   :root-dir "/root/dir"                ; Root directory of the project
;;   :items '((:messages-class "java/com/actionitem/client/localization/Messages.java")
;;            (:messages-text "resources/com/actionitem/client/localization/Messages.properties"))
;;   :sources "*.java"                    ; The file extension for etags
;;   :find-file '("*.java" "*.html")      ; The file extension (one or more) for finding files. May be a string or a list of strings.
;;   :exclude "SNAPSHOT|generated")       ; The grep regexp for files and directories to exclude from search.

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

(defun get-property (data property)
  "Returns the value of the specified property.
Example: (get-property '((:a 1) (:b 2)) :b) -> 2"
  (let (result)
    (dolist (item data)
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

(defun get-items (proj)
  (get-property proj :items))

(defun get-messages-class (proj)
  (concat
   (get-root-dir proj)
   (get-property (get-items proj) :messages-class)))

(defun get-messages-text (proj)
  (concat
   (get-root-dir proj)
   (get-property (get-items proj) :messages-text)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions are needed for the
;; function my-grep.

(defun my-create-files-window (files-buffer-name)
  (if (one-window-p)
      (split-window-vertically))
  (switch-to-buffer-other-window files-buffer-name))

(defun my-create-files-buffer (files-string files-buffer-name)
  (save-excursion
    (if (not (equal nil (get-buffer files-buffer-name)))
	(kill-buffer files-buffer-name))
    (get-buffer-create files-buffer-name)
    (set-buffer files-buffer-name)
    (insert files-string)))

(defun my-open-file-at-point ()
  (interactive)
  (let* ((start (line-beginning-position))
	 (end (line-end-position))
	 (str (buffer-substring (+ 2 start) end)))
    (find-file (concat (get-root-dir my-project) str))))

(defun my-decorate-line ()
    (let ((link-start (line-beginning-position))
	(link-end (line-end-position)))
    (add-text-properties link-start
			 link-end
			 '(mouse-face highlight
				      help-echo "Open this file"))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<RET>") 'my-open-file-at-point)
      (define-key map [mouse-1] 'my-open-file-at-point)
      (put-text-property link-start link-end 'keymap map))))

(defun my-make-files-buffer-clickable ()
  (dotimes (i (count-screen-lines))
    (goto-line (+ 1 i))
    (my-decorate-line))
  (setq buffer-read-only t))

(defun my-grep ()
  "The function runs grep on the project sources excluding the specified
 files. Then it opens another window with the search results. The files list
 is clickable."
  (interactive)
  (let* ((files-buffer-name "Files")
	 (find-files (get-find-file my-project))
	 (root-dir (get-root-dir my-project))
	 (prompt (cons "All" find-files))
	 files
	 text
	 command
	 results)
    (setq files (completing-read "In which files to search:" prompt))
    (setq text (read-string "Text to search: "))
    (if (string= files "All")
	(setq command (construct-find-file-string find-files root-dir))
      (setq command (construct-find-file-string (list files) root-dir)))
    (setq command (concat command
			  (construct-grep-string (get-exclude my-project))
			  "|xargs grep -s -l \"" text "\""))    
    (setq results (shell-command-to-string command))
    (my-create-files-buffer results files-buffer-name)
    (my-create-files-window files-buffer-name)
    (my-make-files-buffer-clickable)))


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
(global-set-key (kbd "C-<f7>") 'my-grep)
(global-set-key (kbd "C-<f9>") 'my-project-load)


;; Java-specific functions
;;

(defun my-localized-text ()
  (interactive)
  (save-excursion
    (let* ((jpath (get-messages-class my-project))
	   (tpath (get-messages-text my-project))
	   (jbuf (find-file jpath))
	   (tbuf (find-file tpath))
	   (name (read-string "Variable name: "))
	   (text (read-string "Text: ")))
      (save-excursion
	(with-current-buffer tbuf
	  (end-of-buffer)
	  (insert (concat "\n" name " = " text))
	  (save-buffer)
	  (kill-buffer tbuf))	   
	(with-current-buffer jbuf
	  (end-of-buffer)
	  (search-backward "}")
	  (insert (concat "\n    @Key(\"" name "\")\n    String " name "();\n"))
	  (save-buffer)
	  (kill-buffer jbuf)))
      (insert (concat "MessageFactory.messages()." name "()")))))


;; Semantic


;; Getters/setters
(defun current-class ()
  (semantic-current-tag-of-class 'type))

(defun class-var (var-name class)
  (let ((members
	 (semantic-tag-get-attribute class :members)))
    (first (semantic-find-tags-by-name var-name members))))

(defun make-getter (type name capitalized-name)
  (concat "\npublic " type " get" capitalized-name "() {\n"
	  "\treturn " name ";\n"
	  "}\n\n"))

(defun make-setter (type name capitalized-name)
  (concat "\npublic void set" capitalized-name "(" type " " name ") {\n"
	  "this." name " = " name ";\n"
	  "}\n\n"))

(defun make-accessor ()
  (interactive)
  (let* ((class (current-class))
	 (tag (class-var (current-word) class))
	 (name (semantic-tag-name tag))
 	 (capitalized-name (capitalize name))
	 (type (semantic-tag-get-attribute tag :type))
	 (start (- (semantic-tag-end class) 1)))
    (save-excursion
      (goto-char start)
      (insert (make-getter type name capitalized-name))
      (insert (make-setter type name capitalized-name))
      (indent-region start (point)))))











