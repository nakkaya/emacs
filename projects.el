(setq currentProject "" )
(setq na-list-of-projects (list) )
(setq na-project-alist '())

(defun na-project-load-list ()
   ""
   (interactive)
   (setq file (expand-file-name "~/.my-project" ) )
   (when (file-readable-p file)
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-min))
       (while (not (eobp))
	 (setq line (buffer-substring-no-properties 
		     (progn (beginning-of-line) (point))
		     (progn (end-of-line) (point))))
	 (setq na-list-of-projects (cons (car(split-string line)) na-list-of-projects))
	 (setq na-project-alist (cons (split-string line ) na-project-alist ) )
         (forward-line)))))
(na-project-load-list)

(defun na-project-save-list()
(interactive)
(with-temp-buffer
(dolist (project na-project-alist)
  (dolist (item project )
    (insert (concat item " "))
      )
  (insert "\n"))
(write-region (point-min) (point-max) "~/.my-project")))

(defun na-find-project(proName)
(setq returnProject nil )
;;tmp project list
(setq pList na-project-alist)
;;iterate through the project list
(while pList
  (setq project (car pList))
  ;;get project name
  (setq pName (car project) )    
  ;;when found launch shell set tags 
  (if (string-equal pName  proName )
      (setq returnProject project))
(setq pList (cdr pList)))
returnProject)

(defun na-visit-javadoc()
(interactive)
(beginning-of-buffer)
;; locate package
(setq packageName "" )
(if (re-search-forward 
		   "package \\(.*?\\);" nil t)
		  (setq packageName 
			(buffer-substring-no-properties 
			 (match-beginning 1) (- (point) 1) ) ) )
;; locate class name
(setq className "" )
(if (re-search-forward 
		   "class .*?\\(.*?\\){" nil t)
		  (setq className 
			(buffer-substring-no-properties 
			 (match-beginning 1) (- (point) 1) ) ) )

(setq classPath (concat  packageName "/"  className ".html" ) )

(with-temp-buffer
(insert classPath)
;;replace . with /
(while (search-forward "." nil t)
  (replace-match "/" nil t))
;;replace space with nothing
(beginning-of-buffer)
(while (search-forward " " nil t)
  (replace-match "" nil t))
(setq classPath (buffer-string) ))

(setq project (na-find-project currentProject ))
(if (not (eq project nil))
    (progn 
      (setq name (car project) )
      (setq path (car(cdr project)) )
      ))    

(setq classPath (concat  path "doc/" classPath ) )
(browse-url (expand-file-name classPath) ))

(define-key java-mode-map (kbd "C-c C-v") 'na-visit-javadoc)

(defun na-create-tags()
(interactive)
  (setq project (na-find-project currentProject ))
  (if (not (eq project nil))
      (progn 
	(setq pName (car project) )
	(setq pPath (car(cdr project)) )
	(setq pLang (car(cdr (cdr project))))

	(setq currentBuffer (current-buffer) )

	(term "/bin/bash")	
	(if (string= pLang "Java" )
	    (term-send-string "*terminal*" "find . -type f -name \"*.java\" | etags -" ))
	(if (string= pLang "Cpp" )
	    (term-send-string "*terminal*" "find . -type f -name \"*.cpp\" -o -name \"*.h\" | etags - " ))
	(term-send-input)

	(switch-to-buffer currentBuffer)
	)))


(defun na-project-kill-terms()
  ;;kill previous buffers
  (if (not (eq (get-buffer "build")  nil ) )
      (kill-buffer "build"))
  (if (not (eq (get-buffer "*terminal*")  nil ) )
      (kill-buffer "*terminal*")))

(defun na-switch-project ( projectName )
  "Switch to a given project."
  (interactive
   (list (completing-read "Project: " na-list-of-projects )))

  (setq currentProject projectName )

  (setq project (na-find-project currentProject ))
  (if (not (eq project nil))
      (progn 
	(setq pName (car project) )
	(setq pPath (car(cdr project)) )
	;;create tag file path
	(setq tagFile (concat pPath "TAGS") )
	;;set it
	(if (not (eq (get-buffer "TAGS")  nil ) )
	    (kill-buffer "TAGS"))
	(tags-reset-tags-tables)
	(setq tags-file-name  tagFile )

	(na-project-kill-terms)

	;;setup shells one for build one for running
	;;build
	(term "/bin/bash")
	(term-send-string "*terminal*" (concat "cd " pPath "build/") )
	(term-send-input)
	(rename-buffer "build" )
	;;main dir
	(term "/bin/bash")
	(term-send-string "*terminal*" (concat "cd " pPath) )
	(term-send-input)
	
	;;visit project name
	(find-tag pName))))

(defun na-create-compiled-project-directory()
  (term "/bin/bash")
  (term-send-string "*terminal*" (concat "cd") )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectPath) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "mkdir " newProjectName) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectName) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "mkdir " "build") )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " "build") )
  (term-send-input)
  (rename-buffer "build" )
  (term "/bin/bash")
  (term-send-string "*terminal*" (concat "cd") )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectPath) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectName) )
  (term-send-input)
  
  (if (string= progLanguage "Java" )
      (na-create-poject-skeleton-java newProjectName newProjectPath))
  (if (string= progLanguage "Cpp" )
      (na-create-poject-skeleton-cpp newProjectName newProjectPath)))

(defun na-create-dynamic-project-directory()
  (term "/bin/bash")
  (term-send-string "*terminal*" (concat "cd") )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectPath) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "mkdir " newProjectName) )
  (term-send-input)
  (term-send-string "*terminal*" (concat "cd " newProjectName) )
  (term-send-input))

(defun na-create-project(newProjectPath)
  (interactive "FProject Path: ")
(setq newProjectName (read-string "Project Name: ") )
(setq progLanguage (completing-read "Language: " '("Java" "Cpp" "Clojure") ) )

(if (eq (y-or-n-p "Temporary Project? ") nil )
    (progn 
      (setq projObject (list newProjectName newProjectPath progLanguage))
      (setq na-list-of-projects (cons (car projObject) na-list-of-projects))
      (setq na-project-alist (cons projObject na-project-alist ) )
      (na-project-save-list)))

(na-project-kill-terms)

;;create project directories
(if (or (string-equal progLanguage "Java") (string-equal progLanguage "Cpp"))
    (na-create-compiled-project-directory)
  (na-create-dynamic-project-directory) ))

(defun na-create-poject-skeleton-java( newProjectName  newProjectPath )
;;create ant file
(find-file (concat newProjectPath "/" newProjectName "/build.xml" ))
(skel-java-ant)
(indent-region (point-min) (point-max))
(save-buffer)
;;create main
(find-file (concat newProjectPath "/" newProjectName "/" newProjectName ".java" ))
(skel-java-main)
(indent-region (point-min) (point-max))
(save-buffer))

(defun na-create-poject-skeleton-cpp( newProjectName  newProjectPath )
;;create main
(find-file (concat newProjectPath "/" newProjectName "/" newProjectName ".cpp" ))
(skel-cpp-main)
(indent-region (point-min) (point-max))
(save-buffer))

;;
;;Skeletons for building stubs
;;
(define-skeleton skel-java-main
  "Create Common Java Main Template"
  nil
  \n >
  "class " newProjectName " {"
  \n >
  "public static void main(String[] args) {"
  \n >
  ""_""
  \n >
  "}"
  \n >
  "}"
  \n >
  )

(define-skeleton skel-java-ant
  "Create an Ant template for project"
  nil
  \n >
  "<project>"
  \n >
  \n >
  "<target name=\"clean\">"
  \n >
  "<delete includeemptydirs=\"true\">"
  \n >  
  "<fileset dir=\"build/\" "
  \n >
  "includes=\"**/*\" "
  \n >
  "excludes=\"**/Icon*\"/>"
  \n >
  "</delete>"
  \n >
  "</target>"

  \n >
  \n >
  "<target name=\"compile\" depends=\"clean\">"
  \n >
  "<javac srcdir=\".\" includes=\"" newProjectName ".java\" destdir=\"build/\" />"
  \n >
  "</target>"

  \n >
  \n >
  "<target name=\"run\" depends=\"compile\">"
  \n >
  "<java fork=\"true\" dir=\"build/\" classname=\"" newProjectName "\"/>"
  \n >
  "</target>"

  \n >
  \n >
  "<target name=\"jar\">"
  \n >
  "<jar destfile=\"build/" newProjectName ".jar\" basedir=\"build/\">"
  \n >
  "<manifest>"
  \n >
  "<attribute name=\"Main-Class\" value=\"" newProjectName "\"/>"
  \n >
  "</manifest>"
  \n >
  "<fileset dir=\"build\">"
  \n >
  "<include name=\"**/*.class\"/>"
  \n >
  "<exclude name=\"**/*.jar\"/>"
  \n >
  "</fileset>"
  \n >
  "</jar>"
  \n >
  "</target>"
  \n >
  "</project>"
  \n >
)

(define-skeleton skel-cpp-main
  ""
  nil
  \n >
  "#include <iostream>"
  \n >
  "using namespace std;"
  \n >
  "int main(int argc, char *argv[]){"
  \n >
  _ 
  \n >
  "return 0;"
  \n >
  "}"
  \n >)
