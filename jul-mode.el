;;; jul-mode.el --- Simple package system for Dragora -*- lexical-binding: t -*-

;; Copyright (C) 2016 Kevin Bloom <kdb4@openmailbox.org>

;; Author: Kevin Bloom <kdb4@openmailbox.org>
;; Created: 16 May 2016
;; Version: 0.2
;; Keywords: application
;; Package-Requires: ((tabulated-list "1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Changelog:

;; 31 May 2016 - Wrote the upgrading functions. They work, however, bugs!
;; 23 May 2016 - Wrote the installation and removal functions. They work!
;; 20 May 2016 - Started adding keys for doing tasks. Got installed stuff
;;               working.
;; 19 May 2016 - Got pulling of all repos and listing them on screen working.
;; 17 May 2016 - Got commication with server.
;; 16 May 2016 - Created package. On this day, I got something working.

;;; Commentary:

;; The idea behind jul-mode is to bring jul, the "package manager" for the
;; Dragora user repo, to Emacs.  This will hopefully make jul and the repos
;; easier to use and more powerful.  That being said, you will (hopefully) be
;; able to do most of your package management in jul-mode as well.

;; It's important to note that all of the code is based on (or from) the source
;; for the built-in Emacs package `package.el'.  Without that source, it would
;; have taken ages to write this.

;;; Todo:

;; * Test installation and removal of packages.
;; * Get updating working

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tabulated-list)

(defgroup jul-package nil
  "Manager for Dragora User packages."
  :group 'applications
  :version "0.2")

(defcustom jul-package-repos
	'(("frusen" . "http://gungre.ch/dragora/repo/frusen/stable/")
		("kelsoo" . "http://gungre.ch/dragora/repo/kelsoo/")
		("mprodrigues" . "http://gungre.ch/dragora/repo/mprodrigues/"))
  "An alist of archives from which to fetch.
The defaults include all the repos found on the gungre.ch site.
Note that frusen has 3 different ones.

Each element has the form (ID . LOCATION).
ID is an archive name, as a string.
LOCATION specifies the base location for the archive.
If it starts with \"http:\", it is treated as a HTTP URL;
otherwise it should be an absolute directory name.
 (Other types of URL are currently not supported.)

Only add locations that you trust, since fetching and installing
a package can run arbitrary code."
  :type '(alist :key-type (string :tag "Repo name")
                :value-type (string :tag "URL or directory name"))
  :risky t
  :group 'jul-package
  :version "0.2")

(cl-defstruct (jul-package-desc
               ;; Rename the default constructor from `make-package-desc'.
               (:constructor jul-package-desc-create)
               ;; Has the same interface as the old `define-package',
               ;; which is still used in the "foo-pkg.el" files. Extra
               ;; options can be supported by adding additional keys.
               (:constructor
                jul-package-desc-from-define
                (name-string version-string arch-string repo-string build-string
														 &aux
														 (name (intern name-string))
														 (version version-string)
														 (arch arch-string)
														 (repo repo-string)
														 (build build-string))))
  "Structure containing information about an individual package.
Slots:

`name'	Name of the package, as a symbol

`version' Version of the package, as a string

`arch' Architecture of the package, as a string

`repo' The name of the archive (as a string) whence this came

`build' The current build, as a string
	package came."
  name
  version
	arch
  repo
	build)

;; Although not common Emacs Lisp practice, I find it helpful to have earmuffs
;; on the "global" variables.
(defvar *jul-package-installed* nil
	"This list contains all the packages that are currently installed on your
system.  Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")
(put '*jul-package-installed* 'risky-local-variable t)

(defvar *jul-package-repo* nil
	"This list contains all the packages that are currently on the gungre.ch
server.  Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")
(put '*jul-package-repo* 'risky-local-variable t)

(defvar *jul-package-temp-dir* "~/.emacs.d/elpa/jul-mode/"
	"This variable contains the directory in which jul-mode stuff is done
temporarily.")
(put '*jul-package-temp-dir* 'risky-local-variable t)

(defvar *jul-package-installed-dir* "/var/db/pkg/"
	"This variable contains the directory in which Dragora keeps installed
packages")
(put '*jul-package-installed-dir* 'risky-local-variable t)

(defvar *jul-database* "http://gungre.ch/jul/"
	"Contains the directory that holds the .db files for jul")
(put '*jul-database* 'risky-local-variable t)

(defvar jul-package-menu-mode-map
  (let ((map (make-sparse-keymap))
				(menu-map (make-sparse-keymap "jul-package")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "u" 'jul-package-menu-mark-unmark)
    (define-key map "d" 'jul-package-menu-mark-delete)
    (define-key map "i" 'jul-package-menu-mark-install)
    (define-key map "U" 'jul-package-menu-mark-upgrades)
    (define-key map "r" 'jul-package-menu-refresh)
    (define-key map "f" 'jul-package-menu-filter)
    (define-key map "x" 'jul-package-menu-execute)
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
									:help "Quit package selection"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
									:help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
									:help "Previous Line"))
		(define-key menu-map [mi]
      '(menu-item "Mark for Install" jul-package-menu-mark-install
									:help "Mark package for installation, move to the next line"))
		(define-key menu-map [mu]
      '(menu-item "Remove mark" jul-package-menu-mark-unmark
									:help "Unmark a package from removal, installation, etc."))
		(define-key menu-map [md]
      '(menu-item "Mark for Removal" jul-package-menu-mark-delete
									:help "Mark package for removal, move to the next line"))
		(define-key menu-map [mupgrades]
      '(menu-item "Mark Upgradable Packages" jul-package-menu-mark-upgrades
									:help "Mark packages that need upgraded"))
		(define-key menu-map [mx]
      '(menu-item "Execute Actions" jul-package-menu-execute
									:help "Perform all the marked actions"))
    map)
  "Local keymap for `jul-package-menu-mode' buffers.")

(defun jul-package-menu--find-upgrades ()
  (let (installed available upgrades)
    ;; Build list of installed/available packages in this buffer.
    (dolist (entry tabulated-list-entries)
      ;; ENTRY is (PKG-DESC [NAME VERSION STATUS DOC])
      (let ((pkg-desc (car entry))
						(repo (aref (cadr entry) 5)))
				(cond ((string= repo "installed")
							 (push pkg-desc installed))
							(t											 ;assuming everything else isn't installed
							 (push (cons (jul-package-desc-name pkg-desc) pkg-desc)
                     available)))))

		;; Remove packages that aren't the same arch
		(let ((arch (jul-package-desc-arch (car (last installed)))))
			(dolist (elt available)							;remove other arch
				(unless (string= arch (jul-package-desc-arch (cdr elt)))
					(setf available (remove elt available)))))

		;; Loop through list of installed packages, finding upgrades.
    (dolist (pkg-desc installed)
			(let ((avail-pkg (assq (jul-package-desc-name pkg-desc) available)))
				(when avail-pkg
					(let ((number-inst-list-ver
								 (mapcar #'string-to-number
												 (split-string
													(jul-package-desc-version pkg-desc) "[.]")))
								(number-avail-list-ver
								 (mapcar #'string-to-number
												 (split-string
													(jul-package-desc-version (cdr avail-pkg)) "[.]"))))
						(when (version-list-<
									 number-inst-list-ver
									 number-avail-list-ver)
							(push avail-pkg upgrades))))))
    upgrades))

(defun jul-package-menu-mark-upgrades ()
	"This function will select all the packages to be upgraded.  Note that when
upgrading the `pkg' program (Dragora's package manipulation program) will
automatically update the installed list.  Therefore, you do not need to select
an installed package for removal."
	(interactive)
	(let ((packs-to-upgrade (jul-package-menu--find-upgrades)))
		(if (null packs-to-upgrade)
				(message "No packages to upgrade.")
			(progn
				(goto-char (point-min))
				(while (not (eobp))
					(let* ((pkg-desc (tabulated-list-get-id))
								 (upgrade (cdr (assq (jul-package-desc-name pkg-desc)
																		 packs-to-upgrade))))
						(cond ((null upgrade)
									 (forward-line 1))
									((equal pkg-desc upgrade)
									 (tabulated-list-put-tag "U" t))
									(t
									 (tabulated-list-put-tag " " t)))))))))


(defun jul-package-desc-full-name (pkg-desc)
	"PKG-DESC should get the tabulated list ID.
This function just concats the package name with the version"
  (format "%s-%s"
          (jul-package-desc-name pkg-desc)
          (jul-package-desc-version pkg-desc)))

(defun jul-package-upgrade (pkg)
	"PKG should be the full package ID from the tabulated list.
This function will download and install PKG using the Dragora's package
manipulation tool 'pkg'."
	(let* ((pack-name (format "%s" (jul-package-desc-name pkg)))
				 (full-tlz (concat pack-name "-"
													 (jul-package-desc-version pkg) "-"
													 (jul-package-desc-arch pkg) "-"
													 (jul-package-desc-build pkg)
													 ".tlz"))
				 (full-tlz-path (concat *jul-package-temp-dir* full-tlz))
				 (repo))
		(dolist (elt jul-package-repos)
			(when (string= (jul-package-desc-repo pkg) (car elt))
				(setf repo (cdr elt))))
		(with-temp-file full-tlz-path
			(url-insert-file-contents (concat repo pack-name "/" full-tlz)))
		(async-shell-command (concat "sudo pkg upgrade " full-tlz-path))))


(defun jul-package-install (pkg-list)
	"PKG-LIST should be the full package ID from the tabulated list.
This function will download and install PKG using the Dragora's package
manipulation tool 'pkg'."
	(let (string-of-pkg)
		(dolist (pkg pkg-list)
			(let* ((pack-name (format "%s" (jul-package-desc-name pkg)))
						 (full-tlz (concat pack-name "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg)
															 ".tlz"))
						 (full-tlz-path (concat *jul-package-temp-dir* full-tlz))
						 (repo))
				(dolist (elt jul-package-repos)
					(when (string= (jul-package-desc-repo pkg) (car elt))
						(setf repo (cdr elt))))
				(with-temp-file full-tlz-path
					(url-insert-file-contents	(concat repo pack-name "/" full-tlz)))
				(setf string-of-pkg (concat string-of-pkg full-tlz-path " "))))
		(async-shell-command (concat "sudo pkg add " string-of-pkg))))

(defun jul-package-delete (pkg-list)
	"PKG-LIST should be the tabulated list ID of the package.
This function will uninstall and remove installed packages using the Dragora
package manipulation tool 'pkg'."
	(let (string-of-pkg)
		(dolist (pkg pkg-list)
			(let* ((full-tlz (concat (format "%s" (jul-package-desc-name pkg)) "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg) ".tlz"))
						 (full-tlz-path (concat *jul-package-installed-dir* full-tlz)))
				(setf string-of-pkg (concat string-of-pkg full-tlz-path " "))))
		(async-shell-command
		 (concat "sudo pkg remove " string-of-pkg))))

(defun jul-package-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.
Optional argument NOQUERY non-nil means do not ask the user to confirm."
  (interactive)
  (unless (derived-mode-p 'jul-package-menu-mode)
    (error "The current buffer is not in Package Menu mode"))
  (let (install-list delete-list upgrade-list cmd pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
				(setq cmd (char-after))
				(unless (eq cmd ?\s)
					;; This is the key PKG-DESC.
					(setq pkg-desc (tabulated-list-get-id))
					(cond ((eq cmd ?D)
								 (push pkg-desc delete-list))
								((eq cmd ?I)
								 (push pkg-desc install-list))
								((eq cmd ?U)
								 (push pkg-desc upgrade-list))))
				(forward-line)))
    (when install-list
      (if (or
           noquery
           (yes-or-no-p
            (if (= (length install-list) 1)
                (format "Install package `%s'? "
                        (jul-package-desc-full-name (car install-list)))
              (format "Install these %d packages (%s)? "
                      (length install-list)
                      (mapconcat #'jul-package-desc-full-name
                                 install-list ", ")))))
					(jul-package-install install-list)))
    ;; Delete packages, prompting if necessary.
    (when delete-list
      (if (or
           noquery
           (yes-or-no-p
						(if (= (length delete-list) 1)
								(format "Delete package `%s'? "
												(jul-package-desc-full-name (car delete-list)))
							(format "Delete these %d packages (%s)? "
											(length delete-list)
											(mapconcat #'jul-package-desc-full-name
																 delete-list ", ")))))
					(jul-package-delete delete-list)))
    (when upgrade-list
      (if (or
           noquery
           (yes-or-no-p
            (if (= (length upgrade-list) 1)
                (format "Upgrade package `%s'? "
                        (jul-package-desc-full-name (car upgrade-list)))
              (format "Upgrade these %d packages (%s)? "
                      (length upgrade-list)
                      (mapconcat #'jul-package-desc-full-name
                                 upgrade-list ", ")))))
					(mapc 'jul-package-upgrade upgrade-list)))
    (if (or delete-list install-list upgrade-list)
				(jul-package-menu--generate t t)
      (message "No operations specified."))))

(defun jul-package-menu-mark-delete (&optional _num)
	"Mark current package for deletion/removal and move to the next line."
	(interactive "p")
	(if (string= (jul-package-menu-get-repo) "installed")
			(tabulated-list-put-tag "D" t)
		(forward-line)))

(defun jul-package-menu-mark-unmark (&optional _num)
	"Clear any marks on a package and move to the next line."
	(interactive "p")
	(tabulated-list-put-tag " " t))

(defun jul-package-menu-mark-install (&optional _num)
  "Mark a package for installation and move to the next line."
  (interactive "p")
  (if (string= (jul-package-menu-get-repo) "installed")
			(forward-line)
		(tabulated-list-put-tag "I" t)))

(defun jul-package-menu-get-repo ()
  (let* ((id (tabulated-list-get-id))
				 (entry (and id (assq id tabulated-list-entries))))
    (if entry
				(aref (cadr entry) 5)
      "")))

(define-derived-mode jul-package-menu-mode tabulated-list-mode "Jul Package Menu"
	"Major mode for browsing a list of packages"
	(setq tabulated-list-format
        `[("Package" 25 nil) ;there will eventually be something (not just nil)
          ("Version" 20 nil)
					("Arch" 10 nil)
					("Build" 10 nil)
          ("Status"  10 nil)
					("Repo" 10 nil)])
	(setq tabulated-list-padding 2)
	;; There will be some sort of refreshing thing
	(setq tabulated-list-sort-key (cons "Repo" nil))
	(add-hook 'tabulated-list-revert-hook 'jul-package-menu--refresh nil)
  (tabulated-list-init-header))

(defun jul-package--push (pkg-desc listname)
	"A simple funtion used to check if the current package is alright in the list
to be printed to the screen."
	(unless (member pkg-desc listname)
		pkg-desc))

(defun jul-hyphenated-name-fixer (split-pack)
	"SPLIT-PACK should be the full name of a package after being split by the
split-string function with '-'.

This function will make sure that it doesn't use a part of the name of the
package as the version, build, or arch.  It does this by checking the size of
SPLIT-PACK."
	(if (> (length split-pack) 4)
			(let* ((fir-word (car split-pack))
						 (sec-word (cadr split-pack))
						 (new-word (concat fir-word "-" sec-word)))
				(jul-hyphenated-name-fixer (cons new-word (remove sec-word
																													(remove fir-word
																																	split-pack)))))
		split-pack))

(defun jul-parse-n-place (file repo)
	(with-temp-buffer
		(insert-file-contents file)
		(let ((num-of-packs (count-lines (point-min) (point-max)))
					(current-pack 0)
					(pack-list nil))
			(while (< current-pack num-of-packs)
				(let ((first-point (point)))
					(end-of-line)
					(copy-region-as-kill first-point (point))
					(let* ((split-pack (jul-hyphenated-name-fixer
															(split-string (car kill-ring) "-")))
								 (struct-pack (jul-package-desc-from-define
															 (car split-pack)
															 (cadr split-pack)
															 (car (cddr split-pack))
															 repo
															 (car (split-string
																		 (cadr (cddr split-pack)) ".tlz")))))
						(setf pack-list (cons (cons
																	 (jul-package-desc-name struct-pack)
																	 struct-pack)
																	pack-list)))
					(forward-line)
					(beginning-of-line))
				(setf current-pack (+ current-pack 1)))
			pack-list)))

(defmacro jul-package--with-work-buffer (location file &rest body)
  "Run BODY in a buffer containing the contents of FILE at LOCATION.
LOCATION is the base location of a package archive, and should be
one of the URLs (or file names) specified in `package-archives'.
FILE is the name of a file relative to that base location.

This macro retrieves FILE from LOCATION into a temporary buffer,
and evaluates BODY while that buffer is current.  This work
buffer is killed afterwards.  Return the last value in BODY."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (if (string-match-p "\\`https?:" ,location)
				 (url-insert-file-contents (concat ,location ,file))
       (unless (file-name-absolute-p ,location)
				 (error "Repo location %s is not an absolute file name"
								,location))
       (insert-file-contents (expand-file-name ,file ,location)))
     ,@body))

(defun jul-package--download-one-repo (repo file)
  "Retrieve an repo FILE from REPO, and cache it.
REPO should be a cons cell of the form (NAME . LOCATION),
similar to an entry in `*jul-package-repo*'.  Save the cached copy to
`*jul-package-temp-dir*' under the 'repos/' directory.
SERVER-FILE is the name of the file on the server.  Since sometimes this must be
empty, we need two seperate args; one for server name and the other for the
local name."
  (let ((dir (expand-file-name (format "repos/%s" (car repo))
															 *jul-package-temp-dir*)))
    (jul-package--with-work-buffer (cdr repo) file
      ;; Read the retrieved buffer to make sure it is valid (e.g. it
      ;; may fetch a URL redirect page).
      ;; (when (stringp (read (current-buffer)))
			(progn
				(make-directory dir t)
        (write-region nil nil (expand-file-name file dir) nil 'silent)))))

;; Needs improved, maybe make a list of directories to remove?
(defun jul-remove-unwanted-directories (list-of-files)
	"LIST-OF-FILES should be the contents of installed directory.
This function will remove the directories we don't wish to show in jul-mode."
	(remove "."
					(remove ".."
									(remove "description"
													(remove "post-install"
																	(remove "pre-post"
																					(remove "removed"
																									list-of-files)))))))

(defun jul-package-refresh-contents ()
	"This function will grab the current version of the database files on the user
repo and add all the packages to a big list.

It also probes the installed directory on your system and makes a list of all
the installed programs."
	(unless (file-directory-p *jul-package-temp-dir*)
		(make-directory *jul-package-temp-dir*))
	(setf *jul-package-repo* nil)					;clear current uninstalled items list
	(setf *jul-package-installed* nil) ;clear current installed items list
	(dolist (elt jul-package-repos)
		(let* ((name (car elt))
					 (database (cons name *jul-database*))
					 (db-file-name (concat name ".db"))
					 (db-location
						(concat *jul-package-temp-dir* "repos/" name "/"  db-file-name)))
			(jul-package--download-one-repo database db-file-name)
			(setf *jul-package-repo* (append
																(jul-parse-n-place db-location name)
																*jul-package-repo*))))
	(let* ((installed-pack-dir-contents (directory-files
																			 *jul-package-installed-dir*))
				 (installed-pack-list
					(jul-remove-unwanted-directories installed-pack-dir-contents)))
		(dolist (elt installed-pack-list)
			(let* ((split-pack (jul-hyphenated-name-fixer (split-string elt "-")))
						 (cur-pack-struct (jul-package-desc-from-define
															 (car split-pack)
															 (cadr split-pack)
															 (car (cddr split-pack))
															 "installed"
															 (cadr (cddr split-pack))))
						 (cur-pack-list (cons (jul-package-desc-name cur-pack-struct)
																	cur-pack-struct)))
				(setf *jul-package-installed* (cons cur-pack-list
																						*jul-package-installed*))))))

(defun jul-package-menu--print-info (pkg-desc)
	"Convert the complicated jul-package-desc-struct, to something the tabulated
mode can read.  PKG-DESC is of form jul-package-desc-struct"
  (let ((name (jul-package-desc-name pkg-desc))
				(version (jul-package-desc-version pkg-desc))
				(arch (jul-package-desc-arch pkg-desc))
				(repo (jul-package-desc-repo pkg-desc))
				(build (jul-package-desc-build pkg-desc))
				(status nil))
		(cond ((string= repo "installed")
					 (setf status " "))						;who knows once it's installed!
					(t
					 (setf status "stable")))
		`(,pkg-desc
			,`[,(list (symbol-name name)) ,version ,arch ,build ,status ,repo])))

(defun jul-package-menu--refresh (&optional packages)
	"Refresh the displayed menu"
  (unless packages (setq packages t))
  (let ((info-list nil)
				(name nil))
    ;; Installed packages:
    (dolist (elt *jul-package-installed*) ;needs to be updated before this
      (setq name (car elt))
      (when packages
				(setq info-list
							(cons (jul-package--push (cdr elt) info-list) info-list))))

    ;; Uninstalled Packages:
    (dolist (elt *jul-package-repo*) ;needs to be updated before this
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
				(setq info-list
							(cons (jul-package--push (cdr elt) info-list) info-list))))

    (setq tabulated-list-entries
					(mapcar #'jul-package-menu--print-info info-list))))

(defun jul-package-menu--generate (remember-pos packages)
  "Populate the Package Menu.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display."
  (jul-package-menu--refresh packages)
  (setf (car (aref tabulated-list-format 0)) "Package")
  (tabulated-list-init-header)
  (tabulated-list-print remember-pos))

(defun jul-list-package (&optional packages)
 	"A mode used to browser, install, and remove Dragora binaries from the user
repo and system."
	(interactive)
	(let* ((buf (get-buffer-create "*jul-package-list*"))
				 (win (get-buffer-window buf)))
		(with-current-buffer buf
      (jul-package-menu-mode)
			(jul-package-refresh-contents)
      (jul-package-menu--generate nil packages))
		(if win
				(select-window win)
			(switch-to-buffer buf))))

;;; jul-mode.el ends here
