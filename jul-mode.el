;;; jul-mode.el --- Simple package system for Dragora -*- lexical-binding: t -*-

;; Copyright (C) 2016 Kevin Bloom <kdb4@openmailbox.org>

;; Author: Kevin Bloom <kdb4@openmailbox.org>
;; Created: 16 May 2016
;; Version: 0.3.9
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

;; 12 August 2016 - Querying to clean only removed selected files (bug fix)
;; 11 August 2016 - Bug fixes, querying to clean temp after install/upgrade,docs
;; 10 August 2016 - Fixed a big mark for installation bug (archs any and noarch)
;; 9 August 2016 - Added sha1sum checks
;; 8 August 2016 - Added a `jul-package-list-installed' command
;; 3 August 2016 - Made the version comparitor better, still needs work!
;; 2 August 2016 - Blocked install of other archs, added emacs-arch variable
;; 1 August 2016 - Output buffers are nice now
;; 28 July 2016 - Implemented cleaning of temp direcrory, pkg output buffer
;; 27 July 2016 - Got auto refreshing and multi-function per exceute working
;; 25 July 2016 - Added the 'tom' repo
;; 8 June 2016 - Works with newest version of jul
;; 6 June 2016 - Rewritting jul-mode to work with newer version of jul. (0.3)
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

;; Version 0.3 and up uses the jul program to list the uninstalled packages.
;; So you must have version 0.4 or higher of jul installed for this to
;; work (I think).  All versions must be ran on Dragora 2.2; D3.x is not
;; supported yet.

;;; Todo:

;; Remove temp file from the `jul search' command

;; Filtering

;; Don't allow the installation of a package that is all ready installed.
;; This technically is done already thanks to `pkg' but it only tells you in the
;; output buffer.

;; Better version comparitor in the works!

;; Remove the `sync-flag' variable

;;; BUGS

;; If you have a package that uses a hyphen in the version (like
;; ImageMagick has a version of 7.0.5-6), you will get a package name
;; that includes the version and under the `version' header, you'll
;; get the number after the hyphen. So, as an example, ImageMagick
;; looks like: ImageMagick-7.0.5 and the version is 6 However, the
;; "fix" to this is to follow the correct package naming guidelines
;; which doesn't include hyphenated versions.

;; Tom's repo doesn't really work... Especially the sdl package.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tabulated-list)

(defgroup jul-package nil
  "Manager for Dragora User packages."
  :group 'applications
  :version "0.3.9")

(defcustom jul-package-repos
	'(("frusen" . "http://gungre.ch/dragora/repo/frusen/stable/")
		("kelsoo" . "http://gungre.ch/dragora/repo/kelsoo/")
		("mprodrigues" . "http://gungre.ch/dragora/repo/mprodrigues/")
		("tom" . "http://92.19.232.58:82/dragora/repo/tom/")) ;don't use this yet
  "An alist of repos from which to fetch.
The defaults include all the repos found on the gungre.ch site.

Each element has the form (ID . LOCATION).
ID is an archive name, as a string.
LOCATION specifies the base location for the archive.
If it starts with \"http:\", it is treated as a HTTP URL;
otherwise it should be an absolute directory name.
 (Other types of URL are currently not supported.)

Only add locations that you trust, since fetching and installing
a package can kill you in your sleep."
  :type '(alist :key-type (string :tag "Repo name")
                :value-type (string :tag "URL or directory name"))
  :risky t
  :group 'jul-package
  :version "0.3.9")

(cl-defstruct (jul-package-desc
               ;; Rename the default constructor from `make-package-desc'.
               (:constructor jul-package-desc-create)
               (:constructor
                jul-package-desc-from-define
                (name-string version-string arch-string repo-string build-string
														 description-string
														 &aux
														 (name (intern name-string))
														 (version version-string)
														 (arch arch-string)
														 (repo repo-string)
														 (build build-string)
														 (description description-string))))
  "Structure containing information about an individual package.
Slots:

`name'	Name of the package, as a symbol

`version' Version of the package, as a string

`arch' Architecture of the package, as a string

`repo' The name of the archive (as a string) whence this came

`build' The current build, as a string

`description' A short description of the package, as a string"
  name
  version
	arch
  repo
	build
	description)

;; Although not common Emacs Lisp practice, I find it helpful to have earmuffs
;; on the "global" variables.
(defvar *jul-package-installed* nil
	"This list contains the packages installed on your system.
Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")
(put '*jul-package-installed* 'risky-local-variable t)

(defvar *jul-package-repo* nil
	"This list contains all the packages that are currently on the gungre.ch
server.  Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")
(put '*jul-package-repo* 'risky-local-variable t)

(defvar *jul-package-temp-dir* "~/.emacs.d/elpa/jul-mode/"
	"This variable contains the directory in which jul-mode stuff is done
temporarily.

It is recommended you pick a nice place.")
(put '*jul-package-temp-dir* 'risky-local-variable t)

(defvar *jul-package-installed-dir* "/var/db/pkg/"
	"This variable contains the directory in which Dragora keeps installed
packages")
(put '*jul-package-installed-dir* 'risky-local-variable t)

(defvar *emacs-arch* (if (string-match "x86_64" (version))
											"x86_64"
											"i486")
	"This variable is so you can easily determine which packages you can install."
	)

(defvar sync-flag t
	"Non-nil means sync with server.  When nil, don't.

This temperary variable is used to make sure you don't sync with the server
when you don't want to.  For example, when you try to refresh the list of
packages, but you are only viewing the installed ones, this will make sure that
you don't sync with the server on the refresh.")

(defvar jul-package-menu-mode-map
  (let ((map (make-sparse-keymap))
				(menu-map (make-sparse-keymap "jul-package")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "u" 'jul-package-menu-mark-unmark)
    (define-key map "d" 'jul-package-menu-mark-delete)
    (define-key map "i" 'jul-package-menu-mark-install)
    (define-key map "U" 'jul-package-menu-mark-upgrades)
    (define-key map "r" 'jul-package-menu-refresh)
    (define-key map "f" 'jul-package-menu-filter) ;not implemented yet
    (define-key map "x" 'jul-package-menu-execute)
		(define-key map "C" 'jul-package-clean-temp-dir)
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
		(define-key menu-map [mC]
			'(menu-item "Clean temp directory" jul-package-clean-temp-dir
									:help
									"Remove all .tlz & sha1sum files from the temp directory"))
    map)
  "Local keymap for `jul-package-menu-mode' buffers.")

(defun jul-package-menu-refresh ()
	"Refresh the package list."
  (interactive)
  (unless (derived-mode-p 'jul-package-menu-mode)
    (user-error "The current buffer is not a Package Menu"))
  (jul-package-refresh-contents sync-flag)
  (jul-package-menu--generate t t))

(defun jul-package-clean-temp-dir ()
	"This function just remove all files from the temperary directory.

It purges the directory of all tlz and sha1sums."
	(interactive)
	(shell-command (format "rm %s*.tlz %s*.sha1sum"
									*jul-package-temp-dir* *jul-package-temp-dir*))
	(message "Temp directory cleaned!"))

(defun jul-package-menu--find-upgrades ()
	"This function looks though all the current tabulated entries and finds the
ones that are in need of upgrading.

First it seperates them into two list, installed and available, then it removes
the stuff that isn't the same architecture as the stuff you have installed.
Then it compares versions and will place the ones with the newer version into
the upgrades list."
  (let (installed available upgrades)
    ;; Build list of installed/available packages in this buffer.
    (dolist (entry tabulated-list-entries)
      ;; ENTRY is (PKG-DESC [NAME VERSION ARCH BUILD REPO])
      (let ((pkg-desc (car entry))
						(repo (aref (cadr entry) 4)))
				(cond ((string= repo "installed")
							 (push pkg-desc installed))
							(t											 ;assuming everything else isn't installed
							 (push (cons (jul-package-desc-name pkg-desc) pkg-desc)
                     available)))))

		;; Remove packages that aren't the same arch
		(dolist (elt available)							;remove other arch
			(let ((pack-arch (jul-package-desc-arch (cdr elt))))
				(unless (or
								 (string= *emacs-arch* pack-arch)
								 (string= "noarch" pack-arch)
								 (string= "any" pack-arch))
					(setf available (remove elt available)))))

		;; Loop through list of installed packages, finding upgrades.
    (dolist (pkg-desc installed)
			(let ((avail-pkg (assq (jul-package-desc-name pkg-desc) available)))
				(when avail-pkg
					(let ((number-inst-list-ver
								 (mapcar #'string-to-number
												 (split-string
													(jul-package-desc-version pkg-desc)
													"[a-z._]")))
								(number-avail-list-ver
								 (mapcar #'string-to-number
												 (split-string
													(jul-package-desc-version (cdr avail-pkg))
													"[a-z._]"))))
						(when (version-list-<
									 number-inst-list-ver
									 number-avail-list-ver)
							(push avail-pkg upgrades))))))
    upgrades))

(defun jul-package-menu-mark-upgrades ()
	"This function will select all the packages to be upgraded.
Note that when upgrading the `pkg' program (Dragora's package
manipulation program) will automatically update the installed list.
Therefore, you do not need to select an installed package for
removal."
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

;; This probably could be written better... But so could 80% of this program
(defun sha1sum-check (full-pack-sha1)
	"This function check the sha1. Return t if okay, nil if not.
FULL-PACK-SHA1 is the full name of the package you wish to check."
	(let ((cur-dir (cadr (split-string (pwd) " "))))
		(with-temp-buffer
			(shell-command (concat "cd " *jul-package-temp-dir* " ; sha1sum -c "
														 full-pack-sha1)
										 (current-buffer))
			(goto-char (point-min))
			(when (search-forward "OK" nil t)
				t))
		(shell-command "cd " cur-dir)))

(defun jul-package-upgrade (pkg-list)
	"PKG-LIST should a list of all the packages to be upgraded of the form
tabulated-list-id.
This function will download and upgrade all packages using
the Dragora's package manipulation tool 'pkg'.

This function also checks the sha1sum of the package.  If it fails, that package
doesn't get added to the string-of-pkg string.  As long as 1 package passes the
check, the `pass' lexical variable will let you upgrade."
	(let (string-of-pkg pass)
		(dolist (pkg pkg-list)
			(let* ((pack-name (format "%s" (jul-package-desc-name pkg)))
						 (full-tlz (concat pack-name "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg)
															 ".tlz"))
						 (full-sha1 (concat full-tlz ".sha1sum"))
						 (full-tlz-path (concat *jul-package-temp-dir* full-tlz))
						 (full-sha1-path (concat *jul-package-temp-dir* full-sha1))
						 (repo))
				(dolist (elt jul-package-repos)
					(when (string= (jul-package-desc-repo pkg) (car elt))
						(setf repo (cdr elt))))
				(with-temp-file full-tlz-path
					(url-insert-file-contents (concat repo pack-name "/" full-tlz)))
				(with-temp-file full-sha1-path
					(url-insert-file-contents	(concat repo pack-name "/" full-sha1)))
				(when (sha1sum-check full-sha1-path)
					(message "sha1sum check passed!")
					(setf pass t)									;at least one package has to pass
					(setf string-of-pkg (concat string-of-pkg full-tlz-path " ")))))
		(when pass
			(let ((buf (get-buffer-create "*pkg upgrade Output*")))
				(switch-to-buffer buf)
				(shell-command (concat "echo "
															 (read-passwd "Password: ") " | sudo -S pkg upgrade "
															 string-of-pkg) buf))
			(switch-to-buffer "*jul-package-list*"))
		(jul-package-menu-refresh)))

(defun jul-package-install (pkg-list)
	"PKG-LIST should a list of all the packages to be upgraded of the
form tabulated-list-id.  This function will download and install PKG
using the Dragora's package manipulation tool 'pkg'.

This function also checks the sha1sum of the package.  If it fails, that package
doesn't get added to the string-of-pkg string.  As long as 1 package passes the
check, the `pass' lexical variable will let you install."
	(let (string-of-pkg pass)
		(dolist (pkg pkg-list)
			(let* ((pack-name (format "%s" (jul-package-desc-name pkg)))
						 (full-tlz (concat pack-name "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg)
															 ".tlz"))
						 (full-sha1 (concat full-tlz ".sha1sum"))
						 (full-tlz-path (concat *jul-package-temp-dir* full-tlz))
						 (full-sha1-path (concat *jul-package-temp-dir* full-sha1))
						 (repo))
				(dolist (elt jul-package-repos)
					(when (string= (jul-package-desc-repo pkg) (car elt))
						(setf repo (cdr elt))))
				(with-temp-file full-tlz-path
					(url-insert-file-contents	(concat repo pack-name "/" full-tlz)))
				(with-temp-file full-sha1-path
					(url-insert-file-contents	(concat repo pack-name "/" full-sha1)))
				(when (sha1sum-check full-sha1-path)
					(message "sha1sum check passed!")
					(setf pass t)									;at least one package has to pass
					(setf string-of-pkg (concat string-of-pkg full-tlz-path " ")))))
		(when pass
			(let ((buf (get-buffer-create "*pkg add Output*")))
				(switch-to-buffer buf)
				(shell-command (concat "echo "
															 (read-passwd "Password: ") " | sudo -S pkg add "
															 string-of-pkg) buf))
			(switch-to-buffer "*jul-package-list*"))
		(jul-package-menu-refresh)))

(defun jul-package-delete (pkg-list)
	"PKG-LIST should a list of all the packages to be removed of the
form tabulated-list-id.  This function will uninstall and remove
installed packages using the Dragora package manipulation tool 'pkg'."
	(let (string-of-pkg)
		(dolist (pkg pkg-list)
			(let* ((full-tlz (concat (format "%s" (jul-package-desc-name pkg)) "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg) ".tlz"))
						 (full-tlz-path (concat *jul-package-installed-dir* full-tlz)))
				(setf string-of-pkg (concat string-of-pkg full-tlz-path " "))))
		(let ((buf (get-buffer-create "*pkg remove Output*")))
			(switch-to-buffer buf)
			(shell-command (concat "echo "
														 (read-passwd "Password: ") " | sudo -S pkg remove "
														 string-of-pkg) buf))
		(if (member "*jul-package-list*" (mapcar #'buffer-name (buffer-list)))
				(switch-to-buffer "*jul-package-list*")
			(switch-to-buffer "*jul-package-installed-list*"))
		(jul-package-menu-refresh)))

(defun jul-package-remove-tlz (pkg-list)
	"PKG-LIST should be a list of all the packages to be removed from the temp
directory of the from tabulated-list-id."
	(let (string-of-pkg)
		(dolist (pkg pkg-list)
			(let* ((full-tlz (concat (format "%s" (jul-package-desc-name pkg)) "-"
															 (jul-package-desc-version pkg) "-"
															 (jul-package-desc-arch pkg) "-"
															 (jul-package-desc-build pkg) ".tlz*"))
						 (full-tlz-path (concat *jul-package-temp-dir* full-tlz)))
				(setf string-of-pkg (concat string-of-pkg full-tlz-path " "))))
		(shell-command (concat "rm " string-of-pkg)))
	(message "Files removed!"))

(defun jul-package-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
Packages marked for upgrade are downloaded and upgraded;
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
					(jul-package-upgrade upgrade-list)))
		;; Remove the tlz files that you probably don't need
		(when (or install-list upgrade-list)	;anything with a new .tlz
			(let ((tlz-list (append install-list upgrade-list)))
				(if (or
						 noquery
						 (yes-or-no-p
							(if (= (length tlz-list) 1)
									(format "Clean package `%s' from temp directory? "
													(jul-package-desc-full-name (car tlz-list)))
								(format "Clean these %d packages (%s) from temp directory? "
												(length tlz-list)
												(mapconcat #'jul-package-desc-full-name
																	 tlz-list ", ")))))
						(jul-package-remove-tlz tlz-list))))
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
	"Clear any mark on a package and move to the next line."
	(interactive "p")
	(tabulated-list-put-tag " " t))

(defun jul-package-menu-mark-install (&optional _num)
  "Mark a package for installation and move to the next line.

This function will only let you select packages that are the
same arch as your system (and anything that isn't arch
specific)."
  (interactive "p")
	(let ((pack-arch (jul-package-menu-get-arch)))
		(if (or (string= (jul-package-menu-get-repo) "installed")
						(not (or (string= *emacs-arch* pack-arch)
										 (string= "noarch" pack-arch)
										 (string= "any" pack-arch))))
				(forward-line)
			(tabulated-list-put-tag "I" t))))

(defun jul-package-menu-get-repo ()
	"Grab the current package's repo."
  (let* ((id (tabulated-list-get-id))
				 (entry (and id (assq id tabulated-list-entries))))
    (if entry
				(aref (cadr entry) 4)
      "")))

(defun jul-package-menu-get-arch ()
		"Grab the current package's arch."
  (let* ((id (tabulated-list-get-id))
				 (entry (and id (assq id tabulated-list-entries))))
    (if entry
				(aref (cadr entry) 2)
      "")))

(define-derived-mode jul-package-menu-mode tabulated-list-mode "Jul Package Menu"
	"Major mode for browsing a list of packages"
	(setq tabulated-list-format
        `[("Package" 25 nil) ;there will eventually be something (not just nil)
          ("Version" 20 nil)
					("Arch" 10 nil)
					("Build" 10 nil)
					("Repo" 13 nil)
					("Description" 0 nil)])
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

(defun jul-space-fixer (split-pack)
	"SPLIT-PACK should be the full name of a package after being split by the
split-string function with ' '.

This function will return a list that contains 3 elements: the repo, the full
package name, and the description (if there is one) of the form
'(DESCRIPTION REPO PACKAGE)."
	(let (pkg-list description)
		(dolist (elt split-pack)
			(unless (string= elt "")
				(setf pkg-list (cons elt pkg-list))))
		(setf pkg-list (reverse pkg-list)) 	;so that description is in right order
		(while (> (length pkg-list) 2)
			(let ((word (car (cddr pkg-list))))
				(setf description (concat description word " "))
				(setf pkg-list (remove word pkg-list))))
		(if description
				(push description pkg-list)
			(push " " pkg-list))))

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

(defun jul-parse-n-place (file)
	"Grab each package that is found in FILE and place into a list.

This is rather inefficient and will be changed soon."
	(with-temp-buffer
		(insert-file-contents file)
		(let ((num-of-packs (count-lines (point-min) (point-max)))
					(current-pack 0)
					(pack-list nil))
			(while (< current-pack num-of-packs)
				(let ((first-point (point)))
					(end-of-line)
					(copy-region-as-kill first-point (point))
					(let* ((raw-output (jul-space-fixer
															(split-string (car kill-ring) " ")))
								 (split-pack (jul-hyphenated-name-fixer
															(split-string (car (cddr raw-output)) "-")))
								 (struct-pack (jul-package-desc-from-define
															 (car split-pack)
															 (cadr split-pack)
															 (car (cddr split-pack))
															 (cadr raw-output)
															 (car (split-string
																		 (cadr (cddr split-pack)) ".tlz"))
															 (car raw-output))))
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

(defun jul-package--download-one-repo (file repo)
  "Retrieve an repo FILE from REPO, and cache it.
REPO should be a cons cell of the form (NAME . LOCATION),
similar to an entry in `*jul-package-repo*'."
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

(defun jul-package-refresh-contents (&optional sync)
	"This function will grab the current version of the
database files on the user repo and add all the packages to a
big list.  SYNC is used to sync with jul or not.  If SYNC is
nil, then do NOT sync with the server.  If non-nil, sync with
server.  It also probes the installed directory on your
system and makes a list of all the installed programs."
	(setf *jul-package-repo* nil)					;clear current uninstalled items list
	(setf *jul-package-installed* nil) ;clear current installed items list

	(when sync
		(unless (file-directory-p *jul-package-temp-dir*)
			(make-directory *jul-package-temp-dir*))
		(let ((temp-file (concat *jul-package-temp-dir* "temp")))
			(shell-command "jul sync")
			(shell-command (concat "jul search > " temp-file))
			(message "Syncing jul...")
			(setf *jul-package-repo* (jul-parse-n-place temp-file)))
		(message "Sync complete"))

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
															 (cadr (cddr split-pack))
															 " "))
						 (cur-pack-list (cons (jul-package-desc-name cur-pack-struct)
																	cur-pack-struct)))
				(setf *jul-package-installed* (cons cur-pack-list
																						*jul-package-installed*)))))
	(message "Complete"))

(defun jul-package-menu--print-info (pkg-desc)
	"Convert the complicated jul-package-desc-struct, to something the tabulated
mode can read.  PKG-DESC is of form jul-package-desc-struct"
  (let ((name (jul-package-desc-name pkg-desc))
				(version (jul-package-desc-version pkg-desc))
				(arch (jul-package-desc-arch pkg-desc))
				(repo (jul-package-desc-repo pkg-desc))
				(build (jul-package-desc-build pkg-desc))
				(description (jul-package-desc-description pkg-desc)))
		`(,pkg-desc
			,`[,(list (symbol-name name)) ,version ,arch ,build ,repo ,description])))

(defun jul-package-menu--refresh (&optional packages)
	"Refresh the displayed menu."
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
		(when *jul-package-repo*
			(dolist (elt *jul-package-repo*) ;needs to be updated before this
				(setq name (car elt))
				(when (or (eq packages t) (memq name packages))
					(setq info-list
								(cons (jul-package--push (cdr elt) info-list) info-list)))))

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
	(setf sync-flag t)
	(let* ((buf (get-buffer-create "*jul-package-list*"))
				 (win (get-buffer-window buf)))
		(with-current-buffer buf
      (jul-package-menu-mode)
			(jul-package-refresh-contents sync-flag)
      (jul-package-menu--generate nil packages))
		(if win
				(select-window win)
			(switch-to-buffer buf))))

(defun jul-list-installed (&optional packages)
	"A mode used to browser and remove Dragora binaries from the system."
	(interactive)
	(setf sync-flag nil)
	(let* ((buf (get-buffer-create "*jul-package-installed-list*"))
				 (win (get-buffer-window buf)))
		(with-current-buffer buf
      (jul-package-menu-mode)
			(jul-package-refresh-contents sync-flag)
      (jul-package-menu--generate nil packages))
		(if win
				(select-window win)
			(switch-to-buffer buf))))

;;; jul-mode.el ends here
