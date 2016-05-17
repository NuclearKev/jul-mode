;;; jul-mode.el --- Simple package system for Dragora -*- lexical-binding: t -*-

;; Copyright (C) 2016 Kevin Bloom <kdb4@openmailbox.org>

;; Author: Kevin Bloom <kdb4@openmailbox.org>
;; Created: 16 May 2016
;; Version: 0.1
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

;; 16 May 2016 - Created package. On this day, I got something working.

;;; Commentary:

;; The idea behind jul-mode is to bring jul, the "package manager" for the
;; Dragora user repo, to Emacs.  This will hopefully make jul and the repos
;; easier to use and more powerful.  That being said, you will (hopefully) be
;; able to do most of your package management in jul-mode as well.

;;; Todo:

;; * Get communication with server
;; * Get parsing of server working
;; * Get downloading from server working
;; * Get installation and upgrading working

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tabulated-list)

(defgroup jul-package nil
  "Manager for Dragora User packages."
  :group 'applications
  :version "0.1")

(defcustom jul-package-repos
	'(("frusen-stable" . "http://gungre.ch/dragora/repo/frusen/stable/")
		("frusen-oldstable" . "http://gungre.ch/dragora/repo/frusen/old-stable/")
		("frusen-unstable" . "http:///gungre.ch/dragora/repo/frusen/unstable/")
		("kelsoo" . "http:///gungre.ch/dragora/repo/kelsoo")
		("mp" . "http:///gungre.ch/dragora/repo/frusen/mprodrigues/"))
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
  :version "0.1")

(cl-defstruct (jul-package-desc
               ;; Rename the default constructor from `make-package-desc'.
               (:constructor jul-package-desc-create)
               ;; Has the same interface as the old `define-package',
               ;; which is still used in the "foo-pkg.el" files. Extra
               ;; options can be supported by adding additional keys.
               (:constructor
                jul-package-desc-from-define
                (name-string version-string repo-string
														 &aux
														 (name (intern name-string))
														 (version version-string)
														 (repo repo-string))))
  "Structure containing information about an individual package.
Slots:

`name'	Name of the package, as a symbol.

`version' Version of the package, as a string

`repo' The name of the archive (as a string) whence this
	package came."
  name
  version
  repo)

(defvar jul-package-installed nil
	"This list contains all the packages that are currently installed on your
system.  Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")

(defvar jul-package-repo nil
	"This list contains all the packages that are currently on the gungre.ch
server.  Each element is of the form (NAME . J-PKG-DESC) where NAME is the name
of the package and J-PKG-DESC is a cl-struct-jul-package-desc.")


(define-derived-mode jul-menu-mode tabulated-list-mode "Jul Package Menu"
	"Major mode for browsing a list of packages"
	(setq tabulated-list-format
        `[("Package" 18 nil) ;there will eventually be something (not just nil)
          ("Version" 13 nil)
          ("Status"  10 nil)
					("Repo" 10 nil)])
	(setq tabulated-list-padding 2)
	;; There will be some sort of refreshing thing
	(setq tabulated-list-sort-key (cons "Repo" nil))
	(add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil)
  (tabulated-list-init-header))

(defun jul-package--push (pkg-desc listname)
	"A simple funtion used to check if the current package is alright in the list
to be printed to the screen."
	(unless (member pkg-desc listname)
		pkg-desc))

(defun jul-package-menu--print-info (pkg-desc)
	"Convert the complicated jul-package-desc-struct, to something the tabulated
mode can read.  PKG-DESC is of form jul-package-desc-struct"
  (let ((name (jul-package-desc-name pkg-desc))
				(version (jul-package-desc-version pkg-desc))
				(repo (jul-package-desc-repo pkg-desc))
				(status nil))
		(cond ((equal repo "frusen-oldstable")
					 (setf status "old-stable"))
					((equal repo "frusen-unstable")
					 (setf status "unstable"))
					(t
					 (setf status "stable")))
		`(,pkg-desc ,`[,(list (symbol-name name)) ,version ,status ,repo])))

(defun jul-package-menu--refresh (&optional packages)
	"Refresh the displayed menu"
  (unless packages (setq packages t))
  (let ((info-list nil)
				(name nil))
    ;; Installed packages:
    (dolist (elt jul-package-installed) ;needs to be updated before this
      (setq name (car elt))
      (when packages
				(setq info-list
							(cons (jul-package--push (cdr elt) info-list) info-list))))

    ;; Uninstalled Packages:
    (dolist (elt jul-package-repo) ;needs to be updated before this
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

(defun jul-mode (&optional packages)
 	"A mode used to browser, install, and remove Dragora binaries from the user
repo."
	(interactive)
	(let* ((buf (get-buffer-create "*jul-package-list*"))
				 (win (get-buffer-window buf)))
		(with-current-buffer buf
      (jul-menu-mode)  ;this is in the archives section
      (jul-package-menu--generate nil packages))
		(if win
				(select-window win)
			(switch-to-buffer buf))))

;;; jul-mode.el ends here
