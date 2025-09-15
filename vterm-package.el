;;; vterm-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst vterm-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst vterm-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "vterm.el" (vterm-package-where)))
    (package-buffer-info)))

(defun vterm-package-name ()
  (concat "vterm-" (package-version-join
			      (package-desc-version
			       (vterm-package-desc)))))

(defun vterm-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (vterm-package-desc))
	(pkg-dir (expand-file-name (vterm-package-name)
				   (vterm-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (copy-file (expand-file-name "vterm.el" (vterm-package-where))
	       (expand-file-name "vterm.el" pkg-dir))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
