;;; rbt.el --- Integrate reviewboard with emacs.

;; Author: Joe Heyming <joeheyming@gmail.com>
;; Version: 0.1
;; Keywords: reviewboard, rbt
;;
;; Useful commands:
;; rbt-status --> shows your current open reviewboard items
;;
;; rbt-review --> when no arguments are supplied, we try to review the current last diff on git HEAD.
;;   If you are currently hovering over a number, we try to use that as a review id
;;    This is useful if you are looking at rbt-status, then run rbt-review when looking at r/123
;;
;; rbt-review-commit --> useful when your cursor is currently over a git commit hash id
;;
;; rbt-review-close --> useful when your cursor is currently over a review id
;;
;; rbt-review-discard --> useful when your cursor is currently over a review id

(provide 'rbt)

(defun rbt-custom-compile (cmd)
  "Run a custom compile command in a custom buffer named *rbt*"
  (let ((mybuf (current-buffer)))
    (if (get-buffer "*rbt*")
        (kill-buffer "*rbt"))
    (let ((default-dir (vc-get-root)))
      (compile cmd)
      (switch-to-buffer "*compilation*")
      (rename-buffer "rbt")
      (switch-to-buffer mybuf)
      )
    ))

;; Save the last root you did an RBT command with.
(defvar rbt-last-root)

(defun vc-get-root ()
  (interactive)
  (condition-case nil
      (setq rbt-last-root (vc-call-backend 'Git 'root (file-name-directory (buffer-file-name))))
    (error rbt-last-root)))

(defun rbt-status ()
  "runs rbt status and puts the output in *rbt*"
  (interactive)
  (rbt-custom-compile "rbt status")
  (other-window 1)
  )

(defun rbt-current-review-id ()
  "Gets the current review id under your cursor, if any"
  (if (string-match "\\([0-9]+\\)" (current-word))
      (match-string 0 (current-word))))

(defun rbt-review (&optional review-id commit-id)
  "Runs rbt review. If no review id is supplied, we create a new review with the git HEAD.
  You can override the review id and the commit id calling this function externally"
  (interactive)
  (if (not review-id) 
      (setq review-id (rbt-current-review-id)))
  (if (not commit-id) (setq commit-id "--parent=HEAD~1"))

  (message (format "Review %s, Commit: %s" review-id commit-id))
  (setq review-id (if review-id (format "-r %s" review-id) ""))
  (rbt-custom-compile (format "rbt post %s --parent master -g -o %s" review-id commit-id))
  )

(defun rbt-review-commit (review-id)
  "Tries to extract a commit from the current word you are looking at, then runs rbt-review
  The user must supply a review id to go with the commit id."
  (interactive (list (read-string "Review id: ")))
  (rbt-review review-id (current-word)))

(defun rbt-review-close (&optional review-id)
  "Close a review. Tries to get the review id from the current word."
  (interactive)
  (if (not review-id) 
      (setq review-id (rbt-current-review-id)))
  (rbt-custom-compile (format "rbt close --close-type submitted %s" review-id) ))

(defun rbt-review-discard (&optional review-id)
  "Discard a review. Tries to get the review id from the current word."
  (interactive)
  (if (not review-id) 
      (setq review-id (rbt-current-review-id)))
  (rbt-custom-compile (format "rbt close --close-type discarded %s" review-id) ))
