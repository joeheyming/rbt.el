;;; rbt.el --- Integrate reviewboard with emacs.

;; Author: Joe Heyming <joeheyming@gmail.com>
;; Version: 0.1
;; Keywords: reviewboard, rbt
;; Package-Requires: ((popup "0.5.3"))
;;
;;; Commentary:
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
;; rbt-review-with-selected --> useful when you don't remember the review id,
;;   and want to be prompted which review to use.
;;
;; rbt-review-close --> useful when your cursor is currently over a review id
;;
;; rbt-review-discard --> useful when your cursor is currently over a review id


;;; Code:

(require 'json)
(require 'popup)

(defun rbt-custom-compile (cmd)
  "Run a custom compile CMD in a custom buffer named *rbt*."
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
  "Get the current root of your version control system."
  (condition-case nil
      (setq rbt-last-root (vc-call-backend 'Git 'root (file-name-directory (buffer-file-name))))
    (error rbt-last-root)))

(defun rbt-status ()
  "Run 'rbt status' and puts the output in *rbt*."
  (interactive)
  (rbt-custom-compile "rbt status")
  (other-window 1)
  )

(defun rbt-current-review-id ()
  "Gets the current review id under your cursor, if any."
  (if (string-match "\\([0-9]+\\)" (or (current-word) ""))
      (match-string 0 (current-word))))

(defvar rbt-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap [return] 'popup-select )
    (define-key keymap [tab] 'popup-select )
    keymap)
  )


(defun rbt-select-review()
  "Query with popup containing the current reviews for this user.
Return the selected review or nil"
  (interactive)
  (let*
      ((reviews-json
        (json-read-from-string (shell-command-to-string (format "rbt api-get /review-requests/ --from-user=$USER"))))
       (reviews (mapcar #'(lambda(x) (format "%s -- %s"
                                            (assoc-default 'id x)
                                            (assoc-default 'summary x)
                                            ))
                        (assoc-default 'review_requests reviews-json)))
       (selected-review))
    (setq selected-review (popup-menu* reviews :keymap rbt-popup-keymap))
    (if selected-review (car (split-string selected-review " -- ")))
    ))

(defun rbt-review (&optional review-id commit-id)
  "Run 'rbt review'.
If no review id is supplied, we create a new review with the git HEAD.
Optional argument REVIEW-ID A review id from reviewboard.
Optional argument COMMIT-ID A git commit id."
  (interactive)
  (if (not review-id)
      (setq review-id (rbt-current-review-id)))
  (if (not commit-id) (setq commit-id "--parent=HEAD~1"))

  (message (format "Review %s, Commit: %s" review-id commit-id))
  (let* (
        (review-id-arg (if (> (length review-id) 0) (format "-r %s" review-id) ""))

        ;; Read any json info about this specific review request
        (review-json
         (if review-id
             (json-read-from-string (shell-command-to-string (format "rbt api-get /review-requests/%s/" review-id)) )
           nil))

        ;; locate the target people out of the review
        (target_people (format "%s" (mapconcat 'identity (mapcar #'(lambda(x) (assoc-default 'title x)) (assoc-default 'target_people (assoc-default 'review_request review-json) )) ", ")))

        ;; construct a target people arg for rbt post
        (target-people-arg (if (> (length target_people) 0) (format "--target-people \"%s\"" target_people)  ""))

        ;; construct the rbt post command
        (post-command (format "rbt post %s %s --parent master -g -o %s" review-id-arg target-people-arg commit-id))
        )
    (message (format "Running: %s" post-command))
    (rbt-custom-compile post-command)
    )
  )

(defun rbt-review-commit ()
  "Run `rbt-review' with a commit from the current word you are looking at.
The user will be asked which review to use with the commit."
  (interactive)
  (let ((review-id (rbt-select-review))
        (commit (current-word)))

    (message (format "review-id = %s" review-id))
    (message (format "commit = %s" commit))
    
    (rbt-review review-id commit)))

(defun rbt-review-with-selected()
  "Ask the user which review to post the latest git changes to"
  (interactive)
  (rbt-review (rbt-select-review)))

(defun rbt-review-close (&optional review-id)
  "Close a review.  Try to get the REVIEW-ID from the current word."
  (interactive)
  (if (not review-id)
      (setq review-id (rbt-current-review-id)))
  (rbt-custom-compile (format "rbt close --close-type submitted %s" review-id) ))

(defun rbt-review-discard (&optional review-id)
  "Discard a review.  Try to get the REVIEW-ID from the current word."
  (interactive)
  (if (not review-id)
      (setq review-id (rbt-current-review-id)))
  (rbt-custom-compile (format "rbt close --close-type discarded %s" review-id) ))

(provide 'rbt)

;;; rbt.el ends here
