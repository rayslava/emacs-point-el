;;; point.el --- improvement reading p@point.im

;; Copyright (C) 2009  mad
;; Copyright (C) 2009  vyazovoi
;; Copypaste     2010-2011 nextus
;; Copypaste     2013-2014 rayslava

;; Author: mad <owner.mad.epa@gmail.com>
;; Modification for psto: nextus <txdevel@gmail.com>
;; Modification for point: rayslava <rayslava@gmail.com>

;; Keywords: juick, psto, point
;; Version: 0.2p

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Markup message recivied from p@point.im and some usefull keybindings.

;;; Installing:

;; 1. Put point.el to you load-path
;; 2. put this to your init file:
;; 3. Set tmp dir

;; (require 'point)
;; (setq point-tmp-dir "~/.emacs.d/avatars/point")
;; and any useful settings

;;; Code:

(require 'button)
(require 'browse-url)
(require 'json)
(require 'jabber-chatbuffer)

(defvar point-reply-id-add-plus t
  "Set to t then id inserted with + (e.g NNNN+).
Useful for people more reading instead writing")

(defvar point-overlays nil)

(defvar point-bot-jid "p@point.im")

(defvar point-image-buffer "*point-avatar-dir*")

(defvar point-point-last-message nil)

(defvar point-avatar-size 24
  "Avatar size (24, 40 or 80)")

(defvar point-avatar-internal-stack nil
  "Internal var")

(defvar point-avatar-update-day 4
  "Update (download) avatar after `point-avatar-update-day'")

(defvar point-icon-mode t
  "This mode display avatar in buffer chat")

(defvar point-tmp-dir
  (expand-file-name (concat "point-images-" (user-login-name))
                    temporary-file-directory))
(if (not (file-directory-p point-tmp-dir))
    (make-directory point-tmp-dir))

;;; Hotkeys configuration

(define-key jabber-chat-mode-map [mouse-1] 'point-go-url)
(define-key jabber-chat-mode-map "g" 'point-go-url)
(define-key jabber-chat-mode-map "п" 'point-go-url)

(define-key jabber-chat-mode-map "s" 'point-go-subscribe)
(define-key jabber-chat-mode-map "ы" 'point-go-subscribe)

(define-key jabber-chat-mode-map "u" 'point-go-unsubscribe)
(define-key jabber-chat-mode-map "г" 'point-go-unsubscribe)

(define-key jabber-chat-mode-map "\C-x\M-p" 'point-go-to-nick-back)
(define-key jabber-chat-mode-map "\C-x\M-n" 'point-go-to-nick-forward)

(define-key jabber-chat-mode-map "\M-p" 'point-go-to-post-back)
(define-key jabber-chat-mode-map "\M-n" 'point-go-to-post-forward)

(define-key jabber-chat-mode-map "\M-c" 'point-reply-to-post-comment)

;;; Faces for coloring point up
(defgroup point-faces nil "Faces for displaying Point.im msg"
  :group 'point)

(defface point-id-face
  '((t (:weight bold)))
  "face for displaying id"
  :group 'point-faces)

(defface point-user-name-face
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'point-faces)

(defface point-bold-face
  '((t (:weight bold)))
  "face for markdown **text**"
  :group 'point-faces)

(defface point-italic-face
  '((t (:slant oblique :weight extra-light)))
  "face for markdown *text*"
  :group 'point-faces)

(defface point-quote-face
  '((t (:foreground "gray")))
  "face for markdown > text"
  :group 'point-faces)

(defface point-striked-out-face
  '((t (:strike-through t :weight bold)))
  "face for ^W word"
  :group 'point-faces)

;;; Service variables start

(defvar point-last-comins-begin -1)
(defvar point-last-comins-end -1)
(defvar point-comment-search-count 1)

(defvar point-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar point-user-name-regex "[^0-9A-Za-z\\.]\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")
(defvar point-bold-regex "\\*\\*\\(.*\\)\\*\\*")
(defvar point-italic-regex "\\*\\(.*\\)\\*")
(defvar point-quote-regex "^>\\([[:ascii:]]*?\\)\\(^#\\|\\(?:\n\\{2\\}\\)\\)")
(defvar point-striked-out-regex "\\([[:graph:]]\\)+^[Ww]")

;;; Workaround to overcome point &amp bug
(defvar point-amp-regex "\\(&amp;\\|&\\)#\\([0-9]+\\);")
(defvar point-amp-subst-list
  '(("&amp;" . "&")
    ("&lt;"  . "<")
    ("&gt;"  . ">")))

(defun point-markup-chat (from buffer text proposed-alert &optional force)
  "Markup message from `point-bot-jid'.
Where FROM is jid sender, BUFFER is buffer with message TEXT
Use FORCE to markup any buffer"
  (if (or force (string-match point-bot-jid from))
      (with-current-buffer buffer
        (when (null force)
          (condition-case nil
              (jabber-truncate-top)
            (wrong-number-of-arguments
             (jabber-truncate-top buffer)))
          (setq point-point-last-message
                (re-search-backward "\\[[0-9]+:[0-9]+\\].*>" nil t)))
	(point-fix-amps)
        (point-markup-user-name)
        (point-markup-id)
	(point-markup-bold)
	(point-markup-italic)
	(point-markup-quotes)
	(point-markup-striked-out)
        (when (and point-icon-mode window-system)
          (clear-image-cache)
          (point-avatar-insert)))))

(add-hook 'jabber-alert-message-hooks 'point-markup-chat)

(defun point-avatar-file-check (name)
  (defun check-file (exts)
    (if (consp exts)
        (let ((filename (concat point-tmp-dir "/" name (car exts))))
          (if (file-exists-p filename)
              filename
            (check-file (cdr exts))))
      ""))
  (check-file '(".jpg" ".png")))

(defun point-avatar-insert ()
  (goto-char (or point-point-last-message (point-min)))
  (setq point-avatar-internal-stack nil)
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\(: @\\|>[ ]+\n@\\|#[a-z]+ @\\)\\([0-9A-Za-z@\\.\\_\\-]+\\)" nil t) ;; FIXME
      (let* ((name (match-string-no-properties 2))
             (from (match-beginning 0)))
        (point-avatar-download name from)))))

(defun point-avatar-download-cb (status name from point-buffer)
  (let* ((result-buffer (current-buffer))
         (buffer-file-coding-system 'binary)
         (file-coding-system 'binary)
         (coding-system-for-write 'binary)
         (real-url (plist-get status :redirect))
         (ext-start
          (string-match "\\(jpg\\|png\\)" real-url))
         (ext-end (match-end 0))
         (ext (substring real-url ext-start ext-end)))
    (delete-region (point-min) (re-search-forward "\n\n" nil t))
    (write-region (point-min) (point-max)
                  (concat point-tmp-dir "/" name "." ext))
    (kill-buffer (current-buffer))
    (kill-buffer result-buffer)
    (point-put-avatar name from point-buffer)))

(defun point-put-avatar (name from buffer)
  (let* ((icon-string "\n ")
         (filename (point-avatar-file-check name)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char from)
        (let ((ext (substring filename (string-match "\\(jpg\\|png\\)" filename))))
          (set-text-properties
           1 2
           `(display
             (image :type
                    ,(if (equal ext "png") 'png
                       (if (equal ext "jpg") 'jpeg
                         nil))
                    :file ,filename))
           icon-string)
          (re-search-forward "@" nil t)
          (goto-char (- (point) 1))
          (insert (concat icon-string " ")))))))

(defun point-avatar-download (name from)
  "Download avatar from point.im"
  (let ((filename (point-avatar-file-check name)))
    (if (and (not (string-equal "" filename))
             (file-exists-p filename)
             (< (time-to-number-of-days
                 (time-subtract (current-time)
                                (nth 5 (file-attributes filename))))
                point-avatar-update-day))
        (point-put-avatar name from (current-buffer))
      (let* ((size point-avatar-size)
             (avatar-url
              (format "https://point.im/avatar/%s/%d" name size))
             (url-request-method "GET"))
        (unless (assoc-string name point-avatar-internal-stack)
          (push name point-avatar-internal-stack)
          (url-retrieve avatar-url
                        'point-avatar-download-cb
                        (list name from (current-buffer))))))))

(defmacro point-markup (regex face &optional action)
  "Generates a markup function which searches for `regex' in text and applies an overlay with `face' to it
   If `action' is defined button is created over an overlay"
  `(progn
     (goto-char (or point-point-last-message (point-min)))
     (while (re-search-forward ,regex nil t)
       (when (match-string 1)
	 (point-add-overlay (match-beginning 1) (match-end 1)
			    ,face)
	 ,(when action
	      `(make-button (match-beginning 1) (match-end 1)
			    'action ,action))))))

(defun point-markup-user-name ()
  "Markup user-name matched by regex `point-user-name-regex'"
  (point-markup point-user-name-regex 'point-user-name-face 'point-insert-user-name))

(defun point-markup-id ()
  "Markup id matched by regex `point-id-regex'"
  (point-markup point-id-regex 'point-id-face 'point-insert-id))

(defun point-markup-bold ()
  "Markups text matched by regex `point-bold-regex'"
  (point-markup point-bold-regex 'point-bold-face))

(defun point-markup-italic ()
  "Markups text matched by regex `point-italic-regex'"
  (point-markup point-italic-regex 'point-italic-face))

(defun point-markup-quotes ()
  "Markups text matched by regex `point-quote-regex'"
  (point-markup point-quote-regex 'point-quote-face))

(defun point-markup-striked-out ()
  "Markups text matched by regex `point-striked-out-regex'"
  (point-markup point-striked-out-regex 'point-striked-out-face))

(defun point-fix-amps ()
  "Markup user-name matched by regex `point-amp-regex'"
  (let ((inhibit-read-only t))
    (goto-char (or point-point-last-message (point-min)))
    (while (re-search-forward point-amp-regex nil t)
      (when (match-string 2)
	(replace-match (char-to-string (string-to-number (match-string 2))))))
    (dolist (subst-pair point-amp-subst-list)
      (goto-char (or point-point-last-message (point-min)))
      (while (search-forward (car subst-pair) nil t)
	(replace-match (cdr subst-pair))))))


(defun point-insert-user-name (button)
  "Inserting reply id in conversation buffer"
  (let ((user-name (buffer-substring-no-properties
                    (overlay-start button)
                    (- (re-search-forward "[\n :]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer point-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (point-find-buffer)
    (goto-char (point-max))
    (insert (concat user-name " ")))
  (recenter 10))

(defun point-insert-id (button)
  "Inserting reply id in conversation buffer"
  (let ((id (buffer-substring-no-properties
             (overlay-start button)
             (- (re-search-forward "[\n: ]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer point-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (point-find-buffer)
    (goto-char (point-max))
    ;; usually #NNNN supposed #NNNN+
    (if (string-match "/" id)
        (insert (concat id " "))
      (insert (concat id (if point-reply-id-add-plus "+" " ")))))
  (recenter 10))

(defun point-find-buffer ()
  "Find buffer with `point-bot-jid'"
  (interactive)
  (when (not (string-match (jabber-chat-get-buffer point-bot-jid)
                           (buffer-name)))
    (delete-window)
    (let ((point-window (get-window-with-predicate
                         (lambda (w)
                           (string-match (jabber-chat-get-buffer point-bot-jid)
                                         (buffer-name (window-buffer w)))))))
      (select-window point-window))))

(defadvice jabber-chat-with (around jabber-chat-with-around-advice
                                    (jc jid &optional other-window) activate)
  "Used for markup history buffer"
  ad-do-it
  ;; FIXME: this activate ever when open buffer with point@point.net,
  ;; maybe adviced `jabber-chat-insert-backlog-entry' instead
  ;; `jabber-chat-with'.
  (when (string-match-p point-bot-jid jid)
    (save-excursion
      (goto-char (point-min))
      (setq point-point-last-message (point-min))
      (point-markup-chat point-bot-jid (current-buffer) nil nil t)
      (setq point-point-last-message (point-max)))))

(defun point-next-button ()
  "move point to next button"
  (interactive)
  (if (next-button (point))
      (goto-char (overlay-start (next-button (point))))
    (progn
      (goto-char (point-max))
      (message "button not found"))))

(defun point-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay point-overlays)))

(defun point-delete-overlays ()
  (dolist (overlay point-overlays)
    (delete-overlay overlay))
  (setq point-overlays nil))


(defun point-go-url ()
  (interactive)
  (if (and (equal (get-text-property (point) 'read-only) t)
	   (thing-at-point-looking-at point-id-regex))
	  (let* ((part-of-url (match-string-no-properties 1))
		 (part-of-url (replace-regexp-in-string "#" "" part-of-url))
		 (part-of-url (replace-regexp-in-string "/" "#" part-of-url)))
	    (message part-of-url)
	    (browse-url (concat "http://point.im/" part-of-url))))
  (if (and (equal (get-text-property (point) 'read-only) t)
	   (thing-at-point-looking-at point-user-name-regex))
	(let* ((part-of-url (match-string-no-properties 1))
	       (part-of-url (replace-regexp-in-string "@" "" part-of-url)))
	  (message part-of-url)
	  (browse-url (concat "http://" part-of-url ".point.im/"))))
    (unless (string= last-command "mouse-drag-region")
      (self-insert-command 1)))

(defun point-send-message (to text)
  "Send TEXT to TO imediately"
  (interactive)
  (save-excursion
    (let ((buffer (jabber-chat-create-buffer (jabber-read-account) to)))
      (set-buffer buffer)
      (goto-char (point-max))
      (delete-region jabber-point-insert (point-max))
      (insert text)
      (jabber-chat-buffer-send))))

(defun point-go-subscribe ()
     (interactive)
     (if (or (looking-at "#[a-z]+") (looking-at "@[0-9A-Za-z@\.\-]+"))
         (point-send-message point-bot-jid
                             (concat "S " (match-string-no-properties 0)))
       (self-insert-command 1)))

(defun point-go-unsubscribe ()
     (interactive)
     (if (or (looking-at "#[a-z]+") (looking-at "@[0-9A-Za-z@\.\-]+"))
         (point-send-message point-bot-jid
                             (concat "U " (match-string-no-properties 0)))
       (self-insert-command 1)))

(defun point-go-to-nick-back ()
  (interactive)
  (re-search-backward "@[A-Za-z0-9\\.\\_\\-]+:$" nil t)
  (backward-char))

(defun point-go-to-nick-forward ()
  (interactive)
  (re-search-forward "@[A-Za-z0-9\\.\\_\\-]+:$" nil t)
  (backward-char))

(defun point-go-to-post-back ()
  (interactive)
  (re-search-backward "^#[A-Za-z0-9\\.\\_\\-]+" nil t))

(defun point-go-to-post-forward ()
  (interactive)
  (re-search-forward "^#[A-Za-z0-9\\.\\_\\-]+" nil t))

(defun point-is-ro-at-point (where)
  (member 'read-only (text-properties-at where)))

(defun point-find-readonly-end ()
  (save-excursion
    (end-of-buffer)
    (let ((curpos (point)))
      (while (and (not (point-is-ro-at-point curpos))
                  (> curpos 0))
        (setq curpos (previous-property-change curpos)))
      curpos)))

(defun point-do-reply-to-post-comment ()
  (if (eq last-command 'point-reply-to-post-comment)
      (setq point-comment-search-count (+ 1 point-comment-search-count ))
    (setq point-comment-search-count 1))
  
  (let ((re (point-find-readonly-end)))
    (if (point-is-ro-at-point (point))
        ;; we might be on comment. jump to the next space sym
        (progn (re-search-forward "\\ ")
               (goto-char (match-beginning 0)))
      ;; start searching from editable space (to avoid counting pasted commend)
      (goto-char re))

    (if (re-search-backward
         point-id-regex nil t point-comment-search-count)
        (progn
          (when (> point-comment-search-count 1)
            (delete-region point-last-comins-begin point-last-comins-end))
          (end-of-buffer)
          (goto-char (+ 4 re)) ;; in jabber-el editable space begins 4 symbols starting from regions border (don't know why)
          (setq point-last-comins-begin (point))
          (setq point-last-comins-end (+ 1 (point) (- (match-end 0)
                                                (match-beginning 0))))
          (insert-buffer-substring-no-properties (current-buffer)
                                                 (match-beginning 0)
                                                 (match-end 0))
          (insert " "))
      (message "No comments found"))))

(defun point-reply-to-post-comment ()
  "Searches above the point for comment(post) #foo123/bar1 and
places it in the beginning of editable region. Further
invocations cause the insertion of farther comments."
  (interactive)
  (save-excursion
    (point-do-reply-to-post-comment))
  (end-of-buffer))


(provide 'point)
;;; point.el ends here
