;;; point.el --- improvement reading p@point.im

;; Copyright (C) 2009  mad
;; Copyright (C) 2009  vyazovoi
;; Copypaste     2010-2011  nextus
;; Copypaste     2013  rayslava

;; Author: mad <owner.mad.epa@gmail.com>
;; Modification for psto: nextus <txdevel@gmail.com>
;; Modification for point: rayslava <rayslava@gmail.com>

;; Keywords: juick, psto, point
;; Version: 0.1p

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

(require 'jabber-chatbuffer)

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

(defvar point-reply-id-add-plus t
  "Set to t then id inserted with + (e.g NNNN+).
Useful for people more reading instead writing")

(defvar point-overlays nil)

(defvar point-bot-jid "p@point.im")

(defvar point-image-buffer "*point-avatar-dir*")

(defvar point-point-last-message nil)

(defvar point-avatar-internal-stack nil
  "Internal var")

(defvar point-avatar-update-day 4
  "Update (download) avatar after `point-avatar-update-day'")

(defvar point-icon-mode t
  "This mode display avatar in buffer chat")

;; NEED?
(defvar point-icon-hight nil
  "If t then show 96x96 avatars")

(defvar point-tmp-dir
  (expand-file-name (concat "point-images-" (user-login-name))
                    temporary-file-directory))
(if (not (file-directory-p point-tmp-dir))
    (make-directory point-tmp-dir))

(defvar point-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar point-user-name-regex "[^0-9A-Za-z\\.]\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")

(defun point-markup-chat (from buffer text proposed-alert &optional force)
  "Markup message from `point-bot-jid'.
Where FROM is jid sender, BUFFER is buffer with message TEXT
Use FORCE to markup any buffer"
  (if (or force (string-match point-bot-jid from))
      (save-excursion
        (set-buffer buffer)
        (when (null force)
          (condition-case nil
              (jabber-truncate-top)
            (wrong-number-of-arguments
             (jabber-truncate-top buffer)))
          (setq point-point-last-message
                (re-search-backward "\\[[0-9]+:[0-9]+\\].*>" nil t)))
        (point-markup-user-name)
        (point-markup-id)
        (when (and point-icon-mode window-system)
          (clear-image-cache)
          (point-avatar-insert)))))

(add-hook 'jabber-alert-message-hooks 'point-markup-chat)

(defun point-avatar-insert ()
  (goto-char (or point-point-last-message (point-min)))
  (setq point-avatar-internal-stack nil)
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\(: @\\|>[ ]+\n@\\|#[a-z]+ @\\)\\([0-9A-Za-z@\\.\\_\\-]+\\)" nil t) ;; FIXME
      (let* ((icon-string "\n ")
             (name (match-string-no-properties 2))
             (fake-png (concat point-tmp-dir "/" name ".png")))
        (goto-char (match-beginning 0))
        (point-avatar-download name)
        (set-text-properties
         1 2 `(display
               (image :type png
                      :file ,fake-png))
         icon-string)
        (re-search-forward "@" nil t)
        (goto-char (- (point) 1))
        (insert (concat icon-string " "))
        (re-search-forward "" nil t))))) ;; FIXME

(defun point-avatar-download (name)
  "Download avatar from point.net"
  (if (or (assoc-string name point-avatar-internal-stack)
          (and (file-exists-p (concat point-tmp-dir "/" name ".png"))
               (< (time-to-number-of-days
                   (time-subtract (current-time)
                                  (nth 5 (file-attributes (concat point-tmp-dir "/" name ".png")))))
                  point-avatar-update-day)))
      nil
    (let ((avatar-url (concat "http://" name ".point.im/"))
          (url-request-method "GET"))
      (push name point-avatar-internal-stack)
      (url-retrieve avatar-url
                    '(lambda (status name)
                       (let ((result-buffer (current-buffer)))
                         (goto-char (point-min))
                           (if (re-search-forward "/a/40/[A-Za-z0-9\\.\\_\\-]*\.png" nil t)
                                (point-avatar-download-and-save (match-string 0) name)
                              (point-avatar-download-and-save "/a/40/" name ".png"))
                           (kill-buffer result-buffer)))
                    (list name)))))

(defun point-avatar-download-and-save (link name)
  "Extract image from LINK and save it with NAME in `point-tmp-dir'"
  (let* ((filename (substring link (string-match "\\(0.png\\|0/[A-Za-z0-9\\.\\_\\-]+\.png\\)" link)))
         (avatar-url (concat "https://i.point.im/" filename))
         (url-request-method "GET"))
    (url-retrieve avatar-url
                  '(lambda (status name)
                     (let ((result-buffer (current-buffer))
                           (buffer-file-coding-system 'binary)
                           (file-coding-system 'binary)
                           (coding-system-for-write 'binary))
                       (delete-region (point-min) (re-search-forward "\n\n" nil t))
                       (write-region (point-min) (point-max) (concat point-tmp-dir "/" name ".png"))
                       (kill-buffer (current-buffer))
                       (kill-buffer result-buffer)))
                  (list name))))

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

(defun point-markup-user-name ()
  "Markup user-name matched by regex `point-regex-user-name'"
  (goto-char (or point-point-last-message (point-min)))
  (while (re-search-forward point-user-name-regex nil t)
    (when (match-string 1)
      (point-add-overlay (match-beginning 1) (match-end 1)
                         'point-user-name-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'point-insert-user-name))))

(defun point-markup-id ()
  "Markup id matched by regex `point-regex-id'"
  (goto-char (or point-point-last-message (point-min)))
  (while (re-search-forward point-id-regex nil t)
    (when (match-string 1)
      (point-add-overlay (match-beginning 1) (match-end 1)
                         'point-id-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'point-insert-id))))

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

(provide 'point)
;;; point.el ends here
