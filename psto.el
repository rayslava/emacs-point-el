;;; psto.el --- improvement reading psto@psto.net

;; Copyright (C) 2009  mad
;; Copyright (C) 2009  vyazovoi
;; Copypaste     2010-2011  nextus

;; Author: mad <owner.mad.epa@gmail.com>
;; Modification for psto: nextus <txdevel@gmail.com>

;; Keywords: juick, psto
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

;; Markup message recivied from psto@psto.net and some usefull keybindings.

;;; Installing:

;; 1. Put psto.el to you load-path
;; 2. put this to your init file:
;; 3. Set tmp dir

;; (require 'psto)
;; (setq psto-tmp-dir "~/.emacs.d/avatars/psto")
;; and any useful settings

;;; Code:

(require 'button)
(require 'browse-url)

(require 'jabber-chatbuffer)

(defgroup psto-faces nil "Faces for displaying Psto msg"
  :group 'psto)

(defface psto-id-face
  '((t (:weight bold)))
  "face for displaying id"
  :group 'psto-faces)

(defface psto-user-name-face
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'psto-faces)

(defvar psto-reply-id-add-plus t
  "Set to t then id inserted with + (e.g NNNN+).
Useful for people more reading instead writing")

(defvar psto-overlays nil)

(defvar psto-bot-jid "psto@psto.net")

(defvar psto-image-buffer "*psto-avatar-dir*")

(defvar psto-point-last-message nil)

(defvar psto-avatar-internal-stack nil
  "Internal var")

(defvar psto-avatar-update-day 4
  "Update (download) avatar after `psto-avatar-update-day'")

(defvar psto-icon-mode t
  "This mode display avatar in buffer chat")

;; NEED?
(defvar psto-icon-hight nil
  "If t then show 96x96 avatars")

(defvar psto-tmp-dir
  (expand-file-name (concat "psto-images-" (user-login-name))
                    temporary-file-directory))
(if (not (file-directory-p psto-tmp-dir))
    (make-directory psto-tmp-dir))

(defvar psto-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar psto-user-name-regex "[^0-9A-Za-z\\.]\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")

(defun psto-markup-chat (from buffer text proposed-alert &optional force)
  "Markup  message from `psto-bot-jid'.
Where FROM is jid sender, BUFFER is buffer with message TEXT
Use FORCE to markup any buffer"
  (if (or force (string-match psto-bot-jid from))
      (save-excursion
        (set-buffer buffer)
        (when (null force)
          (if (version< jabber-version "0.8.0")
              (jabber-truncate-top)
            (jabber-truncate-top buffer))
          (setq psto-point-last-message
                (re-search-backward "\\[[A-Za-z]+:[0-9]+\\].*>" nil t))) ;; FIXME
        (psto-markup-user-name)
        (psto-markup-id)
        (when (and psto-icon-mode window-system)
          (clear-image-cache)
          (psto-avatar-insert)))))

(add-hook 'jabber-alert-message-hooks 'psto-markup-chat)

(defun psto-avatar-insert ()
  (goto-char (or psto-point-last-message (point-min)))
  (setq psto-avatar-internal-stack nil)
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\(: @\\|>[ ]+\n@\\|#[a-z]+ @\\)\\([0-9A-Za-z@\\.\\_\\-]+\\)" nil t) ;; FIXME
      (let* ((icon-string "\n ")
             (name (match-string-no-properties 2))
             (fake-png (concat psto-tmp-dir "/" name ".png")))
        (goto-char (match-beginning 0))
        (psto-avatar-download name)
        (set-text-properties
         1 2 `(display
               (image :type png
                      :file ,fake-png))
         icon-string)
        (re-search-forward "@" nil t)
        (goto-char (- (point) 1))
        (insert (concat icon-string " "))
        (re-search-forward "" nil t))))) ;; FIXME

(defun psto-avatar-download (name)
  "Download avatar from psto.net"
  (if (or (assoc-string name psto-avatar-internal-stack)
          (and (file-exists-p (concat psto-tmp-dir "/" name ".png"))
               (< (time-to-number-of-days
                   (time-subtract (current-time)
                                  (nth 5 (file-attributes (concat psto-tmp-dir "/" name ".png")))))
                  psto-avatar-update-day)))
      nil
    (let ((avatar-url (concat "http://" name ".psto.net/"))
          (url-request-method "GET"))
      (push name psto-avatar-internal-stack)
      (url-retrieve avatar-url
                    '(lambda (status name)
                       (let ((result-buffer (current-buffer)))
                         (goto-char (point-min))
                           (if (re-search-forward "/img/a/80/[A-Za-z0-9\\.\\_\\-]*\.png" nil t)
                                (psto-avatar-download-and-save (match-string 0) name)
                              (psto-avatar-download-and-save "/img/a/40.png" name))
                           (kill-buffer result-buffer)))
                    (list name)))))

(defun psto-avatar-download-and-save (link name)
  "Extract image from LINK and save it with NAME in `psto-tmp-dir'"
  (let* ((filename (substring link (string-match "\\(0.png\\|0/[A-Za-z0-9\\.\\_\\-]+\.png\\)" link)))
         (avatar-url (concat "http://psto.net/img/a/4" filename))
         (url-request-method "GET"))
    (url-retrieve avatar-url
                  '(lambda (status name)
                     (let ((result-buffer (current-buffer))
                           (buffer-file-coding-system 'binary)
                           (file-coding-system 'binary)
                           (coding-system-for-write 'binary))
                       (delete-region (point-min) (re-search-forward "\n\n" nil t))
                       (write-region (point-min) (point-max) (concat psto-tmp-dir "/" name ".png"))
                       (kill-buffer (current-buffer))
                       (kill-buffer result-buffer)))
                  (list name))))

(define-key jabber-chat-mode-map [mouse-1] 'psto-go-url)
(define-key jabber-chat-mode-map "g" 'psto-go-url)
(define-key jabber-chat-mode-map "п" 'psto-go-url)

(define-key jabber-chat-mode-map "s" 'psto-go-subscribe)
(define-key jabber-chat-mode-map "ы" 'psto-go-subscribe)

(define-key jabber-chat-mode-map "u" 'psto-go-unsubscribe)
(define-key jabber-chat-mode-map "г" 'psto-go-unsubscribe)

(define-key jabber-chat-mode-map "\M-p" 'psto-go-to-post-back) ;; FIXME
(define-key jabber-chat-mode-map "\M-n" 'psto-go-to-post-forward) ;; FIXME

(defun psto-go-to-post-back ()
  (interactive)
  (re-search-backward "@[a-z0-9]+:$" nil t))

(defun psto-go-to-post-forward ()
  (interactive)
  (re-search-forward "@[a-z0-9]+:$" nil t))

(defun psto-markup-user-name ()
  "Markup user-name matched by regex `psto-regex-user-name'"
  (goto-char (or psto-point-last-message (point-min)))
  (while (re-search-forward psto-user-name-regex nil t)
    (when (match-string 1)
      (psto-add-overlay (match-beginning 1) (match-end 1)
                         'psto-user-name-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'psto-insert-user-name))))

(defun psto-markup-id ()
  "Markup id matched by regex `psto-regex-id'"
  (goto-char (or psto-point-last-message (point-min)))
  (while (re-search-forward psto-id-regex nil t)
    (when (match-string 1)
      (psto-add-overlay (match-beginning 1) (match-end 1)
                         'psto-id-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'psto-insert-id))))

(defun psto-insert-user-name (button)
  "Inserting reply id in conversation buffer"
  (let ((user-name (buffer-substring-no-properties
                    (overlay-start button)
                    (- (re-search-forward "[\n :]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer psto-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (psto-find-buffer)
    (goto-char (point-max))
    (insert (concat user-name " ")))
  (recenter 10))

(defun psto-insert-id (button)
  "Inserting reply id in conversation buffer"
  (let ((id (buffer-substring-no-properties
             (overlay-start button)
             (- (re-search-forward "[\n: ]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer psto-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (psto-find-buffer)
    (goto-char (point-max))
    ;; usually #NNNN supposed #NNNN+
    (if (string-match "/" id)
        (insert (concat id " "))
      (insert (concat id (if psto-reply-id-add-plus "+" " ")))))
  (recenter 10))

(defun psto-find-buffer ()
  "Find buffer with `psto-bot-jid'"
  (interactive)
  (when (not (string-match (jabber-chat-get-buffer psto-bot-jid)
                           (buffer-name)))
    (delete-window)
    (let ((psto-window (get-window-with-predicate
                         (lambda (w)
                           (string-match (jabber-chat-get-buffer psto-bot-jid)
                                         (buffer-name (window-buffer w)))))))
      (select-window psto-window))))

(defadvice jabber-chat-with (around jabber-chat-with-around-advice
                                    (jc jid &optional other-window) activate)
  "Used for markup history buffer"
  ad-do-it
  ;; FIXME: this activate ever when open buffer with psto@psto.net,
  ;; maybe adviced `jabber-chat-insert-backlog-entry' instead
  ;; `jabber-chat-with'.
  (when (string-match-p psto-bot-jid jid)
    (save-excursion
      (goto-char (point-min))
      (setq psto-point-last-message (point-min))
      (psto-markup-chat psto-bot-jid (current-buffer) nil nil t)
      (setq psto-point-last-message (point-max)))))

(defun psto-next-button ()
  "move point to next button"
  (interactive)
  (if (next-button (point))
      (goto-char (overlay-start (next-button (point))))
    (progn
      (goto-char (point-max))
      (message "button not found"))))

(defun psto-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay psto-overlays)))

(defun psto-delete-overlays ()
  (dolist (overlay psto-overlays)
    (delete-overlay overlay))
  (setq psto-overlays nil))


(defun psto-go-url ()
  (interactive)
  (if (and (equal (get-text-property (point) 'read-only) t)
	   (thing-at-point-looking-at psto-id-regex))
	  (let* ((part-of-url (match-string-no-properties 1))
		 (part-of-url (replace-regexp-in-string "#" "" part-of-url))
		 (part-of-url (replace-regexp-in-string "/" "#" part-of-url)))
	    (message part-of-url)
	    (browse-url (concat "http://psto.net/" part-of-url))))
  (if (and (equal (get-text-property (point) 'read-only) t)
	   (thing-at-point-looking-at psto-user-name-regex))
	(let* ((part-of-url (match-string-no-properties 1))
	       (part-of-url (replace-regexp-in-string "@" "" part-of-url)))
	  (message part-of-url)
	  (browse-url (concat "http://" part-of-url ".psto.net/")))))

(defun psto-send-message (to text)
  "Send TEXT to TO imediately"
  (interactive)
  (save-excursion
    (let ((buffer (jabber-chat-create-buffer (jabber-read-account) to)))
      (set-buffer buffer)
      (goto-char (point-max))
      (delete-region jabber-point-insert (point-max))
      (insert text)
      (jabber-chat-buffer-send))))

(defun psto-go-subscribe ()
     (interactive)
     (if (or (looking-at "#[a-z]+") (looking-at "@[0-9A-Za-z\\.\\-\\_@\.\-]+"))
         (psto-send-message psto-bot-jid
                             (concat "S " (match-string-no-properties 0)))
       (self-insert-command 1)))

(defun psto-go-unsubscribe ()
     (interactive)
     (if (or (looking-at "#[a-z]+") (looking-at "@[0-9A-Za-z\\.\\-\\_@\.\-]+"))
         (psto-send-message psto-bot-jid
                             (concat "U " (match-string-no-properties 0)))
       (self-insert-command 1)))

(provide 'psto)
;;; psto.el ends here
