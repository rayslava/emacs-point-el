;;; juick.el --- improvement reading juick@juick.com

;; Copyright (C) 2009  mad
;; Copyright (C) 2009  vyazovoi
;; Copypaste     2010  nextus

;; Author: mad <owner.mad.epa@gmail.com>
;; Modification for psto: nextus <arzavt@gmail.com>

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

;; XXX: if jabber load through `jabber-autloads'
(require 'jabber-chatbuffer)

(defgroup juick-faces nil "Faces for displaying Juick msg"
  :group 'juick)

(defface juick-id-face
  '((t (:weight bold)))
  "face for displaying id"
  :group 'juick-faces)

(defface juick-user-name-face
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'juick-faces)

(defface juick-tag-face
  '((t (:foreground "red4" :slant italic)))
  "face for displaying tags"
  :group 'juick-faces)

(defface juick-bold-face
  '((t (:weight bold :slant normal)))
  "face for displaying bold text"
  :group 'juick-faces)

(defface juick-italic-face
  '((t (:slant italic)))
  "face for displaying italic text"
  :group 'juick-faces)

(defface juick-underline-face
  '((t (:underline t :slant normal)))
  "face for displaying underline text"
  :group 'juick-faces)

(defvar juick-reply-id-add-plus nil
  "Set to t then id inserted with + (e.g NNNN+).

Useful for people more reading instead writing")

(defvar juick-overlays nil)

(defvar juick-bot-jid "psto@psto.net")

(defvar juick-image-buffer "*psto-avatar-dir*")

(defvar juick-point-last-message nil)

(defvar juick-avatar-internal-stack nil
  "Internal var")

(defvar juick-avatar-update-day 4
  "Update (download) avatar after `juick-avatar-update-day'")

(defvar juick-icon-mode t
  "This mode display avatar in buffer chat")

(defvar juick-icon-hight nil
  "If t then show 96x96 avatars")

(defvar juick-tag-subscribed '()
  "List subscribed tags")

(defvar juick-auto-subscribe-list nil
  "This list contained tag or username for auto subscribe")

(defvar juick-bookmark-file (expand-file-name "~/.emacs.d/.psto-bkm"))

(defvar juick-bookmarks '())

(defvar juick-api-aftermid nil)

(defvar juick-timer-interval 120)
(defvar juick-timer nil)

(defvar psto-tmp-dir
  (expand-file-name (concat "psto-images-" (user-login-name))
                    temporary-file-directory))

(if (not (file-directory-p psto-tmp-dir))
    (make-directory psto-tmp-dir))

(defvar juick-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar juick-user-name-regex "[^0-9A-Za-z\\.]\\(@[0-9A-Za-z@\\.\\-\\_]+\\)")
;;(defvar juick-tag-regex "\\(\\* [A-Za-z]+\\)")
(defvar juick-bold-regex "[\n ]\\(\\*[^\n]+*\\*\\)[\n ]")
(defvar juick-italic-regex "[\n ]\\(/[^\n]+/\\)[\n ]")
(defvar juick-underline-regex "[\n ]\\(\_[^\n]+\_\\)[\n ]")

(defun juick-markup-chat (from buffer text proposed-alert &optional force)
  "Markup  message from `juick-bot-jid'.

Where FROM is jid sender, BUFFER is buffer with message TEXT

Use FORCE to markup any buffer"
  (if (or force (string-match juick-bot-jid from))
      (save-excursion
        (set-buffer buffer)
        (when (null force)
          (if (version< jabber-version "0.8.0")
              (jabber-truncate-top)
            (jabber-truncate-top buffer))
          (setq juick-point-last-message
                (re-search-backward "\\[[0-9]+:[0-9]+\\].*>" nil t)))
        (juick-markup-user-name)
        (juick-markup-id)
;;        (juick-markup-tag)
        (juick-markup-bold)
        (juick-markup-italic)
        (juick-markup-underline)
        (when (and juick-icon-mode window-system)
          (clear-image-cache)
          (juick-avatar-insert)))))

(add-hook 'jabber-alert-message-hooks 'juick-markup-chat)

(defun juick-avatar-insert ()
  (goto-char (or juick-point-last-message (point-min)))
  (setq juick-avatar-internal-stack nil)
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\(: @\\|>[ ]+\n@\\|#[a-z]+ @\\)\\([0-9A-Za-z@\\.\\-]+\\)" nil t)
      (let* ((icon-string "\n ")
             (name (match-string-no-properties 2))
             (fake-png (concat psto-tmp-dir "/" name ".png")))
        (goto-char (match-beginning 0))
        (juick-avatar-download name)
        (set-text-properties
         1 2 `(display
               (image :type png
                      :file ,fake-png))
         icon-string)
        (re-search-forward "@" nil t)
        (goto-char (- (point) 1))
        (insert (concat icon-string " "))
        (re-search-forward "" nil t)))))

(defun juick-avatar-download (name)
  "Download avatar from psto.net"
  (if (or (assoc-string name juick-avatar-internal-stack)
          (and (file-exists-p (concat psto-tmp-dir "/" name ".png"))
               (< (time-to-number-of-days
                   (time-subtract (current-time)
                                  (nth 5 (file-attributes (concat psto-tmp-dir "/" name ".png")))))
                  juick-avatar-update-day)))
      nil
    (let ((avatar-url (concat "http://" name ".psto.net/"))
          (url-request-method "GET"))
      (push name juick-avatar-internal-stack)
      (url-retrieve avatar-url
                    '(lambda (status name)
                       (let ((result-buffer (current-buffer)))
                         (goto-char (point-min))
                           (if (re-search-forward "/img/a/80/[0-9]*\.png" nil t)
                                (juick-avatar-download-and-save (match-string 0) name)
                              (juick-avatar-download-and-save "/img/a/40.png" name))
                           (kill-buffer result-buffer)))
                    (list name)))))

(defun juick-avatar-download-and-save (link name)
  "Extract image from LINK and save it with NAME in `psto-tmp-dir'"
  (let* ((filename (substring link (string-match "\\(0.png\\|0/[0-9]+\.png\\)" link)))
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

(define-key jabber-chat-mode-map [mouse-1] 'juick-go-url)
(define-key jabber-chat-mode-map "\M-p" 'juick-go-to-post-back)
(define-key jabber-chat-mode-map "\M-n" 'juick-go-to-post-forward)

(defun juick-go-to-post-back ()
  (interactive)
  (re-search-backward "@[a-z0-9]+:$" nil t))

(defun juick-go-to-post-forward ()
  (interactive)
  (re-search-forward "@[a-z0-9]+:$" nil t))

(defun juick-markup-user-name ()
  "Markup user-name matched by regex `juick-regex-user-name'"
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-user-name-regex nil t)
    (when (match-string 1)
      (juick-add-overlay (match-beginning 1) (match-end 1)
                         'juick-user-name-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'juick-insert-user-name))))

(defun juick-markup-id ()
  "Markup id matched by regex `juick-regex-id'"
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-id-regex nil t)
    (when (match-string 1)
      (juick-add-overlay (match-beginning 1) (match-end 1)
                         'juick-id-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'juick-insert-id))))

(defun juick-markup-tag ()
  "Markup tag matched by regex `juick-regex-tag'"
  (goto-char (or juick-point-last-message (point-min)))
  ;;; FIXME: I dont know how to recognize a tag point
  (while (re-search-forward (concat juick-user-name-regex  "\: ") nil t)
    ;;(goto-char (+ (point) (length (match-string 1))))
    (let ((count-tag 0))
      (while (and (looking-at "\\* ")
                  (<= count-tag 5))
        (let ((beg-tag (point))
              (end-tag (- (re-search-forward "[\n ]" nil t) 1)))
          (juick-add-overlay beg-tag end-tag 'juick-tag-face)
          (make-button beg-tag end-tag 'action 'juick-find-tag))
        (setq count-tag (+ count-tag 1))))))

(defun juick-markup-italic ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-italic-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-italic-face)
    (goto-char (- (point) 1))))

(defun juick-markup-bold ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-bold-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-bold-face)
    (goto-char (- (point) 1))))

(defun juick-markup-underline ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-underline-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-underline-face)
    (goto-char (- (point) 1))))

;;; XXX: maybe merge?
(defun juick-insert-user-name (button)
  "Inserting reply id in conversation buffer"
  (let ((user-name (buffer-substring-no-properties
                    (overlay-start button)
                    (- (re-search-forward "[\n :]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer juick-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (juick-find-buffer)
    (goto-char (point-max))
    (insert (concat user-name " ")))
  (recenter 10))

(defun juick-insert-id (button)
  "Inserting reply id in conversation buffer"
  (let ((id (buffer-substring-no-properties
             (overlay-start button)
             (- (re-search-forward "[\n ]" nil t) 1))))
    (when (string-match-p (jabber-chat-get-buffer juick-bot-jid)
                          (buffer-name))
      (message "Mark set")
      (push-mark))
    (juick-find-buffer)
    (goto-char (point-max))
    ;; usually #NNNN supposed #NNNN+
    (if (string-match "/" id)
        (insert (concat id " "))
          (insert (concat id (if juick-reply-id-add-plus "+" " ")))))
  (recenter 10))

(defun juick-find-tag (button)
  "Retrive 10 message this tag"
  (save-excursion
    (let ((tag (buffer-substring-no-properties
                (overlay-start button)
                (re-search-forward "\\([\n ]\\|$\\)" nil t))))
      (juick-find-buffer)
      (delete-region jabber-point-insert (point-max))
      (goto-char (point-max))
      (insert tag)))
  (jabber-chat-buffer-send))

(defun juick-find-buffer ()
  "Find buffer with `juick-bot-jid'"
  (interactive)
  (when (not (string-match (jabber-chat-get-buffer juick-bot-jid)
                           (buffer-name)))
    (delete-window)
    (let ((juick-window (get-window-with-predicate
                         (lambda (w)
                           (string-match (jabber-chat-get-buffer juick-bot-jid)
                                         (buffer-name (window-buffer w)))))))
      (if juick-window
          (select-window juick-window)
        ;; XXX: if nil open last juick@juick buffer
        (jabber-chat-with nil juick-bot-jid)))))

(defadvice jabber-chat-with (around jabber-chat-with-around-advice
                                    (jc jid &optional other-window) activate)
  "Used for markup history buffer"
  ad-do-it
  ;; FIXME: this activate ever when open buffer with psto@psto.net,
  ;; maybe adviced `jabber-chat-insert-backlog-entry' instead
  ;; `jabber-chat-with'.
  (when (string-match-p juick-bot-jid jid)
    (save-excursion
      (goto-char (point-min))
      (setq juick-point-last-message (point-min))
      (juick-markup-chat juick-bot-jid (current-buffer) nil nil t)
      (setq juick-point-last-message (point-max)))))

(defadvice jabber-chat-send (around jabber-chat-send-around-advice
                                    (jc body) activate)
  "Check and correct juick command"
  (if (string-match juick-bot-jid jabber-chatting-with)
      (let* ((body (cond
                    ((string= "№" body)
                     "#")
                    ((string= "№+" body)
                     "#+")
                    ((string= "РУДЗ" body)
                     "HELP")
                    ((string= "help" body)
                     "HELP")
                    ((string= "d l" body)
                     "D L")
                    (t
                     body))))
        ad-do-it)
    ad-do-it))

(defun juick-next-button ()
  "move point to next button"
  (interactive)
  (if (next-button (point))
      (goto-char (overlay-start (next-button (point))))
    (progn
      (goto-char (point-max))
      (message "button not found"))))

(defun juick-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay juick-overlays)))

(defun juick-delete-overlays ()
  (dolist (overlay juick-overlays)
    (delete-overlay overlay))
  (setq juick-overlays nil))

(provide 'psto)
;;; psto.el ends here
