;;; point.el --- p@point.im
;; Package-Version: 0

;; Loosely based on https://github.com/rayslava/emacs-point-el

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

(require 'cl-lib)
(require 'browse-url)
(require 'jabber-chatbuffer)
(require 'jabber-keymap)

;; faces
(defgroup point-faces nil "Faces for displaying Point.im msg"
  :group 'point)

(defface point-id-face
  '((t (:foreground "yellow" :weight bold)))
  "face for displaying id"
  :group 'point-faces)

(defface point-user-name-face
  '((t (:foreground "DeepSkyBlue" :weight bold :slant normal)))
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

(defface point-tag-face
  '((t (:foreground "LawnGreen" :slant oblique :weight extra-light)))
  "face for markdown *text*"
  :group 'point-faces)

(defface point-quote-face
  '((t (:foreground "gray" :slant oblique)))
  "face for markdown > text"
  :group 'point-faces)

(defface point-striked-out-face
  '((t (:strike-through t :weight bold)))
  "face for ^W word"
  :group 'point-faces)

;; misc vars
(defvar point-reply-id-add-plus t
  "Set to t then id inserted with + (e.g NNNN+).
Useful for people more reading instead writing")

(defvar point-bot-jid "p@point.im")


;; regexes, second group will be fontified
(defvar point-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar point-user-name-regex "[[:space:]]\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")
(defvar point-tag-regex  "\\(\\*[^\\*]+?\\)[[:space:]]")
(defvar point-bold-regex "\\*\\*\\(.*\\)\\*\\*")
(defvar point-italic-regex "\\*\\(.*?\\)\\*[[:space:]]")
(defvar point-quote-regex "^>\\([[:ascii:]]*?\\)\\(^#\\|\\(?:\n\\{2\\}\\)\\)")
(defvar point-striked-out-regex "\\([[:graph:]]+\\)^[Ww]")

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

(defun point-unfontify (start end)
  "Remove `point' fontification from the given region."
  (dolist (overlay (overlays-in start end))
    (if (overlay-get overlay 'point)
	(delete-overlay overlay))))

(defun make-mouse-face (face)
  (make-symbol (concat "mouse-" (symbol-name face))))

(defun overlay-put* (overlay prop value)
  (when value
      (overlay-put overlay prop value)))

(defun point-make-url (m type)
  "Make a url from matched text"
  (let ((m* (substring m 1)))
    (pcase type
      (`tag (concat "https://point.im/?tag=" m*))
      (`user (concat "https://" m* ".point.im/" ))
      (`id (concat "https://point.im/"
                    (replace-regexp-in-string "/" "#" m*)))
      (whatever nil))))

(cl-defun point-propertize (start end re face &key mouse-face help-echo keymap commands type)
  ;;   "Create and propertize all regions matching `re' with
  ;; `face', `mouse-face', `help-echo' and `keymap' properties"
  (goto-char (or start (point-min)))
  (while (re-search-forward re end t)
    (let* ((s (match-beginning 0))
           (e (match-end 0))
           (m (match-string-no-properties 1))
           this-overlay)
      ;; (message "found %s" m)
      (setq this-overlay (make-overlay s e))
      (overlay-put this-overlay 'face face)
      (overlay-put this-overlay 'evaporate t)
      (overlay-put* this-overlay 'matched-text m)
      ;; TODO: commands
      (overlay-put* this-overlay 'commands commands)
      ;; type: id, nick, tag
      (overlay-put* this-overlay 'type type)
      (overlay-put this-overlay 'keymap (or keymap point-highlight-keymap))
      (overlay-put this-overlay 'point t)
      ;; only for the link
      (when (and m type)
        (overlay-put this-overlay 'url (point-make-url m type))
        (overlay-put this-overlay 'follow-link t)
        (overlay-put this-overlay 'goto-address t)
        (if mouse-face
            (overlay-put this-overlay 'mouse-face mouse-face)
          (let ((mf (make-mouse-face face)))
            (copy-face face mf)
            (set-face-underline mf t)
            (overlay-put this-overlay 'mouse-face mf)))
        (overlay-put* this-overlay 'help-echo help-echo)))))


(defun point-fontify (&optional start end)
  "Fontify point ids and usernames in the current buffer"
  (interactive)
  (point-unfontify (or start (point-min)) (or end (point-max)))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (point-propertize start end point-id-regex 'point-id-face :type 'id)
      (point-propertize start end point-user-name-regex 'point-user-name-face :type 'user)
      (point-propertize start end point-italic-regex 'point-italic-face)
      (point-propertize start end point-bold-regex 'point-bold-face)
      (point-propertize start end point-tag-regex 'point-tag-face :type 'tag)
      (point-propertize start end point-quote-regex 'point-quote-face)
      (point-propertize start end point-striked-out-regex 'point-striked-out-face))))


(defun jabber-chat-point (xml-data who mode)
  "Call `point-fontify' on the newly written text."
  ;;  (when (string-match point-bot-jid (jabber-xml-get-attribute xml-data 'from))
  (when (eq mode :insert)
    (ignore-errors
      (require 'point)
      (let ((end (point))
            (limit (max (- (point) 10000) (1+ (point-min)))))
        ;; We only need to fontify the text written since the last
        ;; prompt.  The prompt has a field property, so we can find it
        ;; using `field-beginning'.
        (point-fontify (field-beginning nil nil limit) end)))))

(add-to-list 'jabber-chat-printers 'jabber-chat-point t)

(defun prop-at-point (propname)
  "Overlay property at point"
  (let (prop)
    (dolist (overlay (overlays-at (point)) prop)
      (setq prop (overlay-get overlay propname)))
    prop))

(defun matched-at-point ()
  (prop-at-point 'matched-text))

(defun point-go-url ()
  (interactive)
  (let ((url (prop-at-point 'url)))
    (when url
      (browse-url url)
      (unless (string= last-command "mouse-drag-region")
        (self-insert-command 1)))))

(defun point-insert ()
  "Inserting reply id in conversation buffer"
  (interactive)
  (let ((id (matched-at-point)))
    ;; (when (string-match-p (jabber-chat-get-buffer point-bot-jid)
    ;;                       (buffer-name))
    ;;   (message "Mark set")
    ;;   (push-mark))
    ;;    (point-find-buffer)
    (goto-char (point-max))
    ;; usually #NNNN supposed #NNNN+
    (if (string-match "/" id)
        (insert (concat id " "))
      (insert (concat id (if point-reply-id-add-plus "+" " ")))))
  (recenter 10))

(defun point-send-action (text-proc)
  (let ((matched-text (matched-at-point)))
    (when matched-text
      (point-send-message point-bot-jid
                          (funcall text-proc matched-text)))))

(defmacro point-simple-action (name prefix)
  `(defun ,name ()
     (interactive)
     (point-send-action
      (lambda (s)
        (concat ,prefix " " s)))))

(point-simple-action point-subscribe "S")
(point-simple-action point-info "")
(point-simple-action point-subscribe-recs "S!")
(point-simple-action point-unsubscribe "U")
(point-simple-action point-unsubscribe-recs "U!")
(point-simple-action point-bl "BL")
(point-simple-action point-ubl "UBL")
(point-simple-action point-wl "WL")
(point-simple-action point-uwl "UWL")
(point-simple-action point-recommend "!")
(point-simple-action point-delete "D")
(point-simple-action point-pin "pin")
(point-simple-action point-unpin "unpin")

(defvar point-highlight-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map jabber-common-keymap)
    (define-key map (kbd "<mouse-2>") 'point-go-url)
    (define-key map (kbd "C-c C-a") 'point-go-url)
    (define-key map (kbd "g") 'point-go-url)
    (define-key map (kbd "b") 'point-bl)
    (define-key map (kbd "w") 'point-wl)
    (define-key map (kbd "u") 'point-unsubscribe)
    (define-key map (kbd "s") 'point-subscribe)
    (define-key map (kbd "d") 'point-delete)
    (define-key map (kbd "RET") 'point-insert)
    (define-key map (kbd "!") 'point-recommend)
    map)
  "Keymap to hold point.el key defs under highlighted IDs.")

;; popup menus
(defvar point-user-menu
  `(("Open in browser" . point-go-url)
    ("User info" . point-info)
    ;; ("User tags" . point-user-tag)
    ("Subscribe" . point-subscribe)
    ("Subscribe to recommendations" . point-subscribe-recs)
    ;; ("Subscribe to user's tag" . point-user-subscribe-tag)
    ("Unsubscribe" . point-unsubscribe)
    ("Unsubscribe from recommendations" . point-unsubscribe-recs)
    ;; ("Unsubscribe from user's tag" . point-user-unsubscribe-tag)
    ("Add to whitelist" . point-wl)
    ("Delete from blacklist" . point-uwl)
    ("Add to blacklist" . point-bl)
    ("Delete from blacklist" . point-ubl)))

;; TODO: 'point and 'comment types instead of common 'id
(defvar point-id-menu
  `(("Open in browser" . point-go-url)
    ("Info" . point-info)
    ("Subscribe" . point-subscribe)
    ("Unsubscribe" . point-unsubscribe)
    ("Pin" . point-pin)
    ("Unpin" . point-unpin)))

(defvar point-tag-menu
  `(("Open in browser" . point-go-url)
    ("Last posts with tag" . point-info)
    ("Subscribe" . point-subscribe)
    ("Unsubscribe" . point-unsubscribe)
    ("Add to whitelist" . point-wl)
    ("Delete from blacklist" . point-uwl)
    ("Add to blacklist" . point-bl)
    ("Delete from blacklist" . point-ubl)))

(defun point-popup-menu ()
  "Popup menu"
  (interactive)
  (let ((type (prop-at-point 'type)))
    (pcase type
      (`tag (jabber-popup-menu point-tag-menu))
      (`user (jabber-popup-menu point-user-menu))
      (`id (jabber-popup-menu point-id-menu)))))

(define-key jabber-common-keymap "\C-c\C-p" 'point-popup-menu)

(provide 'point)

;;; point.el ends here
