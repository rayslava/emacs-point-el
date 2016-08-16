;;; point-im.el --- fontify p@point.im bot jabber chat

;; Author: @a13
;; Loosely based on https://github.com/rayslava/emacs-point-el
;; Keywords: comm
;; Package-Requires: ((emacs "24") (jabber "0.8.92"))
;; URL: https://github.com/a13/point.el

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
;; This package provides minor-mode for p@point.im bot jabber chat buffer.

;;; Code:
(require 'cl-lib)
(require 'browse-url)
(require 'jabber-menu)
(require 'jabber-chatbuffer)
(require 'jabber-keymap)

;; faces
(defgroup point-im-faces nil "Faces for displaying Point.im msg"
  :group 'point-im)

(defface point-im-id-face
  '((t (:foreground "yellow" :weight bold)))
  "face for displaying id"
  :group 'point-im-faces)

(defface point-im-user-name-face
  '((t (:foreground "DeepSkyBlue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'point-im-faces)

(defface point-im-bold-face
  '((t (:weight bold)))
  "face for markdown **text**"
  :group 'point-im-faces)

(defface point-im-italic-face
  '((t (:slant oblique :weight extra-light)))
  "face for markdown *text*"
  :group 'point-im-faces)

(defface point-im-tag-face
  '((t (:foreground "LawnGreen" :slant oblique :weight extra-light)))
  "face for markdown *text*"
  :group 'point-im-faces)

(defface point-im-quote-face
  '((t (:foreground "gray" :slant oblique)))
  "face for markdown > text"
  :group 'point-im-faces)

(defface point-im-striked-out-face
  '((t (:strike-through t :weight bold)))
  "face for ^W word"
  :group 'point-im-faces)

;; misc vars
(defvar point-im-reply-id-add-plus t
  "Set to t then id inserted with + (e.g NNNN+).
Useful for people more reading instead writing")

(defvar point-im-bot-jid "p@point.im")


;; regexes, first matched group will be fontified
(defvar point-im-id-regex "\\(#[a-z]+\\(/[0-9]+\\)?\\)")
(defvar point-im-user-name-regex "[[:space:]]\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")
(defvar point-im-tag-regex  "\\(\\*[^\\*]+?\\)[[:space:]]")
(defvar point-im-bold-regex "\\*\\*\\(.*\\)\\*\\*")
(defvar point-im-italic-regex "\\*\\(.*?\\)\\*[[:space:]]")
(defvar point-im-quote-regex "^>\\([[:ascii:]]*?\\)\\(^#\\|\\(?:\n\\{2\\}\\)\\)")
(defvar point-im-striked-out-regex "\\([[:graph:]]+\\)^[Ww]")

(defun point-im--send-message (to text)
  "Send to TO TEXT immediately."
  (save-excursion
    (let ((buffer (jabber-chat-create-buffer (jabber-read-account) to)))
      (set-buffer buffer)
      (goto-char (point-max))
      (delete-region jabber-point-insert (point-max))
      (insert text)
      (jabber-chat-buffer-send))))

(defun point-im-unfontify (&optional start end)
  "Remove point-im overlay from the given region from START to END."
  (interactive)
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (dolist (overlay (overlays-in start end))
      (if (overlay-get overlay 'point-im)
          (delete-overlay overlay)))))

(defun point-im--make-mouse-face (face)
  "Create a new face from FACE prefixed by \"mouse-\"."
  (make-symbol (concat "mouse-" (symbol-name face))))

(defun point-im--overlay-put (overlay prop value)
  "Set one property of overlay OVERLAY: give property PROP value VALUE only if VALUE is not nil.  VALUE will be returned."
  (when value
      (overlay-put overlay prop value)))

(defun point-im--make-url (m type)
  "Make an URL from matched text M according to TYPE."
  (let ((m* (substring m 1)))
    (pcase type
      (`tag (concat "https://point.im/?tag=" m*))
      (`user (concat "https://" m* ".point.im/" ))
      (`id (concat "https://point.im/"
                    (replace-regexp-in-string "/" "#" m*)))
      (whatever nil))))

(cl-defun point-im--propertize (start end re face &key mouse-face help-echo keymap commands type)
  "Create and propertize all regions from START to END matching RE with
FACE, MOUSE-FACE, HELP-ECHO and KEYMAP properties."
  (goto-char start)
  (while (re-search-forward re end t)
    (let* ((s (match-beginning 0))
           (e (match-end 0))
           (m (match-string-no-properties 1))
           this-overlay)
      (setq this-overlay (make-overlay s e))
      (overlay-put this-overlay 'face face)
      (overlay-put this-overlay 'evaporate t)
      (point-im--overlay-put this-overlay 'matched-text m)
      ;; TODO: commands
      (point-im--overlay-put this-overlay 'commands commands)
      ;; type: id, nick, tag
      (point-im--overlay-put this-overlay 'type type)
      (overlay-put this-overlay 'keymap (or keymap point-im-highlight-keymap))
      (overlay-put this-overlay 'point-im t)
      ;; only for the link
      (when (and m type)
        (overlay-put this-overlay 'url (point-im--make-url m type))
        (overlay-put this-overlay 'follow-link t)
        (overlay-put this-overlay 'goto-address t)
        (if mouse-face
            (overlay-put this-overlay 'mouse-face mouse-face)
          (let ((mf (point-im--make-mouse-face face)))
            (copy-face face mf)
            (set-face-underline mf t)
            (overlay-put this-overlay 'mouse-face mf)))
        (point-im--overlay-put this-overlay 'help-echo help-echo)))))


(defun point-im-fontify (&optional start end)
  "Fontify entities in the region between START and END.
If both are nil, from begin to the end of the buffer)."
  (interactive)
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (point-im-unfontify start end)
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (point-im--propertize start end point-im-id-regex 'point-im-id-face :type 'id)
        (point-im--propertize start end point-im-user-name-regex 'point-im-user-name-face :type 'user)
        (point-im--propertize start end point-im-italic-regex 'point-im-italic-face)
        (point-im--propertize start end point-im-bold-regex 'point-im-bold-face)
        (point-im--propertize start end point-im-tag-regex 'point-im-tag-face :type 'tag)
        (point-im--propertize start end point-im-quote-regex 'point-im-quote-face)
        (point-im--propertize start end point-im-striked-out-regex 'point-im-striked-out-face)))))


(defun point-im-jabber-chat-printer (xml-data who mode)
  "Call `point-fontify' on the newly written text.
Don't care about XML-DATA and WHO, MODE should be :insert.
See `jabber-chat-printers' for full documentation."
  (when (eq mode :insert)
    (ignore-errors
      (require 'point-im)
      (let ((end (point))
            (limit (max (- (point) 10000) (1+ (point-min)))))
        ;; We only need to fontify the text written since the last
        ;; prompt.  The prompt has a field property, so we can find it
        ;; using `field-beginning'.
        (point-im-fontify (field-beginning nil nil limit) end)))))

(define-minor-mode point-im-mode
  "Toggle Point mode."
  :lighter " ℗"
  (let ((point-im-buf (jabber-chat-get-buffer point-im-bot-jid)))
    (if point-im-mode
        (progn
          (add-to-list 'jabber-chat-printers 'point-im-jabber-chat-printer t)
          (when point-im-buf
            (with-current-buffer point-im-buf
              (point-im-fontify))))
      (progn
        (when point-im-buf
          (with-current-buffer point-im-buf
            (point-im-unfontify)))
        (delete 'point-im-jabber-chat-printer jabber-chat-printers)))))

(defun point-im-prop-at-point (prop-name)
  "Get an overlay property PROP-NAME at point."
  (let (prop)
    (dolist (overlay (overlays-at (point)) prop)
      (setq prop (overlay-get overlay prop-name)))
    prop))

(defun point-im-matched-at-point ()
  "Get an overlay property 'matched-text at point."
  (point-im-prop-at-point 'matched-text))

(defun point-im-go-url ()
  "Open entity URL in browser using `browse-url'."
  (interactive)
  (let ((url (point-im-prop-at-point 'url)))
    (when url
      (browse-url url)
      (unless (string= last-command "mouse-drag-region")
        (self-insert-command 1)))))

(defun point-im-insert ()
  "Insert reply id in conversation buffer."
  (interactive)
  (let ((id (point-im-matched-at-point)))
    (when id
      (message "Mark set")
      (push-mark)
      (goto-char (point-max))
      ;; usually #NNNN supposed #NNNN+
      (if (string-match "/" id)
          (insert (concat id " "))
        (insert (concat id (if point-im-reply-id-add-plus "+" " ")))))
    (recenter 10)))

(defun point-im--send-action (text-proc)
  "Send a matched-text processed by TEXT-PROC."
  (let ((matched-text (point-im-matched-at-point)))
    (when matched-text
      (point-im--send-message point-im-bot-jid
                          (funcall text-proc matched-text)))))

(defmacro point-im--simple-action (name prefix)
  "Make an action NAME for a simple PREFIX command."
  `(defun ,name ()
     (interactive)
     (point-im--send-action
      (lambda (s)
        (concat ,prefix " " s)))))

(point-im--simple-action point-im-subscribe "S")
(point-im--simple-action point-im-info "")
(point-im--simple-action point-im-subscribe-recs "S!")
(point-im--simple-action point-im-unsubscribe "U")
(point-im--simple-action point-im-unsubscribe-recs "U!")
(point-im--simple-action point-im-bl "BL")
(point-im--simple-action point-im-ubl "UBL")
(point-im--simple-action point-im-wl "WL")
(point-im--simple-action point-im-uwl "UWL")
(point-im--simple-action point-im-recommend "!")
(point-im--simple-action point-im-delete "D")
(point-im--simple-action point-im-pin "pin")
(point-im--simple-action point-im-unpin "unpin")

(defmacro point-im--moving-action (name search-fn re)
  "Create action NAME using SEARCH-FN applied to RE."
  `(defun ,name (count)
     (interactive "P")
     (funcall ,search-fn ,re nil t (or count 1))
     (let ((to (match-beginning 1)))
       (when to
         (goto-char to)))))

(point-im--moving-action point-im-id-backward
                         #'re-search-backward point-im-id-regex)
(point-im--moving-action point-im-id-forward
                         #'re-search-forward point-im-id-regex)

(point-im--moving-action point-im-user-name-backward
                         #'re-search-backward point-im-user-name-regex)
(point-im--moving-action point-im-user-name-forward
                         #'re-search-forward point-im-user-name-regex)


;; popup menus
(defvar point-im-user-menu
  `(("Open in browser" . point-im-go-url)
    ("User info" . point-im-info)
    ;; ("User tags" . point-im-user-tag)
    ("Subscribe" . point-im-subscribe)
    ("Subscribe to recommendations" . point-im-subscribe-recs)
    ;; ("Subscribe to user's tag" . point-im-user-subscribe-tag)
    ("Unsubscribe" . point-im-unsubscribe)
    ("Unsubscribe from recommendations" . point-im-unsubscribe-recs)
    ;; ("Unsubscribe from user's tag" . point-im-user-unsubscribe-tag)
    ("Add to whitelist" . point-im-wl)
    ("Delete from blacklist" . point-im-uwl)
    ("Add to blacklist" . point-im-bl)
    ("Delete from blacklist" . point-im-ubl)))

;; TODO: 'post and 'comment types instead of common 'id
(defvar point-im-id-menu
  `(("Open in browser" . point-im-go-url)
    ("Info" . point-im-info)
    ("Delete" . point-im-delete)
    ("Subscribe" . point-im-subscribe)
    ("Unsubscribe" . point-im-unsubscribe)
    ("Pin" . point-im-pin)
    ("Unpin" . point-im-unpin)))

(defvar point-im-tag-menu
  `(("Open in browser" . point-im-go-url)
    ("Last posts with tag" . point-im-info)
    ("Subscribe" . point-im-subscribe)
    ("Unsubscribe" . point-im-unsubscribe)
    ("Add to whitelist" . point-im-wl)
    ("Delete from blacklist" . point-im-uwl)
    ("Add to blacklist" . point-im-bl)
    ("Delete from blacklist" . point-im-ubl)))

(defun point-im-popup-menu ()
  "Popup menu."
  (interactive)
  (let ((type (point-im-prop-at-point 'type)))
    (pcase type
      (`tag (jabber-popup-menu point-im-tag-menu))
      (`user (jabber-popup-menu point-im-user-menu))
      (`id (jabber-popup-menu point-im-id-menu)))))

;; bindings
(defvar point-im-highlight-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map jabber-common-keymap)
    (define-key map (kbd "<mouse-2>") #'point-im-go-url)
    (define-key map (kbd "g") #'point-im-go-url)
    (define-key map (kbd "b") #'point-im-bl)
    (define-key map (kbd "w") #'point-im-wl)
    (define-key map (kbd "u") #'point-im-unsubscribe)
    (define-key map (kbd "s") #'point-im-subscribe)
    (define-key map (kbd "d") #'point-im-delete)
    (define-key map (kbd "RET") #'point-im-insert)
    (define-key map (kbd "!") #'point-im-recommend)
    (define-key map (kbd "C-c C-p") #'point-im-popup-menu)
    map)
  "Keymap to hold point-im.el key defs under highlighted IDs.")

(define-key jabber-chat-mode-map "\C-x\M-p" 'point-im-user-name-backward)
(define-key jabber-chat-mode-map "\C-x\M-n" 'point-im-user-name-forward)
(define-key jabber-chat-mode-map "\M-p" 'point-im-id-backward)
(define-key jabber-chat-mode-map "\M-n" 'point-im-id-forward)

(provide 'point-im)

;;; point-im.el ends here
