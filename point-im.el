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
(require 'jabber-chat)
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
(defvar point-im-user-name-regex "\\B\\(@[0-9A-Za-z@\\.\\_\\-]+\\)")
(defvar point-im-tag-regex  "\\(\\*[^\\*]+?\\)[[:space:]]")
(defvar point-im-stag-regex  "[[:space:]]\\(\\*\\*[^\\*]+?\\*\\*\\)[[:space:]]")
(defvar point-im-bold-regex "\\*\\*\\(.*\\)\\*\\*")
(defvar point-im-italic-regex "\\*\\(.*?\\)\\*[[:space:]]")
(defvar point-im-quote-regex "^>.*\n?[^\n]+")
(defvar point-im-striked-out-regex "\\([[:graph:]]+\\)^[Ww]")
(defvar point-im-md-striked-out-regex "~~\\([[:graph:]]+\\)~~")

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

(defun point-im--prepare-stag (tag)
  "Convert TAG with spaces to GET parameter."
  (let ((stripped (substring tag 2 -2)))
    (replace-regexp-in-string "[[:space:]]+" "+" stripped)))


(defun point-im--make-url (m type)
  "Make an URL from matched text M according to TYPE."
  (let ((m* (substring m 1)))
    (pcase type
      (`tag (concat "https://point.im/?tag=" m*))
      (`user (concat "https://" m* ".point.im/" ))
      (`id (concat "https://point.im/"
                   (replace-regexp-in-string "/" "#" m*)))
      (`stag (concat "https://point.im/?tag=" (point-im--prepare-stag m)))
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
        (point-im--overlay-put this-overlay 'url (point-im--make-url m type))
        (overlay-put this-overlay 'follow-link t)
        (overlay-put this-overlay 'goto-address t)
        (if mouse-face
            (overlay-put this-overlay 'mouse-face mouse-face)
          (let ((mf (point-im--make-mouse-face face)))
            (copy-face face mf)
            (set-face-underline mf t)
            (overlay-put this-overlay 'mouse-face mf)))
        (point-im--overlay-put this-overlay 'help-echo help-echo)))))

(defvar point-im-re-face-alist
  `((,point-im-id-regex point-im-id-face :type id)
    (,point-im-user-name-regex point-im-user-name-face :type user)
    (,point-im-italic-regex point-im-italic-face)
    (,point-im-bold-regex point-im-bold-face)
    (,point-im-tag-regex point-im-tag-face :type tag)
    (,point-im-stag-regex point-im-tag-face :type stag)
    (,point-im-quote-regex point-im-quote-face)
    (,point-im-striked-out-regex point-im-striked-out-face)
    (,point-im-striked-out-regex point-im-striked-out-face)
    (,point-im-md-striked-out-regex point-im-striked-out-face))
  "Alist of elements (RE FACE-SYMBOL &key ...).
For keyword arguments see `point-im--propertize'")

(defun point-im-fontify (&optional start end)
  "Fontify entities in the region between START and END.
If both are nil, from begin to the end of the buffer)."
  (interactive)
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (point-im-unfontify start end)
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (dolist (fontify-args point-im-re-face-alist)
          (apply #'point-im--propertize start end fontify-args))))))

(defun point-im-jabber-chat-printer (xml-data who mode)
  "Call `point-fontify' on the newly written text.
Don't care about XML-DATA and WHO, MODE should be :insert.
See `jabber-chat-printers' for full documentation."
  (when (eq mode :insert)
    (ignore-errors
      (let* ((end (point))
             (limit (max (- (point) 30000) (1+ (point-min)))))
        ;; We only need to fontify the text written since the last
        ;; prompt.  The prompt has a field property, so we can find it
        ;; using `field-beginning'.
        (point-im-fontify (field-beginning nil t limit) end)))))

(defun point-im-prop-at-point (prop-name)
  "Get an overlay property PROP-NAME at point."
  (let (prop)
    (dolist (overlay (overlays-at (point)) prop)
      (let ((p (overlay-get overlay prop-name)))
        (when p
          (setq prop p))))
    prop))

(defun point-im-matched-at-point ()
  "Get an overlay property 'matched-text at point."
  (point-im-prop-at-point 'matched-text))

(defun point-im-go-url ()
  "Open entity URL in browser using `browse-url'."
  (interactive)
  (let ((url (or (point-im-prop-at-point 'url)
                 (browse-url-url-at-point))))
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
      (insert
       (concat id
               (if (and point-im-reply-id-add-plus
                       (not (string-match "/" id)))
                   "+"
                 " ")))
      (recenter 10)
      t)))

(defun point-im--send-action (text-proc)
  "Send a matched-text processed by TEXT-PROC."
  (let ((matched-text (point-im-matched-at-point)))
    (when matched-text
      (point-im--send-message point-im-bot-jid
                              (funcall text-proc matched-text)))))

(defmacro def-simple-action (name fmt)
  "Make an action NAME for a simple command using FMT format string."
  `(defun ,name ()
     (interactive)
     (point-im--send-action
      (lambda (s)
        (format ,fmt s)))))


(def-simple-action point-im-subscribe "S %s")
(def-simple-action point-im-info "%s")
(def-simple-action point-im-subscribe-recs "S! %s")
(def-simple-action point-im-unsubscribe "U %s")
(def-simple-action point-im-unsubscribe-recs "U! %s")
(def-simple-action point-im-bl "BL %s")
(def-simple-action point-im-ubl "UBL %s")
(def-simple-action point-im-wl "WL %s")
(def-simple-action point-im-uwl "UWL %s")
(def-simple-action point-im-recommend "! %s")
(def-simple-action point-im-delete "D %s")
(def-simple-action point-im-pin "pin %s")
(def-simple-action point-im-unpin "unpin %s")
(def-simple-action point-im-last "%s+")

(defmacro def-moving-action (name search-fn re &optional forward)
  "Create action NAME using SEARCH-FN applied to RE.
If FORWARD is true - search one match further."
  `(defun ,name (count)
     (interactive "P")
     (let* ((pos (point))
            (count (+ (or count 1)
                      ,(if forward 1 0)))
            (search
             (lambda (count)
               (funcall ,search-fn ,re nil t count)
               (let ((target (match-beginning 1)))
                 (and (not (eq target pos))
                      target)))))
       (goto-char
        (or (funcall search count)
            (funcall search count)
            pos)))))

(def-moving-action point-im-id-backward
  #'re-search-backward point-im-id-regex)
(def-moving-action point-im-id-forward
  #'re-search-forward point-im-id-regex t)

(def-moving-action point-im-user-name-backward
  #'re-search-backward point-im-user-name-regex)
(def-moving-action point-im-user-name-forward
  #'re-search-forward point-im-user-name-regex t)

(defun point-im--do-reply-to-post-comment (count)
  "Helper function.
Search COUNT ids back and inserts at `jabber-point-insert'."
  (if (re-search-backward point-im-id-regex nil t count)
      (progn
        (delete-region jabber-point-insert (point-max))
        (goto-char jabber-point-insert)
        (insert (concat (match-string-no-properties 1) " ")))
    (message "No comments found")))

(defvar point-im--comment-search-count 1)

(defvar point-im-reply-goto-end t
  "Go to the end of buffer after inserting id.")

(defun point-im-reply-to-post-comment (count)
  "Search COUNT or one #ids back and insert at `jabber-point-insert'.
Further invocations cause the insertion of farther comments.
When `point-im-reply-goto-end' is not nil - go to the end of buffer"
  (interactive "P")
  (save-excursion
    (setq point-im--comment-search-count
          (+ 1
             (or count 0)
             (if (eq last-command #'point-im-reply-to-post-comment)
                 point-im--comment-search-count
               0)))
    (point-im--do-reply-to-post-comment point-im--comment-search-count))
  (when point-im-reply-goto-end
    (goto-char (point-max))))

;; Composing messages in external markdown/text buffer
(defun point-im-compose (jc)
  "Create a buffer for composing a message."
  (interactive (list (jabber-read-account)))
  (let ((matched-text (point-im-matched-at-point)))
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name
                           (concat "Point-Compose"
                                   (when matched-text
                                     (format "-%s" matched-text)))))
      (insert (if matched-text (concat matched-text " ") ""))
      (if (require 'markdown-mode nil t)
          (funcall 'markdown-mode)
        (funcall 'text-mode))
      (point-im-compose-mode t)
      (setq jabber-buffer-connection jc)
      (switch-to-buffer-other-window (current-buffer))
      (message (substitute-command-keys "\\<point-im-compose-mode-map>\\[point-im-compose-send] to send.")))))

(defun point-im-compose-send ()
  "Send buffer contents to bot."
  (interactive)
  (let ((text (buffer-string)))
    (when (< 0 (length text))
      (jabber-send-message jabber-buffer-connection point-im-bot-jid "" text nil)
      (message "Message sent")))
  (bury-buffer)
  (delete-window))

(defvar point-im-compose-mode-map (make-sparse-keymap)
  "Fake keymap for sending message.")

(define-key point-im-compose-mode-map (kbd "C-c C-c") #'point-im-compose-send)

(define-minor-mode point-im-compose-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil)


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

(defun point-im-popup-menu (prefix)
  "Popup menu.  If PREFIX is mouse event - popup mouse menu."
  (interactive "P")
  (pcase (point-im-prop-at-point 'type)
    (`tag (jabber-popup-menu point-im-tag-menu))
    (`stag (jabber-popup-menu point-im-tag-menu))
    (`user (jabber-popup-menu point-im-user-menu))
    (`id (jabber-popup-menu point-im-id-menu))))

(defun point-im-define-keys (map keymap-alist)
  "Add key bindings to MAP taken from KEYMAP-ALIST."
  (mapc
   (lambda (keypair)
     (pcase keypair
       (`(,key . ,fn)
        (define-key map (kbd key) fn))))
   keymap-alist)
  map)

(defvar point-im-highlight-keymap-alist
  '(("<mouse-2>" . point-im-go-url)
    ("g" . point-im-go-url)
    ("b" . point-im-bl)
    ("w" . point-im-wl)
    ("u" . point-im-unsubscribe)
    ("s" . point-im-subscribe)
    ("d" . point-im-delete)
    ("RET" . point-im-insert)
    ("!" . point-im-recommend)
    ("C-c C-p" . point-im-popup-menu)
    ("<mouse-3>" . point-im-popup-menu)))

;; bindings
(defvar point-im-highlight-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map jabber-common-keymap)
    (point-im-define-keys map point-im-highlight-keymap-alist)
    map)
  "Keymap to hold point-im.el key defs under highlighted IDs.")

(defvar point-im-keymap-alist
  '(("C-x M-p" . point-im-user-name-backward)
    ("C-x M-n" . point-im-user-name-forward)
    ("M-p" . point-im-id-backward)
    ("M-n" . point-im-id-forward)
    ("M-RET" . point-im-reply-to-post-comment)
    ("C-c C-e" . point-im-compose)))

(defvar point-im-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-chat-mode-map)
    (point-im-define-keys map point-im-keymap-alist)
    map)
  "Keymap for `point-im-mode'.")

;; Avy integration
(defmacro def-point-im-avy-jump (name re)
  `(defun ,name (do-not-insert)
     "Avy jump to id, insert into conversation buffer unless DO-NOT-INSERT."
     (interactive "P")
     ;; `avy--generic-jump' returns t on C-g
     (let* ((jump-result (avy--generic-jump ,re nil 'pre))
            (interrupted (eq t jump-result)))
       (unless (or do-not-insert interrupted)
         ;; We don't want a plus here
         (let ((point-im-reply-id-add-plus nil))
           (or
            (point-im-insert)
            (point-im-go-url)))))))

(defvar point-im-avy-keymap-alist
  '(("M-g i" . point-im-avy-goto-id)
    ("M-g u" . point-im-avy-goto-user-name)
    ("M-g t" . point-im-avy-goto-tag)
    ("M-g p" . point-im-avy-goto-any)))

(when (require 'avy nil t)
  (def-point-im-avy-jump point-im-avy-goto-id point-im-id-regex)
  (def-point-im-avy-jump point-im-avy-goto-user-name point-im-user-name-regex)
  (def-point-im-avy-jump point-im-avy-goto-tag point-im-tag-regex)
  (def-point-im-avy-jump point-im-avy-goto-any
    (concat "\\("
            (mapconcat 'identity
                       (list point-im-id-regex
                             point-im-user-name-regex
                             point-im-tag-regex
                             goto-address-url-regexp)
                       "\\|")
            "\\)"))
  (point-im-define-keys point-im-keymap point-im-avy-keymap-alist))


(define-minor-mode point-im-mode
  "Toggle Point mode."
  :lighter " ℗"
  :keymap point-im-keymap
  (if (equal (jabber-chat-get-buffer point-im-bot-jid)
             (buffer-name))
      (if point-im-mode
          (progn
            (add-to-list 'jabber-chat-printers 'point-im-jabber-chat-printer t)
            (point-im-fontify))
        (progn
          (point-im-unfontify)
          (delete 'point-im-jabber-chat-printer jabber-chat-printers)))
    (setq point-im-mode nil)))

(provide 'point-im)

;;; point-im.el ends here
