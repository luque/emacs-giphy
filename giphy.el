(require 'url)
(require 'json)

(defconst giphy-api-key "dc6zaTOxFJmzC")
(defvar url-http-end-of-headers)

(defun giphy-search (search-query)
  (interactive "sSearch query: ")
  (with-current-buffer
      (url-retrieve-synchronously (format "http://api.giphy.com/v1/gifs/search?api_key=%s&q=%s" giphy-api-key search-query))
    (message "Invoking endpoint %s" (format "http://api.giphy.com/v1/gifs/search?api_key=%s&q=%s" giphy-api-key search-query))
    (goto-char url-http-end-of-headers)
    (json-read)))

(defconst sample-url "http://giphy.com/gifs/cat-funny-QgcQLZa6glP2w")


(defun get-gif-name-from-url (url)
  (string-match "http://.*/\\(.*\\)-.*$" url)
  (capitalize (replace-regexp-in-string "-" " " (match-string 1 url)))
  )

(defun format-image-for-helm (image)
  (let ((url (cdr (assoc 'url image)))
        (rating (cdr (assoc 'rating image))))
    (format "%s [%s]\n%s"
            (get-gif-name-from-url url)
            rating
            url)
  )
)

(defun helm-search-giphy ()
  (mapcar (lambda (image)
            (cons (format-image-for-helm image) image))
          (cdr (assoc 'data (giphy-search helm-pattern)))))

(defun insert-image-from-url (&optional url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data 'gif t)))
      (kill-buffer buffer))))

(defun get-original-url-from-image (image-data)
  (cdr (assoc 'url (assoc 'original (assoc 'images image-data)))))

(defun insert-candidate-url (candidate)
  "Inserts candidate URL into current buffer."
  (let ((originalURL (get-original-url-from-image candidate)))
    (message "Inserting %s in buffer..." originalURL)
    (insert originalURL))
    t
  )

(defun insert-candidate-gif (candidate)
  "Inserts candidate GIF into current buffer."
  (let ((originalURL (get-original-url-from-image candidate))
        (original-point (point)))
    (message "Inserting GIF in buffer..." originalURL)
    (when (insert-candidate-url candidate)
     (goto-char original-point)
     (org-inline-image)))
)

(defun open-candidate-url-in-browser (candidate)
  "Opens Giphy URL in browser."
  (let ((embedURL (cdr (assoc 'url candidate))))
    (message "Opening %s in browser..." embedURL)
    (browse-url embedURL))
  )

(defgroup giphy ()
  "Giphy animated GIFs."
  :group 'org
  :prefix "giphy-")

(defcustom giphy-inline-image-root "/tmp/giphy-inline-image/"
  "Root directory where Gipht temp files are stored."
  :type 'directory
  :group 'giphy)

(defun org-inline-image--create-root-maybe ()
  "Create root directory if it doesn't exist yet."
  (unless (file-exists-p giphy-inline-image-root)
    (make-directory giphy-inline-image-root t)))

(defun org-inline-image--get-image-props (file)
  "Return image properties for FILE."
  `(image :type ,(image-type file)
          :file ,file
          :relief 0
          :margin 0))

(defvar org-inline-image-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-inline-image-animate)
    (define-key map (kbd "h") 'org-inline-image-hide)
    map)
  "Keymap active when point is on the image.")

(defun org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'type) 'giphy) (overlays-at (point)))))

(defun org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (overlay-get (org-inline-image--get-current-image) prop))

;; TODO: make removing optional?
(defun org-inline-image-hide ()
  "Hide the inlined image at point.

The file is also removed from the filesystem.  Repeated inlining
will re-download the file."
  (interactive)
  (let ((ov (org-inline-image--get-current-image))
        (original-file (org-inline-image--get 'original-file)))
    (delete-overlay ov)
    (delete-file original-file)))

(defun org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (org-inline-image--get 'display)))
    (when (image-animated-p image-props)
      (image-animate image-props))))

(defun org-inline-image--get-link ()
  "Get link at point."
  (interactive)
  (let (beg end link)
    (cond
     ((org-in-regexp org-bracket-link-regexp)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 1)))
     ((org-in-regexp org-angle-link-re)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 1)))
     ((org-in-regexp org-plain-link-re)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 0))))
    (message "Link found: %s" link)
    (list :beg beg :end end :link link :uri)))

(defun get-giphy-id-from-url (url)
  (message "Get ID for %s" url)
  (string-match "http://.*/\\(.*\\)/.*$" url)
  (match-string 1 url)
  )

(defun org-inline-image ()
  "Inline an image."
  (interactive)
  (org-inline-image--create-root-maybe)
  (-when-let (link-data (org-inline-image--get-link))
    (message "resolved-link %s" (plist-get link-data :link))
    (let* ((resolved-link (plist-get link-data :link))
           (name (concat giphy-inline-image-root (get-giphy-id-from-url resolved-link))))
      (when (url-copy-file resolved-link name)
        (let ((ov (make-overlay (plist-get link-data :beg) (plist-get link-data :end)))
              (image-props (org-inline-image--get-image-props name)))
          (overlay-put ov 'type 'giphy)
          (overlay-put ov 'display image-props)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'original-file name)
          (overlay-put ov 'keymap org-inline-image-keymap)
          (when (image-animated-p image-props)
            (image-animate image-props))
          (goto-char (plist-get link-data :beg)))))))

(setq helm-sources-giphy
      '((name . "Giphy Results")
        (candidates . helm-search-giphy)
        (volatile)
        (multiline)
        (action .  (("Insert GIF into buffer" . insert-candidate-gif)
                    ("Insert GIF's URL into buffer" . insert-candidate-url)
                    ("Open Giphy GIF page on browser" . open-candidate-url-in-browser))
                ))
)

(defun helm-giphy ()
  (interactive)
  (helm :sources '(helm-sources-giphy)
        :prompt "Search query: "
        :buffer "*helm-giphy*")
)

(provide 'help-giphy)
