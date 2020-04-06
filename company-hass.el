;;; Company mode setup
(defun company-hass--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-hass--candidates (prefix)
  (let (res)
    (dolist (item hass-entities-friendly-names)
      (when (string-prefix-p prefix (car item))
        (push (company-hass--make-candidate item) res)))
    res))

(defun company-hass--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-hass--annotation (candidate)
  (format " (%s)" (get-text-property 0 'meta candidate)))

(defun company-hass (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-hass))
    (prefix (company-grab-symbol))
    (candidates (company-hass--candidates arg))
    (annotation (company-hass--annotation arg))
    (meta (company-hass--meta arg))))

(provide 'company-hass)
