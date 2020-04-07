;;; Company mode setup
(defun company-hass--fuzzy-match (prefix candidate)
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun company-hass--make-candidate (candidate)
  (let ((text (car candidate))
        (annotation (cadr candidate)))
    (propertize text :annotation annotation)))

(defun company-hass--candidates (prefix)
  (let (resultItem)
    (dolist (item hass-entities-friendly-names)
      (when (company-hass--fuzzy-match prefix (car item))
        (push (company-hass--make-candidate item) resultItem)))
    resultItem))

(defun company-hass--annotation (candidate)
  (format " (%s)" (get-text-property 0 :annotation candidate)))

(defun company-hass (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-hass))
    (prefix (and (eq major-mode 'yaml-mode)
                 (company-grab-symbol)))
    (candidates (company-hass--candidates arg))
    (annotation (company-hass--annotation arg))))

(provide 'company-hass)
