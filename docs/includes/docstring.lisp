(defun download-website-text (url)
  "Downloads @cl:param(url) and strips all HTML tags."
  ...)

(defclass metal ()
  ((cost :reader cost
         :initarg cost
         :type float
         :documentation "All instances @b(must) initialize cost to a floating
         point value.")))
