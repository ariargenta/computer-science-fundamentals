;; 1.1.2 - Naming and the Environment
(princ "Variables") (terpri)

(defvar \*size\* 2)

(princ "\'size\' variable value: ")
(princ \*size\*) (terpri)
(princ "5 * size = ")
(princ (* 5 \*size\*)) (terpri)

(defvar \*pi\* 3.14159)
(defvar \*radius\* 10)

(princ "PI * radius^2 = ")
(princ (* pi (* \*radius\* \*radius\*))) (terpri)

(defvar \*circumference\* (* 2 \*pi\* \*radius\*))

(princ "circumference = ")
(princ \*circumference\*) (terpri)