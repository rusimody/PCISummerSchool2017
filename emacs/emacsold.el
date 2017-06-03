;;; For emacsen older than 24.4 which give errors such as
;;; (void-function with-eval-after-load etc...
;;; Add the following at the head of files giving errors
;;; such as ~/.emacs.d/gnu-apl-mode/gnu-apl-mode.el

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))
