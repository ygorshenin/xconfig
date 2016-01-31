(require 'nnir)
(require 'smtpmail)

(load-file "~/.personal.el")

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name *full-name*
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp-ssl (server port user password)
  "Sets related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun change-smtp ()
  "Changes the SMTP server according to the current From line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (address . auth-spec) in *smtp-accounts*
          if (string-match address from)
            do (return (apply 'set-smtp-ssl auth-spec))
          finally (error "Cannot infer SMTP information."))))

(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
    "Call `change-smtp' before every `smtpmail-via-smtp'."
    (with-current-buffer smtpmail-text-buffer (change-smtp)))
(ad-activate 'smtpmail-via-smtp)

(setq gnus-posting-styles *posting-styles*)

(setq user-full-name *full-name*
      mml2015-signers (list *signature*)
      nntp-authinfo-file "~/.authinfo.gpg"

      ;;; foo and bar are used here because I'm using two gmail
      ;;; accounts in gnus.
      gnus-select-method '(nnimap "foo"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap))
      gnus-secondary-select-methods '((nnimap "bar"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl)
                                              (nnir-search-engine imap)))

      mml2015-encrypt-to-self t

      gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date))

      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-false-root "▷ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-single-leaf "└─►"
      gnus-sum-thread-tree-vertical "│ "

      gnus-summary-display-arrow t
      gnus-summary-line-format "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-20,20f%}  %3{│%} %1{%B%}%s\n"


      gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40G%)\n"
      gnus-topic-line-format "%i %A: %(%{%n%}%) %v\n"

      gnus-use-cache t

      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
