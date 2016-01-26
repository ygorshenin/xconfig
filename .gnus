(require 'nnir)

;;; Defines *work-mail*, *full-name* and public key *signature*.
(load-file "~/.personal.el")

(setq user-mail-address *work-mail*
      user-full-name *full-name*
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

      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-auth-credentials "~/.authinfo.gpg"
      message-send-mail-function 'smtpmail-send-it
      mml2015-encrypt-to-self t

      gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date))

      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│"

      gnus-summary-display-arrow t
      gnus-summary-line-format "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-20,20f%}  %3{│%} %1{%B%}%s\n"
      gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40G%)\n"
      gnus-topic-line-format "%i %A: %(%{%n%}%) %v\n"

      gnus-use-cache t

      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
