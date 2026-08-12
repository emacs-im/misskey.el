;; -*- mode: eask; lexical-binding: t -*-

(package "misskey"
         "0.1.0"
         "Publish notes to Misskey-compatible servers")

(website-url "https://github.com/0WD0/misskey.el")
(keywords "convenience" "comm")

(package-file "misskey.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "appkit" :file "../appkit.el")

(script "refresh-local-deps" "eask -f install-file ../appkit.el")
(script "test" "eask test ert --quiet test/*.el")
(script "test-local"
        "eask -f install-file ../appkit.el && eask recompile && eask test ert --quiet test/*.el")
