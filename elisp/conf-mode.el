
(autoload 'conf-mode "conf-mode" () t)
(autoload 'conf-unix-mode "conf-mode" () t)
(autoload 'conf-windows-mode "conf-mode" () t)
(autoload 'conf-javaprop-mode "conf-mode" () t)
(autoload 'conf-space-mode "conf-mode" () t)
(autoload 'conf-xdefaults-mode "conf-mode" () t)

(setq auto-mode-alist
      `(,@auto-mode-alist
        ("\\.docbook\\'" . sgml-mode)
        ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)

        ;; Windows candidates may be opened case sensitively on Unix

        ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" .
         conf-mode)
        ("java.+\\.conf\\'" . conf-javaprop-mode)
        ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
        ;; *.cf, *.cfg, *.conf, *.config[.local|.de_DE.UTF8|...], */config
        ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" .
         conf-mode)
        ("\\.desktop\\'" . conf-unix-mode)

        ("\\`/etc/\\(?:aliases\\|DIR_COLORS\\|ethers\\|.?fstab\\|gnokiirc\\|.*hosts\\
  |ksysguarddrc\\|lesskey\\|login\\.defs\\|mtab\\|opera6rc\\|permissions\\|prot
  |ocols\\|rpc\\|services\\)\\'" . conf-mode)
        ;; either user's dot-files or under /etc or some such
        ("/\\.?\\(?:kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
        ;; alas not all ~/.*rc files are like this

        ("/\\.\\(?:enigma\\|gltron\\|hxplayer\\|net\\|neverball\\|qt/.+\\|realplayer\
  \|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)

        ("/\\.\\(?:gdbtkinit\\|grip\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'"
         . conf-mode)
        ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
        ("/X11.+app-defaults/" . conf-xdefaults-mode)
        ("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
        ;; this contains everything twice, with space and with colon :-(
        ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)))
(setq magic-mode-alist
      `(("# xmcd" . conf-unix-mode)
        ,@magic-mode-alist))
