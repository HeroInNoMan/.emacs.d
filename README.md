My emacs configuration.
=======================

# Structure: #

    .emacs.d/
    │
    ├── README.md     # info file with TODO-list
    │
    ├── init.el       # bootstrap file, tangles emacs.org into emacs.el and loads it
    │
    ├── emacs.org     # main config file, literate programming style
    │
    ├── emacs.el      # generated from emacs.org (not under vc)
    │
    ├── custom.el     # customize file (generated, not under vc)
    │
    ├── env.el        # environment-aware lisp config called by emacs.el
    │
    ├── elisp/        # plugins, modules (managed manually)
    │
    ├── misc/         # emacs-related stuff (managed manually)
    │
    ├── elpa/         # plugins, modules (managed by elpa, not under vc)
    │
    ├── backups/      # backup files (not under vc)
    │
    └── private.el    # private variables definitions (not under vc)

# TODO #

* eclipse-style formatting system for java files
* ctags with projectile
