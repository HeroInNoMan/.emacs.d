My emacs configuration.
=======================

# Structure: #

    .emacs.d/
    │
    ├── README.md     # info file with TODO-list
    │
    ├── init.el       # bootstrap file, tangles emacs.org into emacs.el and loads it
    │
    ├── emacs.org     # main config file, literate style
    │
    ├── emacs.el      # generated from emacs.org (not under vc)
    │
    ├── custom.el     # customize file (mostly generated) called by emacs.el
    │
    ├── env.el        # system-aware lisp config called by emacs.el
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

# Actions #

* eclipse-style formatting system
* ctags with projectile
* complete IDE for javascript
* spaceline: manage battery status
