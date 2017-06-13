My emacs configuration.
=======================

# Structure: #

    .emacs.d/
    │
    ├── README.md     # info file with TODO-list
    │
    ├── init.el       # main config file
    │
    ├── custom.el     # customize file (mostly generated) called by init.el
    │
    ├── env.el        # system-aware lisp config called by init.el
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
* auto-save draft buffers
* complete IDE for javascript
* spaceline: manage battery status
