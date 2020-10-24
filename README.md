# My Emacs config

This repository contains my emacs configuration files.

## Custom config files

This emacs config will attempt to load two files that are not under version control and can be used to further customize the Emacs configuration. 

These two files are expected to be located within the ``.emacs_lisp/config`` folder and are:
- `init-pre.el`
- `init-post.el`

Both of these files may contain a function `config-init-pre` (`config-init-post` respectively) that will be called after the file has been loaded. `init-pre.el` is loaded as soon as possible during the Emacs initialisation, whereas `init-post.el` is called at the very end of the Emacs initialisation, just before loading the custom file.
