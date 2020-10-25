# My Emacs config

This repository contains my emacs configuration files.

# Customisations

This config allows some level of customisation in a few key places. Those are described below.

## Custom config files

This emacs config will attempt to load two files that are not under version control and can be used to further customize the Emacs configuration. 

These two files are expected to be located within the ``.emacs_lisp/config`` folder and are:
- `init-pre.el`
- `init-post.el`

Both of these files may contain a function `config-init-pre` (`config-init-post` respectively) that will be called after the file has been loaded. `init-pre.el` is loaded as soon as possible during the Emacs initialisation, whereas `init-post.el` is called at the very end of the Emacs initialisation, just before loading the custom file.

## Programming languages

The `init-programming.el` Emacs configuration file will attempt to compile and load any files contained in the `.emacs_lisp/programming` directory. Discovery of files within that directory is automatic.

It is, however, possible to customize which files get loaded by adding a filed called `skip.txt` within that directory. Each line should contain the name of a file that should be skipped when loading the programming languages. Within that file, any lines starting with '#' or ';' are automatically ignored.
