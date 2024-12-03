# buffer-terminator.el - Automatically Terminate Inactive Buffers
![Build Status](https://github.com/jamescherti/buffer-terminator.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/buffer-terminator.el)
![](https://raw.githubusercontent.com/jamescherti/buffer-terminator.el/main/.images/made-for-gnu-emacs.svg)

The `buffer-terminator` Emacs package automatically terminates inactive buffers to help maintain a clean and efficient workspace. It also helps improve Emacs performance by reducing the number of open buffers.

It provides configurable options to determine which buffers to keep, a timeout for inactivity, and periodic cleanup intervals.

By default, `buffer-terminator-mode` terminates all file-visiting and dired buffers that have been inactive for longer than the duration specified by `buffer-terminator-inactivity-timeout`. Special buffers are ignored by default, but you can configure `buffer-terminator` to include them by adjusting the relevant option provided below.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [buffer-terminator.el - Automatically Terminate Inactive Buffers](#buffer-terminatorel---automatically-terminate-inactive-buffers)
  - [Features](#features)
  - [Installation](#installation)
    - [Install with straight (Emacs version < 30)](#install-with-straight-emacs-version--30)
    - [Installing with use-package and :vc (Built-in feature in Emacs version >= 30)](#installing-with-use-package-and-vc-built-in-feature-in-emacs-version--30)
  - [Configuration](#configuration)
    - [Timeout for Inactivity](#timeout-for-inactivity)
    - [Cleanup Interval](#cleanup-interval)
    - [Verbose Mode](#verbose-mode)
    - [Preserve Special Buffers](#preserve-special-buffers)
    - [Ignore Specific Buffers](#ignore-specific-buffers)
  - [Author and License](#author-and-license)
  - [Links](#links)

<!-- markdown-toc end -->

## Features

- Automatically kills inactive buffers based on a configurable timeout.
- Excludes specific buffers using buffer names, regular expressions, or special buffer types.
- Verbose mode for logging terminated buffers.
- Supports customizable intervals for periodic cleanup.
- Ensures special buffers and important user-defined buffers are preserved.

## Installation

### Install with straight (Emacs version < 30)

To install `buffer-terminator` with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package buffer-terminator
  :ensure t
  :straight (buffer-terminator
             :type git
             :host github
             :repo "jamescherti/buffer-terminator.el")
  :config
  (buffer-terminator-mode 1))
```

### Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install `buffer-terminator` with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package buffer-terminator
  :demand t
  :vc (:url "https://github.com/jamescherti/buffer-terminator.el"
       :rev :newest)
  :config
    (buffer-terminator-mode 1))
```

## Configuration

### Timeout for Inactivity

Set the inactivity timeout (in seconds) after which buffers are considered inactive (default is 60 minutes):

```elisp
(setq buffer-terminator-inactivity-timeout (* 60 60)) ; 60 minutes
```

### Cleanup Interval

Define how frequently the cleanup process should run (default is every 5 minutes):

```elisp
(customize-set-variable
 'buffer-terminator-kill-buffers-interval (* 5 60)) ; 5 minutes
```

(Using `customize-set-variable` allows `buffer-terminator-kill-buffers-interval` to update the timer dynamically, without the need to restart `buffer-terminator-mode`.)

### Verbose Mode

Enable verbose mode to log buffer cleanup events:

```elisp
(setq buffer-terminator-verbose t)
```

### Preserve Special Buffers

Keep all special buffers by setting the following option to `t`:

```elisp
(setq buffer-terminator-keep-special-buffers t)
```

**IMPORTANT:** It is generally NOT recommended to set this to nil. If you choose to set it to nil, ensure that the special buffers you want to keep are added to `buffer-terminator-ignore-buffer-names` and `buffer-terminator-ignore-buffer-regexps`.

### Ignore Specific Buffers

Exclude specific buffers from being killed by name:

```elisp
(setq buffer-terminator-ignore-buffer-names '("*scratch*"
                                              "*Messages*"
                                              "*Async-native-compile-log*"
                                              "*Compile-Log*"))
```

Or by regular expression:

```elisp
(setq buffer-terminator-ignore-buffer-regexps '("\\` \\*Minibuf-.*\\*\\'"
                                                "\\` \\*Echo Area "))
```

Keep in mind that when `buffer-terminator-keep-special-buffers` is set to `t`, special buffers that start and end with `*` are automatically ignored and will not be killed, even if they are not explicitly listed in `buffer-terminator-ignore-buffer-regexps` or `buffer-terminator-ignore-buffer-names`.

## Author and License

The *buffer-terminator* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [buffer-terminator.el @GitHub](https://github.com/jamescherti/buffer-terminator.el)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
