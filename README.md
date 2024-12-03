# buffer-terminator.el - Automatically Terminate Inactive Buffers
![Build Status](https://github.com/jamescherti/buffer-terminator.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/buffer-terminator.el)
![](https://raw.githubusercontent.com/jamescherti/buffer-terminator.el/main/.images/made-for-gnu-emacs.svg)

The `buffer-terminator` Emacs package automatically terminates inactive buffers to help maintain a clean and efficient workspace.

It provides configurable options to determine which buffers to keep, a timeout for inactivity, and periodic cleanup intervals.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [buffer-terminator.el - Automatically Terminate Inactive Buffers](#buffer-terminatorel---automatically-terminate-inactive-buffers)
  - [Features](#features)
  - [Installation](#installation)
    - [Install with straight (Emacs version < 30)](#install-with-straight-emacs-version--30)
    - [Installing with use-package and :vc (Built-in feature in Emacs version >= 30)](#installing-with-use-package-and-vc-built-in-feature-in-emacs-version--30)
  - [Configuration](#configuration)
    - [Ignore Specific Buffers](#ignore-specific-buffers)
    - [Timeout for Inactivity](#timeout-for-inactivity)
    - [Cleanup Interval](#cleanup-interval)
    - [Verbose Mode](#verbose-mode)
    - [Preserve Special Buffers](#preserve-special-buffers)
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

### Verbose Mode

Enable verbose mode to log buffer cleanup events:

```elisp
(setq buffer-terminator-verbose t)
```

### Preserve Special Buffers

Keep all special buffers by setting the following option to `t`:

```elisp
(setq buffer-terminator-ignore-special-buffers t)
```

## Author and License

The *buffer-terminator* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [buffer-terminator.el @GitHub](https://github.com/jamescherti/buffer-terminator.el)
