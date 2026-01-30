# buffer-terminator.el - Safely Terminate Emacs Buffers Automatically to Enhance Emacs' Performance and Reduce Clutter in the Buffer List
![Build Status](https://github.com/jamescherti/buffer-terminator.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/buffer-terminator-badge.svg)](https://melpa.org/#/buffer-terminator)
[![MELPA Stable](https://stable.melpa.org/packages/buffer-terminator-badge.svg)](https://stable.melpa.org/#/buffer-terminator)
![License](https://img.shields.io/github/license/jamescherti/buffer-terminator.el)
![](https://jamescherti.com/misc/made-for-gnu-emacs.svg)

The **buffer-terminator** Emacs package *automatically and safely kills buffers*, ensuring a clean and efficient workspace while *enhancing the performance of Emacs* by reducing open buffers, which minimizes active modes, timers, processes...

Beyond performance, *buffer-terminator* provides other benefits. For instance, if you occasionally need to close annoying or unused buffers, *buffer-terminator* can handle this automatically, eliminating the need for manual intervention. (The default configuration is suitable for most users. However, the *buffer-terminator* package is highly customizable. You can define specific rules for retaining or terminating buffers by modifying the `buffer-terminator-rules-alist` with your preferred set of rules.)

Activating `(buffer-terminator-mode)` safely terminates all buffers that have been inactive for longer than the duration specified by `buffer-terminator-inactivity-timeout` (default: 30 minutes). It checks every `buffer-terminator-interval` (default: 10 minutes) to determine if a buffer should be terminated.

The following buffers are not terminated by default:
- Special buffers (These buffers are non-file buffers that: start with a space, or start and end with `*`, or whose major mode is derived from `special-mode`, or they serve as the Minibuffer).
- Modified file-visiting buffers that have not been saved; the user must save them first.
- Buffers currently displayed in any visible window or tab-bar tab. (This also includes buffers indirectly made visible, such as `org-src` source edit buffers, which cause their originating buffers to be considered visible, or `markdown-mode` edit-indirect buffers that reference the original Markdown file.)
- Buffers associated with running processes.

If this package enhances your productivity, please show your support by ⭐ starring buffer-terminator on GitHub to help more users discover its benefits.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [buffer-terminator.el - Safely Terminate Emacs Buffers Automatically to Enhance Emacs' Performance and Reduce Clutter in the Buffer List](#buffer-terminatorel---safely-terminate-emacs-buffers-automatically-to-enhance-emacs-performance-and-reduce-clutter-in-the-buffer-list)
  - [Installation from MELPA](#installation-from-melpa)
  - [Configuration](#configuration)
    - [Verbose Mode](#verbose-mode)
    - [Timeout for Inactivity](#timeout-for-inactivity)
    - [Cleanup Interval](#cleanup-interval)
    - [Rules](#rules)
  - [Frequently asked questions](#frequently-asked-questions)
    - [What problem is buffer-terminator aiming to solve?](#what-problem-is-buffer-terminator-aiming-to-solve)
    - [How about modifying the quit-window function to kill buffers as soon as they are closed, instead of using buffer-terminator?](#how-about-modifying-the-quit-window-function-to-kill-buffers-as-soon-as-they-are-closed-instead-of-using-buffer-terminator)
    - [If this actually improves performance, I’d love to see some benchmarks or real-world numbers](#if-this-actually-improves-performance-id-love-to-see-some-benchmarks-or-real-world-numbers)
    - [I prefer keeping buffers open because it is easier for me to reopen them](#i-prefer-keeping-buffers-open-because-it-is-easier-for-me-to-reopen-them)
    - [How is this different from the builtin midnight-mode?](#how-is-this-different-from-the-builtin-midnight-mode)
  - [Testimonials from users](#testimonials-from-users)
  - [Author and License](#author-and-license)
  - [Links](#links)

<!-- markdown-toc end -->

## Installation from MELPA

To install *buffer-terminator* from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))
```

## Configuration

### Verbose Mode

Enable verbose mode to log buffer cleanup events:

```elisp
(setq buffer-terminator-verbose t)
```

### Timeout for Inactivity

Set the inactivity timeout (in seconds) after which buffers are considered inactive (default is 30 minutes):

```elisp
(setq buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
```

### Cleanup Interval

Define how frequently the cleanup process should run (default is every 10 minutes):

```elisp
(customize-set-variable 'buffer-terminator-interval (* 10 60)) ; 10 minutes
```

(Using `customize-set-variable` allows `buffer-terminator-interval` to update the timer dynamically, without the need to restart `buffer-terminator-mode`.)

### Rules

By default, *buffer-terminator* automatically determines which buffers are safe to terminate.

However, if you need to define specific rules for keeping or terminating certain buffers, you can configure them using `buffer-terminator-rules-alist`.

The `buffer-terminator-rules-alist` variable holds instructions for keeping or terminating buffers based on their names or regular expressions. Each rule is a cons cell where the key is a symbol indicating the rule type, and the value is either string or a list of strings.

Here is an example:
```elisp
(setq buffer-terminator-rules-alist
      ;; kill-buffer-name: Always kill buffers whose names match important-buffer-name1 and important-buffer-name2
      '((kill-buffer-name . ("temporary-buffer-name1"
                             "temporary-buffer-name2"))

        ;; keep-buffer-name: Always keep buffers whose names match important-buffer-name1 and important-buffer-name2
        (keep-buffer-name . ("important-buffer-name1"
                             "important-buffer-name2"))

        ;; kill-buffer-name: Always kill buffers whose names match temporary-buffer-name3
        (kill-buffer-name . "temporary-buffer-name3")

        ;; keep-buffer-name-regexp: Always keep buffers matching a regular expression
        (keep-buffer-name-regexp . ("\\` \\*Minibuf-[0-9]+\\*\\'"))

        ;; kill-buffer-name-regexp: Always kill buffers matching a regular expression
        (kill-buffer-name-regexp . "compile-angel")

        ;; Retain special buffers (DO NOT REMOVE).
        ;;
        ;; (If you choose to kill special buffers by removing the following,
        ;; ensure that the special buffers you want to keep are added
        ;; keep-buffer-name or keep-buffer-name-regexp rules above.)
        ;;
        ;; DO NOT REMOVE special buffers unless you know of what you are doing.
        (keep-buffer-property . special)

        ;; Retain process buffers.
        ;;
        ;; (Process buffers are buffers where an active process is running.
        ;; Removing the following will result in the termination of such
        ;; buffers, potentially disrupting active processes like vterm.)
        (keep-buffer-property . process)

        ;; Retain visible buffers (DO NOT REMOVE).
        ;;
        ;; Visible buffer are those currently displayed in any window.
        ;; It is generally discouraged to set this to nil, as doing so may result
        ;; in the termination of visible buffers, except for the currently active
        ;; buffer in the selected window.
        ;;
        ;; DO NOT REMOVE visible buffers unless necessary.
        (keep-buffer-property . visible)

        ;; Kill inactive buffers.
        ;; (This can be customized with `buffer-terminator-inactivity-timeout'
        ;; and `buffer-terminator-interval'.)
        (kill-buffer-property . inactive)

        ;; Call a function that decides the fate of a buffer. It returns:
        ;;   :kill    Indicates that the buffer should be killed.
        ;;   :keep    Indicates that the buffer should be kept.
        ;;   nil      Leave decision to the next rule specified
        ;;            in `buffer-terminator-rules-alist`.
        ;; (call-function . function-name)

        ;; Keep the remaining buffers that were not retained by previous rules
        (return . :keep)))
```

Here is another [example by gavv, one of the first buffer-terminator users.](https://github.com/jamescherti/buffer-terminator.el/issues/2#issuecomment-2526372129)

## Frequently asked questions

### What problem is buffer-terminator aiming to solve?

- Some users prefer terminating inactive buffers to improve Emacs' performance by reducing the number of open buffers. This, in turn, decreases the load from active modes, timers, and other processes associated with those buffers. Buffer-local modes and their timers consume both CPU and memory. Why keep them alive when they can be safely removed?
- Some users prefer to keep only the buffers they actively need open, helping to declutter the buffer list. Decluttering the buffer list can also improve the performance of other packages. For example, saving and loading an [easysession](https://github.com/jamescherti/easysession.el) or desktop.el is much faster when the buffer list is reduced.
- Some users prefer that buffers not part of an active window be automatically closed, as they are not actively needed.
- *Buffer-terminator* helps users by automatically closing unnecessary buffers, eliminating the need for manual cleanup.
- Some Emacs packages continue interacting with open buffers, even when they are buried ([Reddit post: A function to periodically wipe buffers not recently shown; thoughts?](https://www.reddit.com/r/emacs/comments/1h15mni/a_function_to_periodically_wipe_buffers_not/)).

### How about modifying the quit-window function to kill buffers as soon as they are closed, instead of using buffer-terminator?

Using quit-window works if the goal is to immediately kill buffers upon closing a window, but it can lead to unintended consequences.

For instance, if a buffer is displayed in another tab or window, it will still be closed simply because one window showing it was closed. Additionally, killing buffers immediately is not always desirable. Delaying the closure preserves the state of file buffers and Dired buffers, including opened or closed headings, folds, and other buffer-specific context.

**Buffer Terminator addresses these issues by performing additional checks before killing a buffer.** By default, it verifies whether the buffer is visible in any other window or tab-bar tab, ensuring that buffers are only closed when truly no longer needed. Moreover, buffer-terminator does not terminate buffers immediately; a configurable delay is applied to provide a grace period, allowing users to continue working with a buffer if it is still required.

### If this actually improves performance, I’d love to see some benchmarks or real-world numbers

Because each Emacs user's configuration is unique, the performance benefits of using the *buffer-terminator* Emacs package depend on the number of enabled modes and active timers in that specific setup.

Leaving buffers open keeps their associated timers active, and the number of timers grows with the number of Emacs packages in use. Timers are responsible for scheduling functions to run at specific intervals or after a delay, often managing background tasks like updating buffers, fetching data, or performing periodic checks. Since each active timer triggers a function, an excessive number of timers can increase CPU usage, potentially leading to performance degradation in Emacs' single-threaded environment.

Additionally, using *buffer-terminator* to reduce the buffer list can improve the performance of packages that iterate over the `buffer-list` function to operate on buffers. Since these packages iterate over open buffers, a shorter buffer list allows for faster execution of their operations. (For example, the built-in *desktop.el* package or the [easysession](https://github.com/jamescherti/easysession.el) package, which save and restore open buffers/frames, can be affected by buffer list length. If `buffer-list` is too long, Emacs startup may slow down, as it needs to restore a larger set of buffers.)

### I prefer keeping buffers open because it is easier for me to reopen them

There is little benefit in leaving unused buffers open on the off-chance they might be needed later. If needed again, these buffers can be quickly reopened using *recentf*, *project.el*, *dired*, or similar tools.

### How is this different from the builtin midnight-mode?

Midnight-mode does not address the problem that buffer-terminator solves, which is the safe and frequent termination of inactive buffers.

Midnight mode and `clean-buffer-list` are for killing buffers once a day. The Midnight option `clean-buffer-list-delay-general` specifies the number of days before a buffer becomes eligible for auto-killing, rather than using seconds or minutes as a timeout.

In contrast, `buffer-terminator` allows specifying the timeout interval in seconds (Default: 30 minutes), enabling more frequent termination of inactive buffers.

The `buffer-terminator` package offers additional features that are not supported by midnight and `clean-buffer-list`, including:
- The *buffer-terminator* package is more customizable than Midnight Mode. It allows users to specify a customized list of rules using `buffer-terminator-rules-alist`, enabling them to determine which buffers should be killed based on factors such as inactivity, visibility, buffer name, whether it's a file or process buffer, and other conditions.
- Buffer-terminator does not kill visible buffers in other tabs, even if they exceed the timeout. This prevents disruptions to editing workflows.
Buffer-terminator provides the option to choose whether to keep or kill specific types of buffers, such as those associated with processes or file-visiting buffers.
- Buffer-terminator avoids relying on `buffer-display-time`, which is not always updated reliably. For instance, `buffer-display-time` may not reflect activity when switching to a window or tab displaying a specific buffer.
- Buffer-terminator does not kill special buffers by default, whereas Midnight kills all special buffers by default unless the user tells Midnight to ignore them. Midnight's behavior can disrupt packages like Corfu, Cape, Consult, Eglot, Flymake, and others that rely on special buffers to store data.
- Buffer-terminator can also kill specific special buffers. It is useful, for example, if the user want to keep special buffers, but with a few exceptions: The user still want to kill *Help* and *helpful ...* buffers (and maybe some other buffers related to documentation) if they weren't used for a while.

## Testimonials from users

- [ouboub](https://github.com/jamescherti/buffer-terminator.el/issues/10#issuecomment-3190700326): "I just did, thanks for nice package, wish I have known about it earlier..."
- [2ck](https://github.com/jamescherti/buffer-terminator.el/issues/11#issue-3729240100): "thank you for this package which managed to actually kill my old buffers, which midnight-mode couldn't."

## Author and License

The *buffer-terminator* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2026 James Cherti

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
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
