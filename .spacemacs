;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ansible
     python
     sql
     csv
     yaml
     (typescript :variables
                 typescript-fmt-tool 'prettier
                 typescript-linter 'eslint
                 )
     javascript
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     git
     helm
     html
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     lsp
     markdown
     (multiple-cursors :variables multiple-cursors-backend 'mc)
     org

     ;; See https://github.com/arnm/mermaid-layer
     ;; Need to clone the repo into ~/.emacs.d/private/
     mermaid

     (php :variables php-backend 'lsp)
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     version-control
     terraform
     (treemacs :variables treemacs-use-icons-dired nil)
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ob-http
                                      keychain-environment
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         gruvbox)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Inconsolata for Powerline"
                               ;;"Source Code Pro"
                               :size 12.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   org-mode
                                                   pdf-view-mode
                                                   text-mode)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.n
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-to-list 'exec-path "/Users/iantruslove/.nvm/versions/node/v18.17.0/bin" t)
  (keychain-refresh-environment)
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ian/config-smartparens ()
  (use-package smartparens
    :defer t
    :diminish ""
    :bind (("C-k"         . sp-kill-hybrid-sexp)
           ("C-M-k"       . sp-kill-sexp)
           ("<backspace>" . sp-backward-delete-char)

           ("M-)" . sp-forward-slurp-sexp)
           ("C-M-)" . sp-forward-barf-sexp)
           ("M-("  . sp-backward-slurp-sexp)
           ("C-M-("  . sp-backward-barf-sexp)

           ("M-s" . sp-splice-sexp)
           ("M-r" . sp-raise-sexp)
           ("C-M-r" . sp-splice-sexp-killing-backward))

    :config
    (progn
      ;; load up smartparens default config
      ;; (https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el)
      (require 'smartparens-config))))


(defun ian/config-snippets ()
  ;; ;; This following statement causes warnings
  ;; (setq-default dotspacemacs-configuration-layers
  ;;               '((auto-completion :variables
  ;;                                  auto-completion-enable-snippets-in-popup t)))
  )


(defun ian/config-org-agenda ()
  (setq org-agenda-dim-blocked-tasks nil
        org-agenda-compact-blocks t)

  (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items (quote (closed state)))

  ;; Limit restriction lock highlighting to the headline only
  (setq org-agenda-restriction-lock-highlight-subtree nil)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))

  (setq org-agenda-span 'day)

  (setq org-agenda-sticky t)

  (setq org-agenda-persistent-filter t)

  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-custom-commands
        (quote (("G" "Goal View"          ;4M

                 (;; (agenda ""
                  ;;         ((org-agenda-span 1)
                  ;;          (org-agenda-time-grid nil)
                  ;;          (org-agenda-show-all-dates nil)
                  ;;          (org-agenda-skip-function
                  ;;           '(my/org-agenda-skip-without-match "+projects"))
                  ;;          (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
                  ;;          (org-deadline-warning-days 1000) ))

                  (tags-todo "TODO=\"NEXT\"+goals+current|TODO=\"TODO\"+goals+current"
                             ((org-agenda-overriding-header "Active Goals")
                              (org-agenda-hide-tags-regexp "goal\\|current\\|scope_")
                              (org-agenda-sorting-strategy
                               '(category-keep))))

                  (tags-todo "TODO=\"NEXT\"+projects+strategic|TODO=\"TODO\"+projects+strategic"
                             ((org-agenda-overriding-header "Strategic Projects (goal-supporting)")
                              (org-agenda-hide-tags-regexp "projects\\|strategic")
                              ;;(org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(deadline-down category-keep))))
                  ))

                ;; ("N" "Notes" tags "NOTE"
                ;;  ((org-agenda-overriding-header "Notes")
                ;;   (org-tags-match-list-sublevels t)))

                ("D" "Decisions" tags "DECISION"
                 ((org-agenda-overriding-header "Decisions")
                  (org-tags-match-list-sublevels t)))

                ("T" "Tech Debt" tags "TECH_DEBT"
                 ((org-agenda-overriding-header "Tech Debt")
                  (org-tags-match-list-sublevels t)))

                ;; ("h" "Habits" tags-todo "STYLE=\"habit\""
                ;;  ((org-agenda-overriding-header "Habits")
                ;;   (org-agenda-sorting-strategy
                ;;    '(todo-state-down effort-up category-keep))))

                ("h" "Home" tags-todo "#home"
                 ((org-agenda-overriding-header "Home Tasks")
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-sorting-strategy
                   '(category-keep))
                  ))

                ("A" "Agenda"
                 ((agenda "" nil)

                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-agenda-hide-tags-regexp "REFILE")
                         (org-tags-match-list-sublevels t)))

                  (tags-todo "DEADLINE<\"<now>\""
                             ((org-agenda-overriding-header "Overdue Tasks")))

                  (tags-todo "-#home-CANCELLED+TODO=\"NEXT\"-{^@.*}|-CANCELLED+PRIORITY=\"A\"-{^@.*}"
                             ((org-agenda-overriding-header (concat "Project Next and High Priority Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-agenda-hide-tags-regexp "projects")
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down priority-down effort-up category-keep))))

                  (tags-todo "-#home-REFILE+TODO=\"NEXT\"-{^@.*}|-#home-REFILE+TODO=\"TODO\"+PRIORITY=\"A\"-{^@.*}"
                             ((org-agenda-overriding-header (concat "Standalone Next and High Priority Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              ;; (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              ;; (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              ;; (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(priority-down todo-state-down effort-up category-keep))))

                  (tags-todo "-#home+TODO=\"TASK\""
                             ((org-agenda-overriding-header "Delegated Tasks")
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))


                  (tags-todo "-#home+TODO=\"TODO\"+{^@.*}|-#home+TODO=\"NEXT\"+{^@.*}"
                             ((org-agenda-overriding-header "Tasks to Delegate")
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))

                  (tags-todo "-#home-goals-{^@.*}-REFILE-CANCELLED-BLOCKED-LATER-PRIORITY=\"A\"/!+TODO|+NEXT"
                             ((org-agenda-overriding-header (concat "Other Standalone Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))

                  (tags-todo "-#home-CANCELLED+BLOCKED|-#home+LATER/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))

                  ;; (tags "-REFILE/"
                  ;;       ((org-agenda-overriding-header "Tasks to Archive")
                  ;;        (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                  ;;        (org-tags-match-list-sublevels nil)))
                  )
                 nil)

                ("P" "Projects"
                 ((agenda "" nil)

                  (tags-todo "-#home-CANCELLED+TODO=\"NEXT\"-{^@.*}|-CANCELLED+PRIORITY=\"A\"-{^@.*}"
                             ((org-agenda-overriding-header (concat "Project Next and High Priority Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-agenda-hide-tags-regexp "projects")
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down priority-down effort-up category-keep))))

                  (tags-todo "-#home-LATER-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-hide-tags-regexp "projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))

                  (tags-todo "-#home-CANCELLED/!-LATER"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-hide-tags-regexp "projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))

                  (tags-todo "-#home-REFILE-CANCELLED-BLOCKED-LATER/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including BLOCKED and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-hide-tags-regexp "projects")
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep)))))
                 nil)))))


(defun ian/config-org-tags ()
  ;; Tags with fast selection keys
  (setq org-tag-alist (quote (
                              ;; Traffic Lights
                              (:startgroup)
                              ("tlp_green")
                              ("tlp_orange")
                              ("tlp_red")
                              (:endgroup)

                              ;; GTD scope
                              (:startgroup)
                              ("@errand" . ?E)
                              ("@office" . ?O)
                              ("@home" . ?H)
                              ("@work" . ?W)
                              (:endgroup)

                              ;; How the time was spent
                              (:startgroup)
                              ("deep" . ?d)
                              ("shallow"  . ?s)
                              ("idle" . ?i)
                              ("meeting" . ?m)
                              (:endgroup)

                              ;; What the scope of the goal is
                              (:startgroup)
                              ("scope_pg")
                              ("scope_r_and_d")
                              ("scope_eng")
                              ("scope_prof_svcs")
                              ("scope_personal")
                              (:endgroup)

                              ("BLOCKED" . ?b)
                              ("LATER" . ?l)
                              ("PERSONAL" . ?P)
                              ("WORK" . ?W)
                              ("ORG" . ?o)
                              ("crypt" . ?e)
                              ("NOTE" . ?n)
                              ("DECISION" . ?D)
                              ("TECH_DEBT" . ?T)
                              ("CANCELLED" . ?c)
                              ("FLAGGED" . ??))))

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)


  (setq org-tags-match-list-sublevels t)

  )


(defun ian/config-org-tasks-todos ()
  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "TASK(f)" "|" "DONE(d)")  ;; TASK items are work delegated to others
                (sequence "BLOCKED(b@/!)" "LATER(l!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "forest green" :weight bold)
                ("DONE" :foreground "blue" :weight bold)
                ("TASK" . (:foreground "light sea green"))
                ("BLOCKED" :foreground "orange" :weight bold)
                ("LATER" :foreground "gray" :weight bold)
                ("CANCELLED" :foreground "blue" :weight bold)
                ("MEETING" :foreground "pink" :weight bold)
                ("PHONE" :foreground "pink" :weight bold))))

  (setq org-use-fast-todo-selection t)

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("BLOCKED" ("BLOCKED" . t))
                ("LATER" ("BLOCKED") ("LATER" . t))
                (done ("BLOCKED") ("LATER"))
                ("TODO" ("BLOCKED") ("CANCELLED") ("LATER"))
                ("NEXT" ("BLOCKED") ("CANCELLED") ("LATER"))
                ("DONE" ("BLOCKED") ("CANCELLED") ("LATER")))))
  )


(defun ian/config-org-capture ()
  ;; See documentation at https://orgmode.org/manual/Capture-templates.html
  (setq org-capture-templates
        (quote (("t" "todo in Journal" entry (file+datetree journal)
                 "* TODO %?\n%U\n%a\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("T" "todo to refile" entry (file refile)
                 "* TODO %?\n%U\n%a\n"
                 :clock-in t :clock-resume t)

                ("d" "Delegate a task" entry (file+datetree journal)
                 "* TASK @%\\2 %^{What}   \t:@%^{Who}:\n%U\n%l\n"
                 :clock-in t :clock-resume t :immediate-finish t :empty-lines 1)

                ("D" "A task that needs delegating later" entry (file+datetree journal)
                 "* TODO %^{What}   \t:@%^{Who}:\n%U\n%l\n"
                 :clock-in t :clock-resume t :immediate-finish t :empty-lines 1)

                ("n" "note" entry (file+datetree journal)
                 "* %? :NOTE:\n%U\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("n" "note" entry (file+datetree journal)
                 "* %? :NOTE:\n%U\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("j" "Journal" entry (file+datetree journal)
                 "* %?\n%U\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("m" "Meeting" entry (file+datetree journal)
                 "* MEETING %? :meeting:\n%U"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("e" "Email" entry (file+datetree journal)
                 "* EMAIL %? :email:\n%U"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("r" "Code Review" entry (file+datetree journal)
                 "* Code Review: %?  :code_review:\n%U\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("P" "Daily Planning" entry (file+datetree journal)
                 (file "~/.emacs.d.Ian/modes/org-capture-daily-planning.org")
                 :clock-in t :clock-resume t :empty-lines 1)

                ("W" "Weely Planning" entry (file+datetree journal)
                 (file "~/.emacs.d.Ian/modes/org-capture-weekly-planning.org")
                 :clock-in t :clock-resume t :empty-lines 1)

                ("R" "Daily Review" entry (file+datetree journal)
                 "* Review\n%U\n** What did I achieve today? :wdiat:\n*** %?\n** What did I learn today? :wdilt:\n*** \n** What do I need to do tomorrow?\n*** TODO \n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("s" "shoutout" entry (file+datetree journal)
                 "* @%? :shoutout:\n%U\n"
                 :clock-in t :clock-resume t :empty-lines 1)

                ("d" "respond" entry (file+datetree journal)
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
                 :clock-in t :clock-resume t :immediate-finish t :empty-lines 1)

                ("w" "org-protocol" entry (file refile)
                 "* TODO Review %c\n%U\n"
                 :immediate-finish t)

                ("h" "Habit" entry (file refile)
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
  )


(defun ian/config-org-babel ()

  (require 'ob-http)
  ;; (use-package ob-mermaid)

  (setq org-babel-sh-command "bash"
        org-confirm-babel-evaluate nil
        org-babel-default-header-args:dot '((:cmdline . "-Kdot -Tpng"))
        org-babel-default-header-args:sh '((:results . "verbatim drawer")))

  (setq org-src-tab-acts-natively nil)
  (setq ob-mermaid-cli-path "/Users/iantruslove/.nvm/versions/node/v18.17.0/bin/mmdc")

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((dot . t)
           (ditaa . t)
           (emacs-lisp . t)
           (http . t)  ;; See https://github.com/zweifisch/ob-http
           (js . t)
           ;;(mermaid . t)
           (org . t)
           (plantuml . t)
           (python . t)
           (shell . t)
           (sql . t))))
  )


(defun ian/config-org-crypt ()
  (require 'org-crypt)
  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)

  ;; Full-file encryption
  (require 'epa-file)
  (epa-file-enable)
  )


(defun ian/config-org-basics ()
  (setq org-superstar-headline-bullets-list '("◉" "▶" "✸" "●" "◆" "○" "▸" "•")
        org-hide-leading-stars t
        org-startup-indented t

        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil

        org-startup-folded 'fold
        org-clone-delete-id t
        org-enforce-todo-dependencies t

        org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto)))

        org-special-ctrl-k t
        org-special-ctrl-a/e t
        org-yank-adjusted-subtrees t

        org-log-into-drawer t

        org-use-speed-commands t

        ;; REFILE
        ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        ;; Use full outline paths for refile targets - we file directly with IDO
        org-refile-use-outline-path t
        ;; Targets complete directly with IDO
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes (quote confirm)
        org-refile-target-verify-function 'bh/verify-refile-target)

  (run-at-time "00:59" 600 'org-save-all-org-buffers))


(defun ian/config-org ()
  (require 'org)
  (with-eval-after-load 'org
    (add-to-list 'load-path (expand-file-name "~/.emacs.d.Ian/modes/"))
    (require 'org-lib)
    (require 'org-clocktable-by-tag)

    ;;(defvar organizer (concat org-directory "organizer.org"))
    (defvar journal (concat org-directory "/journal.org"))
    (defvar refile (concat org-directory "/refile.org"))

    (ian/config-org-basics)
    (ian/config-org-agenda)
    (ian/config-org-tags)
    (ian/config-org-tasks-todos)
    (ian/config-org-capture)
    (ian/config-org-babel)
    ))


(defun ian/config-highlight-symbol ()
  (setq ahs-idle-interval 1.0)
  (global-auto-highlight-symbol-mode t)
  (with-eval-after-load 'auto-highlight-symbol
    (global-set-key (kbd "M-p") #'spacemacs/quick-ahs-backward)
    (global-set-key (kbd "M-n") #'spacemacs/quick-ahs-forward)
    (global-set-key (kbd "M-S-p") #'ahs-backward-definition)
    (global-set-key (kbd "M-S-n") #'ahs-forward-definition))
  )


(defun ian/config-os ()
  (if (eq system-type 'darwin)
      (progn
        ;; gls is the coreutils ls, installed with `brew install coreutils`
        (setq insert-directory-program "gls")))

  (if window-system
      (scroll-bar-mode -1))

  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (global-set-key [mouse-4] (lambda ()
                                (interactive)
                                (scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
                                (interactive)
                                (scroll-up 1)))
    (setq mouse-sel-mode t))

  ;; Set modifier keys on mac
  (if (eq system-type 'darwin)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper))))


(defun ian/config-editor ()
  (setq scroll-conservatively 101
        scroll-margin 3
        scroll-preserve-screen-position 't)

  ;; Don't have undo-tree create all its messy tilde files:
  (setq undo-tree-auto-save-history nil)

  (setq js-indent-level 2
        typescript-indent-level 2
        web-mode-code-indent-offset 2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)

  (add-hook 'yaml-mode 'indent-guide-mode)

  (with-eval-after-load 'magit-mode
    ;;(global-set-key (kbd "C-c g") #'magit-status) ;; I should learn the spacemacs way
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
    (setq-default magit-process-popup-time 10
                  magit-diff-refine-hunk nil
                  magit-auto-revert-mode t
                  magit-bury-buffer-function 'magit-mode-quit-window
                  magit-display-buffer-function 'magit-display-buffer-traditional)
    )

  (with-eval-after-load 'lsp-mode
    (bind-key (kbd "M-.") 'lsp-find-definition lsp-mode-map))

  (ian/config-os)
  (ian/config-editor)
  (ian/config-smartparens)
  (ian/config-snippets)
  (ian/config-highlight-symbol)
  (ian/config-org)

  ;; Helm's minibuffer actions popup stopped working, with the error "cannot
  ;; split window or parent of side window". This is a workaround:
  (setq-default helm-display-function 'helm-default-display-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules"))
 '(org-agenda-files
   '("/Users/iantruslove/org/journal.org" "/Users/iantruslove/org/journal_2023.org" "/Users/iantruslove/org/journal_2022.org" "/Users/iantruslove/org/projects.org" "/Users/iantruslove/org/planning.org"))
 '(package-selected-packages
   '(composer php-runtime phpcbf phpunit ansible ansible-doc company-ansible jinja2-mode nord-theme blacken code-cells company-anaconda anaconda-mode cython-mode helm-cscope wfnames helm-pydoc importmagic epc ctable concurrent deferred live-py-mode lsp-pyright lsp-python-ms nose pip-requirements pipenv load-env-vars pippel poetry compat py-isort pydoc pyenv-mode pythonic pylookup pytest pyvenv sphinx-doc stickyfunc-enhance yapfify recompile-on-save sql-indent sqlup-mode csv-mode company-web web-completion-data counsel-css helm-css-scss pug-mode sass-mode haml-mode scss-mode slim-mode tagedit yaml-mode emmet-mode typescript-mode web-mode add-node-modules-path company counsel-gtags counsel swiper ivy dap-mode lsp-docker lsp-treemacs bui yaml lsp-mode markdown-mode ggtags helm-gtags impatient-mode htmlize import-js grizzl js-doc js2-refactor yasnippet multiple-cursors livid-mode nodejs-repl npm-mode prettier-js skewer-mode js2-mode simple-httpd tern web-beautify ws-butler writeroom-mode winum which-key volatile-highlights vim-powerline vi-tilde-fringe uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired toc-org term-cursor symon symbol-overlay string-inflection string-edit-at-point spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline-all-the-icons space-doc restart-emacs request rainbow-delimiters quickrun popwin pcre2el password-generator paradox overseer org-superstar open-junk-file nameless multi-line macrostep lorem-ipsum link-hint inspector info+ indent-guide hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org helm-mode-manager helm-make helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff dotenv-mode dired-quick-sort diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
)
