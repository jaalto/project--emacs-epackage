;; epackage.el --- Distributed Emacs Lisp Package System (DELPS)

;; This file is not part of Emacs

;; Copyright (C)    2009-2011 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto <jari.aalto@cante.net>
;; Maintainer:      Jari Aalto <jari.aalto@cante.net>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.

;; Depends:

;;      o   Emacs 22.1+ (released 2007). Designed only for Emacs.
;;          Note that XEmacs has its own packaging system (pui-*).
;;          http://www.gnu.org/software/emacs
;;      o   git(1) Distributed Version Control System (DVCS). Any version.
;;          http://en.wikipedia.org/wiki/Git_(software)
;;      o   Depends only on standard Emacs. Does not use cl.

;;; Install:

;;  Put this file along your Emacs-Lisp `load-path' and add following
;;  into your ~/.emacs startup file.
;;
;;      ;; If you're behing proxy, be sure to define connection
;;      ;; details before you start Emacs at command line.
;;      ;; Consult http://stackoverflow.com/questions/496277/git-error-fatal-unable-to-connect-a-socket-invalid-argument
;;      ;; for details. From bash shell:
;;      ;;
;;      ;;    export http_proxy=http://<username>:<password>@<proxy host>:<proxy port>
;;      ;;
;;      ;; Or, this may also be possible:
;;      ;;
;;      ;;    git config --global http.proxy http://<username>:<password>@<proxy host>:<proxy port>
;;
;;      ;; If you're behind firewall and Git port 9418 is blocked, you
;;      ;; want to use HTTP and translate addresses with this table:
;;      ;;
;;      ;; (setq epackage--sources-replace-table
;;      ;;       '(("git://github" "http://github")))
;;
;;      ;; -- If you want to customize any of the epackages, like BBDB,
;;      ;; -- do it *here*, before the next `load' command.
;;
;;      ;; One big file to boot all installed epackages
;;      ;; Automatically generated. Do not edit.
;;      (load "~/.emacs.d/epackage/00conf/epackage-loader" 'noerr)
;;
;;      ;;  M-x epackage to start the epackage manager
;;      (autoload 'epackage-manager "epackage" "" t)
;;
;;      (autoload 'epackage-loader-file-generate-boot   "epackage" "" t)
;;      (autoload 'epackage-cmd-autoload-package        "epackage" "" t)
;;      (autoload 'epackage-cmd-enable-package          "epackage" "" t)
;;      (autoload 'epackage-cmd-disable-package         "epackage" "" t)
;;      (autoload 'epackage-cmd-activate-package        "epackage" "" t)
;;      (autoload 'epackage-cmd-deactivate-package      "epackage" "" t)
;;      (autoload 'epackage-cmd-clean-package           "epackage" "" t)
;;      (autoload 'epackage-cmd-remove-package          "epackage" "" t)
;;      (autoload 'epackage-cmd-upgrade-package         "epackage" "" t)
;;      (autoload 'epackage-cmd-upgrade-all-packages    "epackage" "" t)
;;      (autoload 'epackage-cmd-sources-list-download   "epackage" "" t)
;;      (autoload 'epackage-cmd-download-package        "epackage" "" t)
;;      (autoload 'epackage-initialize                  "epackage" "" t)
;;      (autoload 'epackage-version                     "epackage" "" t)
;;      (autoload 'epackage-documentation               "epackage" "" t)
;;
;;      ;; .. Developer functions
;;      ;; Write initial templates from a single *.el
;;      (autoload 'epackage-devel-compose-package-dir    "epackage" "" t)
;;
;;  In addition to Emacs UI, there is also a minimal command line UI:
;;
;;      emacs --batch -Q -l /path/to/epackage.el -f epackage-ui
;;
;; WARNING: Make sure no *alias* override standard git commands in
;; your ~/.gitocnfig or they will create problems.

;;; Commentary:

;;  Preface 2009
;;
;;      NOTE: This extension is in early state; meaning that it is
;;      not in full use yet. The core elements are being planned,
;;      written and tested. Currently there is only the batch command
;;      line UI:
;;
;;          # Or run the provided Makefile: "make ui"
;;          emacs --batch -Q -l /path/to/epackage.el -f epackage-batch-ui-menu
;;
;;      Emacs has been around for decades now. Many new version have
;;      come and gone. And yet there are wealth of useful extensions
;;      available e.g. at <http://emacswiki.org> which enhance
;;      standard Emacs. The typical procedure to add a new extension
;;      to Emacs has been:
;;
;;      o   Find an extension at places like
;;          http://dir.gmane.org/gmane.emacs.sources or
;;          http://www.emacswiki.org
;;      o   Download and save *.el file(s) along `load-path'
;;      o   Read the installation information. Usually embedded in comments
;;          at the beginning of *.el file(s).
;;      o   Modify the Emacs startup file `~/.emacs'
;;          to arrange loading the extension to one's liking.
;;
;;      That's quite a bit of work for each extension; reaching
;;      thousands out there. Not to mention keeping up to date. Many
;;      Linux distributions offer package managers to download and
;;      install programs. E.g. Debian has command *apt-get/aptitude*
;;      [1], Redhat uses *rpm* [2], Suse uses *yast* [3]. So why not
;;      make one for Emacs as well.
;;
;;      The DELPS concept has been designed around two ideas: it
;;      borrows the Debian style package management and it uses
;;      version control for distributing packages.
;;
;;      Each Emacs extension is wrapped into a specific format which
;;      basically follows the Debian [4] packaging style where a
;;      separate control directory named `epackage/' is used for all
;;      the packaging details: activation, autoloads and installation
;;      etc. In addition, each package is imported in and deployed
;;      using Git Distributed Version Control System (DVCS). A
;;      specific *Sources* *List* file(s) list the available Git
;;      repositories from where users can download packages. Once a
;;      package has been downloaded, subsequent downloads are very
;;      efficient because, due to benefits of version control, only
;;      deltas are transferred.
;;
;;      If you're a Emacs user, all these details do not concern you.
;;      From `M-x' `epackage' management view you can the packages.
;;      There are several ways how to install: an *autoload* install
;;      (no Emacs setup changes), *enable* install (hooks, intractive
;;      commands), or *activation* install (the activation code can
;;      change Emacs environment). Later you can upgrade packages. To
;;      refresh list of packages, ask to "get" new *Sources* *List*
;;      that holds information about Git repositories.
;;
;;      If you're a developer who would like to make an extension
;;      available for others, you need to be familiar with the `Git'
;;      distributed version control system.
;;
;;      The epackage system can co-exist with any other packaging
;;      system like ELPA [4]. User's standard Emacs startup files,
;;      like `~/.emacs' are not modified with this system.
;;
;;      [1] http://en.wikipedia.org/wiki/Advanced_Packaging_Tool
;;
;;      [2] http://en.wikipedia.org/wiki/RPM_Package_Manager
;;
;;      [3] http://en.wikipedia.org/wiki/YaST See also
;;      http://en.wikipedia.org/wiki/Yellowdog_Updater,_Modified
;;
;;      [4] http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-debian-control
;;
;;      [5] http://www.emacswiki.org/emacs/ELPA
;;
;;  Epackage - the DVCS packaging format
;;
;;      The epackages are in the form of distributed[1] Git[2]
;;      version control repositories. The traditional packaging
;;      methods, like ELPA[3], rely on archives like *.tar.gz. In
;;      contrast, the DVCS approach offers interesting features over
;;      the traditional archive distribution approach:
;;
;;      o   Efficient downloads; fast, only deltas are transferred.
;;      o   Local modifications are possible; users can create their own
;;          customizations and track them easily,
;;      o   Helping package authors made easy; have you found an error?
;;          Have a spare time to fix it? Generate diff straight from the
;;          version control repository.
;;      o   Select any version; pick latest or
;;          downgrade to a older version with ease.
;;      o   Contains history of package in one place. No more scattered
;;          pieces around Internet.
;;      o   Encourages social collaboration; more easier interacting
;;          with the upstream e.g. through http://github.com
;;          push/pull.
;;
;;      Each extension is prepared for distribution as follows: an
;;      upstream code is imported into a Git repository, the epackage
;;      system is installed on top of upstream code in a separate
;;      directory, the whole git repository is made available online
;;      and information about the availability of new package is
;;      recorded to a separate *Sources* *List* file. The packaging
;;      work can be done by anyone who wants to set up a repository.
;;      It doesn't necesarily need to be done by the original Emacs
;;      extension author (upstream) who may not be familiar with the
;;      `Git' distributed version control system. For more
;;      information about the packaging, refer to section "The DELPS
;;      framework".
;;
;;      [1] DVCS = Distributed Version Control System
;;          http://en.wikipedia.org/wiki/Distributed_revision_control
;;
;;      [2] http://git-scm.org
;;
;;      [3] http://www.emacswiki.org/emacs/ELPA
;;
;;  User commands
;;
;;	[PLANNED: not yet implemented; Use makefile: "make ui"]
;;
;;      Command `M-x' `epackage' is alias for function
;;      `epackage-manager'. It builds buffer where packages can be
;;      browsed, fetched, built and installed. The view contains:
;;
;;          [mode indicators]
;;
;;          name section status v:VERSION package-description
;;          1    2       3      4         5
;;
;;      Mode indicators are:
;;
;;      o   compile - byte compile package on install phase.
;;      o   activate|enable - Auto-enable or activate on install phase
;;          See "status" for more explanation.
;;
;;      The fields are:
;;
;;      o   1 - Unique package name. No two package can have the same name.
;;      o   2 - Package classification. `M-x' `finder-list-keywords'
;;      o   3 - status: (A)activated (E)nabled (I)installed etc.
;;      o   4 - Version number. Only known once package has been downloaded.
;;      o   5 - Short package description
;;
;;      In this view, some of the commands are (see mode help `C-h' `m'):
;;
;;      o   _a_, Install activate configuration for package.
;;          modifies Emacs environment.
;;      o   _A_, Deactivate. Uninstall activate configuration for package.
;;      o   b, Generate boot loader.
;;      o   B, Byte compile boot loader.
;;      o   _c_, Clean package's configuration files (whole uninstall).
;;      o   _d_, Download package.
;;      o   D, run `dired' on package installation directory.
;;          as for new wish list features, report bugs etc.
;;      o   g, Get. Update package sources list.
;;      o   _e_, Enable standard configuration for package.
;;      o   _E_, Disable standard configuration for package.
;;      o   l<key>, (l)ist: available, installed, downloaded, enabled,
;;          activated, autoloaded and not-installed packages.
;;      o   m, mark package (for command install or remove).
;;      o   _o_, Install autoload configuration for package.
;;      o   _r_, Remove; delete package physically from local disk.
;;      o   s<key>, sort command. Change listing by several criterias.
;;      o   u, upgrade package to newer version.
;;      o   U, upgrade all packages
;;      o   v<key>, view command. E.g (a)activation file, (i)info file.
;;      o   q, quit. Run `bury-buffer'.
;;      o   x, execute marked (install, purge, remove).
;;
;;      Planned:
;;
;;      o   edit package's *info* file.
;;      o   email upstream to report a bug in the extension.
;;      o   email epackage maintainer to report a packaging bug.
;;          You can e.g. send a requests to update contents of the
;;          'info' file as needed.
;;
;;      Building the initial list of available packages takes some
;;      time at startup. The package state is shown with following
;;      status indicators:
;;
;;      o   *(A)ctivated*. The package has been downloaded and code to
;;          immediately activate the package is in use. This setting
;;          changes user's Emacs environment as defined by the
;;          packager. The changes typically include modifying hooks to
;;          activate the package e.g. by file extension, adding key
;;          bindings to access new commands etc. You might want to
;;          use (v)iew command to see what exactly happens.
;;      o   *(E)enabled*. One step down from Activated state. Interactive
;;          functions and variables are provided in latent `autoload'
;;          state for user to call with `M-x' <function name>. User
;;          configuration is not modified in any way. Some basic
;;          setup changes like modifying `auto-mode-alist' to activate
;;          modes for certain new file extensions may be provided.
;;      o   *(a)utoloaded*. The package has been downloaded and code to
;;          to provide autoloads to access package functions as been
;;          installed. User can call features with `M-x' <function name>.
;;          If you want full
;;          control over package setup, set package to autoload state
;;          and use `~/.emacs' Emacs startup file  to fully configure
;;          the extension.
;;      o   *(D)ownloaded*. Package has been fetched to local disk,
;;          but that is all. No setup whatsoever. Useful for complete
;;          control and DIY setups.
;;      o   (u)unmaintained. The package has been flagged as unmaintained.
;;      o   (b)uggy. The package has been flagged to have problems if used.
;;      o   (c)ompiled. Package has been byte compiled.
;;      o   (e)macs core. Package has been included in core Emacs.
;;      o   (x)emacs core. Package has been already included in core XEmacs.
;;
;;  About Configuration
;;
;;     Private repositories
;;
;;      Private installed package repositories, or other sources, can
;;      be defined in variable `epackage--sources-file-list'. The
;;      list of files included in there will be combined variable
;;      with `epackage--url-sources-list'. The order of the entries
;;      matter: the packages are read first-served basis. An example:
;;
;;          (setq epackage--sources-file-list  ;; This is the default
;;                '("~/.emacs.d/epackage-local.lst"))
;;
;;      Say the *epackage-local.lst* lists package =foo= and file
;;      pointed by `epackage--url-sources-list' also contains package
;;      =foo=. Because the files will be combined,
;;      *epackage-local.lst* will take precedence; its package =foo=
;;      will be used for download.
;;
;;     Automatic install of packages
;;
;;      The basic operation mode is to do one action at a time to give
;;      user a full control. In daily use it may be desireable to byte
;;      compile package after they have been downloaded. For that, use:
;;
;;          (require 'epackage)
;;          (add-to-list 'epackage--download-action-list 'compile)
;;
;;  The DELPS framework
;;
;;      Quick links for developers:
;;
;;      o   https://github.com/jaalto/project--emacs-epackage-sources-list
;;      o   https://github.com/jaalto/project--emacs-epackage-template
;;
;;      The DELPS system was inspired by the Debian packaging
;;      management. There are two primary actors: (1) the epackage
;;      maintainer and (2) the upstream. These two can be the
;;      same person or two separate persons. In the picture below:
;;
;;      o   _A_ = An Emacs user who wants to install new software
;;      o   (Y)ellow pages = The sources list file that contains
;;          information about available epackages around the globe.
;;      o   _E_ = The epackage. Maintained by a person who has found an
;;          interesting utility and wrapped it in epackage format. He
;;          is the maintainer of epackaged software. He keeps
;;          track of new releases and makes new epackages periodically
;;          available. If the initial packager looses interest,
;;          someone else can continue his work. He supplies the *URL*
;;          to the yellow pages to notify about availability of epackage.
;;      o   _U_ = Upstream. Person or team who wrote Emacs Lisp extension,
;;          the code or utility than enhances Emacs.
;;
;;      In order to find a package, the yellow pages is consulted. It
;;      is seeded and updated by the epackage maintainer that wish to
;;      make his work available. The user A does not need to know any
;;      details of this process; like in Debian, he installs an
;;      epackage and periodically asks for upgrades.
;;
;;      o   The location of Yellow Pages is fixed (%).
;;      o   The location of E's (epackage maintainer) and U's (upstream)
;;          can be anywhere (*).
;;      o   The E and U can be the same person (the upstream).
;;
;;                      %               *               *
;;          A           Y               E               U
;;          =============================================
;;          |           |               | keep eye on   |
;;          |  fetch    |               * ------------> |
;;          * --------> |               | <-----------  |
;;          | <-------- *               | epackage new  |
;;          |  upgrade  | add epackage  | releases      |
;;          |           | location      |               |
;;          |           | <------------ *               |
;;          |           |   (url)       |               |
;;          |                           |               |
;;          |    install "X"            |               |
;;          * ------------------------> |               |
;;          | <------------------------ |               |
;;          |   DVCS repo download      |               |
;;          |                           |               |
;;          |    upgrade "X"            |               |
;;          * ------------------------> |               |
;;          | <------------------------ *               |
;;          |   download DVCS "delta"   |               |
;;          |                           |               |
;;          |  report epackage bug      |               |
;;          * ------------------------> |               |
;;          |  report program bug       |               |
;;          * ----------------------------------------> |
;;          |                           |               |
;;          =============================================
;;
;;  Local directory layout
;;
;;      The packages are installed under root `epackage--root-directory',
;;      which defaults to `~/.emacs.d' respectively. The components below
;;      the root directory are organized as follows:
;;
;;          epackage/               Under epackage--root-directory
;;          |
;;          +-- 00coonf/
;;          |   epackage-loader.el     For user. One big boot file.
;;          |   epackage-load-path.el  Internal. Used during byte-compile.
;;          |   sources.lst            Internal. Package sources.
;;          |
;;          +-- 00install/         Extension "install" files
;;          |   *-<type>.el        autoloads, install, activate...
;;          |
;;          +--packages/           Git DVCS repositories
;;             |
;;             +-- 00sources/      Yellow pages: list of available packages
;;             +-- package/        Downloaded PACKAGE
;;             +-- ...
;;
;;  Epackage specification (draft; level 1)
;;
;;      The Git repository branches used are:
;;
;;      o   *master*, required. The published package.
;;          Branched off from *upstream*. Adds directory
;;          `epackage/' where the packaging information resides.
;;      o   *patches*, optional. Patches to *upstream* code, if any.
;;          This branch is merged to *master*.
;;      o   *upstream*, required. The original unmodified upstream code.
;;          Releases are tagged with label
;;          "upstream/YYYY-MM-DD[--VERSION]". The YYYY-MM-DD is the
;;          date of upstream release or best guess like if only year
;;          is known, use YYYY-01-01. The options part "--VERSION" is
;;          the official version of extension; if known. Not all
;;          extensions include version information. The ISO 8601 date is
;;          needed so that the release date is immediately
;;          available e.g. for post processing and so that the tags sort
;;          nicely by date. An example: `upstream/2009-12-31--0.3-devel'.
;;
;;      The same in pictures. The `master' contains merges from
;;      `patches' and `upstream' branches:
;;
;;          patches        o - o (modifications; merged to master)
;;                       /
;;          upstream    * ---- o
;;                       \      \ (merge)
;;          master        o ---- o - =>         contains epackage/ directory
;;
;;      The packaging method borrows concept from Debian where a
;;      separate control directory is used for packaging information.
;;      The control directory name `epackage/' is not configurable.
;;      The layout of an epackaged Emacs extension looks like following:
;;
;;          <PACKAGE, the Emacs extension root dir>
;;          | <upstream files and possible directories>
;;          |
;;          +- .git/                       Version control branches
;;          |
;;          +-- epackage/
;;              info                       required: The information file
;;              lisp		           optional: Location of Emacs Lisp files
;;              ignore                     optional: regexp to ignore Emacs Lisp files
;;              PACKAGE-epkg-0loaddefs.el  optional: extracted ###autoload statements
;;              PACKAGE-epkg-autoloads.el  optional: autoload statements (manual)
;;              PACKAGE-epkg-clean.el      optional: Code to run "make clean" equivalent
;;              PACKAGE-epkg-compile.el    optional: Code to byte compile the extension
;;              PACKAGE-epkg-configure.el  optional: Code to run ./configure
;;              PACKAGE-epkg-examples.el   optional: Customization examples
;;              PACKAGE-epkg-install.el    required: Code to make the extension available. Not required fo lib-* packages.
;;              PACKAGE-epkg-uninstall.el  optional: Code to remove the extension
;;              PACKAGE-epkg-xactivate.el  optional: Code to activate the extension
;;
;;      All these configuration files are combined in a single loader
;;      file. Loading a single file is faster than spending time in
;;      loading small file along `load-path'. The alphabetic order
;;      makes it possible to combine the install parts safely
;;      together:
;;
;;              ls |
;;              egrep -vi '00|clean|compile|configure|examples|uninstall' |
;;              xargs cat > PACKAGE-00-loader.el
;;
;;     The *-0loaddefs.el
;;
;;      This file contains extracted `##autoload' definitions. The file
;;      is usually automatically generated. The file does not modify
;;      user's environment. If extension does not contains any
;;      `###autoload' definitions, the manually crafted `*-install.el'
;;      file can be used as a substitute. In case of missing
;;      `##autoload' stanzas, you're encouraged to contact upstream
;;      with a possible patch. The "zero" at the start of the name is
;;      to help proper sorting ordering of files. Mnemonic: "if you
;;      load this file, you can start calling extension's features".
;;      The file ends in:
;;
;;          (provide 'PACKAGE-epkg-0loaddefs)
;;
;;     The *-autoloads.el
;;
;;      This file contains manually written `autoload' statements.
;;      This file acts as a backup if there is no `###autoload'
;;      definitions. Its purpose it to publish prospective functions
;;      (interactive or not) that might be called from programs or by
;;      the user. Mnemonic: "if you load this file, you can write lisp
;;      code to call the functions, or you can call extension's
;;      interactive functions via `M-x'". The file ends in:
;;
;;          (provide 'PACKAGE-epkg-autoloads)
;;
;;     The *-clean.el
;;
;;      This file contains command(s) to remove files that can be
;;      generated. This file is very rarely neeed. It may be useful with
;;      bigger packages that come with a `Makefile' or `./configure'
;;      script. Mnemonic: "Same as if you would run 'make clean'".
;;      Exception: the byte compiled files do not need deleting. They are
;;      deleted prior calling this file.
;;
;;     The *-compile.el
;;
;;      This file contains commands to byte compile the extension. The
;;      file is run at the root directory of the extension with
;;      `load-path' set to include all the relevant directories.
;;      Evaluating the file must byte compile all that is needed.
;;      Possible variables and functions defined here must have
;;      `PACKAGE-epkg-*' prefix to keep the Emacs name space clean.
;;      *Exception:* packages that only have a single "*.el" file do
;;      not need to define this file. There is no `provide' statement
;;      in this file. An example for a simple extension consisting of
;;      two files:
;;
;;          (dolist (file '("foo-lib.el" "foo.el"))
;;            (byte-compile-file file))
;;
;;     The *-configure.el
;;
;;      This file contains command to configure the extension's build
;;      system. This file is very rarely neeed. It may be useful with
;;      bigger packages that come with a `Makefile' or `./configure'
;;      script. Mnemonic: "Same as if you would invoke ./configure".
;;      This file is only necessary if the *.el files cannot be used
;;      "as is" to install the package. The `./configure' may e.g.
;;      write loaddefs or autoloads or assemble package in a way that
;;      produces an installable extension.
;;
;;     The *-examples.el
;;
;;      This file contains anything the upstream may have explained in
;;      the comments, or interesting snippets various users have found
;;      useful to customize the extension. It provides a showcase, or
;;      scratch book, to present anything that might be useful to be
;;      put into `~/.emacs' startup file. Mnemonic: "Look examples in
;;      this file for ideas how to make more out of the extension".
;;
;;      This file is not intended to be loadable and it must not
;;      contain any `provide' statements. All functions and private
;;      variables written must start with prefix `my-PACKAGE-* so
;;      that they can be easily be copied to user's own setup.
;;
;;      It is recommend that any attempt to load this file generates
;;      an error. Add something like this to the beginning of file to
;;      remind that it is user's responsibility to copy the relevant
;;      code:
;;
;;          ;; Prevent loading this file. Study the examples.
;;          (error "PACKAGE-epkg-examples.el is not a config file.")
;;
;;     The *-install.el (required; unless package name is lib-*)
;;
;;      This file publishes user variables and interactive `M-x'
;;      functions in *autoload* state. It may make conservative
;;      changes to Emacs environment: those of modifying
;;      `auto-mode-alist' or setting up hooks. The *-install* in name
;;      refers to standard installation, or availability for that
;;      matter, of interactive functions. *Note:* try to avoid
;;      `require' or `load' commands as much as possible. That helps
;;      keeping Emacs startup fast and lean. Mnemonic: "if you load
;;      this file, the extension is up and ready to be used in your
;;      Emacs. You can start calling extension's functions or load new
;;      files that activate the extension's features". The file ends
;;      in:
;;
;;          (provide 'PACKAGE-epkg-install)
;;
;;     Note: If package name starts with =lib-=, this file is not
;;     required. Libraries that are used for building other extensions
;;     are not requied to provide any install files. The autoloads
;;     publish available functions.
;;
;;     The *-uninstall.el
;;
;;      This file does the opposite of `*-install.el' and
;;      `*-activate.el' It runs commands to remove the extension as if
;;      it has never been loaded. Due to the nature of Emacs, it is
;;      not really practical to completely try to uninstall the
;;      package. The actual symbols (defined functions and variables)
;;      are not removed. The uninstallation usually covers undoing
;;      changes to *-hook, *-function and `auto-mode-alist' and to
;;      similar variables. To shake free from extension completely,
;;      restart Emacs after uninstall a epackage. The file ends in:
;;
;;          (provide 'PACKAGE-epkg-uninstall)
;;
;;     The *-xactivate.el
;;
;;      This file does the same as *-install.el, but it can do more.
;;      Instead of being conservative, it can modify current
;;      environment by adding more custom functions to hooks or
;;      arrange key bindings so that when pressed, a feature is loaded
;;      and activated. It may also loop through `buffer-list' to
;;      activate features in existing buffers immediately. It is best
;;      that any custom settings, like variables and prefix keys, are
;;      defined in `~/.emacs' *before* this file gets loaded. As with
;;      `*-install.el', try to avoid any `require' or `load' commands
;;      and stick to `autoload'. To keep Emacs namespace clean, name
;;      all custom variables or functions as `PACKAGE-epkg-*'.
;;      Mnemonic: "If you load this file, the bells and whistles are
;;      turned on". The "x" at the start of the name is to help proper
;;      sorting ordering of configuration files. The file layoyt:
;;
;;          ;; Description:
;;          ;; <explain in paragraph or two what is preconfigured>
;;
;;          <activation statements>
;;
;;          (provide 'PACKAGE-ekg-xactivate)
;;
;;  The 'ignore' file
;;
;;      List of Emacs regular expression entries on their own lines to
;;      ignore files in upstream package. The epackage-devel-*
;;      functions examine the packaging structure and can create files
;;      like `*-0autoloads.el'. If this file exists, it is read and
;;      files matches are ignored. The regexp(s) matches path relative
;;      to the package root directory. An example:
;;
;;          ;; Comments on their own lines start with a semicolon
;;	    one.el\|two.el
;;          tree.el
;;
;;  The 'lisp' file
;;
;;	This file contains Emacs Lisp file directories relative to the
;;	root of package. Empty lines and standalone comments on their
;;	own lines starting with semicolon(;) are ignored. Comments
;;	must not be placed at the directory lines. If all the Emacs
;;	Lisp files are in the package's root directory, this file not
;;	needed. The file is used internally to find out if the package
;;	has been byte compiled or not.
;;
;;  The 'info' file
;;
;;      A RFC 2822 (email) formatted file, which contains information
;;      about the extension. The header field names are not case
;;      insensitive; but if you use the default *get.sh*, it expects
;;      the Vcs-* field to be case-sensitive. Continued lines must be
;;      indented with only 1 space. Required fields are marked with
;;      asterisk (*). In the long description part, new paragraphs are
;;      separated by a single dot(.) character on their own line. The
;;      layout of the `info' somewhat mirrors concepts of `control' file in
;;      Debian packaging system which is explained in
;;      <http://www.debian.org/doc/debian-policy/ch-controlfields.html>.
;;
;;          *Package: <unique name, all lowercase>
;;          *Section: <data | extensions | files | languages | mail | tools | M-x finder-list-keywords>
;;          License: <GPL-[23]+ | BSD | Apache-2.0 | ... | Custom | None>
;;          Licence-Text: <only, if license is "Custom">
;;          *Depends: emacs (>= 20)
;;          Recommends:
;;          Status: [ <keyword> ...]
;;          Compat: [ <epackage version> ]
;;          *Maintainer: First Last <first.last@example.com>
;;          Bugs: [ URL ]
;;          *Upstream: First Last <first.last@example.com>
;;          Upstream-Bugs: [ URL ]
;;          Vcs-Type:
;;          Vcs-Url:
;;          Vcs-Browser:
;;          Vcs-Args:
;;          Vcs-User:
;;          Vcs-Password:
;;          Homepage:
;;          Wiki: http://www.emacswiki.org/emacs/<page name>
;;          X-<Field>-<Name>: [anything]
;;          Commentary: <single *.el file>
;;          *Description: <short one line>
;;           [<Longer description>]
;;           .
;;           [<Longer description, next paragraph>]
;;
;;      An example:
;;
;;          Package: test-package
;;          Section: extensions
;;          License: GPL-2+
;;          Depends: emacs (>= 21)
;;          Status: unmaintained
;;          Maintainer: Joe Average <joe@example.org>
;;          Upstream: John doe <jdoe@example.com>
;;          Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/test-package.el
;;          Homepage: http://example.com
;;          Wiki: http://www.emacswiki.org/emacs/TheTestPackage
;;          Commentary: test-package.el
;;          Description: test package with various functions
;;           Main command [C-u] M-x test-package runs various tests on
;;           the current lisp code. With a prefix argument, shows also
;;           notes and minor details.
;;           .
;;           Note: 2010-12-03 the code hasn't been touched since 2004.
;;
;;  Details of the info file fields
;;
;;      Notes: Use one space to indent a continued field. Limit
;;      maximum line length to 80 characters. In Emacs, see variable
;;      `fill-column' and set it to a little less, like 75. The *info*
;;      file must be saved as UTF-8 in case it contains non-ASCII
;;      characters.
;;
;;     Bugs
;;
;;      URL to report epackaging issues of current extension. The URL can
;;      be an email address or a link to an issue tracker. In case the
;;      field is empty or missing, the `Maintainer' field is used.
;;      Epackaging issues that are candidate for reporting: request to
;;      update to the newest upstream release, suggestions for updating
;;      *Description* or other fields, broken URLs in the *Description*
;;      or other fields etc.
;;
;;      See *Upstream-Bugs* or *Upstream* for reporting Emacs extension
;;      usage problems.
;;
;;     Commentary
;;
;;      This field contains a path, relative to epackage root
;;      directory, to a single Emacs Lisp file which contains
;;      documentation suitable for `M-x' `finder-commentary'. In order
;;      to find documentation, this field must exist even for
;;      packages that contain single Emacs Lisp file. Extension
;;      developers should study core Emacs *lisp-mnt.el* and function
;;      `lm-commentary'. The documentation read from file is enclosed
;;      in between tags:
;;
;;          ;;; Commentary:
;;
;;          ;;; Change Log:
;;
;;     Compat
;;
;;      The compatibility level used in the package. The format may
;;      change in time and this field indicates which layout was used. If
;;      the value is missing or is empty, the latest is assumed. Usually
;;      an epackage maintainer should follow the latest format to prevent
;;      installation problems. See section "Epackage Compatibility
;;      Levels" for more information.
;;
;;      Note: yhis field should be left empty. It is part of the
;;      specification, but hopefully never needed.
;;
;;     Conflicts
;;
;;      List of packages that must be removed before install can be
;;      done. This field follows the guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;     Description (required)
;;
;;      The first line of this field is a concise description that
;;      fits on maximum line length of 80 characters; word
;;      "Description: " included. The long description should explain
;;      the essential M-x commands to use the package. The details of
;;      the extension are explained in the following paragraphs which
;;      are separated from each other with a single dot(.) on their
;;      own lines. The paragraphs are indented by one space.
;;
;;      Guidelines:
;;
;;	o   For modes, start short description with
;;          "major mode for ..." or "minor mode for..."
;;      o   For libraries, start short description with
;;          "library of ..."
;;      o   For client communication, start short description with
;;          "client for ..."
;;      o   If extension is related to a specific operating system,
;;          add this information in to the first line. E.g. add word
;;          "Cygwin" or "(ms)" to mark "Microsoft" OS. The details of
;;          Operating system integration can be explained in the long
;;          description part.
;;      o   Don't use word "Emacs" on the first line. It's redundant.
;;
;;      Examples:
;;
;;          Package: cygwin-mount
;;	    Description: Add Cygwin mount point support (ms)
;;
;;          Package: lib-xml-rpm
;;	    Description: library of remote procedure calls alls over HTTP
;;
;;     Depends (required)
;;
;;      List of dependencies in all lowercase: Emacs flavor and
;;      external packages required. Listing packages that are included
;;      in core Emacs would be unnecessary and slow down parsing. The
;;      Emacs flavor can have an optional version information enclosed
;;      in parenthesis using comparison operators ">=", "<=" and
;;      logical "!". A between range is not defined. The logical *or*
;;      operator works only between Emacs flavors and is indicated
;;      with vertical bar "|".
;;
;;      In case an extension works only in imited versions of Emacs,
;;      this information should be written to the end of `Description'
;;      (which see). Old packages that are not updated to work for
;;      latest Emacs releases are candidate for removal from the
;;      official Epackage Sources List. Examples:
;;
;;          Depends: foo
;;          Depends: emacs (>= 22.2) | xemacs (>= 20), foo
;;
;;      To mark that package does not work in XEmacs, use "!". The
;;      version parameter is ignored with logical *not* but
;;      the parenthesis are still required:
;;
;;          Depends: emacs (>= 22.2), xemacs (!), foo
;;
;;      _Limitations_: The *vertical* *bar*, OR-operator(|), is not
;;      really used. It is only respected on the Emacs flavor part.
;;      Using OR-operator anywhere else causes treating the elments as
;;      if written "exension | extension" => "extension, extension".
;;
;;      The *version* *information* is a no-op anewhere else than
;;      Emacs flavor check. This kind of fine grained package
;;      dependencies has never been in use with Emacs Lisp extensions.
;;      There is no support for version numbers in Emacs Lisp commands
;;      `provide', `require', `load', `load-file' and `load-library'.
;;      Extensions typically check the available features with
;;      `boundp' and `fboundp' to see if they have the required
;;      environment. So don't write:
;;
;;          Depends: emacs (>= 22.2), xemacs (!), foo (>= 0.9)
;;                                                    |
;;                               Ignored. Has no effect.
;;
;;      See also section "Development notes: depends".
;;
;;     Homepage
;;
;;      URL to the project's homepage. It is recommended to use
;;      addresses that don't move; those of http://Freecode.com,
;;      http://www.Sourceforge.com, http://Launchpad.net,
;;      http://Github.com, http://Bitbucket.com etc. The Freecode is
;;      especially good because it provides project information in
;;      coherent manner. Through Freecode it is also possible to
;;      browse related software and subscribe to project
;;      announcements. Freecode is also easy for the upstream
;;      developers to set up because it does not require heavy project
;;      management; only links.
;;
;;      In any case, the homepage URL should not directly point to the
;;      developer's volatile personal homepage if there are
;;      alternatives. It is good idea to encourage "garage" upstream
;;      developers to set up their software at some project hosting
;;      site which encourage collaboration and provide infrastructure
;;      e.g. for issue tracking. For more information, see
;;      <http://en.wikipedia.org/wiki/Comparison_of_open_source_software_hosting_facilities>.
;;
;;     License
;;
;;      The valid License abbreviations should follow list defined at
;;      <http://dep.debian.net/deps/dep5/#license-specification> and
;;      <http://spdx.org/licenses/>. "License specification / Short
;;      name". A special word "None" should be used if the software
;;      has no license information in any of the source files.
;;      Examples of valid license tokens:
;;
;;          GPL-2, GPL-2+, GPL-3, GPL-3+, BSD-2-clause, Apache-2.0
;;
;;      If License is not any of the OSI known standard licenses
;;      <http://www.opensource.org/licenses>, or if it contains
;;      additional text to an existing licence, it must be labelled
;;      "Custom". See field *License-Text*.
;;
;;      In case the software is dual licenced, or there is different
;;      license for documentation, it would be good to explain these
;;      in additial extra field:
;;
;;          X-License-Comment: code GPL-2+, documentation GFDL
;;
;;     License-Text
;;
;;      In case "License: Custom" the full license text should be
;;      included here. In case the License is known to The Linux
;;      Foundation SPDX database, URL to that database is enough. Do not
;;      point to any other web page, as these may be changed, removed or
;;      updated without a notice. An example:
;;
;;          License: Custom
;;          License-Text: http://spdx.org/licenses/Ruby
;;          X-License-Comment: Ruby license is dual: GPL or custom text
;;
;;     Maintainer (required)
;;
;;      The epackage maintainer. Contains the name and address of the
;;      person who maintains ad hosts the epackage Git repository. If the
;;      upstream is also the epackage maintainer, the content of this
;;      field is identical to *Upstream* field.
;;
;;     Package (required)
;;
;;      The name of the package in all lowercase satisfying regexp
;;      "[a-z][a-z0-9-]+". Usually base name of the extension file or
;;      the canonical known name in case of bigger packages like
;;      "gnus". An example "html-helper-mode.el" => package name is
;;      "html-helper-mode". No two packages can have the same name.
;;      Please notify upstream if about package name problems.
;;
;;      Guidelines:
;;
;;      o   Length of package name is not limited.
;;      o   Add `*-mode', In case of minor or major modes. Always
;;          add this suffix even if extension name does not
;;          explicitly say so. An example "python.el" => name package
;;          "python-mode". This helps searching for package names.
;;      o   In extension is ment to be a library (e.g. xml-rpc.el), start
;;          package name with `lib-*'. This way user
;;          who is browsing the list of packages can ignore or complete
;;          these on minibuffer prompts easily.
;;
;;      Note: There may be exotically named extensions like "crypt++",
;;      but the *epackage* name must not contains special characters. Use
;;      name "crypt-plusplus" if nothing else comes to a mind. Consider
;;      contacting upstream to discuss about possible name change.
;;
;;     Recommends
;;
;;      List of packages which the extension can support or take
;;      advantage of. E.g. this field would list package B if A can
;;      take advantage of package B. However it is not a requirement
;;      to install B for package A to work. This field is must *not*
;;      be used to announce related packages. That information can be
;;      mentioned in a separarate paragraph like "SEE ALSO" in the end
;;      of *Description* field or in file `*-examples.el'. The
;;      *Recommends* field follows guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-binarydeps>
;;
;;     Section (required)
;;
;;      This field contains category keyword. The valid keywords are
;;      those listed in `M-x' `finder-list-keywords'.
;;
;;     Status
;;
;;      This field lists succinct information about the package. Each
;;      keyword has a unique meaning. The allowed list is:
;;
;;          keyword := core-emacs[-NN.N]
;;                     | core-xemacs[-NN.N]
;;                     | unmaintained
;;                     | broken
;;                     | unsafe
;;                     | stable
;;                     | unstable
;;                     | experimental
;;
;;      The `core-*' values mark the extension or its features being
;;      included (or will be) in the mentioned [X]Emacs. The optional
;;      NN.N announces in which Emacs flavor the feature was included;
;;      e.g. *core-emacs-22.1*. Value `unmaintained' means that the
;;      original developer has vanished or abandoned the project and
;;      is no longer available for contacting or further development.
;;      Value `unsafe' means that the not all the symbols are name
;;      space clean (prefix-*); meaning that some of the commands
;;      might clash with existing function in Emacs. The current
;;      release status of package can be indicated with terms `stable'
;;      (no more actively developed, bugs shaken out), `unstable'
;;      (package is in active development) or `experimental' (no
;;      guarantees, not necessarily tested, this is the latest code).
;;      Value `broken' means that there are known problems,
;;      limitations or that the package may not work in some Emacs
;;      version. Further information about "brokeness" should be
;;      supplied in the end of *Description:* field in section "BUGS"
;;      or similar.
;;
;;     Upstream
;;
;;      The upstream developer's name and email address. Multiple
;;      developers or alternative addresses are separated by commas,
;;      just like in email. The role can be expressed in RFC 2822
;;      comment-parenthesis. An example:
;;
;;          Upstream: John Doe (Author) <jdoe@example.com>,
;;           Joe Average (Co-developer) <jave@example.com>
;;
;;      Note: Don't just copy the addresses from source code. It is of no
;;      use for anybody if the address is no longer valid. Contact
;;      upstream to verify the address before making epackage available:
;;
;;          To: upstream@example.com
;;          Subject: Emacs package.el
;;
;;          Your software is bundled with the Distributed Emacs Lisp
;;          Package System called epackage. Would you verify that this
;;          address is still your preferred contact. Simply reply
;;          shortly "ok" or let me know if you prefer to use another
;;          address.
;;
;;	    Thanks,
;;          John Doe
;;
;;     Upstream-Bugs
;;
;;      URL to report issues of current extension. The URL can be an
;;      email address(es) or a link to an issue tracker. In case the field
;;      is empty or missing, the `Upstream' field is used.
;;
;;     Vcs-Browser
;;
;;      The URL address to the version control browser of the upstream
;;      repository. This field follows the guidelines of
;;      <http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-vcs>
;;
;;     Vcs-Type
;;
;;      Version Control System type information of *Vcs-Browser*. The
;;      value is the lowercase name of a version control program; cvs,
;;      svn, bzr, hg, git etc. A special value "http" can be used to
;;      signify direct HTTP download. This field follows the guidelines of
;;      <http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-vcs>.
;;      An example of an Emacs extension hosted directly at a web
;;      page:
;;
;;          Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/vline.el
;;
;;     Vcs-Url
;;
;;      The Version Control System repository URL without any options.
;;      For CVS, this is the value of `CVSROOT' which contains the
;;      protocol name. This field follows the guidelines of
;;      <http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-vcs>.
;;      An example:
;;
;;          Vcs-Type: cvs
;;          Vcs-Url: :pserver:anonymous@example.com/reository/foo
;;
;;     Vcs-Args
;;
;;      Additional arguments passed to VCS program after specifying
;;      the *Vcs-Url* E.g. CVS directories may need a specific module
;;      to check out. A setup like below would yield command: "cvs -d
;;      <Vcs-Url> co -d upstream <Vcs-Args>"
;;
;;          Vcs-Type: cvs
;;          Vcs-Url: :pserver:anonymous@example.com/reository/foo
;;          Vcs-Args: module
;;
;;     Vcs-User
;;
;;      Login name used to access The Version Control System
;;      repository. In case the repository cannot be accessed simply
;;      by visiting the `Vcs-Url' (or in the case of CVS: pressing
;;      RETURN at login prompt), this is the used login name;
;;      typically `anonymous' or the like.
;;
;;     Vcs-Password
;;
;;      Password for the Version Control System repository. In some
;;      extremely rare cases a generic password, like "guest" to
;;      access repository, may be needed.
;;
;;     Wiki
;;
;;      This field points to extension page (or page that talks about
;;      it) at <http://www.emacswiki.org>. If the extension does not
;;      yet have a page, encourage upstream to create one.
;;
;;     X-*
;;
;;      Any other custom fields can be inserted by using the `X-*' field
;;      notation. Examples:
;;
;;          X-Comment: This comment.
;;          X-Upstream-Homepage: URL
;;
;;      If converting extension to a epackage needs special treatment
;;      please document those in field like:
;;
;;              X-Packaging:
;;               Before BBDB can be used, the autoloads file must be generated.
;;               Run command:
;;               .
;;                  ./configure && make autoloads
;;               .
;;               And it will generate file lisp/bbdb-autoloads.el
;;
;; Epackage compatibility levels
;;
;;      The latest epackage format is always described in section
;;      "Epackage specification" above. Below you can find list of
;;      formats and changes.
;;
;;      o   2011-12-03 Draft spec. Compatibility level 1.
;;
;;
;; Batch command line interface
;;
;;      Several FUNCTIONS can be accessed from command line in a
;;      manner of:
;;
;;          emacs --batch -Q -l /path/to/epackage.el -f FUNCTION
;;
;;      The functions and their command line arguments are:
;;
;;          ;; Interactive, menu driven
;;          epackage-batch-ui-menu
;;
;;          epackage-batch-ui-upgrade-all-packages
;;          epackage-batch-upgrade-package PACKAGE ...
;;          epackage-batch-download-package PACKAGE ...
;;          epackage-batch-remove-package PACKAGE ...
;;          epackage-batch-clean-package PACKAGE ...
;;          epackage-batch-activate-package PACKAGE ...
;;          epackage-batch-deactivate-package PACKAGE ...
;;          epackage-batch-enable-package PACKAGE ...
;;          epackage-batch-disable-package PACKAGE ...
;;          epackage-batch-ui-list-installed-packages
;;          epackage-batch-ui-list-not-installed-packages
;;          epackage-batch-ui-list-downloaded-packages
;;          epackage-batch-ui-loader-file-generate
;;          epackage-batch-ui-loader-file-byte-compile
;;
;;          ;; This command upgrades the sources list
;;          epackage-batch-ui-sources-list-upgrade
;;
;; Development notes
;;
;;     XEmacs
;;
;;      This extension was written in Emacs 23, but it may work in
;;      Emacs 22 (2007) although that has not been tested. No support
;;      for older Emacs versions is on the chart. Real life, daily
;;      work and my own other Open Source projects take their share. I
;;      have to regret that I will not be having resources to port or
;;      support this utility to XEmacs. Please send patches if you
;;      take the code to ride in XEmacs. Fortunately XEmacs already
;;      includes its own package manager: see the `pui-*' commands.
;;
;;     Depends
;;
;;      The *OR-operator(|)*, is not really implemented. The packages
;;      "emacs" and "xemacs" are treated specifically and the effect
;;      of "|" is actually the same as if it were written with comma:
;;
;;          Depends: emacs (>= 22) | xemacs (> 21.3)
;;          Depends: emacs (>= 22), xemacs (> 21.3)
;;
;;      Writing an algorithm for package depends clause that would
;;      understand variety of operations (>=, <=, !, |) is
;;      challenging. Take for examples the Debian package depends
;;      guidelines described at
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>
;;      which was the source of inspiration for the used syntax. The
;;      Debian packaging system is centralized, so it has the
;;      knowledge about all the available packages and their version
;;      numbers. In Debian, then commands can build the full
;;      dependency list and check if install is even possible. In
;;      contrast, the epackage sources list refers to distributed
;;      locations. The available versions or further depends
;;      information can only be determined only after the package has
;;      been downloaded by reading the "Depends:" field. Because of
;;      this, the distributed system:
;;
;;      o   Cannot know beforehand what epackages would be required for X
;;      o   Cannot know beforehand if it is possible to even install
;;          package fully to satisfy all depends.
;;      o   Cannot ask to install a specific version because the
;;          version information is only available *after* the package
;;          has been downloaded from the git tags.
;;      o   Cannot easily know en masse to which packages updates
;;          would be available. Because there is no central place to
;;          read, each repository would need to be checked separately
;;          (network perfomance penalty).
;;
;;      In daily use these issues don't much matter. If package X
;;      requires Y, the Y will be downloaded. If Y further requires Z,
;;      the Z will be downloaded etc. Somewhere in the chain the
;;      downloads stops. It is just that no progress indicator can be
;;      presented to tell how many more packages there is to load.
;;      Most of the Emacs Lisp extensions are self standing and have
;;      no external dependencies; contrast to Linux software that have
;;      huge library dependencies.
;;
;;      Regarding the requirement for a specific version of the package
;;      in form of:
;;
;;          Depends: foo (>= 0.9)
;;                       |
;;                       No-op. Will not be used.
;;
;;      Emacs extensions have never had any Perl like "use PACKAGE
;;      VERSION" statements, thus there is not much point of
;;      implementing this the `epackage.el'. The syntax is there in
;;      case somewhere in the future Emacs modifies the `require' and
;;      relevant `load' calls to accept optional version argument. For
;;      now, as it has always been, the extension developers ensure
;;      that the extensions work together with the help of tests like
;;      `boundp', `fboundp' and `featurep'. If an extension breaks due
;;      to change in some other extension, it is best to notify the
;;      original developer and get the code updated. Compatibility
;;      problems between extensions are usually temporary. In case the
;;      upstream developer is no longer there to to fix things, the
;;      extension is best to be left forgotten and removed from
;;      epackages Yellow Pages. Or, if you have the time and skills,
;;      you can start maintaining an old extension to bring it new
;;      life and becoming the new upstream.
;;
;;     Depends and removing packages
;;
;;      The depends system was added to *ease* *installing* of
;;      packages. But we can't have one without touching the other
;;      issues: what if package is removed? Say package A requires
;;      both B and C. Currently user has total control and can remove
;;      package C and make A non-working. Nothing prevents removing or
;;      disabling packages as one wishes. In order to do the removals
;;      in a safe fashion, the dependency graphs of all packages would
;;      need to be collected and maintained.
;;
;;      Currently there are no removal dependecy checks of any kind.
;;
;;     Version
;;
;;      Why is there no "Version:" field in the `info' file that
;;      would announce the extension version? The Git repository is
;;      supposed to have tags for all upstream versions of the
;;      package. It would be duplicate and manual work to keep the
;;      info::Version field in synch with the tags of Git repository.
;;      The user is 90% interested in downloading an extension, not
;;      really some specific versions of the extension. This is a
;;      little different than in Linux distributions, where one can
;;      upgrade from one version to a newer version. In Epackage, one
;;      always updates full Git repository, thus bringing all
;;      possible versions of the extension available; and activating
;;      the latest.
;;
;;      To select old versions, user must work on the git repos
;;      directly. There are no plans to support selecting previous
;;      versions because that would bring instability to the whole
;;      system. Imagine this: A depends on B, but user selects
;;      specific version of B - which is older and won't work with
;;      other packages. Multiply this problem with N versions of
;;      several extensions. Summary: it's best to stick with the
;;      latest and send bug reports to upstream on errors.
;;
;; TODO
;;
;;      [Within groups, sorted by priority]
;;
;;      General
;;
;;      o   Sources List: Download problem, broken repository link.
;;          => Offer sending error report mail the Sources List maintainer
;;
;;      o   Boot: What if user manually deletes directories?
;;          What to do with left over config files? Do we need self health
;;          check on boot?
;;
;;      o   Sources List: If package A is no longer in there, but it is
;;          downloaded, the package has been removed. Notify user about
;;          obsolete package. Package may also have been renamed.
;;
;;      o   [already implemented?] After download. Trying to install or
;;          activate package, check emacs compatibility and refuse to
;;          install if not met.
;;
;;      REPO
;;
;;      o   Check validity of "git tag -l" and upstream/* against the
;;          specification. Two dashes etc.
;;
;;      o   Better Fetch, pull conflict notifications. Now a Git error.
;;
;;      o   What if epackage maintainer kills the repo and re-instantiates it
;;          from fresh? Symptoms: can't pull, because repos have diverged and
;;          do not have common objects. SOLUTION: offer deleting repo and
;;          downloading it again. Warn if there are any local modifications,
;;          the user might want to have a backup (*.b). Can we do that? What
;;          if a backup already exists?
;;
;;      o   Git tags (versions of packages), where is this information kept?
;;          Cache? Affects GUI.
;;
;;      o   New updates available? Git polling mechanism with idle timers?
;;
;;      GUI
;;
;;      o   Write M-x epackage-manager (the real interface).
;;      o   Cache. Build it dynamically from packages and
;;          combine with package information (e.g. version).
;;
;;      o   If user selects DETAIL view, collect
;;          information to another buffer dynamically (info, git tags,
;;          current git branch)
;;
;;      o   Rescan current information? (what is installed, what is not)
;;          => Keep cache? Or regenerate, or scan at startup every time?
;;
;;      Extensions
;;
;;      o   Big packages that come with configure? What to do with them?
;;
;;      Some day in the future:
;;
;;      o   Do something for *.texinfo files in big packages.
;;      o   Verify Compatibility Level of downloaded epackage
;;      o   Handle Conflicts field
;;
;;      o   Edit yellow pages catalog?
;;          => Submit/update yellow pages catalog changes?
;;          => version controlled, patches? Interface to automatic email?
;;
;;      o   The epackage/*-compile.el is run with `eval-current-buffer'.
;;          What about security considerations? Is there any need, because
;;          these are Git repositories and maintainers should be trusted
;;          => possible solution: require detached GPG signing of *-compile.el
;;
;;      o   Package removal: present some analysis command to show what
;;          would happen if package X would be removed. Are other packages
;;          depending on X or can it be removed safely?

;;; Change Log:

;;; Code:

(eval-when-compile
  ;; Forward declarations, not really variable definitions.
  (defvar auto-revert-mode)
  (defvar auto-revert-tail-mode)
  (defvar finder-known-keywords)
  (defvar global-auto-revert-mode)

  (defvar elint-builtin-variables)
  (defvar elint-log-buffer)

  (autoload 'lm-copyright-mark "lisp-mnt")
  (autoload 'lm-commentary-mark "lisp-mnt")
  (autoload 'lm-commentary-mark "lisp-mnt")
  (autoload 'lm-header "lisp-mnt")
  (autoload 'lm-history-mark "lisp-mnt")
  (autoload 'lm-verify "lisp-mnt")
  (autoload 'lm-get-package-name "lisp-mnt")
  (autoload 'lm-summary "lisp-mnt")
  (autoload 'lm-version "lisp-mnt")
  (autoload 'lm-summary "lisp-mnt")
  (autoload 'lm-commentary "lisp-mnt")
  (autoload 'lm-creation-date "lisp-mnt")
  (autoload 'lm-last-modified-date "lisp-mnt")
  (autoload 'lm-maintainer "lisp-mnt")

  (defvar checkdoc-diagnostic-buffer)
  (autoload 'checkdoc-continue "checkdoc")
  (autoload 'checkdoc-start-section "checkdoc")

  (autoload 'dired-make-relative-symlink "dired-x")
  (autoload 'generate-file-autoloads "autoload")
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'mail-setup "sendmail")

  (defvar pcomplete-default-completion-function)
  (defvar pcomplete-parse-arguments-function)
  (autoload 'pcomplete-here "pcomplete" nil 'macro)

  (autoload 'url-http-parse-response "url")

  (defvar whitespace-style)
  (autoload 'whitespace-replace-action "whitespace"))

;;;###autoload
(autoload 'mail-fetch-field "mail-utils")

(eval-and-compile
  (if (featurep 'xemacs)
      (message
       "** WARNING: epacakge.el has not been designed to work with XEmacs")))

(defconst epackage--version-time "2011.1223.0910"
  "Package's version number in format YYYY.MMDD.HHMM.")

(defconst epackage--maintainer "jari.aalto@cante.net"
  "Maintainer's email address.")

(defconst epackage--url-doc-emacswiki "http://www.emacswiki.org/emacs/DELPS"
  "URL to documentation: Emacswiki")

(eval-and-compile                       ;We need this at runtim
  (defconst epackage-w32-p
    (or (memq system-type '(ms-dos windows-nt))
        (memq window-system '(win32 w32 mswindows)))
    "Non-nil under Windows, DOS operating system."))

(defgroup epackage nil
  "Implementation of Distributed Emacs Lisp package system (DELPS)."
  ;;  :link '(function-link view-mode)
  ;;  :link '(custom-manual "(emacs)Misc File Ops")
  :group 'tools)

;;; ................................................ &variables-custom ...

;;;###autoload
(defcustom epackage--install-action-list '(enable boot)
  "*TYPE of actions to run after package is installed.
Default value is: '(enable boot)

What to do after package has been downloaded and user selects
install of one of types: autoload, enabel, activate. Possible
values are:

    boot        Generate boot loader file
    enable      Enable customizations immediately in currently running Emacs

The `boot' means updating `epackage--loader-file-boot' after every install
action. The `enable' means evaluating the configuration files immediately
for currently running Emacs; i.e. to take the extension into use."
  :type  '(choice
           (const enable)
           (const boot)
           (const nil))
  :group 'epackage)

;;;###autoload
(defcustom epackage--download-action-list '(enable package-depends)
  "*TYPE of actions to run after package download.
Default value is: '(enable package-depeds)

The order of TYPEs in list is not significant. The \"install\"
TYPE can be of of following. For more information about install
TYPEs, refer to \\[epackage-documentation].

    activate     ;; Will also turn on autoload
    autoload
    enable       ;; Will also turn on autoload
    compile

To install also dependant packages, add:

    package-depeds

To check package validity and colloct information in
`epackage--buffer-lint', add:

    lint

An example. The following would automatically compile and enable
package after download and fetch all depends:

  '(compile enable lint package-depends).

Note: the symbol names have been named so that when sorted, the
actions can be run safely in order.

See also variable `epackage--depends-handling'."
  :type  '(list symbol)  ;; FIXME, list names of symbols
  :group 'epackage)

;;;###autoload
(defcustom epackage--depends-handling 'warn
  "*How to treat package depends. The default is 'warn.
Possible values:

    'warn       Warn about unsatisfied depends and proceed with install.

    'error      Signal error on unsatisfied depends. Refuse to install.

    nil         Do nothing. Bypass depends checks. Useful if you want
                to mass download many packages as the code to check
                depends would be quite time consuming.

This variable has no effect without `epackage--download-action-list'
which see."
  :type  '(choice
           (const warn)
           (const error)
           (const nil))
  :group 'epackage)

(defcustom epackage--sources-list-and-repository-sync-flag t
  "*Non-nil means to recreate any changed repositories.
When this variable is non-nil, whenever function
`epackage-cmd-sources-list-download' is called, all the URLs in
the list are matched against the git 'origin' URLs in respective
downloaded repositories. If package's sources list URL differ
from the repository on disk, the package will be deleted and
downloaded again to keep it in synch.

It's like this: the URLs point to different locations:

    sources list        ---> A
    (yellow pages)

    downloaded          ---> B
    package

After the synchronization they point to the same loction:

    sources list          -+-> A
    (yellow pages)         |
                           |
    delete/re-downloaded  -+
    package

When non-nil, this verification takes place after every sources list update."
  :type  'boolean
  :group 'epackage)

(defcustom epackage--loader-file-byte-compile-flag t
  "*Non-nil means to byte compile `epackage--loader-file-boot'.
If non-nil, after calling `epackage-loader-file-generate-boot', the file
returned by `epackage-file-name-loader-file' is byte compiled."
  :type  'boolean
  :group 'epackage)

(defvar epackage--sources-list-url
  "git://github.com/jaalto/project--emacs-epackage-sources-list.git"
  "URL to the location of the official Git package list.
This is the Git repository that contains the canonical list of
available packages.

The file contains information about package names and their
repository download URLs. Empty lines and comment on their own
lines started with character '#' are ignored. There must be no
leading whitespaces in front of PACKAGE-NAME.

  # Comment
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  ...

An example:

  foo git://example.com/repository/foo.git

This list is combined with additional sources list in
variable `epackage--sources-file-list'.")

(defcustom epackage--url-epkg-template
  "git://github.com/jaalto/project--emacs-epackage-template.git"
  "URL to the location of the epackage template repository.
This is the Git repository that contains the canonical ttemplate files
to be used when creating new epackages. Needed by the developers."
  :type  'string
  :group 'epackage)

(defcustom epackage--sources-file-list
  '("~/.emacs.d/epackage-local.lst")
  "*List of files that are in the form of `epackage--sources-list-url'.
In here you can list additional package repositories. Non-existing files
will be ignored. Default is:

  '(\"~/.emacs.d/epackage-local.lst\")

The files listed will be combined before `epackage--sources-list-url'
into a the main package sources list file whose path is returned
by function `epackage-file-name-sources-list-main'."
  :type  '(list string)
  :group 'epackage)

;;;###autoload
(defcustom epackage--sources-replace-table
  (if (memq system-type '(windows-nt ms-dos))
      '(("git://github" "http://github")))
"Replace each found REGEXP with STRING in sources list.

Format:
  '((regexp string [regexp submatch] ...))

Use case:

  If you're behind a firewall that blocks git port, change all
  git:// protocols to http:// to access the repositories.

  ;; Use this
  (setq epackage--sources-replace-table
        '((\"git://github\" \"http://github\")))

for more in depth manipulation, see  `epackage--build-sources-list-hook'."
  :type  '(repeat
	   (list
	    (string :tag "Regexp")
	    (string :tag "Replace")
	    (choice (const nil) (integer :tag "match level"))))
  :group 'epackage)

;;; ................................................. &variables-hooks ...

(defcustom epackage--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'epackage)

(defcustom epackage--initialize-hook nil
  "*Hook run after function `epackage-initialize' is run."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-autoload-hook nil
  "*Hook run when epackage is autoloaded."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-enable-hook nil
  "*Hook run when epackage is enabeled."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-disable-hook nil
  "*Hook run when epackage is disabled."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-activate-hook nil
  "*Hook run when epackage is activated."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-deactivate-hook nil
  "*Hook run when epackage is Deactivated."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-clean-hook nil
  "*Hook run when epackage is install is cleaned."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-download-hook nil
  "*Hook run when epackage is downloaded.
Variable `package' is available."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-remove-hook nil
  "*Hook run when epackage is removed.
Variable `package' is available."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-type-hook nil
  "*Hook run when epackage is installed.
Variables `from' `to' and configuration `type' are available.
The TYPE is one of `epackage--layout-mapping'."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-config-delete-type-hook nil
  "*Hook run when epackage install configuration is deleted.
Variables `file' and configuration `type' are available.
during hook. The TYPE is one of `epackage--layout-mapping'."
  :type  'hook
  :group 'epackage)

(defcustom epackage--install-config-delete-all-hook nil
  "*Hook run when epackage all install configuration is deleted."
  :type  'hook
  :group 'epackage)

(defcustom epackage--build-sources-list-hook nil
  "*Hook run after function `epackage-build-sources-list'.
This hook is run in combined sources list buffer just content is
written to file returned by function
`epackage-file-name-sources-list-main'.

See also `epackage--sources-replace-table'."
  :type  'hook
  :group 'epackage)

;;; ............................................. &variables-info-mode ...
;;; No two-dash variables due to `define-minor-mode'.

(defcustom epackage-info-mode-name " eInfo"
  "String to display in the mode line when Epackage Info Mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "Epackage Info Mode Text"
  :group 'epackage
  :type 'string)

(defvar epackage-info-mode nil
  "*Non-nil when Epackage Info Mode is active.
Never set this variable directly, use the command
`epackage-info-mode' instead.")

(defcustom epackage-info-mode-hook nil
  "Functions to run when Epackage Info Mode is called."
  :tag "Epackage Info Called Hook"
  :group 'epackage
  :type 'hook)

(defcustom epackage-info-mode-on-hook nil
  "Functions to run when Epackage Info Mode is enabled."
  :tag "Epackage Info Mode On Hook"
  :group 'epackage
  :type 'hook)

(defcustom epackage-info-mode-off-hook nil
  "Functions to run when Epackage Info Mode is disabled."
  :tag "Epackage Info Mode Off Hook"
  :group 'epackage
  :type 'hook)

(defvar epackage-info-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map ...)
    ;; Navigation in the document
    (define-key map "\t" 'epackage-info-mode-tab-command)
    (define-key map (kbd "C-a") 'epackage-info-mode-move-beginning-of-line)
    (define-key map (kbd "C-c c") 'epackage-info-mode-cmd-url-commentary)
    (define-key map (kbd "C-c d") 'epackage-info-mode-cmd-goto-description)
    (define-key map (kbd "C-c h") 'epackage-info-mode-cmd-url-homepage)
    (define-key map (kbd "C-c l") 'epackage-info-mode-cmd-lint)
    (define-key map (kbd "C-c mm") 'epackage-info-mode-cmd-email-maintainer)
    (define-key map (kbd "C-c mu") 'epackage-info-mode-cmd-email-upstream)
    (define-key map (kbd "C-c mU") 'epackage-info-mode-cmd-email-upstream-ping)
    (define-key map (kbd "C-c s") 'epackage-info-mode-cmd-goto-status)
    (define-key map (kbd "C-c w") 'epackage-info-mode-cmd-url-wiki)
    map)
  "Keymap used by function `epackage-info-mode'.")

(easy-menu-define
  epackage-info-mode-menu
  epackage-info-mode-map
  "Menu for Epackage Info mode."
  '("eInfo"
    ["Goto field Description" epackage-info-mode-cmd-goto-description]
    ["Goto field Status" epackage-info-mode-cmd-goto-status]
    ["Email maintainer" epackage-info-mode-cmd-email-maintainer]
    ["Email upstream" epackage-info-mode-cmd-email-upstream]
    ["Email upstream ping" epackage-info-mode-cmd-email-upstream-ping]
    ["Visit URL commentary" epackage-info-mode-cmd-url-commentary]
    ["Visit URL homepage" epackage-info-mode-cmd-url-homepage]
    ["Visit URL wiki" epackage-info-mode-cmd-url-wiki]
    "----"
    ["Lint epackage" epackage-info-mode-cmd-lint t] ))

;; Other than standard mode variables (two dash)

(defcustom epackage--devel-template-directory nil
  "Directry of template files to make an Epackage.
Used by `epackage-devel-compose-main'."
  :group 'epackage
  :type 'directory)

(defvar epackage--whitespace-trailing-regexp
  "\\(\\(\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\|\x0C\\)+\\)$"
  "Specify trailing characters regexp.
Copy from whitespace.el with ^L character added.")

(defcustom epackage--info-mode-email-ping-subject "Emacs epackage: %s"
  "Subject in `epackage-info-mode-cmd-email-upstream-ping'.
Format string %s is used to inser package name."
  :tag "Email message subject for initial upstream contact."
  :group 'epackage
  :type 'string)

(defcustom epackage--info-mode-email-ping-body
  (format "\
Hi,

Your software is bundled with the Distributed Emacs Lisp Package
System[*] called epackage. Would you verify that this address is
still your preferred contact. Simply reply shortly \"ok\" or let me
know if you prefer to use another address.

\[*] %s

Thanks,
"
	  epackage--url-doc-emacswiki)
  "Body text in `epackage-info-mode-cmd-email-upstream-ping'."
  :tag "Email message body text for initial upstream contact."
  :group 'epackage
  :type 'string)

;; Color design concepts:
;; - Required fields with one color
;; - Optional fields with rest color
;; - Licence field needs special treatment:
;;   * GPL, ok
;;   * OSI, ok but a deviation from status quo in Emacs Lisp
;;   * Custom, bring to reader's attention

(defvar epackage--info-mode-font-lock-keywords
  '(("^\\(Package\\): *\\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-type-face))
    ("^\\(Description\\): *\\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-type-face))
    ("^\\(License\\): *\\([^ \t\r\n]+\\)"
     (1 'font-lock-keyword-face t)
     (2 'font-lock-type-face))
    ("^\\(License\\): *\\(GPL\\)"
     (1 'font-lock-keyword-face t)
     (2 'default t))
    ("^\\(License\\): *\\(Custom\\|None\\)"
     (1 'font-lock-doc-face t)
     (2 'font-lock-warning-face t))
    ;; FIXME: The color does mumbo-jumbo. Try editing
    ;; text at first line, then second. It sets colors on/off
    ("^\\(License-Text\\):\\(.*\\(?:[\r\n] .*\\)+\\)"
     (1 'font-lock-builtin-face)
     (2 'font-lock-type-face t))
    ("^\\(Status\\):"
     (1 'font-lock-keyword-face))
    ("^Status: .*\\(unmaintained\\)"
     (1 'font-lock-comment-face t))
    ("^Status: .*\\(broken\\)"
     (1 'font-lock-warning-face t))
    ;; Required fields
    ("^\\(Section\\|Maintainer\\|Upstream\\|Description\\):"
     1 'font-lock-keyword-face)
    ("^\\(X-[^ \t\r\n]+\\):"
     1 'font-lock-string-face)
    ("^\\([A-Z-][^ \t\r\n]+\\):"
     1 'font-lock-builtin-face))
  "Keywords to hilight Epackage Info mode.")

(defconst epackage--info-mode-completions-status
  '("core-emacs"                        ;XEmacs is not listed by purpose
    "unmaintained"
    "broken"
    "unsafe"
    "stable"
    "unstable"
    "experimental")
  "List of completions for Status field.")

(defconst epackage--info-mode-completions-license
  '("GPL-2+"                        ;XEmacs is not listed by purpose
    "GPL-3+"
    "BSD"
    "Apache-2.0"
    "Custom"
    "None")
  "List of completions for License field. This is not a user variable.
The valid License abbreviations should follow list defined at:

    License specification / Short name
    <http://dep.debian.net/deps/dep5/#license-specification>.")

(defvar epackage--info-mode-completions-section nil
  "List of completions for Section field.
This variable will be dynamically initialized when used for the first
time in `epackage-info-mode-tab-command'.")

(defvar epackage--info-mode-completions-current nil
  "Set dynamically in `epackage-info-mode-tab-command'.")

(defconst epackage--info-licence-list-url
  "http://spdx.org/licenses/"
  "URL of offical licence list.")

(defconst epackage--info-mode-license-list
  '( ;; Special cases
    "Custom"
    "None"

    ;; Call at this point, M-x epackage-devel-licence-list-http-insert
    "AFL-1.2"
    "AFL-2"
    "AFL-2.1"
    "AFL-3"
    "APL-1"
    "ANTLR-PD"
    "Apache-1"
    "Apache-1.1"
    "Apache-2"
    "APSL-1"
    "APSL-1.1"
    "APSL-1.2"
    "APSL-2"
    "Artistic-1"
    "Artistic-2"
    "AAL"
    "BSL-1"
    "BSD-2-Clause"
    "BSD-3-Clause"
    "BSD-4-Clause"
    "CECILL-1"
    "CECILL-1.1English"
    "CECILL-2"
    "CECILL-B"
    "CECILL-C"
    "ClArtistic"
    "CDDL-1"
    "CPAL-1"
    "CPL-1"
    "CATOSL-1.1"
    "CC-BY-1"
    "CC-BY-2"
    "CC-BY-2.5"
    "CC-BY-3"
    "CC-BY-ND-1"
    "CC-BY-ND-2"
    "CC-BY-ND-2.5"
    "CC-BY-ND-3"
    "CC-BY-NC-1"
    "CC-BY-NC-2"
    "CC-BY-NC-2.5"
    "CC-BY-NC-3"
    "CC-BY-NC-ND-1"
    "CC-BY-NC-ND-2"
    "CC-BY-NC-ND-2.5"
    "CC-BY-NC-ND-3"
    "CC-BY-NC-SA-1"
    "CC-BY-NC-SA-2"
    "CC-BY-NC-SA-2.5"
    "CC-BY-NC-SA-3"
    "CC-BY-SA-1"
    "CC-BY-SA-2"
    "CC-BY-SA-2.5"
    "CC-BY-SA-3"
    "CC0-1"
    "CUA-OPL-1"
    "EPL-1"
    "eCos-2"
    "ECL-1"
    "ECL-2"
    "EFL-1"
    "EFL-2"
    "Entessa"
    "ErlPL-1.1"
    "EUDatagrid"
    "EUPL-1"
    "EUPL-1.1"
    "Fair"
    "Frameworx-1"
    "AGPL-3"
    "GFDL-1.1"
    "GFDL-1.2"
    "GFDL-1.3"
    "GPL-1"
    "GPL-1.0+"
    "GPL-2"
    "GPL-2.0+"
    "GPL-2.0-with-autoconf-exception"
    "GPL-2.0-with-bison-exception"
    "GPL-2.0-with-classpath-exception"
    "GPL-2.0-with-font-exception"
    "GPL-2.0-with-GCC-exception"
    "GPL-3"
    "GPL-3.0+"
    "GPL-3.0-with-autoconf-exception"
    "GPL-3.0-with-GCC-exception"
    "LGPL-2.1"
    "LGPL-2.1+"
    "LGPL-3"
    "LGPL-3.0+"
    "LGPL-2"
    "LGPL-2.0+"
    "gSOAP-1.3b"
    "HPND"
    "IPL-1"
    "IPA"
    "ISC"
    "LPPL-1"
    "LPPL-1.1"
    "LPPL-1.2"
    "LPPL-1.3c"
    "Libpng"
    "LPL-1.02"
    "MS-PL"
    "MS-RL"
    "MirOS"
    "MIT"
    "Motosoto"
    "MPL-1"
    "MPL-1.1"
    "Multics"
    "NASA-1.3"
    "Naumen"
    "NGPL"
    "Nokia"
    "NPOSL-3"
    "NTP"
    "OCLC-2"
    "ODbL-1"
    "PDDL-1"
    "OGTSL"
    "OSL-1"
    "OSL-2"
    "OSL-3"
    "OLDAP-2.8"
    "OpenSSL"
    "PHP-3.01"
    "PostgreSQL"
    "Python-2"
    "QPL-1"
    "RPSL-1"
    "RPL-1.5"
    "RHeCos-1.1"
    "RSCPL"
    "Ruby"
    "SAX-PD"
    "OFL-1.1"
    "SimPL-2"
    "Sleepycat"
    "SugarCRM-1.1.3"
    "SPL-1"
    "Watcom-1"
    "NCSA"
    "VSL-1"
    "W3C"
    "WXwindows"
    "Xnet"
    "XFree86-1.1"
    "YPL-1.1"
    "Zimbra-1.3"
    "Zlib"
    "ZPL-1.1"
    "ZPL-2"
    "ZPL-2.1"
    )
  "Valid license abbreviations. See http://spdx.org/licenses/
Last updated 2011-12-12.")

(defconst epackage--info-mode-license-regexp
  (regexp-opt
   epackage--info-mode-license-list)
  "Regexp of `epackage--info-mode-license-list'.")

;;; ............................................... &variables-private ...

(defvar epackage--buffer-ui-simple "*Epackage Batch UI*"
  "Epackage Manager buffer for `epackage-ui-simple'.")

(defconst epackage--buffer-princ-default (get-buffer "*Messages*")
  "Default value for `epackage--buffer-princ-default'.")

(defvar epackage--buffer-princ epackage--buffer-princ-default
  "Output stream destination buffer.")

(defvar epackage--sources-list-regexp
  `,(concat "^\\(%s\\)\\>"
            "[ \t]+\\([^ \t\r\n]+\\)"
            ;;  In case there i no description, do not *require*
            ;;  a match
            "\\(?:[ \t]+\\([^ \t\r\n]+.+[^ \t\r\n]+\\)\\)?")
  "Regexp to match entries described in `epackage--sources-list-url'.
The %s marks the package name.")

(defcustom epackage--root-directory
  (let ((dir (if (featurep 'xemacs)
		 "~/.xemacs.d"
	       "~/.emacs.d")))
    (cond
     ((file-directory-p dir)
      dir)
     ((null (getenv "HOME"))
      ;; (memq system-type '(windows-nt ms-dos))
      ;; No known package installation root directory.
      (error
       (concat
	"Epackage: [ERROR] Can't determine location of Emacs Lisp files."
	"See `epackage--root-directory'."
	"Please define environment variable HOME")))
     (t
      (make-directory dir)
      dir)))
  "*Location of Emacs Lisp files. The default is ~/.emacs.d directory.
Directory must not contain a trailing slash."
  :type  'directory
  :group 'epackage)

(defvar epackage--symlink-support-flag
  (if epackage-w32-p
      nil
    t)
  "Non-nil means symlinks are supported.
The value must be nil under Windows Operating System.")

(defvar epackage--directory-name "epackage"
  "Name of package directory under `epackage--root-directory'.
Use function `epackage-directory' for full path name.")

(defconst epackage--directory-name-pkg "packages"
  "Directory under `epackage--root-directory' where to download.
Use function `epackage-directory-packages' for full path name.")

(defconst epackage--directory-name-conf "00conf"
  "The name of local sources list repository.
Use `epackage-directory-loader' for full path name.")

(defconst epackage--sources-package-name "00sources"
  "The name of local sources list repository directory.
Copy of `epackage--sources-list-url'.")

(defconst epackage--directory-name-install "00install"
  "Install directory under `epackage--root-directory'.
This directory contains control files from packages.")

(defvar epackage--sources-file-name-official "epackage.lst"
  "Name of official sources list file that lists available packages.
Do not touch. See variable `epackage--sources-list-url'.")

(defvar epackage--package-control-directory "epackage"
  "Name of directory inside VCS controlled package.")

;; Autoload due to `epackage-pkg-info-p'
;;;###autoload
(defvar epackage--pkg-info-file-name "info"
  "Name of information file of epackage.
Do not touch. See variable `epackage--sources-list-url'.")

(defvar epackage--sources-file-name-main "sources.lst"
  "Name of the combined sources list file that lists available packages.
Do not touch. See User configurable variables `epackage--sources-list-url'
and `epackage--sources-file-list'.")

(defvar epackage--loader-file-name "epackage-loader.el"
  "File that contains package enabling and activation code.
Use function `epackage-file-name-loader-file' for full path name.
Make fle with `epackage-loader-file-generate'.
See also variable `epackage--loader-file-byte-compile-flag'.")

(defvar epackage--loader-file-load-path "epackage-load-path.el"
  "File that contains `load-path' definitions.
Not a user file. This is used internally during byte compiling
packages.")

(defconst epackage--date-month-list
  '(("Jan" 1)
    ("Feb" 2)
    ("Mar" 3)
    ("Apr" 4)
    ("May" 5)
    ("Jun" 6)
    ("Jul" 7)
    ("Aug" 8)
    ("Sep" 9)
    ("Oct" 10)
    ("Nov" 11)
    ("Dec" 12))
  "Assoc list of months '((MONTH NUMBER) ...).")

(defconst epackage--date-month-regexp
  `,(concat
     "\\<"
     (regexp-opt
      (mapcar #'car epackage--date-month-list))
     "\\>")
  "Regexp matching short month names.")

(defconst epackage--date-weekday-regexp
  `,(concat
     "\\<"
     (regexp-opt
      '("Mon"
	"Tue"
	"Wed"
	"Thu"
	"Fri"
	"Sat"
	"Sun"))
     "\\>")
  "Regexp matching short month names.")

(defconst epackage--lisp-file-exclude-regexp
  "\\(?:^\\|/\\)\\(?:test\\.el\\)"
  "Regexp to exclude lisp files.
Used in function `epackage-devel-generate-compile-main'.")

(defconst epackage--directory-exclude-regexp
  (concat
   (regexp-opt
    '("/CVS"
      "/RCS"
      "/cvs"
      "/rcs"
      "/.bzr"
      "/.darcs"
      "/.git"
      "/.hg"
      "/.mtn"
      "/.svn"
      "/cert"
      "/doc"
      "/html"
      "/howto"
      "/images"
      "/manual"
      "/patches"
      "/pics"
      "/test"
      "/tests"
      "/tex"
      "/texinfo"))
   "$")
  "Regexp to exclude dirctory names.
See function 'epackage-directory-recursive-list-default'
which also excludes directories than contain file .nosearch.")

(defconst epackage--info-layout-mapping
  '(("Package" "[a-z0-9-]+")
    ("Section" "[a-z]+")
    ("Depends" "[a-z0-9-]+")
    ("Maintainer" "[^ \t+r\n]+@[^ \t+r\n]+")
    ("Email" "[^ \t+r\n]+@[^ \t+r\n]+")
    ("Description" "[^ \t+r\n]+"))

  "Required fields and test regexp for `epackage--pkg-info-file-name'.
Format is:
  '((FIELD CONTENT-TEST-REGEXP) ...).")

(defconst epackage--layout-mapping
  '((activate   "-epkg-xactivate.el")
    (autoload   "-epkg-autoloads.el")
    (enable     "-epkg-install.el"  'required '(file)) ;; not for lib-* packages
    (examples   "-epkg-examples.el")
    (compile    "-epkg-compile.el")
    (info       "info"  'required)
    (lisp       "lisp")
    (ignore     "ignore")
    (loaddefs   "-epkg-0loaddefs.el")
    (uninstall  "-epkg-uninstall.el"))
  "File type mapping table for files in `epackage--package-control-directory'.
Format is:
    '((TYPE FILENAME [REQUIRED-FLAG] [TYPE-FLAG]) ...)

Description:

    TYPE	    Property type name.

    FILENAME	    If FILENAME starts with '-', then the package name
		    is prefixed to the FILENAME. Say package name
		    'foo', and suffix is '-install', the full
		    filename is 'foo-install.el.

    REQUIRED-FLAG   If non-nil, this configuration file is required

    TYPE-FLAG	    List of types the REQUIRED-FLAG applies. Valid
		    values are `file' and `lib'. If value is nil,
		    REQUIREd-FLAG is treated as it this were set to '(file lib).

		    For example, to require that file only exists
		    only in library packages, set this value to '(lib).")

(defvar epackage--buffer-autoload "*Epackage autoloads*"
  "Buffer to use for gathering manual autoload definitions.
See function `epackage-autoload-create-on-directory'.")

(defvar epackage--buffer-doc "*Epackage documentation*"
  "Buffer displayed by `epackage-doscumentation'.")

(defvar epackage--buffer-info "*Epackage info*"
  "Buffer displayed by `epackage-pkg-info-display'.")

(defvar epackage--buffer-lint "*Epackage Lint*"
  "Buffer displayed by `epackage-pkg-lint-package'.")

;; FIXME Emacs 24.1 patch
(defconst epackage--buffer-finder-commentary "*Finder-package*"
  "Buffer name of call `finder-commentary'.")

(defvar epackage--buffer-emacs-messages
  (if (featurep 'xemacs)
      "*Message-Log*"
    "*Messages*")
  "Buffer name of Emacs messages.")

(defconst epackage--byte-compile-buffer-name
  (or (and (boundp 'byte-compile-log-buffer) ;Emacs 24.1
           byte-compile-log-buffer)
      "*Compile-Log*")
  "Buffer name of byte compilation results.")

(defvar epackage--initialize-flag nil
  "Non-nil means that package has been initialized.
Set by function `epackage-initialize'. Do not touch.")

(defvar epackage--program-git nil
  "Location of program git(1).

This variable will be set by `epackage-initialize'
which calls function `epackage-require-git'.")

(defvar epackage--process-output "*Epackage process*"
  "Output of `epackage--program-git'.")

(defvar epackage--debug t
  "If non-nil, activate debug.")

(defvar epackage--depends-satisfy-running nil
  "If non-nil, `epackage-pkg-depends-satisfy' is running.
This variable is zeroed at the start of
`epackage-pkg-depends-satisfy'. It holds information about
subsequent downloaded packages to satisfy depends chains.

The information is used to rollback the installations of packages
in case the depends hits a dead end. Take an example:

   A > B > C > D

Say, there is no D, so final depends can't be satisfied. The
*new* packages downloaded to that point were A, B and C. When we
roll back, the packages A, B and C are not removed from disk,
only their install configurations are so that they don't become
active. User has to manually select \"remove\" to physically
delete downloaded packages.

If B was already installed, then the calls would be:

   A > (B already) > C > D

And the packages to roll back would be only A and C.")

(defvar epackage--batch-ui-menu-prompt "== epackage ([?mq]/cmd): "
  "Command line UI menu prompt.")

;; Indent less used commands; but keep in alphabetical order,
;; so that we know what keys are left to use.

(defconst epackage--batch-ui-menu-string "
a       Install (a)ctivate configuration; modifies Emacs environment
A       Uninstall (A)ctivate configuration
b         Generate (b)oot loader
c       Clean install configuration files (whole uninstall)
d       Download package
e       Install standard (e)nable configuration
E       Uninstall standard (E)nable configuration
g       Get sources list; update list of available packages
i         Display (i)nfo file
I         Display commentary (I)nfo (if any)
l       List installed packages
L       List downloaded packages
n       List (n)ot installed packages
o       Install aut(o)load configuration from package
p       List available (p)ackages
r       Remove package physically from local disk
t          Ac(t)ion toggle: after download, install enable configuration
T          Ac(T)ion toggle: after download, install activate configuration
u       Upgrade single package
U       Upgrade all packages
y         B(y)te compile package
Y         Action toggle: after download, b(y)te compile package
?,q,m,v   Help, Quit, Menu, Version"
  "UI menu to run epackage from command line.")

(defconst epackage--batch-ui-menu-actions
  '((?a epackage-cmd-activate-package)
    (?b epackage-batch-ui-loader-file-generate)
    (?A epackage-batch-ui-deactivate-package)
    (?c epackage-batch-ui-clean-package)
    (?d epackage-batch-ui-download-package)
    (?e epackage-batch-ui-enable-package)
    (?E epackage-batch-ui-disable-package)
    (?g epackage-batch-ui-sources-list-upgrade)
    (?i epackage-batch-ui-display-package-info)
    (?I epackage-batch-ui-display-package-documentation)
    (?l epackage-batch-ui-list-installed-packages)
    (?L epackage-batch-ui-list-downloaded-packages)
    (?m epackage-batch-ui-display-menu)
    (?n epackage-batch-ui-list-not-installed-packages)
    (?o epackage-batch-ui-autoload-package)
;;    (?O epackage-batch-ui-uninstall-package)
    (?r epackage-batch-ui-remove-package)
    (?t epackage-batch-ui-download-action-enable-toggle)
    (?T epackage-batch-ui-download-action-activate-toggle)
    (?u epackage-batch-ui-upgrade-package)
    (?U epackage-batch-ui-upgrade-all-packages)
    (?v epackage-batch-ui-display-version)
    (?p epackage-batch-ui-list-available-packages)
    (?q quit)
    (?Q quit)
    (?y epackage-batch-ui-byte-compile-package)
    (?Y epackage-batch-ui-download-action-compile-toggle))
  "UI menucommand and actions. Format: '((KEY FUNCTION) ...).

Use from command line:

  emacs --batch -Q -l ./epackage.el -f epackage-batch-ui-menu")

(defconst epackage--batch-ui-menu-help "
In a nutshell
-------------
To install: (d)ownload, (e)enable, (b)oot loader generate, (q)uit.

Packages management
-------------------
download        Download to disk. No install whatsoever.

upgrade         Download (uU)pdates for single or all.

info            Show downloaded (i)nformation file.
                Use command \"List available (p)ackages\" prior download.

install         Several configuration choices:
                * aut(o)load. Install only minimal functions
                  that will be made available in autoload state.
                  If you want to configure everything manually in
                  ~/.emacs startup file (expert install).
                * standard = (e)nable autoload code and very basic
                  functionality of user commmands for M-x calls.
                * (a)ctivate = install fancy setup with hooks,
                  bindings and the like. Modifies Emacs setup.

clean           Delete all install configuration files. The extension
                will not be available for later use. M-x calls
                are no longer available. This does not remove the
                download from disk (see \"remove\").

remove          Physically remove configuration files and files
                from download directory. The opposite of download.

Other actions
-------------
\(b)oot loader    Write boot loader that contains all activated
                configurations. Must be generated/updated
                after each epackage management change. This is intended to be
                loaded from ~/.emacs with

                (load \"~/.emacs.d/epackage/00conf/epackage-loader\" 'noerr)

\(g)et           Get new sources list and store it locally. This is the basis
                for selecting packages to install. Must be run periodically."
  "UI menu help.")

(defconst epackage--layout-template-compile
  "\
\(dolist (file
         '(%s))
   (if (and (boundp 'verbose)
            verbose)
       (message \"Byte Compile %%s\" file))
   (if (file-exists-p file)
       (byte-compile-file file)
     (message \"** Byte compile error. Not found: %%s\" file)))
"
  "Format string containing the epackage/*-compile.el file.
The files are written to %s. See function
`epackage-devel-generate-compile'.")

(defconst epackage--layout-template-info
  "\
Package:
Section: data | extensions | files | languages | mail | tools <M-x finder-list-keywords>
License: GPL-2+ | BSD | Apache-2.0 | ... | Custom | None
License-Text: <only for type Custom; full text or URL to page in http://spdx.org/licenses/>
Depends: emacs (>= 22)
Status: core-emacs[-NN.N] unmaintained broken unsafe stable unstable experimental
Compat:
Maintainer:
Bugs:
Upstream:
Upstream-Bugs:
Vcs-Type:
Vcs-Url:
Vcs-Browser:
Vcs-Args:
Vcs-User:
Vcs-Password:
Homepage:
Wiki: http://www.emacswiki.org/emacs/
Commentary:
Description: <short one line>
 [<Longer description>]
 .
 Note: YYYY-MM-DD the code hasn't been touched since YYYY.
"
  "String containing the epackage/info file.")

;;; ................................................ &functions-simple ...

(put 'epackage-with-write-file 'epackage-with-case-fold-search 0)
(put 'epackage-with-write-file 'epackage-with-case-fold-search '(body))
(defmacro epackage-with-case-fold-search (&rest body)
  "Run BODY with `case-fold-search' set to nil."
  `(let (case-fold-search)
     ,@body))

(defmacro epackage-nconc (list place)
  "Add LIST to PLACE, modify PLACE."
  `(setq ,place (nconc ,list ,place)))

(defmacro epackage-push (x place)
  "A close `push' CL library macro equivalent: (push X PLACE)."
  `(setq ,place (cons ,x ,place)))

(defmacro epackage-assoc (key list)
  "Access of KEY in LIST and return its value.
An example:  '((a 1) (b 3))  => key \"a\". Returns 1."
  `(nth 1 (assoc ,key ,list)))

(defmacro epackage-fatal (format &rest args)
  "Call `error' with FORMAT and ARGS. Mark message with FATAL tag."
  `(error (concat "Epackage: [FATAL] " ,format) ,@args))

(defmacro epackage-error (format &rest args)
  "Call `error' with FORMAT and ARGS. mark message with ERROR tag."
  `(error (concat "Epackage: [ERROR] " ,format) ,@args))

(defmacro epackage-princ (format &rest args)
  "Call `message' with FORMAT and ARGS. Mark message with WARN tag."
  `(princ (concat (format ,format ,@args) "\n") epackage--buffer-princ))

(defmacro epackage-warn (format &rest args)
  "Call `message' with FORMAT and ARGS. Mark message with WARN tag."
  `(message (concat "Epackage: [WARN] " ,format) ,@args))

(defmacro epackage-message (format &rest args)
  "Call `message' with FORMAT and ARGS."
  `(message (concat "Epackage: " ,format) ,@args))

(put 'epackage-ignore-errors 'lisp-indent-function 0)
(put 'epackage-ignore-errors 'edebug-form-spec '(body))
(defmacro epackage-ignore-errors (&rest body)
  "Run BODY and ignore errors. Like CL `ignore-errors'."
  `(condition-case error
       (progn
         ,@body)
     (error)))                          ;variable test, not a function call

(put 'epackage-with-w32 'lisp-indent-function 0)
(put 'epackage-with-w32 'edebug-form-spec '(body))
(defmacro epackage-with-w32 (&rest body)
  "Run BODY in Windows like operating system."
  `(when epackage-w32-p
     ,@body))

(put 'epackage-with-debug 'lisp-indent-function 0)
(put 'epackage-with-debug 'edebug-form-spec '(body))
(defmacro epackage-with-debug (&rest body)
  "Run BODY if variable `epackage--debug' is non-nil."
  `(when epackage--debug
     ,@body))

(put 'epackage-with-byte-compile-buffer 'lisp-indent-function 0)
(put 'epackage-with-byte-compile-buffer 'edebug-form-spec '(body))
(defmacro epackage-with-byte-compile-buffer (&rest body)
  "Run BODY if variable `epackage--byte-compile-buffer' is non-nil."
  `(if (get-buffer epackage--byte-compile-buffer-name)
       (with-current-buffer (get-buffer epackage--byte-compile-buffer-name)
         ,@body)))

(put 'epackage-with-lint-buffer 'lisp-indent-function 0)
(put 'epackage-with-lint-buffer 'edebug-form-spec '(body))
(defmacro epackage-with-lint-buffer (&rest body)
  "Run BODY in `epackage--buffer-lint' (will be rcrated is needed)."
  `(with-current-buffer (get-buffer-create epackage--buffer-lint)
     ,@body))

(put 'epackage-with-checkdoc-buffer 'lisp-indent-function 0)
(put 'epackage-with-checkdoc-buffer 'edebug-form-spec '(body))
(defmacro epackage-with-checkdoc-buffer (&rest body)
  "Run BODY in `epackage--buffer-checkdoc' (will be rcrated is needed)."
  `(with-current-buffer (get-buffer-create (or (and (boundp 'checkdoc-diagnostic-buffer)
						    checkdoc-diagnostic-buffer)
					       "*Style Warnings*"))
     (toggle-read-only -1)
     (let ((buffer-undo-list t))
       ,@body)))

(put 'epackage-verbose-message 'lisp-indent-function 0)
(put 'epackage-verbose-message 'edebug-form-spec '(body))
(defmacro epackage-verbose-message (&rest args)
  "If variable `verbose' is non-nil, call `message' with ARGS."
  `(when verbose
     (epackage-message ,@args)))

(put 'epackage-with-message 'lisp-indent-function 2)
(put 'epackage-with-message 'edebug-form-spec '(body))
(defmacro epackage-with-message (verbose message &rest body)
  "If VERBOSE, display MESSAGE before and after (\"..done\") BODY."
  `(progn
     (if ,verbose
         (epackage-message "%s" (concat ,message "...")))
     (prog1
         ,@body
       (if ,verbose
           (epackage-message "%s" (concat ,message "...done"))))))

(put 'epackage-with-buffer-autoload 'lisp-indent-function 0)
(put 'epackage-with-buffer-autoload 'edebug-form-spec '(body))
(defmacro epackage-with-buffer-autoload (&rest body)
  "Create `epackage--buffer-autoload' and and run BODY."
  `(with-current-buffer (get-buffer-create epackage--buffer-autoload)
     ,@body))

(put 'epackage-with-write-file 'lisp-indent-function 0)
(put 'epackage-with-write-file 'edebug-form-spec '(body))
(defmacro epackage-with-write-file (&rest body)
  "Disable backup and run BODY."
  `(let ((backup-inhibited t)
         (backup-enable-predicate 'ignore)
         (version-control 'never)
         make-backup-files)
     ,@body))

(defsubst epackage-sort (list)
  "Sort LIST of string."
  (sort list
	(lambda (a b)
	  (string< a b))))

(defsubst epackage-erase-buffer ()
  "Disable undo and erase current buffer."
  (let ((buffer-undo-list t))
    (unless (eq (point-min) (point-max))
      (delete-region (point-min) (point-max)))))

(defsubst epackage-url-extract-host (url)
  "Extract from URL the HOST portion."
  (if (or (string-match "^[^:]+@\\([^/:]+\\):" url) ;login@host:
	  (string-match "^[^:]+://\\([^/]+\\)" url)) ;http://
      (match-string 1 url)))

(defsubst epackage-auto-revert-mode-p ()
  "Check if auto-revert is active in current buffer."
  (or (and (boundp 'auto-revert-mode)
	   auto-revert-mode)
      (and (boundp 'auto-revert-tail-mode)
	   auto-revert-tail-mode)
      (and (boundp 'global-auto-revert-mode)
	   global-auto-revert-mode)))

(defsubst epackage-turn-on-auto-revert-mode ()
  "Turn on `auto-revert-mode' unless already active in current buffer."
  (when (and (buffer-file-name)         ;; Must be file buffer
	     (not (epackage-auto-revert-mode-p)))
    (auto-revert-mode 1)))

(defsubst epackage-append-to-buffer (buffer string &optional begin)
  "Append to BUFFER a STRING. Optionally at the BEGIN."
  (with-current-buffer buffer
    (if begin
        (goto-char (point-min))
      (goto-char (point-max)))
    (insert string)))

(defsubst epackage-add-provide-to-buffer (&optional file)
  "Add `provide' based on FILE to the end if not yet there.
Optional FILE defaults to `buffer-file-name'.
Point is not preserved.
Return:
  point, if added."
  (goto-char (point-min))
  (unless (re-search-forward "^(provide" nil t)
    (let ((name (file-name-sans-extension
		 (file-name-nondirectory
		  (or file
		      buffer-file-name
		      (epackage-error
		       "Can't add provide from empty buffer-file-name"))))))
      (goto-char (point-max))
      (unless (looking-at "^[ \t]*$")
	(insert "\n"))
      (insert (format "(provide '%s)\n" name))
      (point))))

(defsubst epackage-add-provide-to-file (file)
  "Add `provide' statement to FILE if not yet there."
  (with-temp-buffer
    (insert-file-contents file)
    (when (epackage-add-provide-to-buffer file)
      (epackage-write-region (point-min) (point-max) file))))

(defsubst epackage-write-region (start end file)
  "Like  'write-region' START END FILE; but disable backup etc."
  (epackage-with-write-file
    (write-region start end file)))

(defsubst epackage-save-buffer ()
  "Like 'save-buffer' FILE; but disable backup etc."
  (epackage-with-write-file
    (save-buffer)))

(defsubst epackage-time ()
  "Return ISO 8601 YYYY-MM-DD HH:MM:SS."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defsubst epackage-file-name-basename (dir)
  "Like `file-name-nondirectory' but always return last component of DIR.
An example:  /path/to/  => to"
    (when (string-match "^.+/\\([^/]+\\)/?$" dir)
      (match-string 1 dir)))

(defsubst epackage-download-action-activate-p ()
  "Check `epackage--download-action-list' for activate."
  (memq 'activate epackage--download-action-list))

(defsubst epackage-download-action-enable-p ()
  "Check `epackage--download-action-list' for enable."
  (memq 'enable epackage--download-action-list))

(defsubst epackage-download-action-install-p ()
  "Check `epackage--download-action-list' for activate and enable."
  (or (epackage-download-action-activate-p)
      (epackage-download-action-enable-p)))

(defsubst epackage-file-name-directory-previous (dir)
  "Return previous directory by removing one component from DIR.
Return nil of there is nothing to remove .i.e. the result wold be \"/\"."
  (let ((path (file-name-as-directory dir)))
    (when (string-match "\\(.+\\)/[^/]+/?$" path)
      (match-string 1 dir))))

(defsubst epackage-string-p (string)
  "Return STRING if value is non-empty. Otherwise return nil."
  (and (stringp string)
       (not (string-match "^[ \t\r\n]*$" string))
       string))

(defsubst epackage-package-name-library-p (package)
  "Check if PACKAGE name is library."
  (epackage-with-case-fold-search
   (unless (stringp package)
     (epackage-error "package name is not a string")
     (string-match "^lib-" package))))

(defsubst epackage-package-name-valid-p (package)
  "Check if PACKAGE name is valid."
  (epackage-with-case-fold-search
   (unless (stringp package)
     (epackage-error "package name is not a string"))
   ;; "a-" is an invalid package name
   (string-match "^[a-z]\\(?:[a-z0-9-]+\\)?[a-z0-9]$" package)))

(defsubst epackage-error-if-invalid-package-name (package &optional msg)
  "Check if PACKAGE name is valid or signal error with optional MSG."
  (unless (epackage-package-name-valid-p package)
    (epackage-error "Not a valid package name: %s" package)))

(defsubst epackage-directory-root ()
  "Return root directory."
  (format "%s%s"
          (expand-file-name
           (file-name-as-directory epackage--root-directory))
          (if (stringp epackage--directory-name)
              epackage--directory-name
            (epackage-error
              "epackage--directory-name is not a string"))))

(defsubst epackage-directory-conf ()
  "Location of `epackage--directory-name-conf'."
  (format "%s/%s"
          (epackage-directory-root)
          epackage--directory-name-conf))

(defsubst epackage-directory-loader ()
  "Location of `epackage--directory-name-conf'."
  (epackage-directory-conf))

(defsubst epackage-file-name-loader-boot ()
  "Return path to boot loader file."
  (format "%s/%s"
          (epackage-directory-loader)
          epackage--loader-file-name))

(defsubst epackage-file-name-loader-load-path ()
  "Return path to `load-path' loader file."
  (format "%s/%s"
          (epackage-directory-loader)
          epackage--loader-file-load-path))

(defsubst epackage-directory-packages ()
  "Return top level directory of downloaded packages."
  (format "%s/%s"
          (epackage-directory-root)
          epackage--directory-name-pkg))

(defsubst epackage-file-name-compose (package path)
  "Return file name under PACKAGE directory with PATH added.
An exmaple: (epackage-file-name-compose \"foo\" \"foo.el\")."
  (format "%s%s"
          (epackage-directory-packages)
          (if (string= "" path)
              ""
            (concat "/" path))))

(defsubst epackage-directory-package-root (package)
  "Return root directory of PACKAGE."
  (format "%s/%s/%s"
          (epackage-directory-root)
          epackage--directory-name-pkg
          package))

(defsubst epackage-directory-package-git-root (package)
  "Return root directory of PACKAGE Git control dir."
  (format "%s/.git"
          (epackage-directory-package-root package)))

(defsubst epackage-directory-package-git-config (package)
  "Return Git config file of PACKAGE."
  (format "%s/config"
          (epackage-directory-package-git-root package)))

(defsubst epackage-directory-install ()
  "Return location of install configuration directory."
  (format "%s/%s"
          (epackage-directory-root)
          epackage--directory-name-install))

(defsubst epackage-directory-package-control (package)
  "Return control directory of PACKAGE."
  (let ((root (epackage-directory-package-root package)))
    (format "%s/%s" root epackage--package-control-directory)))

(defsubst epackage-file-name-package-info (package)
  "Return path to `epackage--pkg-info-file-name' of PACKAGE."
  (format "%s/%s"
          (epackage-directory-package-control package)
          epackage--pkg-info-file-name))

(defsubst epackage-file-name-nondirectory (dir)
  "Return last component in DIR.
Examples:
    /path/to/dir        =>  dir
    /path/to/dir/       =>  dir."
  (epackage-with-w32
    ;; Convert to forward slashes
    (setq dir (expand-file-name dir)))
  (if (string-match "/\\([^/]+\\)/?$" dir)
      (match-string-no-properties 1 dir)))

(defsubst epackage-file-content-as-string (file)
  "Return content of FILE as string."
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))

(defsubst epackage-download-action-enable (action)
  "Add ACTION to `epackage--download-action-list'.
Return `epackage--download-action-list'."
  (progn
    (add-to-list 'epackage--download-action-list action)
    epackage--download-action-list))

(defsubst epackage-download-action-disable (action)
  "Remove ACTION from `epackage--download-action-list'.
Return `epackage--download-action-list'."
  (progn
    (setq epackage--download-action-list
          (delq action epackage--download-action-list))
    epackage--download-action-list))

(defsubst epackage-layout-mapping-file (type)
  "Return nth 1 of TYPE listed in `epackage--layout-mapping'."
  (nth 1 (assq type epackage--layout-mapping)))

(defsubst epackage-layout-file-name (dir package type)
  "Return file name under DIR of PACKAGE and layout TYPE."
  (format "%s%s/%s"
	  (file-name-as-directory dir)
	  epackage--package-control-directory
	  (let* ((type (or (epackage-layout-mapping-file type)
			   (error "Unknown epackage layout type: %s" type))))
	    (if (string-match "^-" type)
		(concat (downcase package) type)
	      type))))

(defsubst epackage-eval-file (file &optional security)
  "Evaluate FILE with optionally checking SECURITY.
If SECURITY is non-nil, signal error if
- GPG signature is missing at location FILE.gpg
- GPG signature is invalid at location FILE.gpg."
  (with-temp-buffer
    (insert-file-contents file)
    ;; FIXME: Implement SECURITY
    (eval-buffer)))

(defsubst epackage-eval-file-safe (file &optional security)
  "Evaluate FILE. Optionally check SECURITY.
See `epackage-eval-file' for SECURITY argument handling.

Return:
    non-nil on succes."
  (condition-case error
      (progn
        (epackage-eval-file file)
        t)
    (error
     (epackage-message
       "[ERROR] Enable; Syntax error in %s: %s"
       file (cdr error))
     nil)))

(defsubst epackage-directory-list-exclude-p (exclude dir)
  "Check if EXCLUDE regexp match DIR."
  ;; This fucntion exists so that you can point edebugger to it.
  (string-match exclude dir))

;;  Test Drivers:
;;  (epackage-date-to-iso "20080830")
;;  (epackage-date-to-iso "2011.1.12")
;;  (epackage-date-to-iso "1/19/98")
;;  (epackage-date-to-iso "Wed Jun 17 14:26:21 2009 (-0700)")
(defun epackage-date-to-iso (str)
  "Convert US date M/D/YY[YY] into ISO 8601 format YYYY-MM-DD."
  (cond
   ((not (stringp str))					; skip
    nil)
   ;; YYYY-MM-DD
   ((string-match "\\<\\([12][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]\\)\\>" str)
    (match-string-no-properties 1 str))
   ;; YYYYMMDD
   ((string-match
     `,(concat
	"\\<"
	"\\([12][0-9][0-9][0-9]\\)"
	"\\([0-1][0-9]\\)"
	"\\([0-3][0-9]\\)"
	"\\>")
     str)
    (let ((y (match-string-no-properties 1 str))
	  (m (string-to-number (match-string-no-properties 2 str)))
	  (d (string-to-number (match-string-no-properties 3 str))))
      (format "%s-%02d-%02d" y m d)))
   ;; YYYY.MM?.DD?
   ((string-match
     `,(concat "\\<\\([0-9][0-9][0-9][0-9]\\)[.]"
	       "\\([0-9][0-9]?\\)[.]"
	       "\\([0-9][0-9]?\\)\\>")
     str)
    (let ((y (match-string-no-properties 1 str))
	  (m (string-to-number (match-string-no-properties 2 str)))
	  (d (string-to-number (match-string-no-properties 3 str))))
      (format "%s-%02d-%02d" y m d)))
   ;; US format MM?/DD?/YY(YY)?
   ((string-match
     `,(concat
	"\\<\\([0-9][0-9]?\\)/"
	"\\([0-9][0-9]?\\)/"
	"\\([0-9][0-9]+\\(?:[0-9][0-9]\\)?\\)\\>")
     str)
    (let ((m (string-to-number (match-string-no-properties 1 str)))
	  (d (string-to-number (match-string-no-properties 2 str)))
	  (y (match-string-no-properties 3 str)))
      (when (= (length y) 2)
	(if (string-match "^[789]" y)
	    (setq y (concat "19" y))
	  (setq y (concat "20" y))))
      (format "%s-%02d-%02d" y m d)))
   ;; "Wed Jun 17 14:26:21 2009 (-0700)"
   ((string-match
     `,(concat
	epackage--date-weekday-regexp
	"[ \t]+"
	"\\(" 	epackage--date-month-regexp "\\)"	; 1 month
	"[ \t]+"
	"\\([0-9]+\\)"					; 2 date
	"[ \t]+"
	"[0-9]+:[0-9:]+"				; time
	"[ \t]+"
	"\\([0-9][0-9][0-9][0-9]\\)")			; 3 year
     str)
    (let ((m (nth 1 (assoc (match-string-no-properties 1 str)
			   epackage--date-month-list)))
	  (d (string-to-number (match-string-no-properties 2 str)))
	  (y (match-string-no-properties 3 str)))
      (format "%s-%02d-%02d" y m d)))))

(defun epackage-insert-file-contents (file)
  "Call `insert-file-contents' and put point after end of insert."
  ;; `insert-file-contents' does not put point after last line
  (let ((marker (if (not (eobp))
		    (make-marker))))
    (if marker
	(set-marker marker (min (1+ (point)) (point-max))))
    (insert-file-contents file)
    (goto-char (if marker
                   (1- (marker-position marker))
                 (point-max)))
    (setq marker nil)))

(defsubst epackage-buffer-remove-empty-lines ()
  "Delete empty lines from current point forward."
  (unless (eq (point-min) (point-max))
    (while (and (not (eobp))
		(re-search-forward "^[ \t\f\r]*$" nil t))
      (delete-region (line-beginning-position)
		     (if (eobp)
			 (point-max)
		       (1+ (point)))))))

(defun epackage-buffer-remove-whitespace-eol ()
  "Clear end of line whitespaces from whole buffer. Including ^L."
  (require 'whitespace) ;; Define whitespace-trailing-regexp
  (let ((whitespace-trailing-regexp
         epackage--whitespace-trailing-regexp))
    (whitespace-replace-action
     'delete-region (point-min) (point-max)
     whitespace-trailing-regexp 1)))

(defun epackage-directory-list (dir &optional exclude)
  "Return all directories under DIR.
Optionally EXCLUDE matching directories."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (not (string-match "/\\.\\.?$" elt))
                 (file-directory-p elt)
                 (or (not (stringp exclude))
                     (not (epackage-directory-list-exclude-p exclude elt))))
        (epackage-push elt list)))
    list))

(defun epackage-directory-recursive-list (dir list &optional exclude)
  "Return all directories under DIR recursively to LIST.
Exclude directories than contain file .nosearch
or whose path name matches EXCLUDE."
  (let ((dirs (epackage-directory-list dir exclude)))
    (epackage-push dir list)
    (dolist (elt dirs)
      (cond
       ((file-exists-p (concat elt "/.nosearch")))
       (t
        (epackage-push elt list)
        (epackage-directory-recursive-list elt list exclude))))
    list))

(defun epackage-lisp-file-list (list)
  "Return list of *.e files from LIST or directories."
  (let (ret
        files)
    (dolist (dir list)
      (setq files (directory-files
                   dir
                   'full-path
                   "\\.el$"))
      (setq ret (append ret files)))
    ret))

(defsubst epackage-directory-recursive-list-default (dir)
  "Return all directories under DIR recursively.
Exclude directories matching `epackage--directory-exclude-regexp'
and `epackage--directory-name'."
  (let (list)
    (epackage-directory-recursive-list
     dir
     list
     (concat epackage--directory-exclude-regexp
             "\\|/" epackage--directory-name "$"))))

(defsubst epackage-directory-recursive-lisp (dir)
  "Return all Emacs Lisp file directories under DIR recursively.
See `epackage-directory-recursive-list-default' for more information."
  (let (ret)
    (dolist (elt (epackage-directory-recursive-list-default dir))
      ;; Check remaining directories if they contain Emacs Lisp files."
      (if (directory-files elt nil "\\.el$")
          (epackage-push elt ret)))
    ret))

(defsubst epackage-files-recursive-lisp (dir &optional type exclude)
  "Return all Emacs Lisp files under DIR recursively.

Input:

   TYPE     How to return path names:
            'nopath    Only filenames.
            'relative  Path relative to DIR.
            nil        With full path.
   EXCLUDE  Regexp to exclude path names

See `epackage-directory-recursive-list-default' for more information."
  (let ((regexp (format "^%s?/?\\(.+\\)$"  ;; Trailing dir/?/?
			(regexp-quote
			 (expand-file-name dir))))
	list)
    (dolist (elt (epackage-directory-recursive-lisp dir))
      (dolist (file (directory-files elt nil "\\.el$"))
	(when (or (null exclude)
		  (not (string-match exclude file)))
	  (setq file
		(cond
		 ((eq type nil)
		  (format "%s/%s" dir file))
		 ((eq type 'relative)
		  (if (string-match regexp elt)
		      (concat (match-string 1 elt) "/" file)
		    (file-name-nondirectory file)))))
	  (epackage-push file list))))
    list))

(defun epackage-directory-packages-control-file (package type)
  "Return PACKAGE control file of TYPE.
The TYPE is car of list `epackage--layout-mapping'."
  (let ((dir (epackage-directory-package-control package))
        (file (epackage-layout-mapping-file type)))
    (if (not file)
        (epackage-error "[ERROR] Unknown TYPE argument '%s'" type)
      (cond
       ((eq type 'info)
        (format "%s/%s" dir file))
       (t
        (format "%s/%s%s" dir package file))))))

(defun epackage-file-name-install-compose (package type)
  "Return PACKAGE filenme of TYPE in `epackage--directory-name-install'.
The TYPE is car of list `epackage--layout-mapping'."
  (let ((dir (epackage-directory-install))
        (file (nth 1 (assq type epackage--layout-mapping))))
    (if (not file)
        (epackage-error "[ERROR] Unknown TYPE argument '%s'" type)
      (cond
       ((eq type 'info)
        (format "%s/%s-%s" dir package file))
       (t
        (format "%s/%s%s" dir package file))))))

(defun epackage-file-name-package-compose (package type)
  "Return PACKAGE filenme of TYPE in `epackage-directory-package-root'.
The TYPE is car of list `epackage--layout-mapping'."
  (let ((dir (epackage-directory-package-root package))
        (file (nth 1 (assq type epackage--layout-mapping))))
    (if (not file)
        (epackage-error "[ERROR] Unknown TYPE argument '%s'" type)
      (cond
       ((eq type 'info)
        (format "%s/%s/%s" dir epackage--package-control-directory file))
       (t
        (format "%s/%s%s" dir package file))))))

(defsubst epackage-git-directory-p (dir)
  "Check if there is .git under DIR. Return DIR if so."
  (let ((path (concat (file-name-as-directory dir) ".git")))
    (if (file-directory-p path)
        dir)))

(defun epackage-package-loaddefs-p (package)
  "Return file if PACKAGE autoload file exists."
  (let ((file (epackage-file-name-install-compose package 'loaddefs)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-autoload-p (package)
  "Return file if PACKAGE autolaod file exists."
  (let ((file (epackage-file-name-install-compose package 'autoload)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-enabled-p (package)
  "Return file if PACKAGE is enabled."
  (let ((file (epackage-file-name-install-compose package 'enable)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-activated-p (package)
  "Return file if PACKAGE is activated."
  (let ((file (epackage-file-name-install-compose package 'activate)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-byte-compiled-p (package)
  "Return non-nil if PACKAGE has been byte compiled."
  (let ((root (epackage-directory-package-root package))
	status)
    (when (file-directory-p root)
      (let ((list (epackage-pkg-lisp-directory package)))
	(dolist (dir list)
	  (unless status
	    (if (directory-files dir nil "\\.elc$")
		(setq status dir))))))
    status))

(defun epackage-package-installed-p (package)
  "Return non-nil if PACKAGE has been installed."
  (unless (epackage-string-p package)
    (epackage-error "arg 'package' is not a string."))
  (let ((dir (epackage-directory-package-root package)))
    (if (file-directory-p dir)
        dir)))

(defun epackage-package-downloaded-p (package)
  "Return download directory if PACKAGE has been downloaded."
  (unless (epackage-string-p package)
    (epackage-error "arg 'package' is not a string."))
  (let ((dir (epackage-directory-package-root package)))
    (if (file-directory-p dir)
        dir)))

(defun epackage-package-info-p (package)
  "Return path to `epackage--pkg-info-file-name' of PACKAGE if it exists."
  (unless (epackage-string-p package)
    (epackage-error "arg 'package' is not a string."))
  (let ((file (epackage-file-name-package-info package)))
    (if (file-exists-p file)
        file)))

(defsubst epackage-directory-sources-list ()
  "Return sources list build directory.
Location of `epackage--sources-file-name-main'."
  (epackage-directory-conf))

(defsubst epackage-sources-list-official-directory ()
  "Return sources list repository directory.
location of `epackage--sources-file-name-official'."
  (epackage-directory-package-root epackage--sources-package-name))

(defsubst epackage-file-name-sources-list-official ()
  "Return path to `epackage--sources-file-name-official'."
  (format "%s/%s"
          (epackage-sources-list-official-directory)
          epackage--sources-file-name-official))

(defsubst epackage-file-name-sources-list-main ()
  "Return path to `epackage--sources-file-name-main'."
  (format "%s/%s"
          (epackage-directory-sources-list)
          epackage--sources-file-name-main))

(defsubst epackage-directory-p (directory)
  "Check if there is a subdir `epackage--directory-name' under DIRECTORY."
  (file-directory-p
   (concat
    (file-name-as-directory directory)
    epackage--directory-name)))

(defsubst epackage-sources-list-p ()
  "Check existence of `epackage--sources-file-name-main'."
  (let ((file (epackage-file-name-sources-list-main)))
    (if (file-exists-p file)
        file)))

(defsubst epackage-sources-list-kill-buffer ()
  "Kill file buffer `epackage-file-name-sources-list-main'."
  (let ((buffer (get-file-buffer (epackage-file-name-sources-list-main))))
    (when buffer
      (with-current-buffer buffer
	(set-buffer-modified-p (not 'modified))
	(kill-buffer (current-buffer))))))

(defsubst epackage-sources-list-verify ()
  "Signal error if `epackage--sources-file-name-main' does not exist."
  (or (epackage-sources-list-p)
      (epackage-error
        "Missing file %s. Run epackage-initialize."
        (epackage-file-name-sources-list-main))))

(defsubst epackage-initialize-string ()
  "Return message string to suggest running `epackage-initialize'."
  (substitute-command-keys "Run \\[epackage-initialize]"))

(defun epackage-error-initialize (&optional message)
  "Display missing initialize error with optional MESSAGE."
  (epackage-fatal "%s"
                  (concat (if message
                              (concat message ". ")
                            ". ")
                          (epackage-initialize-string))))

(defun epackage-error-no-directory (directory &optional message)
  "If DIRECTORY does not exist, signal error with optional supplied MESSAGE."
  (unless (file-directory-p directory)
    (epackage-error-initialize
     (or message
         (format "No such directory %s" directory)))))

(defun epackage-initialize-verify (&optional message)
  "Signal error with MESSAGE if `epackage--initialize-flag' is non-nil.
This means that `epackage-initialize' has not been run."
  (unless epackage--initialize-flag
    (epackage-error-initialize message)))

(defun epackage-program-git-verify ()
  "Verify variable `epackage--program-git'."
  (when (or (not (stringp epackage--program-git))
            (not (file-exists-p epackage--program-git)))
    (epackage-error
      (substitute-command-keys
       (format
        `,(concat
           "Invalid value in epackage--program-git (%s) "
           "Run \\[epackage-initialize]")
        epackage--program-git)))))

(defun epackage-kill-buffer (buffer &optional verbose)
  "Kill BUFFER, even if modified. Do nothing if BUFFER does not exist.
If optional VERBOSE is non-nil, display progress message."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p (not 'modified))
      (epackage-verbose-message
        "Kill buffer (forced) %s" buffer-file-name)
      (kill-buffer (current-buffer)))))

(defsubst epackage-kill-buffer-list (list &optional verbose)
  "Kill LIST of buffer, even if modified.
If optional VERBOSE is non-nil, display progress message."
  (dolist (buffer list)
    (epackage-kill-buffer buffer verbose)))

(defsubst epackage-pkg-kill-buffer-force (package &optional verbose)
  "Kill all PACKAGE file buffers, even if modified.
If optional VERBOSE is non-nil, display progress message."
  (epackage-kill-buffer-list
   (epackage-pkg-buffer-list package)
   verbose))

(defun epackage-pkg-buffer-list (package)
  "Return list of opened file buffers of PACKAGE."
  (let ((regexp (regexp-quote (epackage-directory-package-root package)))
        name
        list)
    (dolist (buffer (buffer-list))
      (when (and (setq name (buffer-file-name buffer))
                 (string-match regexp name))
        (epackage-push buffer list)))
    list))

(put 'epackage-with-directory 'lisp-indent-function 1)
(put 'epackage-with-directory 'edebug-form-spec '(body))
(defmacro epackage-with-directory (dir &rest body)
  "Set `default-directory' to DIR while running BODY."
  `(let ((default-directory (file-name-as-directory ,dir))) ;Must end in slash
     ,@body))

(put 'epackage-with-binary 'lisp-indent-function 0)
(put 'epackage-with-binary 'edebug-form-spec '(body))
(defmacro epackage-with-binary (&rest body)
  "Disable all interfering `write-file' effects and run BODY."
  `(let ((version-control 'never)
         (backup-inhibited t)
         (buffer-file-coding-system 'no-conversion)
         write-file-functions
         after-save-hook)
     ,@body))

(put 'epackage-with-buffer-emacs-messages 'lisp-indent-function 0)
(put 'epackage-with-buffer-emacs-messages 'edebug-form-spec '(body))
(defmacro epackage-with-buffer-emacs-messages (&rest body)
  "Run BODY in `epackage--buffer-emacs-messages'."
  `(with-current-buffer (get-buffer epackage--buffer-emacs-messages)
     ,@body))

(put 'epackage-with-buffer-info 'lisp-indent-function 0)
(put 'epackage-with-buffer-info 'edebug-form-spec '(body))
(defmacro epackage-with-buffer-info (&rest body)
  "Run BODY in `epackage--buffer-info'.
Create `epackage--buffer-info' for BODY if it doe snot exists."
  `(with-current-buffer (get-buffer-create epackage--buffer-info)
     ,@body))

(put 'epackage-with-package-info-file 'lisp-indent-function 1)
(put 'epackage-with-package-info-file 'edebug-form-spec '(body))
(defmacro epackage-with-package-info-file (package &rest body)
  "For `epackage--package-info-file' of PACKAGE, run BODY.
Signal error if it doesn't. Variable `file' is bound during BODY.
Variable `info-file' is bound during macro.
Call `epackage-turn-on-auto-revert-mode'."
  `(let ((info-file (epackage-file-name-package-info ,package)))
     (unless (file-exists-p info-file)
       (epackage-error "Info file does not exist: %s" info-file))
     (with-current-buffer (find-file-noselect info-file)
       (epackage-turn-on-auto-revert-mode)
       ,@body)))

(put 'epackage-with-sources-list 'lisp-indent-function 0)
(put 'epackage-with-sources-list 'edebug-form-spec '(body))
(defmacro epackage-with-sources-list (&rest body)
  "Run BODY in package list buffer.
Call `epackage-turn-on-auto-revert-mode'."
  `(progn
     (epackage-turn-on-auto-revert-mode)
     (epackage-sources-list-verify)
     (with-current-buffer
         (find-file-noselect (epackage-file-name-sources-list-main))
       (epackage-turn-on-auto-revert-mode)
       ,@body)))

;;; ........................................ &functions-git-primitives ...

(defsubst epackage-git-error-handler (&optional command)
  "On Git error, show proces buffer and signal error incuding COMMAND."
  (display-buffer epackage--process-output)
  (epackage-error "Git %scommand error"
                  (if command
                      (format "'%s' " command)
                    "")))

(put 'epackage-with-process-output 'lisp-indent-function 0)
(put 'epackage-with-process-output 'edebug-form-spec '(body))
(defmacro epackage-with-process-output (&rest body)
  "Run BODY in `epackage--process-output'."
  `(with-current-buffer (get-buffer-create epackage--process-output)
     ,@body))

(defsubst epackage-git-goto-last-output-start ()
  "Move to last marker --CMD START-- and the following line.
Used inside `epackage-with-process-output'."
  (goto-char (point-max))
  (re-search-backward "--CMD START--")
  (forward-line 1))

(put 'epackage-with-last-git-output 'lisp-indent-function 0)
(put 'epackage-with-last-git-output 'edebug-form-spec '(body))
(defmacro epackage-with-last-git-output (&rest body)
  "Run BODY at start of last git output."
  `(epackage-with-process-output
     (epackage-git-goto-last-output-start)
     ,@body))

(defsubst epackage-git-command-ok-p (status)
  "Return non-nil if command STATUS was ok."
  (zerop status))

(defun epackage-git-command-process (&rest args)
  "Run git command with ARGS and send output to `epackage--process-output'."
  (epackage-program-git-verify)
  (epackage-with-debug
    (let ((dir default-directory)) ;; buffer local variable
      (epackage-with-process-output
        (goto-char (point-max))
        (insert
         (format "debug: [%s] git %s\n"
                 dir
                 (prin1-to-string args))))))
  (epackage-with-process-output
    (goto-char (point-max))
    (insert "--CMD START--\n"))
  (apply 'call-process
         epackage--program-git
         (not 'infile)
         (get-buffer-create epackage--process-output)
         (not 'display)
         args))

(put 'epackage-with-git-command 'lisp-indent-function 2)
(put 'epackage-with-git-command 'edebug-form-spec '(body))
(defmacro epackage-with-git-command (dir verbose &rest args)
  "Run git command in DIR, under VERBOSE with ARGS.
If VERBOSE is non-nil, display progress message."
  `(epackage-with-directory ,dir
     (if ,verbose
         (epackage-message
           "Running 'git %s' in %s ..."
           (mapconcat #'concat (list ,@args) " ")
           (abbreviate-file-name ,dir)))
     (unless (epackage-git-command-ok-p
	      (epackage-git-command-process
	       ,@args))
       (epackage-git-error-handler))
     (if ,verbose
	 (epackage-message
	  "Running 'git %s' in %s ...done"
	  (mapconcat #'concat (list ,@args) " ")
	  (abbreviate-file-name ,dir)))))

(put 'epackage-with-git-config 'lisp-indent-function 1)
(put 'epackage-with-git-config 'edebug-form-spec '(body))
(defmacro epackage-with-git-config (package &rest body)
  "Read Git 'config' of PACKAGE and run BODY.
Kill buffer after BODY."
  `(let ((config-file (epackage-directory-package-git-config package)))
     (if (not (file-exists-p config-file))
         (epackage-error "No Git config file: %s" config-file)
       (prog1
           (with-current-buffer (find-file-noselect config-file)
             ,@body)
         (kill-buffer (get-file-buffer config-file))))))

;;; ............................................. &functions-info-file ...

(defun epackage-field-goto (field)
  "Go to the beginning of FIELD only if it exists."
  (let ((case-fold-search t)
	end)
    (goto-char (point-min))
    (when (re-search-forward (concat "^" (regexp-quote field) ":") end t)
      (if (looking-at " ")
	  (forward-char 1)
	(insert " "))
      t)))

(defsubst epackage-field-fetch-value (field)
  "Like `mail-fetch-field', but return FIELD's value only if it exists.
If FIELD is empty or does not exist, return nil."
  (let ((value (mail-fetch-field field)))
    (if (epackage-string-p value)
        value)))

(defsubst epackage-field-forward ()
  "Search for 'field:' forward. Submatch 1 contains field name."
  (re-search-forward "^\\([^ \t\r\n]+\\):" nil t))

(defsubst epackage-field-backward ()
  "Search for 'field:' backward. Submatch 1 contains field name."
  (re-search-backward "^\\([^ \t\r\n]+\\):" nil t))

(defsubst epackage-field-name (&optional region)
  "Return field name near point.
If optional REGION is non-nil, return position of field '(beginning end)."
  (save-excursion
    (when (or (looking-at "^\\([^:]+\\):")
	      (epackage-field-backward))
      (if region
	  (list (match-beginning 1)
		(match-end 1))
	(match-string-no-properties 1)))))

(defun epackage-field-narrow-to (&optional full) ;; FIXME: implement
  "Narrow the buffer to the field data area of the current line.
If optional FULL is non-nil, include field in narrowed region."
  (let ((region (epackage-field-name 'region)) ;; moves backward
	beg
	end)
    (unless region
      (error "Field not found. Cannot narrow."))
    (save-excursion
      (goto-char (line-end-position))
      (setq end (or (epackage-field-forward)
		    (re-search-forward "^[ \t]*$" nil t) ; first empty line
		    (point-max))))
    (cond
     (full
      (setq beg (car region)))
     (t
      (setq beg (1+ (nth 1 region)))	; after colon(:)
      (goto-char beg)
      (if (looking-at " ")		; "field: "
	  (setq beg (1+ beg)))))
    (narrow-to-region beg end)))

(defsubst epackage-field-set (field value &optional overwrite)
  "Set FIELD if it exists to VALUE. If OVERWITE is non-nil, replace old."
  (let ((old (epackage-field-fetch-value field)))
    (when (or overwrite
	      (null old))
      (or (epackage-field-goto field)
	  (epackage-error "No such field to goto: %s" field))
      (when (and overwrite old)
	(delete-region (point) (+ (point) (length old))))
      (if value
	  (insert value))
      t)))

(defun epackage-field-fetch-description ()
  "Return content of 'Description:' '(\"short desc\" \"long desc\").
Remove 1 space indentation and paragraph separator(.) characters."
  (let ((str (epackage-field-fetch-value "Description"))
        short
        long)
    (if (string-match "^\\(.+\\)$" str)
        (setq short (match-string 1 str)))
    ;; The \177 is just arbitrary code that will not appear in text.
    (if (string-match "^\\( [^\177]+\\)" str)
        (setq long (match-string 1 str)))
    ;; Remove one-space indentation
    (setq long (replace-regexp-in-string "^ " "" long))
    ;; Remove pragraph separators.
    (setq long (replace-regexp-in-string "^\\.[ \t]*$" "" long))
    (list short long)))

(defun epackage-field-fetch-status ()
  "Return content of 'Status:'. Items are separated by spaces."
  (let ((str (epackage-field-fetch-value "Status")))
    (when str
      (replace-regexp-in-string "[ \t\r\n]+" " " str))))

(defun epackage-depends-parse-collect ()
  "Collect items from buffer prepared by `epackage-depends-parse-buffer'.

Return:
    '((PACKAGE [OP] [VERSION]) ...)

Examples:
    '((emacs \">=\" \"22\"))
    '((xemacs \"!\" nil))."
  (let ((regexp
         `,(concat
            "\\([a-z][^ ,|!()<>=\t\r\n(]+\\)"
            "\\(?:[ \t]*"
            "([ \t]*"
            "\\(\\(?:!\\|>=\\|<=\\)\\)" ;open paren, item 1
            "[ \t]*"
            "\\([^)]*\\))"              ;close paren, item 2
            "\\)?"))
        list
        value)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq value (match-string-no-properties 3))
      (epackage-push
        (list (match-string-no-properties 1)
              (match-string-no-properties 2)
              (if (and (stringp value)
                       (string= "" value))
                  nil
                value))
        list))
    list))

(defsubst epackage-depends-parse-buffer-prepare ()
  "Arrange all depends entries on their own lines."
  (goto-char (point-min))
  (while (re-search-forward "[ \t\r\n]*,[ \t\r\n]*" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  ;; NOTE: We may use "\n" as field separator. The 1st line must have it.
  (insert "\n"))

(defsubst epackage-depends-parse-buffer ()
  "Parse depends from current buffer. Nothing else must be there.
See `epackage-depends-parse-collect' for returned value format."
  ;; put items on their own lines
  (epackage-depends-parse-buffer-prepare)
  (epackage-depends-parse-collect))

(defsubst epackage-depends-parse-string (string)
  "Parse depends STRING.
See `epackage-depends-parse-collect' for returned value format."
  (with-temp-buffer
   (insert string)
   (epackage-depends-parse-buffer)))

(defsubst epackage-field-fetch-depends ()
  "Return preformatted content of 'Depends:' field.
See `epackage-depends-parse-collect' for returned value format."
  (let ((str (epackage-field-fetch-value "Depends")))
    (if (stringp str)
        (epackage-depends-parse-string str))))

(defsubst epackage-pkg-info-fetch-field (package field)
  "Read PACKAGE and raw information from FIELD.
If field is empty or does not exist, return nil."
  (epackage-with-package-info-file package
    (epackage-field-fetch-value field)))

(defsubst epackage-pkg-info-fetch-field-depends (package)
  "Read PACKAGE and information field 'Depends:' (preformatted).
See `epackage-depends-parse-collect' for returned value format."
  (epackage-with-package-info-file package
    (epackage-field-fetch-depends)))

(defsubst epackage-pkg-info-fetch-field-status (package)
  "Read PACKAGE and information field 'Status:'."
  (epackage-with-package-info-file package
    (epackage-field-fetch-status)))

(defsubst epackage-pkg-info-status-p (package regexp)
  "Check if PACKAGE's status match REGEXP.
Return subexpression 1, or 0; the one that exists."
  (let ((str (epackage-pkg-info-fetch-field-status package)))
    (if (string-match regexp (or str ""))
        (or (match-string-no-properties 1 str)
            (match-string-no-properties 0 str)))))

(defsubst epackage-pkg-info-status-unmaintained-p (package)
  "Check if PACKAGE is marked unmaintained."
  (epackage-pkg-info-status-p package "unmaintained"))

(defsubst epackage-pkg-info-status-emacs-p (package)
  "Check if PACKAGE is included in core Emacs."
  (epackage-pkg-info-status-p
   package "core-emacs-?\\([0-9.]+\\)?"))

(defsubst epackage-pkg-info-status-broken-p (package)
  "Check if PACKAGE is marked broken."
  (epackage-pkg-info-status-p
   package "broken"))

(defsubst epackage-pkg-info-status-unsafe-p (package)
  "Check if PACKAGE is marked broken."
  (epackage-pkg-info-status-p
   package "unsafe"))

;; Autloaded because this is used in hook
;;;###autoload
(defun epackage-pkg-info-p ()
  "Retrn non-nil if buffer file looks like `epackage--pkg-info-file-name'."
  (and buffer-file-name
       (string= (file-name-nondirectory buffer-file-name)
                epackage--pkg-info-file-name)
       (save-excursion
         (goto-char (point-min))
         (mail-fetch-field "Package"))))

(defsubst epackage-pkg-info-status-warnings (package)
  "Return warnings for PACKAGE."
  (let ((str (epackage-pkg-info-fetch-field-status package))
        core
        list
        status)
    (when str
      (when (string-match "core[^ \t]+" str)
        ;; Already included in emacs?
        ;; FIXME: Perhaps add version check
        (setq core (match-string 0 str))
        (if (and (featurep 'emacs)
                 (string-match "^emacs" core))
            (epackage-push core list)))
      (if (string-match "unmaintained" str)
          (epackage-push "unmaintained" list))
      (if (string-match "broken" str)
          (epackage-push "broken" list))
      (if list
          (setq status (mapconcat
                        #'concat
                        (epackage-sort list)
			" ")))
      status)))

(defun epackage-read-file-content-regexp (file)
  "Return concatenated regexps from FILE.
Empty lines and comments on their own lines started with ';' are
ignored"
  (let (str
	regexp)
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents-literally file)
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*\\([^ ;\t\r\n].*\\)" nil t)
	  (setq str (match-string-no-properties 1))
	  (if regexp
	      (setq regexp (format "%s\\|%s" regexp str))
	    (setq regexp str)))))
    regexp))

(defun epackage-pkg-ignore-file-content (package)
  "Return content of ignore file from PACKAGE."
  (let ((dir (epackage-directory-package-root package))
	(file (epackage-file-name-package-compose package 'ignore)))
    (when (and (file-directory-p dir)  ;; FIXME: redundant?
	       (file-exists-p file))
	(epackage-read-file-content-regexp file))))

(defun epackage-pkg-lisp-directory (package)
  "Return list of lisp directories of PACKAGE."
  (let ((dir (epackage-directory-package-root package))
	(file (epackage-file-name-package-compose package 'lisp))
	elt
	list)
    (when (file-directory-p dir)
      (if (not (file-exists-p file))
	  (setq list (list dir))
	(with-temp-buffer
	  (insert-file-contents-literally file)
	  (goto-char (point-min))
	  (setq dir (file-name-as-directory dir))
	  (while (re-search-forward "^[ \t]*\\([^ ;\t\r\n]+\\)" nil t)
	    (setq elt (format "%s%s" dir (match-string-no-properties 1)))
	    (epackage-push elt list)))))
    list))

;;; ................................................... &functions-git ...

(defun epackage-git-buffer-fetch-field (tag field)
  "Read configuration TAG.FIELD from current buffer.
TAG is a regexp. An example

  (epackage-git-buffer-fetch-field \"remote.*origin\" \"url\")

Would match:

\[remote \"origin\"]
        url = git://example.com/package.git"
  (goto-char (point-min))
  (when (re-search-forward
         (format "^[ \t]*\\[.*%s.*\\]" tag)
         nil t)
    ;; Stop at next TAG
    (let ((max (or (save-excursion
                     (if (re-search-forward "^[ \t]*\\[" nil t)
                         (line-beginning-position)))
                   (point-max))))
      (when (re-search-forward
             (format "^[ \t]*%s[ \t]*=[ \t]*\\(.*[^ \t\r\n]\\)"
                     (regexp-quote field))
             max t)
        (match-string-no-properties 1)))))

(defsubst epackage-git-config-fetch-field (package tag field)
  "Read PACKAGE's configuration file: TAG.FIELD."
  (epackage-with-git-config package
    (epackage-git-buffer-fetch-field tag field)))

(defsubst epackage-git-branch-list-master-p (list)
  "Return non-nil if current branch LIST indicates master as current branch."
  (string-match
   ;; At the beginning, or at end, or in the middle by spaces
   "\\(?:^\\| \\)\\*master\\(?: \\|$\\)"
   (mapconcat #'concat list " ")))

(defun epackage-git-command-tag-list (dir &optional verbose)
  "Run 'git tag -l' in DIR.
If optional VERBOSE is non-nil, display progress message.

Return:
    List of branches. The current branch has mark '*' at front."
  (epackage-with-git-command dir verbose
    "tag" "-l")
  (epackage-with-last-git-output
    (let (list)
      (while (re-search-forward "^\\([^ \t\r\n]*+\\)" nil t)
        (epackage-push (match-string-no-properties 1)
                       list))
      list)))

(defun epackage-git-command-init (dir &optional verbose)
  "Run 'git init' in DIR.
If optional VERBOSE is non-nil, display progress message.

Return:
    t or nil   according to success."
  (epackage-with-git-command dir verbose
    "init")
  (epackage-with-last-git-output
    (re-search-forward "^\\(Reinitialized\\|Initialized\\)" nil t)))

(defun epackage-git-command-status-parse-buffer-limit ()
  "Return next limit point of 'status' section in current buffer.
The limist are those of top level heading:

    # Changes to be committed:
    # Changed but not updated:
    # Untracked files:

See manual page of git-status(1)."
  (save-excursion
    (if (or (re-search-forward "^# Changes to be committed:" nil t)
            (re-search-forward "^# Changed but not updated:" nil t)
            (re-search-forward "^# Untracked files:" nil t))
        (line-beginning-position)
      (point-max))))

(defun epackage-git-command-status-parse-generic (heading match)
  "Search for HEADING regexp and if found, collect MATCH of level 1."
  (let (list)
    (when (re-search-forward heading nil t)
      (let ((max (epackage-git-command-status-parse-buffer-limit)))
        (while (re-search-forward match max t)
          (epackage-push (match-string-no-properties 1) list)))
      list)))

(defsubst epackage-git-command-status-parse-buffer-modified ()
  "Parse list of modified files from current point forward."
  ;; # Changed but not updated:
  ;; #   (us    e "git add <file>..." to update what will be committed)
  ;; #   (use "git checkout -- <file>..." to discard
  ;; #
  ;; #    modified:   ChangeLog
  (epackage-git-command-status-parse-generic
   "^# Changed but not updated:"
   "^#[ \t]+modified:[ \t]+\\(.*[^ \t\r\n]\\)"))

(defsubst epackage-git-command-status-parse-buffer-untracked ()
  "Parse list of untracked files from current point forward."
  ;; # Untracked files:
  ;; #   (use "git add <file>..." to include in what will be committed)
  ;; #
  ;; # doc/index.html
  (epackage-git-command-status-parse-generic
   "^# Untracked files:"
   "^#\t+\\([^ \t\r\n].*\\)"))

(defun epackage-git-command-status-parse-buffer-commit ()
  "Parse list of untracked files from current point forward."
  ;; # Changes to be committed:
  ;; #   (use "git reset HEAD <file>..." to unstage)
  ;; #
  ;; # ChangeLog
  (epackage-git-command-status-parse-generic
   "^# Changes to be committed:"
   "^#\t+\\([^ \t\r\n].*\\)"))

(defsubst epackage-git-command-status-modified-parse-main ()
  "Parse list of modified files from command output buffer."
  (epackage-with-last-git-output
    (epackage-git-command-status-parse-buffer-modified)))

(defsubst epackage-git-command-status-untracked-parse-main ()
  "Parse list of modified files from command output buffer."
  (epackage-with-last-git-output
    (epackage-git-command-status-parse-buffer-untracked)))

(defsubst epackage-git-command-status-commit-parse-main ()
  "Parse list of modified files from command output buffer."
  (epackage-with-last-git-output
    (epackage-git-command-status-parse-buffer-commit)))

(defun epackage-git-command-branch-parse-buffer ()
  "Parse list of branches from current point forward."
  (let (list)
    (while (re-search-forward "^\\(\\*?\\) +\\([^ \t\r\n]*\\)" nil t)
      (epackage-push (concat
                      (match-string-no-properties 1)
                      (match-string-no-properties 2))
                     list))
    list))

(defsubst epackage-git-command-branch-parse-main ()
  "Parse list of branched from command output buffer."
  (epackage-with-last-git-output
    (epackage-git-command-branch-parse-buffer)))

(defmacro epackage-git-command-branch-with-args
  (dir &optional verbose &rest args)
  "In DIR, run in optional VERBOSE mode 'git branch ARGS'.
If optional VERBOSE is non-nil, display progress message."
  `(epackage-with-git-command ,dir ,verbose "branch" ,@args))

(defun epackage-git-command-branch-list (dir &optional verbose arg)
  "In DIR, run in optional VERBOSE mode 'git branch [ARG]'.
If optional VERBOSE is non-nil, display progress message.

Return:
    List of branch names."
  ;; FIXME improve macro to handle both cases
  (if arg
      (epackage-with-git-command dir verbose
        "branch" arg)
    (epackage-with-git-command dir verbose
      "branch"))
  (epackage-git-command-branch-parse-main))

(defun epackage-git-branch-list-current-branch (list)
  "Return name makred with '*' from branch LIST; without the '*'."
  (let (ret)
    (dolist (elt list)
      (when (and (not  ret)
                 (string-match "^\\*\\(.+\\)" elt))
        (setq ret (match-string-no-properties 1 elt))))
    ret))

(defsubst epackage-git-command-branch-current-name (dir &optional verbose)
  "Run 'git branch' in DIR and return active branch name.
If optional VERBOSE is non-nil, display progress message."
  (epackage-git-branch-list-current-branch
   (epackage-git-command-branch-list dir verbose)))

(defun epackage-git-command-current-sha (dir &optional verbose)
  "Run 'git rev-parse HEAD' in DIR.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "rev-parse" "HEAD"))

(defun epackage-git-command-checkout-force-head (dir &optional verbose)
  "Run 'git checkout -f HEAD' in DIR.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "checkout" "-f" "HEAD"))

(defun epackage-git-command-pull (dir &optional verbose)
  "Run 'git pull' in DIR.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "pull"))

(defun epackage-git-command-fetch (dir &optional verbose)
  "Run 'git fetch' in DIR.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "fetch"))

(defun epackage-git-command-clone (url dir &optional verbose)
  "Run 'git clone URL DIR' in VCS package directory vault.
If optional VERBOSE is non-nil, display progress message."
  (let ((name (epackage-file-name-basename dir))
        (dir-before (epackage-file-name-directory-previous dir)))
    (epackage-require-ssh url)
    (epackage-with-git-command dir-before verbose
      "clone" url name)))

(defun epackage-git-command-status (dir &optional verbose)
  "Run 'git status' in DIR.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "status"))

(defun epackage-git-status-data (dir &optional verbose)
  "Run `epackage-git-command-status' in DIR and return data.
If optional VERBOSE is non-nil, display progress message.

Return:

    '((modified '(FILE ...))
      (untracked '(FILE ...))
      (commit '(FILE ...)))."
  (epackage-git-command-status dir verbose)
  (let (list
        data)
    (if (setq data (epackage-git-command-status-modified-parse-main))
        (epackage-push (list 'modified data) list))
    (if (setq data (epackage-git-command-status-untracked-parse-main))
        (epackage-push (list 'untracked data) list))
    (if (setq data (epackage-git-command-status-commit-parse-main))
        (epackage-push (list 'commit data) list))
    list))

(defun epackage-git-status-clean-p (package)
  "Return non-nil if PACKAGE's VCS directory is clean.
No pending commits and no modified files."
  (let ((dir (epackage-directory-package-root package))
        list)
    (when (file-directory-p dir)
      (setq list (epackage-git-status-data dir))
      (if (and (not (memq 'commit list))
               (not (memq 'modified list)))
          t))))

(defun epackage-git-master-p (package)
  "Return non-nil if PACKAGE's VCS branch is master."
  (let ((dir (epackage-directory-package-root package)))
    (when (file-directory-p dir)
      (let ((list (epackage-git-command-branch-list dir)))
        (epackage-git-branch-list-master-p list)))))

;;; ................................................ &functions-status ...

(defun epackage-status-install-files (package)
  "Return list of currently installed files for PACKAGE."
  (let ((dir    (epackage-directory-install))
	(regexp (format "%s-.*\\.el$" (regexp-quote package)))
	list)
    (directory-files
     dir
     (not 'full-path)
     regexp)))

(defun epackage-config-status-of-packages (type)
  "Return packages of TYPE of `epackage--layout-mapping'."
  (let* ((dir      (epackage-directory-install))
         (template (or (nth 1 (assq type epackage--layout-mapping))
                       (error
                        `,(concat
                           "Invalid function arg TYPE: %s"
                           "See `epackage--layout-mapping'.")
                        type)))
         (regexp   (concat (regexp-quote template) "$"))
         (match    (concat "\\(.+\\)" regexp))
         list)
    (epackage-initialize-verify
      "Not initialized. Can't use epackage--directory-name-install")
    (dolist (elt (directory-files
                  dir
                  (not 'full-path)
                  regexp))
      (if (string-match match elt)
          (add-to-list 'list (match-string 1 elt))))
    (nreverse list)))

(defsubst epackage-status-enabled-packages ()
  "Return list of packages in `epackage-directory-install'."
  (epackage-config-status-of-packages 'enable))

(defsubst epackage-status-activated-packages ()
  "Return list of packages in `epackage-directory-install'."
  (epackage-config-status-of-packages 'activate))

(defun epackage-status-downloaded-packages ()
  "Return list of packages in `epackage--directory-name-pkg'."
  (let ((dir (epackage-directory-packages))
        list)
    (epackage-initialize-verify "Can't use epackage--directory-name-pkg")
    (dolist (elt (directory-files
                  dir
                  (not 'full-path)))
      (unless (string-match "^00\\|\\." elt)
        (add-to-list 'list elt)))
    (nreverse list)))

(defsubst epackage-status-installed-packages ()
  "Return list of packages in `epackage-directory-install'."
  ;; We don't care autoloads, because 'enable' installation is a
  ;; REQUIREMENT for epackages.
  (let ((list (epackage-config-status-of-packages 'enable)))
    ;; FIXME: is there union() outside of CL that we could use?
    (dolist (elt (epackage-config-status-of-packages 'activate))
      (epackage-push elt list))
    list))

(defun epackage-status-not-installed-packages ()
  "Return list of packages in `epackage-directory-packages'.
Those that are not installed in `epackage-directory-install'."
  (let ((installed (epackage-status-installed-packages))
        (downloaded (epackage-status-downloaded-packages))
        list)
    (dolist (package downloaded)
      (unless (member package installed)
        (epackage-push package list)))
    (nreverse list)))

(defun epackage-package-status-actions (package)
  "Return current status of installed package.
See `epackage--download-action-list'.

Returns:
  '(KEYWORD ...)."
  (let (list)
    ;; Order if the statements matter: keep 'list' in alphabetical order
    (if (epackage-package-byte-compiled-p package)
	(epackage-push 'compile list))
    (if (epackage-package-enabled-p package)
	(epackage-push 'enable list))
    (if (epackage-package-activated-p package)
	(epackage-push 'activate list))
    list))

;;; ............................................. epackage development ...

;; Copy of epackage-with-command-line
(put 'epackage-with-command-line 'lisp-indent-function 0)
(put 'epackage-with-command-line 'edebug-form-spec '(body))
(defmacro epackage-with-command-line (&rest body)
  "Loop `command-line-args-left', run BODY. Variable `item' is bound."
  `(let ((debug-on-error t))
     (dolist (item command-line-args-left)
       ,@body)))

;; Copy of tinylisp-file-name-add-suffix
(defsubst epackage-file-name-add-suffix (file suffix)
  "Convert FILE.EXT into FILE-SUFFIX.EXT."
  (let ((ext (file-name-extension file)))
    (format "%s-%s.%s"
            (file-name-sans-extension file)
            suffix
            ext)))

;; Copy of tinylisp-directory-file-list
(defun epackage-directory-file-list (dir &optional exclude)
  "Return list of Emacs Lisp files. Optionally EXCLUDE by regexp."
  (let (list)
    (dolist (elt (directory-files dir 'full "\\.el$"))
      (when (and (not (string-match "[#~]" elt))
		 (or (null exclude)
		     (not (string-match exclude elt))))
        (push elt list)))
    list))

;; Copy of tinylisp-autoload-write-loaddefs-file (simplified)
(defun epackage-autoload-write-loaddefs-file (file dest &optional verbose)
  "Write ###autoload from FILE to DEST. VERB.
If optional VERBOSE is non-nil, display progress message."
  (let ((generated-autoload-file dest))
      (with-current-buffer (find-file-noselect dest)
	(when buffer-auto-save-file-name
	  (if (file-exists-p buffer-auto-save-file-name)
	      (delete-file buffer-auto-save-file-name))
	  (auto-save-mode -1))
        (goto-char (point-max))
        (let ((point (point)))
          (generate-file-autoloads file)
          ;;  was something inserted?
          (cond
           ((eq (point) point)
            (if verbose
                (epackage-message
                  "[NOTE] No autoload definitions in %s" file)))
           (t
            ;; DVCS git does not necessarily like EOL whitespace or ^L
            (epackage-buffer-remove-whitespace-eol)
            (epackage-save-buffer)
            (kill-buffer (current-buffer))))))))

;; Copy of tinylisp-autoload-generate-loaddefs-file-list
(defun epackage-autoload-generate-loaddefs-file-list
  (file list &optional verbose)
  "Generate to FILE all loaddefs from LIST of files.
If optional VERBOSE is non-nil, display messages."
  (dolist (elt list)
    (epackage-autoload-write-loaddefs-file elt file verbose)))

;; Copy of tinylisp-batch-autoload-generate-loaddefs-file
(defun epackage-batch-autoload-generate-loaddefs-file (&optional suffix)
  "Call `epackage-autoload-write-loaddefs-file' for `command-line-args-left'.
The first argument is the the destination file where loaddefs are stored."
  (let (dest)
    (epackage-with-command-line
      (if dest
          (epackage-autoload-write-loaddefs-file
           item
           dest
           'verbose)
        (setq dest item)))))

;; Copy of tinylisp-autoload-generate-loaddefs-dir
(defun epackage-autoload-generate-loaddefs-dir
  (dir file &optional exclude verbose)
  "Generate loaddefs from DIR to FILE.
Optionally EXCLUDE files by regexp.
If VERBOSE is non-nil, display informational messages."
  (interactive
   "FDLoaddefs from dir: \nFLoaddefs to file: \nsFile ignore regexp: ")
  (let ((regexp "\\(?:loaddef\\|autoload\\).*\\.el\\|[#~]")
        list)
    (if (and (stringp exclude)
             (string-match "^[ \t\r\n]*$" exclude))
        ;; Interactive, no answer. Use default.
        (setq exclude regexp))
    (when (setq list (epackage-directory-file-list dir exclude))
      (if (file-exists-p file)
          (delete-file file))
      (let ((buffer (get-file-buffer file)))
	(when buffer
	  (with-current-buffer buffer
	    (set-buffer-modified-p nil))
	  (kill-buffer buffer)))
      (epackage-autoload-generate-loaddefs-file-list
       file
       list
       (or verbose (interactive-p))))))

;; Copy of ti::package-autoload-create-on-file
(defun epackage-autoload-create-on-file (file buffer)
  "Read FILE and write autoload statements to BUFFER.

Notes:

  Doesn't read ###autoload tags.
  Detects functions by regexps.

Input:

  FILE      Emacs Lisp file to read
  BUFFER    to insert autoloads."
  (let ((fn     (file-name-nondirectory file))
        (regexp `,(concat
                   "^(\\("
                   "defun[*]?"
                   "\\|defmacro[*]?"
                   "\\|defsubst"
                   "\\|defun-maybe"
                   "\\|defsubst-maybe"
                   "\\|defmacro-maybe"
                   "\\|define-compilation-mode"
                   "\\|define-derived-mode"
                   "\\|define-generic-mode"
                   "\\|define-global-minor-mode"
                   "\\|define-globalized-minor-mode"
                   "\\|define-minor-mode"
                   "\\|easy-mmode-define-global-mode"
                   "\\|easy-mmode-define-minor-mode"
                   "\\)"
                   "[ \t]+\\([^ \t\n(]+\\)[ \t]*"))
        list
        args
        func
        type
        str
        iact
        point
        read-buffer
	save-point
        tmp)
    ;;   We want to say (autoload 'func "pacakge" t t)
    ;;   and not        (autoload 'func "pacakge.el" t t)
    ;;   so that .elc files can be used.
    (if (string-match "\\(.*\\).el" fn)
        (setq fn (match-string 1 fn)))
    (unless (setq read-buffer (find-buffer-visiting file))
      (setq read-buffer (setq tmp (find-file-noselect file))))
    (with-current-buffer read-buffer
      (setq save-point (point))
      ;; Can't use forward-sexp
      (unless (string-match "lisp" (symbol-name major-mode))
        (let (emacs-lisp-mode-hook) ;; Run no hooks
          (if emacs-lisp-mode-hook  ;; Quiet ByteCompiler "unused var"
              (setq emacs-lisp-mode-hook nil))
          (emacs-lisp-mode)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq iact nil                  ;interactive flag
              args nil
              ;; match (match-string 0)
              type (match-string 1)
              func (match-string 2))
        (when (and func
                   (progn
                     (goto-char (goto-char (match-end 0)))
                     (when (search-forward "(" nil t)
                       (setq point (point))
                       (backward-char 1)
                       (forward-sexp 1)
                       (backward-char 1)
                       (setq
                        args
                        (replace-regexp-in-string
                         "[ \t]+" " "
                         (subst-char-in-string
                          ;;  Convert multiline args to one line.
                          ?\n ?\ (buffer-substring-no-properties
				  point (point)) ))))))
          (if (or (string-match "define.*mode" type)
                  (re-search-forward
                   "[ \t\n]+([ \t]*interactive"
                   (save-excursion (end-of-defun) (point))
                   t))
              (setq iact "t"))
          (cond
           ((null args)
            (setq args (format ";; %-36s <args not known>\n" func)))
           ((string= args "")
            (setq args (format ";; %s\n" func)))
           ((> (length args) 32)
            (setq args (format ";; %-15s %s\n" func args)))
           (t
            (setq args (format ";; %-36s %s\n" func args))))
          (push (list func args) list)
          ;; (autoload FUNCTION FILE &optional DOCSTRING INTERACTIVE TYPE)
          (setq str (format "(autoload '%-36s %s \"\" %s%s)%s\n"
                            func
                            (format "\"%s\""
                                    (file-name-sans-extension fn))
                            (or iact "nil")
                            (if (string-match "defmacro" type )
                                " 'macro" "")
                            (if (string= type "defsubst")
                                (format ";;%s" type) "")))
          (epackage-append-to-buffer buffer str)
          (setq iact "t")))
      (if tmp			        ; We loaded this to Emacs, remove it
          (kill-buffer tmp)
	(goto-char save-point))		; Restore position
      buffer)))

(defun epackage-autoload-write-autoload-files (&rest args)
  "Not implemented yet".
  ;; FIXME
  nil)

(defun epackage-autoload-generate-autoload-file-list
  (file list &optional verbose)
  "Generate to FILE all autoload definitions from LIST of files.
If optional VERBOSE is non-nil, display progress message."
  (dolist (elt list)
    (epackage-autoload-write-autoload-files elt file verbose)))

;; copy of ti::package-autoload-create-on-directory
(defun epackage-autoload-create-on-directory
  (dir &optional buffer exclude)
  "Create autoloads from lisp files in DIR to current buffer.
Optionally put results to BUFFER.

Note:

  Doesn't care about ###autoload tags; reads only functions.

Input:

  DIR     Directory to read *.el files.
  BUFFER  Optional. Defaults to `current-buffer'.
  EXCLUDE Optional. Exlude files."
  (let ((files (directory-files
                dir
                'full
                "\\.el$"))
	(regexp "\\(?:loaddef\\|autoload\\)\\|[#~]"))
    (if exclude
	(setq regexp (concat regexp "\\|" exclude)))
    (dolist (file files)
      (unless (string-match regexp file)
	(epackage-autoload-create-on-file
	 file
	 (or buffer
	     (current-buffer)))))
    files))

(defun epackage-make-directory (directory &optional no-question error)
  "Create directory, optionally with NO-QUESTION. Signal error if denied.
If optional ERROR is non-nil, signal error if DIRECTORY was not created."
  (unless (file-directory-p directory)
    (cond
     ((or no-question
	  (y-or-n-p (format "Create directory %s " directory)))
      (make-directory directory)
      t)
     (t
      (if error
	  (epackage-error "Directory creation not confirmed: %d" directory))
      nil))))

(defun epackage-devel-licence-list-http-get ()
  "Retrive list of valid licence indentifiers.
Connects to `epackage--info-licence-list-url'.
Return buffer."
  (interactive)
  (let* ((url epackage--info-licence-list-url)
	 (dest (get-buffer-create "*epackage-licenses-spdx.org*"))
	 (buffer (url-retrieve-synchronously url)))
    (if (not buffer)
        (epackage-error "Failed to connect to %s" url)
      buffer)))

(defun epackage-devel-licence-list-http-parse ()
  "Parse result of `epackage-devel-licence-list-http-get'."
  ;; <td about="./AFL-1.2" typeof="spdx:License">
  (let ((regexp `,(concat
		   "<td +about=\"\\./\\([^ \"\t\r\n]+\\)\" *"
		   "typeof=\"spdx:License\">"))
	str
	list)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq str (match-string-no-properties 1))
      ;; Drop dot-zero "*.0"
      (if (string-match "^\\(.+\\)\\(?:\\.0\\)+$" str)
	  (setq str (match-string 1 str)))
      (epackage-push str list))
    (nreverse list)))

(defun epackage-devel-licence-list-http-insert (&optional raw)
  "Insert list of valid licences to current point.
If RAW is non-nil, do not surround lincense names with quotes.
Note: Connects to `epackage--info-licence-list-url'."
  (interactive "P")
  (let ((buffer (epackage-devel-licence-list-http-get))
	list)
    (when buffer
      (with-current-buffer buffer
	(setq list (epackage-devel-licence-list-http-parse)))
      (dolist (str list)
	(if raw
	    (insert str "\n")
	  (insert (format "\"%s\"\n" str)))))))

(defun epackage-devel-generate-compile-write-file (file list)
  "Write to FILE commands to compile LIST of files."
  (when list
    (with-temp-buffer
      (insert
       (format
	epackage--layout-template-compile
	(mapconcat
	 (lambda (x)
	   (format "\"%s\"" x))
	 (epackage-sort list)
	 "\n")))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (indent-sexp)
      (epackage-write-region (point-min) (point-max) file)
      t)))

(defun epackage-devel-generate-uninstall
  (package root &optional dir recursive verbose)
  "Generate PACKAGE examples file relative to ROOT from DIR.
The file is stored under directory ROOT/`epackage--directory-name'.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    DIR   	Optional. Emacs Lisp package directories. This can be a
                one string or list of strings. Defaults to ROOT.
    RECURSIVE   Optional. If non-nil, read all *.el files under DIR.
		Interactive prefix.
    VERBOSE	Optional. If non-nil, display verbose messages.
                Interactive call sets this."
  (interactive
   (let ((root (read-directory-name "Epackage root dir: ")))
     (list
      package
      root
      root
      'interactive)))
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp dir))
	    (not (file-directory-p dir)))
    (epackage-error "Drectory DIR does not exist: %s" dir))
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Directory ROOT does not exist: %s" dir))
  (if (interactive-p)
      (setq verbose t))
  (let* ((file (epackage-layout-file-name root package 'uninstall))
	 (edir (file-name-directory file)))
    (epackage-make-directory edir 'no-question 'error)
    (cond
     ((file-exists-p file)
      (if verbose
	  (epackage-message "Not writing, already exists: %s" file)))
     (t
      (with-temp-buffer
	(insert (format "\
;; Template. Add remove-hook etc calls here.
\(error \"%s is not a configuration file.\")
"
			(file-name-nondirectory file)))
	(epackage-write-region (point-min) (point-max) file))))))

(defun epackage-devel-generate-examples
  (package root &optional dir recursive verbose)
  "Generate PACKAGE uninstall file relative to ROOT from DIR.
The file is stored under directory ROOT/`epackage--directory-name'.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    DIR   	Optional. Emacs Lisp package directories. This can be a
                one string or list of strings. Defaults to ROOT.
    RECURSIVE   Optional. If non-nil, read all *.el files under DIR.
		Interactive prefix.
    VERBOSE	Optional. If non-nil, display verbose messages.
                Interactive call sets this."
  (interactive
   (let ((root (read-directory-name "Epackage root dir: ")))
     (list
      package
      root
      root
      'interactive)))
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp dir))
	    (not (file-directory-p dir)))
    (epackage-error "Drectory DIR does not exist: %s" dir))
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Directory ROOT does not exist: %s" dir))
  (if (interactive-p)
      (setq verbose t))
  (let* ((file (epackage-layout-file-name root package 'examples))
	 (edir (file-name-directory file))
	 (exclude (epackage-read-file-content-regexp
		   (epackage-layout-file-name root package 'ignore))))
    ;; FIXME: Add some extraction code that would rip code examples
    ;; from inside source code files.
    ;; For now, just write dummy template file.
    (epackage-make-directory edir 'no-question 'error)
    (cond
     ((file-exists-p file)
      (if verbose
	  (epackage-message "Not writing, already exists: %s" file)))
     (t
      (with-temp-buffer
	(insert (format "\
;; Prevent loading this file. Study the examples.
\(error \"%s is not a configuration file.\")
;; End of file
"
			(file-name-nondirectory file)))
	(epackage-write-region (point-min) (point-max) file))))))

(defun epackage-devel-generate-compile-main
  (package root &optional dir recursive verbose)
  "Generate PACKAGE compile file relative to ROOT from DIR.
The file is stored under directory ROOT/`epackage--directory-name'.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    DIR   	Optional. Emacs Lisp package directories. This can be a
                one string or list of strings. Defaults to ROOT.
    RECURSIVE   Optional. If non-nil, read all *.el files under DIR.
		Interactive prefix.
    VERBOSE	Optional. If non-nil, display verbose messages.
                Interactive call sets this."
  (interactive
   (let ((root (read-directory-name "Epackage root dir: ")))
     (list
      package
      root
      root
      'interactive)))
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp dir))
	    (not (file-directory-p dir)))
    (epackage-error "Drectory DIR does not exist: %s" dir))
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Directory ROOT does not exist: %s" dir))
  (if (interactive-p)
      (setq verbose t))
  (let* ((file (epackage-layout-file-name root package 'compile))
	 (edir (file-name-directory file))
	 (regexp epackage--lisp-file-exclude-regexp)
	 (ignore (epackage-read-file-content-regexp
		   (epackage-layout-file-name root package 'ignore)))
	 (exclude (if ignore
		      (format "%s\\|%s" ignore regexp)
		    regexp))
	 (list (if recursive
		   (epackage-files-recursive-lisp dir 'relative exclude)
		 (epackage-directory-file-list dir exclude))))
    (epackage-make-directory edir 'no-question 'error)
    (epackage-devel-generate-compile-write-file file list)))

(defun epackage-devel-generate-loaddefs
  (package root &optional dir recursive verbose)
  "Generate PACKAGE loaddefs relative to ROOT from DIR.
The loaddefs file is stored under directory ROOT/`epackage--directory-name'.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    DIR   	Optional. Emacs Lisp package directories. This can be a
                one string or list of strings. Defaults to ROOT.
    RECURSIVE   Optional. If non-nil, read all *.el files under DIR.
		Interactive prefix.
    VERBOSE	Optional. If non-nil, display verbose messages.
                Interactive call sets this."
  (interactive
   (let ((package (read-string "Package name: "))
	 (root (read-directory-name "Epackage root dir: ")))
     (list
      package
      root
      root
      'interactive)))
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp dir))
	    (not (file-directory-p dir)))
    (epackage-error "Drectory DIR does not exist: %s" dir))
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Directory ROOT does not exist: %s" dir))
  (if (interactive-p)
      (setq verbose t))
  (let* ((file (epackage-layout-file-name root package 'loaddefs))
	 (edir (file-name-directory file))
	 (buffer epackage--buffer-autoload)
	 (regexp "\\(?:loaddef\\|autoload\\).*\\.el\\|[#~]")
	 (ignore (epackage-read-file-content-regexp
		   (epackage-layout-file-name root package 'ignore)))
	 (exclude (if ignore
		      (format "%s\\|%s" ignore regexp)
		    regexp)))
    (epackage-make-directory edir 'no-question 'error)
    (cond
     (recursive
      (dolist (elt (epackage-directory-recursive-lisp dir))
	(epackage-autoload-generate-loaddefs-dir elt file exclude verbose)))
     (t
      (if (stringp dir)
	  (epackage-autoload-generate-loaddefs-dir dir file exclude verbose)
	(dolist (elt dir)
	  (epackage-autoload-generate-loaddefs-dir dir file eclude verbose)))))
    (when (file-exists-p file)
      (epackage-add-provide-to-file file))))

;; Copy of tinylisp-batch-autoload-generate-loaddefs-dir
(defun epackage-batch-autoload-generate-loaddefs-dir (&optional exclude)
  "Call `epackage-autoload-generate-loaddefs-dir' for `command-line-args-left'.
Optionally EXCLUDE files by regexp."
  (epackage-with-command-line
   (epackage-autoload-generate-loaddefs-dir
    item
    exclude)))

(defun epackage-devel-generate-autoloads
  (package root dir &optional recursive verbose)
  "Generate PACKAGE autoloads relative to ROOT manually from DIR.
The autoloads are stored under directory ROOT/`epackage--directory-name'.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    DIR   	Emacs Lisp package directories. This can be a one string
                or list of strings.
    RECURSIVE   Optional. If non-nil, read all *.el files under DIR-LIST.
		Interactive prefix.
    VERBOSE	Optional. If non-nil, display verbose message.
                Interactive call sets this.

Return:

    If RECURSIVE, list of files.

Notes:

   If RECURSIVE is set, every directory that contains *.el file
   is considered a candidate. With bigger packages this is not
   so. Take for exampels BBDB, which contains following
   directories; of which only one of them is relevant for
   autoload generation: the lisp/ directory.

	bits/
	bits/bbdb-filters/
	lisp/
	misc/
	testing/
	texinfo/
	utils/

   So, you must manually check and possibly edit the generated results."
  (interactive
   "sPackage name: \nDPackage root dir: \nDRead autoloads from dir: \np")
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp dir))
	    (not (file-directory-p dir)))
    (epackage-error "Drectory does not exist: %s" dir))
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Drectory does not exist: %s" dir))
  (let* ((file (epackage-layout-file-name root package 'autoload))
	 (edir (file-name-directory file))
	 (buffer epackage--buffer-autoload)
	 (exclude (epackage-read-file-content-regexp
		   (epackage-layout-file-name root package 'ignore)))
	 list)
    (if (interactive-p)
	(setq verbose t))
    (epackage-make-directory edir 'error)
    (epackage-with-buffer-autoload
      (epackage-erase-buffer)
      (cond
       (recursive
	(dolist (elt (epackage-directory-recursive-lisp dir))
	  (epackage-nconc
	   (epackage-autoload-create-on-directory elt nil exclude)
	   list)))
       (t
	(if (stringp dir)
	    (setq list (epackage-autoload-create-on-directory dir nil exclude))
	  (dolist (elt dir)
	    (epackage-nconc
	     (epackage-autoload-create-on-directory elt nil exclude)
	     list)))))
      (cond
       ((not (eq (point-min) (point-max)))
	(epackage-add-provide-to-buffer file)
	(epackage-write-region (point-min) (point-max) file)
	(epackage-verbose-message "Wrote %s" file)
	(or list
	    t))
       (t
	(epackage-verbose-message
	  "[WARN] No autoloads found for %s from dir %s" file dir)
	nil)))))

(defun epackage-devel-generate-install
  (package root afile &optional verbose)
  "Generate PACKAGE install relative to ROOT manually from AFILE.
The install is stored under directory ROOT/`epackage--directory-name'.
Extract interactive autoload statements from AFILE.

Input:

    PACKAGE	Epackage name
    ROOT	Epackage root directory (must exists).
    AFILE       The `autoload' statement file.
    VERBOSE	Optional. If non-nil, display verbose message.
                Interactive call sets this."
  (interactive
   "sPackage name: \nFAutoload file: ")
  (epackage-error-if-invalid-package-name package)
  (when (or (not (stringp root))
	    (not (file-directory-p root)))
    (epackage-error "Drectory does not exist: %s" dir))
  (let ((autoloads afile)
	(install (epackage-layout-file-name dir package 'enable)))
    ;; By default we copy all interactive functions to install.
    (if (file-exists-p install)
	(epackage-verbose-message
	  "Already created, not touching %s" install)
      (if (not (file-exists-p autoloads))
	  (epackage-verbose-message
	    "[NOTE] File does not exist %s" autoloads)
	(with-temp-buffer
	  (insert-file-contents autoloads)
	  (goto-char (point-min))
	  ;; Searh line that have "t" at end:
	  ;;
	  ;; (autoload 'command "package" "" t)
	  ;; (autoload 'command "package" "" nil 'macro)
	  (delete-non-matching-lines "t)[ \t]*$")
	  (epackage-add-provide-to-buffer install)
	  (epackage-write-region (point-min) (point-max) install)
	  (epackage-verbose-message "Wrote %s" install))))))

(defun epackage-devel-information-license-gpl-standard ()
  "If buffer contains standard GPL stanza, return GPL[-<version>[+]].
Point is not preserved."
  (let (str)
    (goto-char (point-min))
    (when (re-search-forward
	   "terms.*of.*GNU.*General.*Public.*License" nil t)
      (setq str "GPL")
      (when (re-search-forward
	     ;;  either version 2 of the License, or (at your option)
	     ;;  either version 2, or (at your option)
	     "version[ \t]+\\([2-9]\\)\\(?:,\\|[ \t]+of[ \t].*License\\)" nil t)
	(setq str (format "%s-%s" str (match-string-no-properties 1)))
	(if (re-search-forward "any[ \t]+later[ \t]+version" nil t)
	    (setq str (concat str "+"))))
      str)))

(defun epackage-devel-information-license-mit ()
  "If buffer contains MIT stanza, return MIT.
Point is not preserved."
  ;; The MIT License reads:
  ;; http://en.wikipedia.org/wiki/MIT_License
  ;;
  ;; Permission is hereby granted, free of charge, to any person obtaining
  ;; a copy of this software and associated documentation files (the
  ;; "Software"), to deal in the Software without restriction, including
  ;; without limitation the rights to use, copy, modify, merge, publish,
  ;; distribute, sublicense, and/or sell copies of the Software, and to
  ;; permit persons to whom the Software is furnished to do so, subject to
  ;; the following conditions:
  ;;
  ;; The above copyright notice and this permission notice shall be
  ;; included in all copies or substantial portions of the Software.
  ;;
  ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  ;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  ;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  ;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  ;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  ;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ;;
  (let ((max (min (point-max)
		  (* 50 80))))		; 50 lines
    (goto-char (point-min))
    ;; FIXME: fill in other regexps
    (when (re-search-forward "The MIT license" max t)
      "MIT")))

(defun epackage-devel-information-license-main ()
  "Return license information from current buffer."
  (or (epackage-devel-information-license-gpl-standard)
      (epackage-devel-information-license-mit)))

;; See lm-get-package-name
(defun epackage-devel-information-description-short ()
  "Return DESCRIIPTION from the first line \";;; <name>.el --- <description>\""
  (or (lm-summary)
      ;; FIXME: see if some package does not follow conventions
      nil))

(defun epackage-devel-information-version-from-comment ()
  "Return version from current buffer.
Point is not preserved. An example:

  ;; Version: 1.0"
  (goto-char (point-min))
  (when (re-search-forward
	 "^;;+ *Version: *\\([0-9][^\"]+\\)" nil t)
    (match-string-no-properties 1)))

(defun epackage-devel-information-version-from-lm ()
  "Return version from current buffer.
Point is not preserved. Call `lm-header'."
  (lm-header "version"))

(defun epackage-devel-information-version-from-header ()
  "Return version from current buffer.
Search beginning of buffer for \";;; Version\" or similar.
Point is not preserved."
  (let ((max (min (point-max)
		  (* 15 80))))		; 15 lines
    (goto-char (point-min))
    ;; FIXME: More 'or' cases.
    (if (or (re-search-forward
	     "^;+[ \t]+version[ \t]+\\([1-9][^ \t\r\n]+\\)" max 'noerr))
	(match-string-no-properties 1))))

(defun epackage-devel-information-version-from-variable ()
  "Return version from current buffer.
Point is not preserved. An example:

   ;; (defconst foo-version \"2011.1119.1749\""
  (goto-char (point-min))
  (when (re-search-forward
	 "^(def[a-z].*[ \t\r\n]+[a-z-]+version.* \"\\([0-9][^\"]+\\)"
	 nil t)
    (match-string-no-properties 1)))

(defun epackage-devel-information-version-main ()
  "Return version from current buffer.
Point is not preserved."
  (or (epackage-devel-information-version-from-lm)
      (epackage-devel-information-version-from-header)
      (epackage-devel-information-version-from-variable)
      (epackage-devel-information-version-from-comment)))

(defun epackage-devel-information-maintainer ()
  "Return maintainer from current buffer.
Point is not preserved."
  (or (lm-header "Maintainer")
      (lm-header "Author")))

(defun epackage-devel-information-homepage ()
  "Return homepage from current buffer.
Point is not preserved."
  (or (lm-header "Homepage")
      (lm-header "Home")
      (lm-header "URL")
      (lm-header "URL(en)")))

(defun epackage-devel-information-wiki ()
  "Return homepage from current buffer.
Point is not preserved."
  (let ((str (or (lm-header "Wiki")
		 (lm-header "Emacswiki"))))
    (if (and str
	     (string-match "emacswiki.org" str))
	str)))

(defun epackage-devel-information-date-lm ()
  "Return date from current buffer.
Point is not preserved. Use `lm-header'."
  (or (lm-header "Time-stamp")
      (lm-header "Updated")
      (lm-header "Last-Updated")
      (lm-header "Modified")
      (lm-header "Created")))

(defun epackage-devel-information-date-versionstr ()
  "Return date from current buffer.
Point is not preserved. Examine Version string for YYYY.MMDD."
  ;; See if version is in format YYYY.MMDD
  (let ((version (epackage-devel-information-version-main)))
    (when (and version
	       (string-match
		`,(concat
		   "\\(\\(?:19\\|20\\)[0-9][0-9]\\)\\." ;Year  19xx 20xx
		   "\\([0][1-9]\\|1[012]\\)"		;Month 01-12
		   "\\([0-3][0-9]\\)")			;Date
		version))
      (format "%s-%s-%s"
	      (match-string-no-properties 1 version)
	      (match-string-no-properties 2 version)
	      (match-string-no-properties 3 version)))))

(defun epackage-devel-information-date-main ()
  "Return date from current buffer.
Point is not preserved."
  (let ((str (or (epackage-devel-information-date-lm)
		 (epackage-devel-information-date-versionstr)))
	iso)
    (if str
	(setq iso (epackage-date-to-iso str)))
    (or iso
	str)))

(defun epackage-devel-information-buffer ()
  "Examine current buffer and return list '((field value) ...)
FIELD can be:
  date
  description      the first line description string
  homepage
  license
  upstream
  version.
  wiki"
  (let (str
        list)
    (save-excursion
      (when (setq str (epackage-devel-information-description-short))
	(epackage-push (list "description" str) list))
      (when (setq str (epackage-devel-information-license-main))
	(epackage-push (list "license" str) list))
      (when (setq str (epackage-devel-information-maintainer))
	(epackage-push (list "upstream" str) list))
      (when (setq str (epackage-devel-information-homepage))
	(epackage-push (list "homepage" str) list))
      (when (setq str (epackage-devel-information-wiki))
	(epackage-push (list "wiki" str) list))
      (when (setq str (epackage-devel-information-date-main))
	  (epackage-push (list "date" str) list))
      (when (setq str (epackage-devel-information-version-main))
	(epackage-push (list "version" str) list))
      list)))

;;;###autoload
(defun epackage-devel-compose-package-info
  (package dir &optional verbose alist)
  "Generate info file for PACKAGE in DIR.

Input:

  PACKAGE	Package name. All lowercase.
  DIR		Package root directory.
  VERBOSE	Optional. If non-nil, display informational messages.
  ALIST		Optional. See `epackage-devel-information-buffer'

Notes:

  File is written under `epackage--directory-name' in DIR.
  Do nothing if file already exists."
  (interactive "sEpackage name: \nDLisp package root dir: ")
  (epackage-error-if-invalid-package-name package)
  (if (interactive-p)
      (setq verbose 'interactive))
  (let ((file (format "%s%s/%s"
			(file-name-as-directory dir)
			epackage--package-control-directory
			"info"))
	(maintainer (and user-full-name
			 user-mail-address
			 (format "%s <%s>"
				 user-full-name
				 user-mail-address)))
	(desc (nth 1 (assoc "description" alist))))
    (if (file-exists-p file)
	  (epackage-verbose-message
	    "[NOTE] Not touching existing info file %s" file)
	(with-temp-buffer
	  (insert epackage--layout-template-info)
	  (epackage-field-set "Package" package)
	  (when maintainer
	    (epackage-field-set "Maintainer" maintainer 'replace))
	  (when desc
	    (epackage-field-goto "Description")
	    (unless (eq (point) (line-end-position))
	      (delete-region (point) (line-end-position)))
	    (insert desc))
	  (when alist
	    (let (elt)
	      (dolist (item '("license"
			      "upstream"
			      "homepage"
			      "wiki"))
	      (when (setq elt (assoc item alist))
		(epackage-field-set item (nth 1 elt) 'replace)))))
	  (epackage-write-region (point-min) (point-max) file)
	  (epackage-verbose-message "Wrote %s" file)
	  t))))

;;;###autoload
(defun epackage-devel-compose-package-info-from-current-buffer
  (package dir &optional verbose)
  "Compose initial info file for PACKAGE in DIR by readig current buffer.
The buffer should be the *.el file that contains all the License and
Version information.

Input:
  PACKAGE	Package name. All lowercase.
  DIR		Package root directory.
  VERBOSE	Optional. If non-nil, display informational messages."
  (interactive "sEpackage name: \nDLisp package root dir: \n")
  (if (interactive-p)
      (setq verbose 'interactive))
  (let ((alist (epackage-devel-information-buffer)))
    (epackage-devel-compose-package-info package dir verbose alist)))

;;;###autoload
(defun epackage-devel-compose-package-info-from-file
  (package dir file &optional verbose)
  "Compose initial info file for PACKAGE in DIR by readig FILE.

Input:
  PACKAGE	Package name. All lowercase.
  DIR		Package root directory.
  FILE          The *.el file user to extract information: version etc.
  VERBOSE	Optional. If non-nil, display informational messages."
  (interactive
   "sEpackage name: \nDLisp package root dir: \nfSource *.el file: ")
  (if (interactive-p)
      (setq verbose 'interactive))
  (with-temp-buffer
    (insert-file-contents file)
    (epackage-devel-compose-package-info-from-current-buffer
     package dir verbose)))

;;;###autoload
(defun epackage-devel-compose-package-dir
  (package dir &optional verbose alist)
  "Compose initial templates for PACKAGE in DIR.
Generate autoloads, loaddefs file and write other template files
under `epackage--directory-name'.

Input:
  PACKAGE	Package name. All lowercase.
  DIR		Package root directory.
  VERBOSE	Optional. if non-nil, display informational messages.
  ALIST		Optional. See`epackage-devel-information-buffer'."
  (interactive "sEpackage name: \nDLisp package root dir: ")
  (if (interactive-p)
      (setq verbose 'interactive))
  (epackage-error-if-invalid-package-name package)
  (let ((list
	 (epackage-devel-generate-autoloads
	  package
	  dir dir 'recursive verbose)))
    (let ((file (epackage-layout-file-name dir package 'autoload)))
      (epackage-devel-generate-install package dir file verbose))
    (epackage-devel-generate-loaddefs package dir dir 'recursive verbose)
    (epackage-devel-generate-compile-main package dir dir 'recursive verbose)
    (epackage-devel-generate-examples package dir dir 'recursive verbose)
    (epackage-devel-generate-uninstall package dir dir 'reursive verbose)
    (if (and list
	     (listp list)
	     (eq 1 (length list))
	     (epackage-devel-compose-package-info-from-file
	      package dir (car list) verbose))
	(epackage-devel-compose-package-info package dir verbose))
    t))

;;;###autoload
(defun epackage-devel-compose-git-import (dir &optional verbose)
  "Import Emacs Lisp Package into Git repository.
If VERBOSE is non-nil, display informational messages.

This is the initial step for starting to work with Epackages:

  - Initialize Git repository in DIR
  - Import all code to 'upstream' branch
  - Tag import
  - Branch off to master
  ... after that you can populate epackage/ directory.

Notes:

  If package in DIR contains more than one *.el file, this program
  will abort. Handling big packages that consist of many lisp files
  must be handled manually.

  Also the lisp file being imported must contain information about
  version and last modification date.

Return:

  alist    See function `epackage-devel-information-buffer'."
  (interactive "DLisp package root dir to import: ")
  (if (interactive-p)
      (setq verbose 'interactive))
  (unless (file-directory-p dir)
    (epackage-error "No such directory %s" dir))
  (let ((git (format "%s.git" (file-name-as-directory dir))))
    (when (file-directory-p git)
      (epackage-error "Already Git repository in directory %s" git)))
  (let ((dirs (epackage-directory-recursive-lisp dir))
	files
	list)
    (if (> (length dirs) 1)
	(epackage-error
	 (format
	    `,(concat
	       "Abort. epackage-devel-compose-git-import "
	       "can only handle single *.el file packages (dirs: %d).")
	    (length dirs))))
    (setq files (directory-files dir 'fullpath "\\.el$"))
    (unless files
      (epackage-error "No *.el files found in %s" dir))
    ;; Filter out some common files
    (dolist (elt files)
      (unless (string-match "autoload\\|loaddefs" elt)
	(epackage-push elt list)))
    (if (> (length list) 1)
	(epackage-error
	 (format
	    `,(concat
	       "Abort. epackage-devel-compose-git-import "
	       "can only handle single *.el packages (files: %d).")
	    (length list))))
    (let ((file (car list))
	  version
	  date
	  alist)
    (with-temp-buffer
      (insert-file-contents file)
      (setq alist (epackage-devel-information-buffer))
      (unless (setq date (nth 1 (assoc "date" alist)))
	(epackage-error "Cannot find date field from file %s" file))
      (unless (setq version (nth 1 (assoc "version" alist)))
	(epackage-error "Cannot find version number from file %s" file))
      (epackage-require-git)
      (epackage-git-command-init dir)
      ;;  start directly at upstream branch
      (epackage-with-git-command dir verbose
	"symbolic-ref" "HEAD" "refs/heads/upstream")
      ;; import
      (epackage-with-git-command dir verbose
	"add" ".")
      (let ((message
	     (format "Import upstream %s (%s)" version date)))
	(epackage-with-git-command dir verbose
	  "commit" "-m" message))
      (let ((tag (format "upstream/%s--%s" date version)))
	(epackage-with-git-command dir verbose
	  "tag" tag))
      (epackage-with-git-command dir verbose
	"checkout" "-b" "master")
      alist))))

(defun epackage-devel-compose-1-interactive ()
  "Ask DIR and PACKAGE name."
  (let* ((dir (read-directory-name
	       "Lisp package root directory: "
	       default-directory
	       default-directory
	       'must-match
	       (not 'initial-value)))
	 (default (if (string-match "/\\([^/]+\\)/$" dir)
		      (match-string 1 dir)))
	 (package (read-string "Epackage name: " default)))
    (list dir
	  package)))

;; (epackage-devel-compose-main "~/tmp/ep" "test" t)
(defun epackage-devel-compose-main (package dir &optional verbose)
  "Convert Emacs Lisp Package DIR into Epackage.
If VERBOSE is non-nil, display informational messages.

Notes:
  Only single file packages are handled.
  See caveats from `epackage-devel-compose-git-import'."
  ;; FIXME
  ;; 2011-12-29 It was a nice idea. Unfortunately the Emacs Lisp
  ;; packages usually do not follow any discipline, so there is almost
  ;; no file where this "auto imoort to Git" would be useful. Too many
  ;; chances for error. User is best served to use
  ;; epackage-devel-compose-package-dir and handle Git on command line.
  ;;
  (tinyepackage-error
   (concat "Disabled due to lisp files being imported lacking proper "
	   "structure. Please use epackage-devel-compose-package-dir instead"))
  (interactive
   (append (epackage-devel-compose-1-interactive)
	   (list 'interactive)))	;
  (if (interactive-p)
      (setq verbose 'interactive))
  (unless (file-directory-p dir)
    (epackage-error "[compose-main] No such directory %s" dir))
  (let ((alist (epackage-devel-compose-git-import dir verbose)))
    (prog1
	(epackage-devel-compose-package-dir package dir verbose alist)
      (when verbose
	(let ((file (format "%s%s/%s"
			    (file-name-as-directory dir)
			    epackage--package-control-directory
			    (epackage-layout-mapping-file 'info))))
	  (epackage-message
	   "Epackage %s done, edit %s" package file))))))

;;; ............................................... &functions-package ...

(defun epackage-upgrade-package-files (package verbose)
  "Update installed files from PACKAGE.
If optional VERBOSE is non-nil, display progress messages."
  ;; FIXME: upgrade
  ;; - New or deleted files in <package>/*
  ;; - What about obsolete 00control/* files ?
  (let ((root (epackage-directory-package-control package))
	(install (epackage-directory-install))
	from
	to)
    (dolist (file (epackage-status-install-files package))
      (setq from (format "%s/%s" root file))
      (setq to (format "%s/%s" install file))
      (epackage-enable-file from to nil verbose))))

(defun epackage-upgrade-package-actions (package verbose)
  "Run after upgrade actions: byte compile, install updated files etc.
If optional VERBOSE is non-nil, display progress messages."
  ;; FIXME: upgrade
  ;; - New or deleted files in epackage/*
  ;; - obsolete 00control/* files ?
  (let ((list (epackage-rerun-action-list package verbose))
	actions)
    ;; Any other actions in effect?
    (dolist (elt epackage--download-action-list)
      (unless (memq elt list)
	(epackage-push elt actions)))
    (when actions			 ; Run more actions as needed
      (setq actions (reverse actions)) ; Keep alphabetical order
      (epackage-run-action-list package actions verbose))))

(defun epackage-upgrade-package-git (package &optional verbose)
  "Upgrade PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (epackage-error "No download URL for package '%s'" package))
    (let ((dir (epackage-directory-package-root package)))
      (epackage-with-message verbose (format "Upgrading package %s" package)
        (unless (epackage-git-master-p package)
          (epackage-fatal
            `,(concat
               "Can't upgrade. "
               "Branch name is not \"master\" in '%s'; "
               "repository changed manually or invalid package dir content.")
            dir))
        (unless (epackage-git-status-clean-p package)
          (epackage-fatal
            `,(concat
               "Can't upgrade. "
               "Unclean Git status in '%s'; "
               "possibly changed manually.")
            dir))
        (epackage-git-command-pull dir verbose)))))

(defun epackage-upgrade-package-main (package &optional verbose)
  "Do all steps necessary to upgrade PACKAGE.
If optional VERBOSE is non-nil, display progress message.

NOTE: No Git branch check is verified. The caller must have
ensured that the branch where Git is run is correct e.g. with
function `epackage-git-master-p'."
  (let ((dir (epackage-directory-package-root package)))
    (when dir
	(let ((sha-old (epackage-git-command-current-sha dir))
	      sha)
	  (epackage-upgrade-package-git package verbose)
	  (setq sha (epackage-git-command-current-sha dir))
	  (unless (string= sha sha-old)
	    (epackage-upgrade-package-files package verbose)
	    (epackage-upgrade-package-actions package verbose))))))

(defun epackage-kill-buffer-sources-list ()
  "Kill sources list buffer."
  (let ((buffer (get-file-buffer (epackage-file-name-sources-list-main))))
    (if buffer
        (epackage-kill-buffer buffer))))

;; FIXME: should we run hooks like in epackage-cmd-remove-package
(defun epackage-sources-list-and-repositories-sync (&optional verbose)
  "Verify that URLs still match and rebuild package repositories if needed.
If optional VERBOSE is non-nil, display progress message.
If sources list URLs differ from current Git repositoriy 'origin'
URLs, recreate each repository provided that they are
still in pristine state."
  (let (elt
        package
        dir)
    (dolist (package (epackage-status-downloaded-packages))
      (when (setq elt (epackage-pkg-lint-git-url package verbose))
        (setq package (nth 0 elt)
              dir     (epackage-directory-package-root package))
        (epackage-verbose-message "Rebuild repository of %s" package)
        (cond
         ;; FIXME: should check if repository is not locally modified.
         ((not (epackage-git-master-p package))
          (epackage-warn
            `,(concat
               "Won't re-create due to changed source URL. "
               "Branch name is not \"master\" in %s; "
               "possibly changed manually or invalid package repository.")
            dir))
         (t
          (epackage-recreate-package package verbose)))))))

(defun epackage-sources-list-upgrade (&optional verbose)
  "Update list of available packages.
If optional VERBOSE is non-nil, display progress message.
This is a low level command."
  (let ((dir (epackage-sources-list-official-directory)))
    (unless (file-directory-p dir)
      (epackage-error
        (substitute-command-keys
         (format
          `,(concat "No such directory '%s'. "
                    "Run \\[epackage-initialize]")
          dir))))
    (epackage-git-command-pull dir verbose)))

(defun epackage-replace-regexp-in-buffer (table)
  "Replace according to TABLE '((re replace [match]) ...) in buffer."
  (let ((point (point))
	re
	str
	match
	status)
    (dolist (elt table)
      (setq re (nth 0 elt)
	    str (nth 1 elt)
	    match (or (nth 2 elt) 0))
      (goto-char point)
      (while (re-search-forward re nil t)
	(setq status 'replaced)
	(replace-match match str)))
    status))

(defun epackage-combine-files (file list &optional hooks verbose)
  "Write to FILE a combined content of LIST of files.
If optional HOOKS set, call each hook function before saving to FILE.
If optional VERBOSE is non-nil, display progress message.

Before saving, apply `epackage--sources-replace-table'."
  (with-temp-buffer
    (dolist (elt list)
      (goto-char (point-max))
      (epackage-verbose-message "Combining sources list file %s" elt)
      (insert "###file: " elt "\n")
      (if (file-exists-p elt)
	  (insert-file-contents elt)
	(insert "# ERROR: Not found\n")
	(epackage-warn "Non-existing file for combine: %s" file)))
    (epackage-with-message
        verbose (format "Write master sources list file %s" file)
      (goto-char (point-min))
      (unless (re-search-forward "^[^#\r\n]+://" nil t)
        (epackage-error
          "Can't find any Git repository URLs. Check files %s" list))
      (epackage-replace-regexp-in-buffer
       epackage--sources-replace-table)
      (if hooks
	  (run-hooks hooks))
      (epackage-write-region (point-min) (point-max) file))))

(defun epackage-sources-list-initialize (&optional verbose)
  "Build list of available packages.
If optional VERBOSE is non-nil, display progress message."
  (let ((dir (epackage-directory-sources-list))
        (official (epackage-file-name-sources-list-official)))
    (unless (file-directory-p dir)
      (epackage-error
        (substitute-command-keys
         (format
          `,(concat "No such directory '%s'. "
                    "Run \\[epackage-initialize]")
          dir))))
    (unless (file-exists-p official)
      (epackage-error
        (substitute-command-keys
         (format
          `,(concat "No such file '%s'. "
                    "Run \\[epackage-initialize]")
          official))))
    (epackage-combine-files
     (epackage-file-name-sources-list-main)
     (append epackage--sources-file-list
             (list (epackage-file-name-sources-list-official)))
     epackage--build-sources-list-hook ;; Hooks to run before save.
     verbose)))

(defun epackage-sources-list-build (&optional verbose)
  "Build sources list file.
If optional VERBOSE is non-nil, display progress messages.
This fucntion is meant for interactive use: the message differs
if sources list has already been downloaded or not."
  (epackage-sources-list-kill-buffer)
  (if (epackage-sources-list-p)
      (epackage-with-message verbose "Building package sources list"
        (epackage-sources-list-initialize verbose))
    (epackage-with-message verbose "Initializing package sources list"
      (epackage-sources-list-initialize verbose)))
  (if epackage--sources-list-and-repository-sync-flag
      (epackage-sources-list-and-repositories-sync verbose)))

(defun epackage-download-package-actions (package &optional verbose)
  "Run `epackage--download-action-list' for PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (epackage-run-action-list
   package
   epackage--download-action-list
   verbose))

(defun epackage-download-package (package &optional verbose)
  "Download PACKAGE.
If optional VERBOSE is non-nil, display progress message.

Note: this is a lowlevel function. To respect
`epackage--download-action-list', use `epackage-cmd-download-package'
instead or call `epackage-download-package-run-actions' after this."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (epackage-error "No download URL for package '%s'" package))
    (let ((dir (epackage-directory-package-root package)))
      (epackage-git-command-clone url dir verbose)
      (run-hooks 'epackage--install-download-hook))))

(defsubst epackage-pkg-depends-verify-emacs (depends)
  "Check DEPENDS whose format match `epackage-depends-parse-collect'.
Check only items \"emacs\" or \"xemacs\".
Return list memebr that does not satisfy depends."
  (let* ((emacs  (assoc "emacs" depends))
         (xemacs (assoc "xemacs" depends))
         (flavor (if (featurep 'emacs)
                     emacs
                   xemacs))
         op
         after
         version
         ret)
    ;; Check Eamcs flavor requirement
    (when (and flavor
               (setq version (nth 2 flavor)))
      (setq op (nth 1 flavor))
      (setq after (string< version emacs-version))  ;; A < B   i.e   B > A
      (cond
       ((string= op ">=")
        (if (not (or after
                     (string= emacs-version version)))
            flavor))
       ((string= op "<=")
        (if (not (or (not after)
                     (string= emacs-version version)))
            flavor))
       ((string= op "!")
        flavor)))))

(defun epackage-pkg-depends-verify-main (depends)
  "Check DEPENDS whose format match `epackage-depends-parse-collect'.
Return back depends that are not met."
  (let (package
        tmp
        ret)
    (if (setq tmp (epackage-pkg-depends-verify-emacs depends))
        (epackage-push tmp ret))
    (when depends
      (let ((downloaded (epackage-status-downloaded-packages)))
        (dolist (elt depends)
          (setq package (nth 0 elt))
          (if (not (string-match "^\\x?emacs$" package))
              (if (not (or (member package downloaded)
                           (locate-library package)))
                  (epackage-push elt ret))))))
    ret))

(defun epackage-pkg-depends-rollback (&optional verbose)
  "Roll back according to `epackage--depends-satisfy-running'.
If optional VERBOSE is non-nil, display progress message.
See variable's documentation for more information."
  (when epackage--depends-satisfy-running
    (epackage-with-message verbose "Rollback depends"
      (let (package)
        (dolist (elt epackage--depends-satisfy-running)
          (setq package (nth 1 elt))
          (when (and (epackage-string-p package)
                     (not (string-match "^\\x?emacs$" package)))
            (epackage-config-delete-all package verbose)))
        (setq epackage--depends-satisfy-running nil)))))

(defun epackage-pkg-depends-resolve (package &optional verbose)
  "Strudy PACKAGE 'Depends:' return list of packges to download.
If optional VERBOSE is non-nil, display progress message."
  (let* ((deps (epackage-pkg-info-fetch-field-depends package))
         (missing (epackage-pkg-depends-verify-main deps))
         (emacs  (or (member "emacs" missing)
                     (member "xemacs" missing)))
         ret)
    (if emacs
        (cond
         ((eq 'warn epackage--depends-handling)
          (epackage-warn
           "Missing required dependency for %s: %s" package emacs))
         ((eq 'error epackage--depends-handling)
          (epackage-error
            "Missing required dependency for %s: %s"
            package emacs))))
    (if (and missing
             (not (null epackage--depends-handling)))
        (let (package)
          (dolist (elt missing)
            (setq package (nth 0 elt))
            ;; Skip Emacs version depends. Handled already.
            (unless (string-match "^\\x?emacs$" package)
              (if (not (epackage-sources-list-info-url package))
                  (cond
                   ((eq 'warn epackage--depends-handling verbose)
                    (epackage-pkg-depends-rollback)
                    (epackage-warn
                     "Downloading required dependency for %s: %s"
                     package elt))
                   ((eq 'error epackage--depends-handling)
                    (epackage-pkg-depends-rollback verbose)
                    (epackage-error
                      "Missing required dependency for %s: %s"
                      package elt)))
                (epackage-push package ret))))))
    ret))

(defun epackage-pkg-depends-satisfy (package &optional verbose)
  "Resolve depends of PACKAGE by downloading more packages as needed.
If optional VERBOSE is non-nil, display progress message."
  (let ((missing (epackage-pkg-depends-resolve package verbose)))
    ;; This is recursive, since we're initially called through
    ;; epackage-run-action-list
    (dolist (elt missing)
      (epackage-push elt epackage--depends-satisfy-running)
      (epackage-cmd-download-package elt verbose))))

;;; ................................................ &functions-config ...

(defun epackage-enable-file (from to &optional noerr verbose)
  "Enable by copying or by symlinking file FROM TO.
With optional NOERR, do not signal errors.
If optional VERBOSE is non-nil, display progress message.
See variable `epackage--symlink-support-flag'.

Return:
    non-nil    ok
    nil        nok"
  (cond
   ((file-exists-p from)
    (epackage-verbose-message "Installing %s" to)
    (if epackage--symlink-support-flag
        (dired-make-relative-symlink from to 'overwrite)
      (copy-file from to 'overwrite 'keep-time))
    t)
   (noerr
    (epackage-warn "Ignore non-existing file: %s" from)
    nil)
   (t
    (epackage-error "Missing file: %s" from)
    nil)))

(defun epackage-config-install-handler (type package &optional verbose)
  "Run action of TYPE for PACKAGE. See `epackage--install-action-list'.
FILE is the install configuration file.
If optional VERBOSE is non-nil, display progress message."
  (let ((actions epackage--install-action-list)
        (file    (epackage-directory-packages-control-file
                  package type)))
    (when (file-exists-p file)
      (cond
       ((memq 'enable actions)
        (epackage-eval-file-safe file))
       ((memq 'boot actions)
        (epackage-loader-file-generate-boot verbose))))))

(defun epackage-config-install-action
  (type package &optional noerr verbose)
  "Run install of TYPE for PACKAGE.
With optional NOERR, do not signall errors, display inly messages.
If optional VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((from (epackage-directory-packages-control-file
               package type))
        (to (epackage-file-name-install-compose package type)))
    (when (epackage-enable-file from to noerr verbose)
      (epackage-config-install-handler type package verbose)
      (run-hooks 'epackage--install-type-hook)
      t)))

(defun epackage-config-uninstall-invoke (package &optional verbose)
  "Invoke uninstall file of PACKAGE if it exists.
If optional VERBOSE is non-nil, display progress message."
  (let ((file (epackage-directory-packages-control-file
               package 'uninstall)))
    (when (file-exists-p file)
      (epackage-verbose-message "Run %s" file)
      (epackage-eval-file-safe file))))

(defun epackage-config-install-autoload (package &optional verbose)
  "Install PACKAGE autoload files.
If optional VERBOSE is non-nil, display progress message."
  (let ((status
         (or (epackage-config-install-action 'loaddefs package 'noerr verbose)
             (epackage-config-install-action 'autoload package 'noerr verbose))))
    (when status
      (run-hooks 'epackage--install-autoload-hook)
      status)))

(defun epackage-config-delete-file (file &optional verbose)
  "Delete FILE is file exists.
If optional VERBOSE is non-nil, display progress message.
Run `epackage--install-config-delete-type-hook'."
  (when (file-exists-p file)
    (epackage-verbose-message "Delete %s" file)
    (delete-file file)
    (run-hooks 'epackage--install-config-delete-type-hook)))

(defun epackage-config-delete-action (type package &optional verbose)
  "Delete install configuration TYPE for PACKAGE.
If optional VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((file (epackage-file-name-install-compose package type)))
    (epackage-config-delete-file file verbose)))

(defun epackage-config-delete-all (package &optional verbose)
  "Delete all install configuration files for PACKAGE.
If optional VERBOSE is non-nil, display progress message.
Return:
    List of deleted files."
  (let ((dir (epackage-directory-install))
        list)
    (epackage-error-no-directory dir)
    (dolist (file (directory-files
                   dir
                   'full-path
                   (format "^%s-" package)
                   t))
      (when (file-exists-p file)
        (setq list (cons list file))
        (epackage-verbose-message "Delete %s" file)
        (epackage-config-delete-file file)))
    (if list
        (run-hooks 'epackage--install-config-delete-all-hook))
    list))

;;; ............................................... &functions-display ...

(defun epackage-pkg-info-documentation (package &optional verbose)
  "Display local PACKAGE documentation in another buffer.
If optional VERBOSE is non-nil, display progress message.
Return:
  file name of documentation or nil."
  (let ((file (epackage-pkg-info-fetch-field package "Commentary"))
        path)
    (when file
      (setq path (format "%s/%s"
                         (epackage-directory-package-root package)
                         file))
      ;; FIXME: Do not move cursor. Just display buffer.
      (let ((buffer (current-buffer)))
        (finder-commentary path)
        (pop-to-buffer buffer)
        path))))

(defun epackage-pkg-info-display (package &optional verbose)
  "Display local PACKAGE information in another buffer.
If optional VERBOSE is non-nil, display progress message."
  (epackage-with-package-info-file package
    (epackage-with-buffer-info
      (erase-buffer)
      (insert (epackage-file-content-as-string info-file))
      (display-buffer (current-buffer)))))

;;; ................................................ &functions-loader ...

(defun epackage-loader-file-insert-header ()
  "Insert header comments."
  (insert
    "\
;; Epackge boot file -- automatically generated
;;
;; M-x epackage-loader-file-generate-boot
;; Do not modify. Changes done here will be lost.

"))

(defsubst epackage-loader-file-insert-footer ()
  "Insert Footer."
  (insert
   (format "\
\(provide '%s)

;; End of file
"
           (file-name-sans-extension
            (file-name-nondirectory
             (epackage-file-name-loader-boot))))))

(defun epackage-loader-insert-file-path-list-by-path (path)
  "Insert `load-path' definitions to `current-buffer' from PATH."
  (dolist (dir (epackage-directory-recursive-lisp path))
    ;; Convert absolute paths into HOME (~)
    (setq dir (abbreviate-file-name dir))
    (insert (format
             "(add-to-list 'load-path \"%s\")\n"
             dir))))

(defun epackage-loader-file-insert-path-list () ;; FIXME w32
  "Insert `load-path' commands to `current-buffer'."
  (dolist (package (epackage-status-installed-packages))
    (epackage-loader-insert-file-path-list-by-path
     (epackage-directory-package-root package))))

(defun epackage-loader-file-insert-install-code ()
  "Insert package installation code into `current-buffer'."
  ;; FIXME: If there is both install, xactivate should be install both?
  (let ((list (epackage-sort
	       (directory-files
		(epackage-directory-install)
		'full-path
		"^.*-.*\\.el"
		t))))
    (dolist (file list)
      ;; `insert-file-contents' does not put point after last line
      ;; inserted so we must move ourselves.
      (if (file-exists-p file)
	  (epackage-insert-file-contents file)))))

(defsubst epackage-loader-file-insert-load-path ()
  "Insert Epackage loader boot commands: header and`load-path'."
  (epackage-loader-file-insert-header)
  (epackage-loader-file-insert-path-list))

(defsubst epackage-loader-file-insert-main ()
  "Insert Epackage loader boot commands to current point."
  (epackage-loader-file-insert-load-path)
  (epackage-loader-file-insert-install-code)
  (epackage-loader-file-insert-footer))

(defun epackage-loader-file-byte-compile (&optional verbose)
  "Byte compile `epackage-file-name-loader-boot'.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (let ((file (epackage-file-name-loader-boot)))
    (cond
     ((file-exists-p file)
      (byte-compile-file file))
     (verbose
      (epackage-message "No boot loader file generated to byte compile.")))))

;;; Note really meant for user, but anyways....
;;;###autoload
(defun epackage-loader-file-generate-load-path-main (&optional verbose)
  "Generate `load-path' loader for all installed or activated packages.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (let ((file (epackage-file-name-loader-load-path)))
    (with-temp-buffer
      (epackage-loader-file-insert-load-path)
      (epackage-loader-insert-file-path-list-by-path
       (epackage-directory-install))
      (epackage-write-region (point-min) (point-max) file))))

(defun epackage-loader-file-generate-load-path-maybe (&optional verbose)
  "Generate `epackage-file-name-loader-load-path' file if not exists.
If optional VERBOSE is non-nil, display progress message."
  (let ((file (epackage-file-name-loader-load-path)))
    (unless (file-exists-p file)
      (epackage-loader-file-generate-load-path-main verbose))))

;;; .......................................... &functions-byte-compile ...

(defsubst epackage-byte-compile-loader-file-maybe (&optional verbose)
  "Check `epackage--byte-compile-loader-file' and byte compile.
If optional VERBOSE is non-nil, display progress message."
  (when epackage--loader-file-byte-compile-flag
    (epackage-loader-file-byte-compile verbose)))

(defun epackage-byte-compile-package-guess (package &optional verbose)
  "Run byte compile on PACKAGE only if there is only a single Lisp file.
If optional VERBOSE is non-nil, display progress message.

Return:
  non-nil if byte compile was run."
  (let ((load-path load-path)
        (dir (epackage-directory-package-root package))
        list
        files
        file)
    (setq list (epackage-directory-recursive-list
                dir
                list
                (concat epackage--directory-exclude-regexp
                        "\\|/" epackage--directory-name)))
    ;; FIXME: we assume the single file is not in a subdirectory
    (when (and (eq 1 (length list))
               (eq 1 (length (setq files (epackage-lisp-file-list list)))))
      (setq file (car files))
      (dolist (elt list)
        (epackage-push elt load-path))
      (epackage-loader-file-generate-load-path-maybe)
      (epackage-verbose-message "byte compile %s" file)
      (byte-compile-file file)
      (when verbose
        (epackage-with-byte-compile-buffer
          (display-buffer (current-buffer))))
      t)))

(defun epackage-byte-compile-package-run-file (file &optional verbose)
  "Byte compile using epackage compile FILE.
If optional VERBOSE is non-nil, display progress message."
  (unless (stringp file)
    (epackage-error "Compile, file path argument is not string: %s" file))
  (unless (file-exists-p file)
    (epackage-error "Compile, file does not exisit: %s" file))
  (let* ((load-path load-path)
	 (dir (epackage-file-name-directory-previous
	       (file-name-directory file)))
	 (default-directory dir)
        list)
    (setq list (epackage-directory-recursive-list
                dir
                list
                (concat epackage--directory-exclude-regexp
                        "\\|/" epackage--directory-name)))
    (dolist (elt list)
      (epackage-push elt load-path))
    (epackage-loader-file-generate-load-path-maybe)
    (epackage-eval-file (epackage-file-name-loader-load-path))
    (epackage-verbose-message "byte compile with %s" file)
    (epackage-eval-file file)
    (when verbose
      (epackage-with-byte-compile-buffer
        (display-buffer (current-buffer))))
    t))

(defun epackage-byte-compile-package-standard (package &optional verbose)
  "Run byte compile on PACKAGE with standard compile file.
If optional VERBOSE is non-nil, display progress message.

Note: No error checking is done about existence of
`epackage-directory-packages-control-file'."
  (let ((file (epackage-directory-packages-control-file package 'compile)))
        ;; (dir (epackage-directory-package-root package))
    (epackage-byte-compile-package-run-file file verbose)))

(defun epackage-byte-compile-package-main (package &optional verbose)
  "Run byte compile PACKAGE, if possible.
If optional VERBOSE is non-nil, display progress message.
Return:
  non-nil if byte compile was run."
  (let ((file (epackage-directory-packages-control-file package 'compile)))
    (cond
     ((file-exists-p file)
      (epackage-byte-compile-package-standard package verbose))
     ((epackage-byte-compile-package-guess package verbose))
     (t
      (epackage-verbose-message "Byte compile not supported. Missing %s" file)
      nil))))

;;;###autoload
(defun epackage-loader-file-generate-boot (&optional verbose)
  "Generate boot loader for all enabled and activated packages.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (epackage-loader-file-generate-load-path-main)
  (let ((file (epackage-file-name-loader-boot)))
    (epackage-with-message verbose "Generating boot loader"
      (with-temp-buffer
        (epackage-loader-file-insert-main)
        (epackage-write-region (point-min) (point-max) file)
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))
      (epackage-byte-compile-loader-file-maybe verbose))))

(defun epackage-sources-list-info-parse-line (package)
  "Return list of PACKAGE fields described in `epackage--sources-list-url'.
Point must be at the beginning of line."
  (if (looking-at
       (format epackage--sources-list-regexp
               (regexp-quote package)))
      (list
       (match-string-no-properties 1)
       (match-string-no-properties 2)
       (match-string-no-properties 3))))

(defun epackage-sources-list-info-main (package)
  "Return '(pkg url description) for PACKAGE.
Format is described in variable `epackage--sources-list-url'."
  (epackage-with-sources-list
    (goto-char (point-min))
    (let ((re (format epackage--sources-list-regexp
                      (regexp-quote package))))
      (when (re-search-forward re nil t)
        (list
         (match-string-no-properties 1)
         (match-string-no-properties 2)
         (match-string-no-properties 3))))))

(defun epackage-sources-list-info-url (package)
  "Return URL for PACKAGE."
  (let ((info (epackage-sources-list-info-main package)))
    (when info
      (nth 1 info))))

(defun epackage-sources-list-info-description (package)
  "Return description for PACKAGE or nil."
  (let ((info (epackage-sources-list-info-main package)))
    (when info
      (nth 2 info))))

(defun epackage-sources-list-info-pkg-list ()
  "Return list of packages in alphabetical order."
  (epackage-with-sources-list
    (epackage-with-case-fold-search
     (let (list)
       (goto-char (point-min))
       (while (re-search-forward "^\\([a-z][a-z0-9-]+\\)[ \t]+[a-z]" nil t)
	 (epackage-push (match-string-no-properties 1) list))
       (setq list (epackage-sort list))
       list))))

(defun epackage-require-emacs (&optional verbose)
  "Require Emacs features.
If optional VERBOSE is non-nil, display progress message."
  (unless (fboundp 'url-retrieve-synchronously)
    (epackage-error
      `,(concat
         "this Emacs does not define "
         "`url-retrieve-synchronously' from url.el"))))

(defun epackage-require-git (&optional verbose)
  "Require Git program.
If optional VERBOSE is non-nil, display progress message."
  (cond
   ((null epackage--program-git)
    (let ((bin (executable-find "git")))
      (unless bin
        (epackage-error "program 'git' not found in PATH"))
      (setq epackage--program-git bin)))
   ((and (stringp epackage--program-git)
         (not (file-exists-p epackage--program-git)))
    (epackage-error "Invalid or not exists `epackage--program-git' (%s)"
                    epackage--program-git))
   ((file-executable-p epackage--program-git)) ;All ok
   (t
    (epackage-error "Unknown value in `epackage--program-git' (%s)"
                    epackage--program-git))))

(defun epackage-require-directories (&optional verbose)
  "Buid directory structure.
If optional VERBOSE is non-nil, display progress message."
  (dolist (dir (list
                (epackage-directory-root)
                (epackage-directory-packages)
                (epackage-directory-sources-list)
                (epackage-directory-conf)
                (epackage-directory-install)
                (epackage-directory-loader)))
    (unless (file-directory-p dir)
      (epackage-verbose-message "Making directory %s ..." dir)
      (make-directory dir))))

(defun epackage-ssh-known-host-p (&optional host)
  "Check if HOST exists in ~/.ssh/known_hosts file."
  (let ((file "~/.ssh/known_hosts"))
    (cond
     ((not (file-exists-p file))
      nil)
     (t
      (with-current-buffer (find-file-noselect file)
	;; We need to be notified about changes
	(epackage-turn-on-auto-revert-mode)
	(goto-char (point-min))
	;; <host>,<ip> <key type> <key>
	(let ((re (format "^[ \t]*%s," (regexp-quote host))))
	  (re-search-forward re nil t)))))))

(defun epackage-ssh-config-strict-key-check-disabled-p (&optional host)
  "Check that HOST is set to \"StrictHostKeyChecking no\" in ~/.ssh/config."
  ;; We can't allow any interactive prompts like this:
  ;;
  ;; git clone git@github.com:<url>
  ;; Initialized empty Git repository in <path>
  ;; The authenticity of host 'github.com (65.74.177.129)' can't be established.
  ;; RSA key fingerprint is 16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48.
  ;; Are you sure you want to continue connecting (yes/no)?
  ;;
  (let ((file "~/.ssh/config"))
    (cond
     ((not (file-exists-p file))
      (error (concat "Epacakge: [ERROR] File does not exist: %s. "
		     "Please install SSH" file)))
     (t
      (with-current-buffer (find-file-noselect file)
	;; We need to be notified about changes
	(epackage-turn-on-auto-revert-mode)
	(goto-char (point-min))
	(let ((re (format "^[ \t]*Host[ \t]+.*%s" (regexp-quote host))))
	  (re-search-forward re nil t)))))))

(defun epackage-ssh-help-string (host)
  "Return SSH configuration help message."
  (format
   "\
PROBLEM

    SSH is not configured to be used to access git repositories.

DESCRIPTION

    When git with a ssh protocol is being used, ssh may ask question like this:

	The authenticity of host 'github.com (207.97.227.239)' can't be established.
	RSA key fingerprint is 16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48.
	Are you sure you want to continue connecting (yes/no)? no

    As epackage.el cannot answer to these kind interactive
    questions, the SSH must be configured so that it doesn't ask
    interactive questions.

SOLUTIONS

    (A) Connect manually from command line at least once to %s
        so that host is recorded to ~/.ssh/known_hosts
    (B) Or add to file ~/.ssh/config line:
	Host %s\n\tStrictHostKeyChecking no"
   host
   host))

(defsubst epackage-ssh-url-p (url)
  "Check if URL uses SSH protocol."
  (string-match "ssh://\\|[^@]+@[^:]+:\\|^[^@:]+@[^:]+:" url))

(defsubst epackage-ssh-p (host)
  "Check HOST is known to SSH."
  ;; SSH Protocols are:  ssh://  and  user@host:<path>
  ;; See http://www.kernel.org/pub/software/scm/git/docs/git-clone.html
  (or (epackage-ssh-known-host-p host)
      (epackage-ssh-config-strict-key-check-disabled-p host)))

(defun epackage-require-ssh (url)
  "If Git protocol is SSH, require direct access to SSH without prompts."
  ;; FIXME: Can we test if ssh-agent is running?
  (when (epackage-ssh-url-p url)
    (let ((host (epackage-url-extract-host url)))
      (or (and (stringp host)
	       (epackage-ssh-p host))
	  (let ((epackage-princ (epackage-ssh-help-string host)))
	    (epackage-princ message) ;Record user help to *Messages* buffer
	    (let ((debug-on-error nil)) ;; batch UI: don't display stack trace
	      (error
	       (substitute-command-keys
		(concat "Epackage: [ERROR] SSH configuration problem. "
		      "See recent message with "
		      "\\[view-echo-area-messages]")))))))))

(defun epackage-require-main (&optional verbose)
  "Check requirements to run Epackage.
If optional VERBOSE is non-nil, display progress message."
  ;; FIXME, url.el not yet used.
  ;; (epackage-require-emacs verbose)
  (epackage-require-git verbose)
  (epackage-require-directories verbose))

(defun epackage-url-encode (string)
  "Encode STRING suitable for URL."
  ;; FIXME: This is simplistic. Should probebly use something
  ;; already in Emacs; mmencode*.el or the like.
  (let ((list
         '((":" "%3A")
           ("[+]" "%2B")
           ("\"" "%22")
           ("[ \t]+" "+")))
        re
        replace)
    (dolist (elt list)
      (setq re (nth 0 elt)
            replace (nth 1 elt))
      (setq string (replace-regexp-in-string re replace string)))
    string))

(defun epackage-url-http-parse-respons-error (&optional url)
  "On HTTP GET error, show reponse and signal error for optional URL."
  (let ((status (url-http-parse-response)))
    (when (or (< status 200)
              (>= status 300))
      (display-buffer (current-buffer))
      (error "[ERROR] HTTP access problem %d%s"
             status
             (if url
                 (concat " " url)
               "")))))

(defun epackage-url-retrieve-buffer (url)
  "Download URL and return buffer. Point is at the beginning of data."
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (epackage-error "Can't access url: %s" url))
    (with-current-buffer buffer
      (epackage-url-http-parse-respons-error url)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      buffer)))

(defun epackage-url-save-to-file (url file)
  "Download URL and save to a FILE."
  (let ((buffer (epackage-url-retrieve-buffer url)))
    (unless buffer
      (epackage-error "Can't access url: %s" url))
    (with-current-buffer buffer
      (epackage-with-binary
        (epackage-write-region (point) (point-max) file)
        (kill-buffer (current-buffer))))))

;;; .................................................. &functions-lint ...

(defun epackage-pkg-lint-info-buffer-field-license (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
	(str (epackage-field-fetch-value "License"))
	(regexp `,(concat "^" epackage--info-mode-license-regexp)))
    (when (and str
	       (not (string-match regexp str)))
      (epackage-verbose-message
	"[ERROR] Lint - field Licence, invalid value: %s" str)
      (setq status nil))
    status))

(defun epackage-pkg-lint-info-buffer-field-package (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
	(str (epackage-field-fetch-value "Package")))
    (when (and str
	       (not (epackage-package-name-valid-p str)))
      (epackage-verbose-message
	"[ERROR] Lint - field Package, invalid value: %s" str)
      (setq status nil))
    status))

(defun epackage-pkg-lint-finder-known-keywords ()
  "Return list of valid finder keywords."
  (require 'finder)
  (mapcar
   (lambda (x)
     (symbol-name (car x)))
   finder-known-keywords))

(defun epackage-pkg-lint-info-buffer-field-section (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let* ((status t)
	 (str (epackage-field-fetch-value "Section"))
	 (list (and str
		    (split-string str))))
    (when list
      (let ((keywords (epackage-pkg-lint-finder-known-keywords)))
	(dolist (value list)
	  (unless (member value keywords)
	    (epackage-verbose-message
	      "[ERROR] Lint - field Section, invalid value: %s" str)
	    (setq status nil)))))
    status))

(defun epackage-pkg-lint-info-buffer-field-maintainer (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
	(str (epackage-field-fetch-value "Maintainer")))
    (when (and str
	       (not (string-match "@" str)))
	(epackage-verbose-message
	  "[ERROR] Lint - field Maintainer, invalid email: %s" value)
	(setq status nil))
    status))

(defun epackage-pkg-lint-info-buffer-field-upstream (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
	(str (epackage-field-fetch-value "Upstream")))
    (when (and str
	       (not (string-match "@" str)))
	(epackage-verbose-message
	  "[ERROR] Lint - field Upstream, invalid email: %s" value)
	(setq status nil))
    status))

(defun epackage-pkg-lint-info-buffer-field-status (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let* ((status t)
	 (str (epackage-field-fetch-value "Status"))
	 (list (and str
		    (split-string str))))
    (dolist (value list)
      (unless (member value epackage--info-mode-completions-status)
	(epackage-verbose-message
	  "[ERROR] Lint - field Status, unknown value: %s" value)
	(setq status nil)))
    status))

(defun epackage-pkg-lint-info-buffer-field-all (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t))
    (dolist (function
	     '(epackage-pkg-lint-info-buffer-field-package
	       epackage-pkg-lint-info-buffer-field-section
	       epackage-pkg-lint-info-buffer-field-license
	       epackage-pkg-lint-info-buffer-field-status
	       epackage-pkg-lint-info-buffer-field-upstream
	       epackage-pkg-lint-info-buffer-field-maintainer))
    (unless (funcall function verbose)
      (setq status nil)))
    status))

(defun epackage-pkg-lint-info-buffer-main (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
        field
        value
        regexp)
    (dolist (elt epackage--info-layout-mapping)
      (setq field  (nth 0 elt)
            regexp (nth 1 elt)
            value  (epackage-field-fetch-value field))
      (cond
       ((not (stringp value))
        (epackage-verbose-message
          "[ERROR] Lint - missing required field: %s" field)
        (setq status nil))
       ((not (string-match regexp value))
        (epackage-verbose-message
          "[WARN] Lint - required field syntax error: %s => '%s'"
          field value)
        (setq status nil))))
    (unless (epackage-pkg-lint-info-buffer-field-all verbose)
      (setq status nil))
    status))

(defun epackage-pkg-lint-info-file (file &optional verbose)
  "Check validity of info FILE.
If optional VERBOSE is non-nil, display progress message."
  (with-current-buffer (find-file-noselect file)
    (let* ((dir (epackage-file-name-directory-previous
                 (file-name-directory file)))
           (name (epackage-file-name-basename dir))
           (package (epackage-field-fetch-value "Package")))
      (when (and verbose
               (stringp package)
               (not (string= package name)))
        (epackage-verbose-message
         "[WARN] Lint - field Package does not match directory name: %s, %s"
         package name))
    (epackage-pkg-lint-info-buffer-main verbose))))

(defun epackage-pkg-lint-git-branches (dir &optional verbose)
  "Check validity Git branches of package in DIR.
If optional VERBOSE is non-nil, display progress message.
If valid, return list of required branches."
  (let ((list (epackage-git-command-branch-list
               dir (not 'verbose) "-a"))
        branches
        status
        master
        upstream)
    (dolist (elt list)
      ;; * master
      ;; remotes/origin/upstream
      (unless (and master upstream)
        (when (string-match "^\\(\\*?master\\)$" elt)
          (setq master elt))
        (when (string-match "\\(\\(?:.+/\\)upstream\\)$" elt)
          (setq upstream elt))))
    (if verbose
        (cond
         ((null master)
          (epackage-verbose-message
            "[ERROR] Lint - missing required git branch: master"))
         ((null upstream)
          (epackage-verbose-message
            "[ERROR] Lint - missing required git branch: upstream"))))
    (if (and master upstream)
        (list master upstream))))

(defun epackage-pkg-lint-dir-structure (dir &optional verbose)
  "Check valid directories of package in DIR.
If optional VERBOSE is non-nil, display progress message.
The base name of DIR is taken as the package name. An example:

  ~/.emacs.d/epackage/package/foo  => 'foo' is the package name

Return:
    t  if valid."
  (let* ((package (epackage-file-name-nondirectory dir))
	 (lib-p   (epackage-package-name-library-p package))
	 (status t)
	 list
	 name
	 required
	 type
	 path)
    (dolist (elt epackage--layout-mapping)
      (setq name     (nth 1 elt)
            required (nth 2 elt)
            type     (nth 3 elt))
      (if (string-match "-" name)
          (setq path (format "%s%s/%s%s"
                             (file-name-as-directory dir)
                             epackage--directory-name
                             package
                             name))
        (setq path (format "%s%s/%s"
                           (file-name-as-directory dir)
                           epackage--directory-name
                           name)))
      (when (and required
		 (or (null type)	;applies to all packages
		     (and lib-p
			  (memq 'lib type)))
		 (not (file-exists-p path)))
        (epackage-verbose-message
          "[ERROR] Lint - missing required file: %s" path)
        (setq status nil)))
    status))

;;;###autoload
(defun epackage-pkg-lint-directory (dir &optional verbose)
  "Check validity of package in DIR.
If optional VERBOSE is non-nil, display progress message.

If invalid, return list of classified problems:
  'dir      Missing `epackage--directory-name'
  'files    Missing required `epackage--layout-mapping'.
  'info     Missing file or required fields in info file.
  'git      Missing required Git branches: upstream, master."
  (interactive "DLint epackage directory: ")
  (if (interactive-p)
      (setq verbose 'interactive))
  (let ((edir (format "%s%s"
                      (file-name-as-directory dir)
                      epackage--directory-name))
        list)
    (unless (epackage-pkg-lint-git-branches dir verbose)
      (epackage-push 'git list))
    (cond
     ((not (file-directory-p edir))
      (epackage-verbose-message "[FATAL] Lint - Missing directory: %s" edir)
      (epackage-push 'dir list))
     (t
      (unless (epackage-pkg-lint-dir-structure dir verbose)
        (epackage-push 'files list))
      (let ((file
             (format "%s%s/%s"
                     (file-name-as-directory dir)
                     epackage--package-control-directory
                     epackage--pkg-info-file-name)))
        (cond
         ((file-exists-p file)
          (unless (epackage-pkg-lint-info-file file verbose)
            (epackage-verbose-message "[FATAL] Lint - Missing file: %s" file)
            (epackage-push 'info list)))
         (t
          (epackage-push 'info list))))))
    list))

(defun epackage-pkg-lint-git-url (package &optional verbose)
  "Check that 'origin' URL of PACKAGE to match the one in sources list.
If optional VERBOSE is non-nil, display progress message.
Existence of PACKAGE is not checked.

Return:

   In case there is are mismatched

   '(PACKAGE SOURCES-URL GIT-URL)."
  (let ((url (epackage-sources-list-info-url package))
        (git (epackage-git-config-fetch-field package "remote.*origin" "url")))
    (cond
     ((not (stringp url))             ;FIXME: perhaps better handling
      nil)
     ((not (stringp git))
      nil)
     ((not (string= url git))
      ;; Sources list is not in synch
      (epackage-verbose-message
        "[FATAL] Lint - URL in sources list and git config differ: %s vs, %s"
        url git)
      (list package url git))
     (t
      nil))))

(defun epackage-pkg-lint-downloaded-git-url (package &optional verbose)
  "Check each downloded PACKAGE: that Git 'origin' URL match sources list.
If optional VERBOSE is non-nil, display progress message.

Return prblems:
    '((PACKAGE SOURCES-URL GIT-URL) ...)."
  (let (list
        elt)
  (dolist (package (epackage-status-downloaded-packages))
    (if (setq elt (epackage-pkg-lint-git-url package verbose))
        (epackage-push elt list)))
  list))

(defun epackage-pkg-lint-results (point dir &optional display)
  "Collect Lint results from `epackage--buffer-emacs-messages' at POINT forward.
Display DIR in heading. Optional DISPLAY show `epackage--buffer-lint'."
  (let ((buffer (get-buffer-create epackage--buffer-lint)))
    (with-current-buffer buffer
      (insert (format "-- Lint %s %s\n"
                      (epackage-time)
                      dir)))
    (epackage-with-buffer-emacs-messages
      ;; Start reading Lint messages
      (goto-char point)
      (while (re-search-forward
              "^Epackage:.+Lint - \\(.+\n\\)" nil t)
        (append-to-buffer
         buffer (match-beginning 1) (match-end 1)))
      (if display
          (display-buffer buffer)))))

;;;###autoload
(defun epackage-pkg-lint-directory-interactive (dir)
  "Like `epackage-pkg-lint-directory' but show results from DIR."
  (interactive "DEpackage Lint directory (root): ")
  (if (or (not dir)
          (not (file-directory-p dir)))
      (epackage-error "Can't Lint. No director: %s" dir)
    (let ((point (epackage-with-buffer-emacs-messages
                   (point))))
      (epackage-initialize)
      (epackage-pkg-lint-directory dir)
      (epackage-pkg-lint-results point dir 'display))))

;;;###autoload
(defun epackage-pkg-lint-package (package &optional verbose)
  "Check validity of PACKAGE in DIR.
If optional VERBOSE is non-nil, display progress message.
With VERBOSE display `epackage--buffer-lint'.

This function runs checks that are outside of scope of
`epackage-pkg-lint-directory'. E.g. checking the sources list
in respect to package.

Return:
    See function `epackage-pkg-lint-directory' plus
    value 'git-config if URL does not match sources list."
  (interactive
   (list (epackage-cmd-select-package "Lint package: ")
         'interactive))
  (let ((dir (epackage-directory-package-root package))
        point)
    (if (or (not dir)
            (not (file-directory-p dir)))
        (if verbose
            (epackage-message "Can't Lint. Package does not exist: %s" dir)
          (epackage-error "Can't Lint. Package does not exist: %s" dir))
      (prog1                            ; else
          (let (ret)
            (epackage-with-buffer-emacs-messages
              (setq point (point)))
            (setq ret (epackage-pkg-lint-directory dir verbose))
            (if (epackage-pkg-lint-git-url package verbose)
                (epackage-push 'git-config ret))
            ret)
        (when verbose
          (epackage-pkg-lint-results point dir 'show))))))

(defsubst epackage-lint-extra-delimiter-string (id &optional stop)
  "Return start delimiter string ID. ro If STOP is non-nil, stop string."
  (format "Epackage: %s %s%s"
	  (if stop
	      "STOP"
	    "START")
	  id
	  (if buffer-file-name
	      (format " file: %s" buffer-file-name))))

(defsubst epackage-lint-extra-collect-data (&optional id buffer)
  "Return data surrounded by optional ID from *Messages* or optional BUFFER."
  (with-current-buffer (or buffer "*Messages*")
    (goto-char (point-max))
    (let (end)
      (when (re-search-backward
	     (format "^Epackage: STOP%s" 
		     (if id
			 (concat " " id)
		       ""))
	     nil t)
	(setq end (line-beginning-position))
	(when (re-search-backward
	       (format "^Epackage: START%s"
		       (if id
			   (concat " " id)
			 ""))
	       nil t)
	  (forward-line 1)
	  (unless (eq (point) end)
	    (buffer-substring-no-properties (point) end)))))))

(defun epackage-lint-extra-buffer-run-lm ()
  "Run lm-verify on current buffer."
  (message (epackage-lint-extra-delimiter-string "lm-verify"))
  (lm-verify (not 'file) (not 'showok) (not 'verbose) 'non-fsf-ok)
  ;; Run extra lm checks, see checkdoc-file-comments-engine
  (unless (lm-summary)
    (message "Missing: ;;; package --- Summary"))
  (unless (lm-copyright-mark)
    (message "Missing: ;; Copyright (C) YYYY First Last <address@example.com>"))
  (unless (lm-commentary-mark)
    (message "Missing: ;;; Commentary:"))
  (unless (lm-history-mark)
    (message "Missing: ;;; History:"))
  (unless (lm-code-start)
    (message "Missing: ;;; Code:"))
  (message (epackage-lint-extra-delimiter-string "lm-verify" 'stop))
  (epackage-lint-extra-collect-data))

(defun epackage-lint-extra-buffer-checkdoc-collect-data ()
  "Return checkdoc data after `epackage-lint-extra-buffer-run-checkdoc'."
  (epackage-with-checkdoc-buffer
    (goto-char (point-min))
    (epackage-buffer-remove-empty-lines)
    (goto-char (point-min))
    (when (re-search-forward "^[*][*][*]" nil t)
      (forward-line 1)
      (let ((point (point)))
	;; Delete extra separators
	(while (re-search-forward "^[*][*][*]" nil t)
	  (delete-region (1- (line-beginning-position))
			 (line-end-position)))
	(goto-char point)
	(unless (eobp)
	  (sort-lines nil (point) (point-max))
	  (goto-char (point-max))
	  ;; Final newline
	  (unless (eq (point) (line-beginning-position))
	    (insert "\n"))
	  (buffer-substring-no-properties point (point-max)))))))

(defun epackage-lint-extra-buffer-run-checkdoc ()
  "Run lm-verify on current buffer."
  (require 'checkdoc)
  (let ((file buffer-file-name)
	(checkdoc-bouncy-flag t)	; No silent fixes
	(checkdoc-arguments-in-order-flag t)
	(checkdoc-verb-check-experimental-flag t)
	checkdoc-spellcheck-documentation-flag ;None, too expensive
	(checkdoc-force-docstrings-flag t)
	(checkdoc-force-history-flag t)
	(checkdoc-permit-comma-termination-flag t)
	(checkdoc-autofix-flag 'never)
	(checkdoc-generate-compile-warnings-flag t))
    ;;  (message (epackage-lint-extra-delimiter-string "checkdoc"))
    (epackage-with-checkdoc-buffer
     (epackage-erase-buffer))
    ;; Too bad (checkdoc-current-buffer 'take-notes) would call
    ;; `checkdoc-show-diagnostics'
    ;;
    ;; The following is slimlined version of checkdoc-current-buffer
    ;; - Drop `checkdoc-show-diagnostics' as it would also show, but
    ;;   run aleady ran lm-verify checks.
    (checkdoc-start-section "checkdoc-current-buffer")
    (goto-char (point-min))
    (checkdoc-continue 'take-notes)
    (checkdoc-message-text 'take-notes)
    (checkdoc-rogue-spaces 'take-notes)
    (epackage-lint-extra-buffer-checkdoc-collect-data)))

(defun epackage-lint-extra-buffer-run-elint ()
  "Run `elint-current-buffer' on current buffer.
This is a heavy check and first time initializing will take time."
  (require 'elint)
  ;; From elint-current-buffer, skip displaying buffer
  (or elint-builtin-variables
      (elint-initialize))
  (elint-clear-log (format "Elint %s" (or (buffer-file-name)
					  (buffer-name))))
  (mapc 'elint-top-form (elint-update-env))
  (with-current-buffer elint-log-buffer
    (goto-char (point-min))
    (forward-line 2)
    (unless (eobp)
      (buffer-substring-no-properties (point) (point-max)))))

(defun epackage-lint-extra-buffer-run-other ()
  "Run miscellaneous checks on current buffer.
In order to collect results."
  ;; (message (epackage-lint-extra-delimiter-string "miscellaneous"))
  ;; (message (epackage-lint-extra-delimiter-string "miscellaneous" 'stop))
  ;; (epackage-lint-extra-collect-data)
  (let (str)
    (goto-char (point-min))
    (unless (re-search-forward "^;;;###autoload" nil t)
      (setq str "Missing: ;;;###autoload\n"))
    str))

;;;###autoload
(defun epackage-lint-extra-buffer-main (&optional verbose)
  "Lint current buffer using extra tools.
If optional VERBOSE is non-nil, display progress message.
With VERBOSE display `epackage--buffer-lint'.

`buffer-file-name', if exists, is used for identification.

Return:
  '(ERROR ...)     Type of errors if any."
  (interactive (list 'interactive))
  ;; Lint with all we've got and collect results
  (let ((file (if buffer-file-name
		  (format " file: %s" buffer-file-name)
		""))
	str
	errors)
    (epackage-with-lint-buffer
      (goto-char (point-max))
      ;; Stars are there to support `outline-minor-mode'.
      (insert (format "*** Epackage Lint %s%s\n" (epackage-time) file)))
    (when (setq str (epackage-lint-extra-buffer-run-lm))
      (epackage-with-lint-buffer
	(goto-char (point-max))
	(insert "** Lint lm-verify\n")
	(insert str)
	(epackage-push 'lisp-mnt errors)))
    (epackage-verbose-message "Lint running: miscellaneous...")
    (when (setq str (epackage-lint-extra-buffer-run-other))
      (epackage-with-lint-buffer
	(goto-char (point-max))
	(insert "** Lint miscellaneous\n")
	(insert str)
	(epackage-push 'miscellaneous errors)))
    (epackage-verbose-message "Lint running: checkdoc...")
    (when (setq str (epackage-lint-extra-buffer-run-checkdoc))
      (epackage-with-lint-buffer
	(goto-char (point-max))
	(insert "** Lint checkdoc\n")
	(insert str)
	(epackage-push 'checkdoc errors)))
    (epackage-verbose-message "Lint running: elint...")
    (when (setq str (epackage-lint-extra-buffer-run-elint))
      (epackage-with-lint-buffer
	(goto-char (point-max))
	(insert "** Lint elint\n")
	(insert str)
	(epackage-push 'checkdoc errors)))
    (if verbose
	(display-buffer epackage--buffer-lint))
    errors))

;;;###autoload
(defun epackage-lint-extra-file (file &optional verbose)
  "Lint FILE using extra tools.
If optional VERBOSE is non-nil, display progress message.
With VERBOSE display `epackage--buffer-lint'."
  (interactive
   (list
    (read-file-name "File to Lint extra: ")
    'interactive))
  (let ((new-p (null (get-file-buffer file)))
	buffer)
    (with-current-buffer (find-file-noselect file)
      (epackage-lint-extra-buffer-main verbose)
      (if new-p
	  (setq buffer (current-buffer))))
    (if buffer				; We loaded it, clean up
	(kill-buffer buffer))))

;;; .................................................. &functions-misc ...

(defun epackage-cmd-select-package (&optional message list)
  "Interactively select package with optional MESSAGE from LIST.
Return package name or nil."
  (let (package)
    (if (not (epackage-sources-list-p))
        (epackage-message
          "%s"
          (substitute-command-keys
           `,(concat
              "Can't build package list. "
              "Run \\[epackage-cmd-sources-list-download]")))
      (setq package
            (completing-read
             (if message
                 message
               "Select epackage: ")
             (or list
                 (epackage-sources-list-info-pkg-list))
             (not 'predicate)
             'require-match))
      (if (epackage-string-p package)
          package))))

(put 'epackage-cmd-package-check-macro 'lisp-indent-function 3)
(put 'epackage-cmd-package-check-macro 'edebug-form-spec '(body))
(defmacro epackage-cmd-package-check-macro
  (package verbose message &rest body)
  "Check PACKAGE, be VERBOSE. If nok, display/signal MESSAGE. If ok, run BODY."
  `(cond
    ((or (null package) ;User pressed RETURN to not select any.
         (and (stringp package)
              (string-match "^[ \t]*$" package))))
    ((and (stringp package)
          (not (member package (epackage-sources-list-info-pkg-list))))
     (if (eq ,verbose 'interactive)
         (epackage-warn (format "Unknown package \"%s\"" package))
       (epackage-error (format "Not a known package \"%s\"" package))))
    ((epackage-string-p ,package)
     ,@body)
    ((eq ,verbose 'interactive)
     (epackage-message ,message))
    (t
     (epackage-error ,message))))

(put 'epackage-mail-macro 'lisp-indent-function 2)
(put 'epackage-mail-macro 'edebug-form-spec '(body))
(defmacro epackage-mail-macro (buffer-name to &rest body)
  "Compose mail in BUFFER-NAME, set TO and run BODY."
  `(progn
     (pop-to-buffer ,buffer-name)
     (mail-setup
      ,to
      (not 'subject)
      (not 'in-reply-to)
      (not 'cc)
      (not 'replybuffer)
      (not 'actions))
     ,@body))

(defsubst epackage-mail-buffer-name (package &optional string)
  "Compose email buffer name from PACKAGE and optional STRING."
  (format "*mail epackage %s%s*"
          package
          (if string
              string
            "")))

;;; ....................................................... &mode-info ...

(defun epackage-info-mode-pcomplete-arguments ()
  "Return pcomplete data."
  (save-excursion
    (when (and (not (bobp))
               ;; Previous char must not be space
               (not (eq 32 (char-syntax (char-after (1- (point)))))))
      (let* ((point (point))
             (back  (search-backward-regexp "[ \t\n]" nil t))
             (beg   (if back
                        (+ back 1)
                      point)))
        (list (list "dummy"
                    (buffer-substring-no-properties beg point))
              (point-min) beg)))))

(defun epackage-info-mode-pcomplete-default-completion ()
  "Run pcomplete on `package-info-mode-completions'."
  (pcomplete-here epackage--info-mode-completions-current))

(defun epackage-info-mode-set-pcomplete-variables ()
  "Set up pcomplete variables."
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'epackage-info-mode-pcomplete-arguments)
  (set (make-local-variable 'pcomplete-default-completion-function)
       'epackage-info-mode-pcomplete-default-completion))

(defun epackage-info-mode-set-variables ()
  "Define buffer local variables."
  (set (make-local-variable 'paragraph-start)
       " [^ .\t\r\n]\\|[ \t]+\\.[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t]+\\.[ \t]*$")
  ;;  'Field:' starts a paragraph
  (set (make-local-variable 'adaptive-fill-first-line-regexp)
       "[ \t]*\\.?*$\\|[ \t]*[#>*o][ \t]*")
  (set (make-local-variable 'adaptive-fill-regexp)
       "[ \t]+\\([#>*◦]+[ \t]*\\)*")
  (set (make-local-variable 'tab-stop-list) ;By 4
       '(1 4 8 12 16 20 24 28 32 36 40 44
           48 52 56 60 64 68 72 76 80 84
           88 92 96 100 104 108 112 116 120))
  (set (make-local-variable 'adaptive-fill-mode) t)
  (set (make-local-variable 'colon-double-space) nil)
  (setq fill-prefix " ")
  (setq fill-column 75)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'tab-always-indent) nil)
  (set (make-local-variable 'version-control) 'never)
  (set (make-local-variable 'whitespace-style)
       '(space-before-tab::tab tabs trailing lines-tail)))

(defun epackage-info-mode-set-font-lock ()
  "Define font-lock variable for buffer."
  (set (make-local-variable 'font-lock-defaults)
       '(epackage--info-mode-font-lock-keywords nil t nil nil))
  (if (or font-lock-mode
          global-font-lock-mode)
      (font-lock-fontify-buffer)))

;;;###autoload
(define-minor-mode epackage-info-mode
  "Toggle mode for editing `epackage--pkg-info-file-name'.

With arg, turn mode on if and only if arg is positive.
This is a minor mode that helps editing epackage control files.

See TAB behavior in various fields and positions in
function description of `epackage-info-mode-tab-command'.

\\{epackage-info-mode-map}"
  :group 'epackage :lighter epackage-info-mode-name
  (epackage-info-mode-set-variables)
  (epackage-info-mode-set-pcomplete-variables)
  (epackage-info-mode-set-font-lock)
  (auto-fill-mode 1)
  (whitespace-mode 1)
  epackage-info-mode)

;;;###autoload
(defun turn-on-epackage-info-mode ()
  "Turn on Epackage Info Mode."
  (epackage-info-mode 1))

;;;###autoload
(defun turn-on-epackage-info-mode-maybe ()
  "Turn on Epackage Info Mode if `epackage-pkg-info-p' return non-nil."
  (when (epackage-pkg-info-p)
    (turn-on-epackage-info-mode)))

;;;###autoload
(defun turn-off-epackage-info-mode ()
  "Turn off Epackage Info Mode."
  (epackage-info-mode -1))

(defun epackage-info-mode-cmd-goto-description ()
  "Goto field 'Description:'."
  (interactive)
  (mail-position-on-field "Description" 'soft))

(defun epackage-info-mode-cmd-goto-status ()
  "Goto field 'Status:'."
  (interactive)
  (mail-position-on-field "Status" 'soft))

(defun epackage-info-mode-cmd-email-maintainer ()
  "Compose mail to epackage maintainer."
  (interactive)
  (let ((package (epackage-field-fetch-value "Package"))
        (email (epackage-field-fetch-value "Maintainer")))
    (if email
        (epackage-mail-macro
            (epackage-mail-buffer-name package " maintainer")
	    email)
      (epackage-message "No maintainer information to email to."))))

(defun epackage-info-mode-cmd-email-upstream ()
  "Compose mail to upstream of extension."
  (interactive)
  (let ((package (epackage-field-fetch-value "Package"))
        (email (epackage-field-fetch-value "Upstream")))
    (if email
        (epackage-mail-macro
            (epackage-mail-buffer-name package " upstream")
	    email)
      (epackage-message "No upstream information to email to."))))

(defun epackage-info-mode-cmd-email-upstream-ping ()
  "Compose a ping mail to upstream of extension.
Use this function for the first time contact of Upstream to
verify his email address and to and what address he prefers to
use for contacts."
  (interactive)
  (let ((package (epackage-field-fetch-value "Package"))
        (email (epackage-field-fetch-value "Upstream")))
    (if email
        (epackage-mail-macro
            (epackage-mail-buffer-name package " upstream")
	    email
	  (if (stringp epackage--info-mode-email-ping-subject)
	      (epackage-field-set
	       "Subject"
	       (format
		epackage--info-mode-email-ping-subject
		package)))
	  (goto-char (mail-text-start))
	  (when (stringp epackage--info-mode-email-ping-body)
	    (insert epackage--info-mode-email-ping-body)
	    (when (stringp user-full-name)
	      (insert user-full-name "\n"))))
      (epackage-message "No upstream information to email to."))))

(defun epackage-info-mode-cmd-url-homepage ()
  "Visit Homepage."
  (interactive)
  (let ((url (epackage-field-fetch-value "Homepage")))
    (if url
        (browse-url url)
      (epackage-message "No Homepage URL"))))

(defun epackage-info-mode-cmd-url-wiki ()
  "Visit Wikipage."
  (interactive)
  (let ((url (epackage-field-fetch-value "Wiki")))
    (if url
        (browse-url url)
      (epackage-message "No Wiki URL"))))

(defun epackage-info-mode-cmd-url-commentary ()
  "Run `finder-commentary' on field 'Commentary:'."
  (interactive)
  (let ((file (epackage-field-fetch-value "Commentary")))
    (if (not file)
        (epackage-message "No Commentary field defined")
      (let* ((root (epackage-file-name-directory-previous
                    (file-name-directory buffer-file-name)))
             (dir root)
             (load-path load-path))
        (when (string-match "/" file)
          (setq dir (format "%s/%s"
                            root
                            (file-name-directory file)))
          (setq file (file-name-nondirectory file)))
        (epackage-push dir load-path)
        (finder-commentary file)))))

(defun epackage-info-mode-cmd-lint (dir)
  "Lint epackage in DIR.
In interactive call, the point must be in buffer visiting
`epackage--pkg-info-file-name' under directory `epackage--directory-name'."
  (interactive
   (list
    (epackage-file-name-directory-previous
     (file-name-directory buffer-file-name))))
  (epackage-pkg-lint-directory-interactive dir))

(defun epackage-info-mode-move-beginning-of-line ()
  "Move to beginnig of field, or to the left margin if already there."
  (interactive)
  (let (para-start
        field-start
        field-space)
    (save-excursion
      ;; Check if the beginning of indented 'Description:' paragraph.
      (setq para-start
            (string-match "^[ \t]+$" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (point))))
      (goto-char (line-beginning-position))
      (when (looking-at "^\\([^ \t\r\n]+:\\) *")
        (setq field-start (match-end 1))
        (setq field-space (match-end 0))))
    (cond
     ((and field-space
           (> (point) field-space))
      (goto-char field-space))
     (t
      ;; (move-to-left-margin)
      (goto-char (line-beginning-position))
      (if (and (not para-start)
               (looking-at "^\\([ \t]+\\)[^ \t\r\n]"))
          (goto-char (match-end 1)))))))

(defun epackage-info-mode-pcomplete-list (list)
  "Pcomplete LIST at current point or call `forward-word'."
  (let* ((epackage--info-mode-completions-current list)
         (syntax (char-to-string (char-syntax (char-after (point)))))
         (space-p (string= " " syntax))
         (point (point))
         (word (symbol-at-point))       ;Words with '-' characters
         complete-list
         try)
    (when word
      (setq word (symbol-name word))
      (setq complete-list (all-completions word list))
      (setq try (try-completion word list)))
    (cond
     (space-p
      (cond
       ((eq 1 (length complete-list))
        (pcomplete))
       ((and complete-list
             (not (eq (length word) (length try))))
        (insert (substring try (length word)))
        (message
         "Completions: %s" (mapconcat #'concat complete-list " ")))
       (complete-list
        (message
         "Completions: %s" (mapconcat #'concat complete-list " ")))
       (t
        (message
         "Completions: %s" (mapconcat #'concat list " ")))))
     (t
      (forward-word 1)))))

(defun epackage-info-mode-tab-wiki ()
  "Handle 'Wiki' field. See `epackage-info-mode-tab-command'."
  (cond
   ((and (not (bolp))
         (eolp)
         (string-match ":" (buffer-substring-no-properties
			    (- (point) 2) (point))))
    (insert "http://www.emacswiki.org/emacs/")
    (goto-char (line-end-position)))
   ((and (not (bolp))
         (eolp)
         (string-match "/" (buffer-substring-no-properties
			    (- (point) 1) (point))))
    (let ((search
           "http://www.google.fi/search?hl=en&oe=UTF-8&num=100&q=")
          (str (read-string "Google (empty = cancel): "
                            "site:emacswiki.org ")))
      (when (epackage-string-p str)
        (browse-url (format "%s%s" search (epackage-url-encode str))))))
   (t
    (forward-word 1))))

(defun epackage-info-mode-tab-standard ()
  "The normal tab for use in 'Description:' field."
  (cond
   ((and (bolp)
         (looking-at "^[ \t]+"))
    (goto-char (match-end 0)))
   (t
    (tab-to-tab-stop))))

(defun epackage-info-mode-completions-section-initialize ()
  "Initialize variable `epackage--info-mode-completions-section'.
The values are list of cars from `finder-known-keywords'."
  (unless epackage--info-mode-completions-section
    (require 'finder)			; To define finder-known-keywords
    (setq epackage--info-mode-completions-section
          (mapcar (lambda (elt)
                    (symbol-name (car elt)))
                  finder-known-keywords))))

(defun epackage-info-mode-tab-command ()
  "COntext sensitive tab. Complete at certain fields.
If at field, move to value:

   Field: Value         => Field: Value
     *                            *
     TAB

In fields 'Status' and' License', complete word.

   Status: un           => Status: unmaintained
             *
             TAB

   License: G           => License: GPL-2+
             *
             TAB

In fields 'Status' and 'License', if on space, show available
completions:

   License: *
            TAB

In field 'Wiki' insert base URL, or offer Google search

    Wiki: *             => Wiki: http://www.emacswiki.org/emacs/
          TAB

    Wiki: http://www.emacswiki.org/emacs/
                                         *
                                         TAB
                                         Invokes Google Search

In field 'Section', complete values of `finder-list-keywords'.

In field 'Description', at beginning of line insert or move to
indentation. Elsewhere run `indent-for-tab-command'.

In other fields, run `forward-word'."
  (interactive)
  (let ((case-fold-search t)
        (point (point))
        (field (epackage-field-name)))
    ;; Field:
    ;;   *                 <= point somewhere in field name
    (cond
     ((and (looking-at "[^ \t\r\n]+: *")
           (setq point (match-end 0))
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at "^[^ \t\r\n]+:")))
      (goto-char point))
     ;; Complete inside VALUE part of field
     ((and (stringp field)
           (string-match "status" field))
      (epackage-info-mode-pcomplete-list
       epackage--info-mode-completions-status))
     ((and (stringp field)
           (string-match "section" field))
      (epackage-info-mode-completions-section-initialize)
      (epackage-info-mode-pcomplete-list
       epackage--info-mode-completions-section))
     ((and (stringp field)
           (string-match "license" field))
      (epackage-info-mode-pcomplete-list
       epackage--info-mode-completions-license))
     ((and (stringp field)
           (string-match "wiki" field))
      (epackage-info-mode-tab-wiki))
     ((and (stringp field)
           (string-match "section" field))
      (epackage-info-mode-completions-section-initialize)
      (epackage-info-mode-pcomplete-list
       epackage--info-mode-completions-section))
     ((and (stringp field)
           (not (string-match "description" field)))
      (forward-word 1))
     (t
      (epackage-info-mode-tab-standard)))))

;;;###autoload
(add-hook 'find-file-hook 'turn-on-epackage-info-mode-maybe)

;;; ......................................... &functions-user-commands ...

;;;###autoload
(defun epackage-cmd-download-action-activate-on (&optional verbose)
  "Automatically activate packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'activate)
  (epackage-message "Download action on: activate"))

;;;###autoload
(defun epackage-cmd-download-action-activate-off (&optional verbose)
  "Do not activate packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'activate)
  (epackage-verbose-message "Download action off: activate"))

;;;###autoload
(defun epackage-cmd-download-action-activate-toggle (&optional verbose)
  "Toggle automatic activation of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'activate epackage--download-action-list)
      (epackage-cmd-download-action-activate-off)
    (epackage-cmd-download-action-activate-on)))

;;;###autoload
(defun epackage-cmd-download-action-autoload-on (&optional verbose)
  "Automatically autoload packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'autoload)
  (epackage-verbose-message "Download action on: autoload"))

;;;###autoload
(defun epackage-cmd-download-action-autoload-off (&optional verbose)
  "Do not autoload packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'autoload)
  (epackage-verbose-message "Download action off: autoload"))

;;;###autoload
(defun epackage-cmd-download-action-autoload-toggle (&optional verbose)
  "Toggle automatic autoload of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'autoload epackage--download-action-list)
      (epackage-cmd-download-action-autoload-off)
    (epackage-cmd-download-action-autoload-on)))

;;;###autoload
(defun epackage-cmd-download-action-compile-on (&optional verbose)
  "Automatically compile packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'compile)
  (epackage-verbose-message "Download action on: compile"))

;;;###autoload
(defun epackage-cmd-download-action-compile-off (&optional verbose)
  "Do not compile packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'compile)
  (epackage-verbose-message "Download action off: compile"))

;;;###autoload
(defun epackage-cmd-download-action-compile-toggle (&optional verbose)
  "Toggle automatic compile of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'compile epackage--download-action-list)
      (epackage-cmd-download-action-compile-off)
    (epackage-cmd-download-action-compile-on)))

;;;###autoload
(defun epackage-cmd-download-action-depends-on (&optional verbose)
  "Install depends packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'package-depends)
  (epackage-verbose-message "Download action on: depends"))

;;;###autoload
(defun epackage-cmd-download-action-depends-off (&optional verbose)
  "Do not install depends packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'package-depends)
  (epackage-verbose-message "Download action off: depends"))

;;;###autoload
(defun epackage-cmd-download-action-depends-handling (mode &optional verbose)
  "Set MODE to variable `epackage--depends-handling'.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (let ((list
          '(("warn" . warn)
            ("error" . error)))
         answer)
     (setq answer
           (completing-read
            "Set depends handling mode to: "
            list
            (not 'predicate)
            'require-match))
     (list (cdr-safe (assoc answer list)))))
  (setq epackage--depends-handling mode)
  (epackage-verbose-message "Depends handling set to: %s" mode))

;;;###autoload
(defun epackage-cmd-download-action-depends-toggle (&optional verbose)
  "Toggle depends install of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'depends epackage--download-action-list)
      (epackage-cmd-download-action-depends-off)
    (epackage-cmd-download-action-depends-on)))

;;;###autoload
(defun epackage-cmd-download-action-enable-on (&optional verbose)
  "Automatically enable packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'enable)
  (epackage-verbose-message "Download action on: enable"))

;;;###autoload
(defun epackage-cmd-download-action-enable-off (&optional verbose)
  "Do not enable packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'enable)
  (epackage-verbose-message "Download action off: enable"))

;;;###autoload
(defun epackage-cmd-download-action-enable-toggle (&optional verbose)
  "Toggle automatic enable of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'enable epackage--download-action-list)
      (epackage-cmd-download-action-enable-off)
    (epackage-cmd-download-action-enable-on)))

;;;###autoload
(defun epackage-cmd-download-action-lint-on (&optional verbose)
  "Automatically lint packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-enable 'lint)
  (epackage-verbose-message "Download action on: lint"))

;;;###autoload
(defun epackage-cmd-download-action-lint-off (&optional verbose)
  "Do not lint packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (epackage-download-action-disable 'lint)
  (epackage-verbose-message "Download action off: lint"))

;;;###autoload
(defun epackage-cmd-download-action-lint-toggle (&optional verbose)
  "Toggle automatic lint of packages after download.
If optional VERBOSE is non-nil, display progress message.
See `epackage--download-action-list'."
  (interactive (list 'interactive))
  (if (memq 'lint epackage--download-action-list)
      (epackage-cmd-download-action-lint-off)
    (epackage-cmd-download-action-lint-on)))

;;;###autoload
(defun epackage-cmd-download-action-display-status ()
  "Show `epackage--download-action-list'."
  (interactive)
  (if (not epackage--download-action-list)
      (epackage-message "No download actions set")
    (epackage-message
      "Download actions: %s"
      epackage--download-action-list)))

;;;###autoload
(defun epackage-cmd-email-maintainer (package &optional verbose)
  "Email maintainer of local PACKAGE.
Mail address can only be read from downloaded (locally installed) packages.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (cond
      ((null list)
       (epackage-message "Nowhere to send email, no downloaded packages")
       (list nil 'interactive))
      (t
       (list
        (epackage-cmd-select-package "Email maintainer of epackage: " list)
        'interactive)))))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for maintainer email command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (let ((to (epackage-pkg-info-fetch-field package "Maintainer")))
        (cond
         ((null to)
          (epackage-warn "No maintainer email available for epacakge %s"
                         package))
         (t
          (epackage-mail-macro
              (epackage-mail-buffer-name package " maintainer")
	      to)))))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Maintainer email ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't email maintainer. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-email-upstream (package &optional verbose)
  "Email upstream of local PACKAGE.
Mail address can only be read from downloaded (locally installed) packages.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (cond
      ((null list)
       (epackage-message "Nowhere to send email, no downloaded packages")
       (list nil 'interactive))
      (t
       (list
        (epackage-cmd-select-package "Email upstream of epackage: " list)
        'interactive)))))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for upstream email command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (let ((to (epackage-pkg-info-fetch-field package "Email")))
        (cond
         ((null to)
          (epackage-warn "No upstram email available for epacakge %s"
                         package))
         (t
          (epackage-mail-macro
              (format "*mail epackage %s upstream*" package)
	      to)))))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Upstream email ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't email upstream. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-display-package-documentation (package &optional verbose)
  "Display local PACKAGE documentation.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Display package documentation: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for documentation command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-pkg-info-documentation package verbose))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Displaying documentation ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't display documentation. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-display-package-info (package &optional verbose)
  "Display local PACKAGE info.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Display epackage info: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for display command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-pkg-info-display package verbose))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Displaying info ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't display info. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-byte-compile-package (package &optional verbose)
  "Byte compile PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Byte compile epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for byte compile command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-byte-compile-package-main package verbose))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Byte compile ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't byte compile. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-autoload-package (package &optional verbose)
  "Install PACKAGE as autoload.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Autoload epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "PACKAGE name \"%s\" is invalid for autoload command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-config-install-autoload package verbose))
    (t
     (if (eq verbose 'interactive)
         (epackage-message
           "Autoload install ignored. Package not downloaded: %s"
           package)
        (epackage-message
          "Can't autoload install. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-enable-package (package &optional verbose)
  "Enable PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Enable epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "package name \"%s\" is invalid for enable command"
              package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-config-install-autoload package verbose)
      (if (epackage-config-install-action 'enable package 'noerr verbose)
          (run-hooks 'epackage--install-enable-hook)
        (epackage-message
          "Broken epackage; contact maintainer. Cannot find enable file.")))
     (t
      (if (eq verbose 'interactive)
          (epackage-message
            "Enable ignored. Package not downloaded: %s"
            package)
        (epackage-message
          "Can't enable. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-disable-package (package &optional verbose)
  "Disable PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Disable epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (format "package name \"%s\" is invalid for disable command"
              package)
    (let ((file (epackage-file-name-install-compose package 'enable)))
      (cond
       ((file-exists-p file)
        (epackage-verbose-message "Delete %s" file)
        (delete-file file)
        (run-hooks 'epackage--install-disabled-hook))
       (verbose
        (epackage-message
          "Disable ignored. No enable files installed for package: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-activate-package (package &optional verbose)
  "Activate PACKAGE autoload files.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Activate epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (epackage-message
        "package name \"%s\" is invalid for activate command"
        package)
    (cond
     ((epackage-package-downloaded-p package)
      (epackage-config-install-autoload package verbose)
      (if (epackage-config-install-action 'activate package 'noerr verbose)
          (run-hooks 'epackage--install-activate-hook)
        (epackage-message
          "Activate ignored. Package does not have optional activate file.")))
     (t
      (if (eq verbose 'interactive)
          (epackage-message
            "Activate install ignored. Package not downloaded: %s"
            package)
        (epackage-message
          "Can't activate install. Package not downloaded: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-deactivate-package (package &optional verbose)
  "Deactivate PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Deactivate epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro
      package
      verbose
      (epackage-error
        "package name \"%s\" is invalid for deactivate command"
        package)
    (let ((file (epackage-file-name-install-compose package 'activate)))
      (cond
       ((file-exists-p file)
        (epackage-verbose-message "Delete %s" file)
        (delete-file file)
        (run-hooks 'epackage--install-deactivate-hook))
       (verbose
        (epackage-message
          "Deactivate ignored. No activate files installed for package: %s"
          package))))))

;;;###autoload
(defun epackage-cmd-clean-package (package &optional verbose)
  "Clean all install configuration files of PACKAGE.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (list
      (if list
          (epackage-cmd-select-package "Disable epackage: " list)
        nil)
      'interactive)))
  (epackage-cmd-package-check-macro
      package
      verbose
      (epackage-error
        "package name \"%s\" is invalid for clean command"
        package)
    (let ((list (epackage-config-delete-all package verbose)))
      (cond
       (list
        (run-hooks 'epackage--install-clean-hook))
       (verbose
        (epackage-message
          "Nothing to clean. No files installed for package: %s"
          package)))
      list)))

;;;###autoload
(defun epackage-cmd-config-uninstall-package (package &optional verbose)
  "Run uninstall configuration of PACKAGE.
If optional VERBOSE is non-nil, display progress message.
To clean configuration files, run function `epackage-cmd-clean-package'.

If uninstall file is provided, it usually clean cleans added functions
from hooks and entries from `auto-mode-alist' etc.

Due to the nature of Emacs symbols, it is not really practial to
try to unstall defined symbols (variables, functions) from runtime
environment. To clean those, reboot Emacs."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (list
      (if list
          (epackage-cmd-select-package "Run uninstall of epackage: " list)
        nil)
      'interactive)))
  (epackage-cmd-package-check-macro
      package
      verbose
      (epackage-error
        "package name \"%s\" is invalid for uninstall command"
        package)
    (let ((file (epackage-directory-packages-control-file
                 package 'uninstall)))
      (cond
       ((file-exists-p file)
        (epackage-config-uninstall-invoke package verbose))
       (t
        (epackage-verbose-message
          "Ignore uninstall. No control file supplied in package: %s" package)
        nil)))))

(defun epackage-sources-list-download (&optional verbose)
  "Download sources list file.
If optional VERBOSE is non-nil, display progress message.
This is a lowlevel function.

Return:
  Non-nil if anything was done."
  (let* ((dir  (epackage-sources-list-official-directory))
	 (file (epackage-file-name-sources-list-official))
	 (need-clone (not (file-directory-p dir)))
	 status)
    (unless need-clone          ;; directory exists, check validity
      (cond
       ((not (epackage-git-directory-p dir))
	(epackage-verbose-message "Deleting corrupted Git dir: %s"
				  (abbreviate-file-name dir))
	;; Something is very wrong. Start from fresh.
	(delete-directory dir 'recursive)
	(setq need-clone t))
       ((not (file-exists-p file))
	(epackage-verbose-message "Restoring state of dir: %s"
				  (abbreviate-file-name dir))
	;; Somebody deleted the file; reset git
	(setq status 'git-checkout-force)
	(epackage-git-command-checkout-force-head dir verbose))))
    (when need-clone
      (setq status 'git-clone)
      (let* ((url epackage--sources-list-url)
	     (host (epackage-url-extract-host url)))
	(epackage-git-command-clone url dir verbose)))
    status))

;;;###autoload
(defun epackage-cmd-sources-list-upgrade (&optional verbose)
  "Upgrade sources list.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (epackage-kill-buffer-sources-list)
  (unless (epackage-sources-list-p)
    (epackage-with-message verbose "Wait, downloading sources list"))
  (let ((status (epackage-sources-list-download verbose)))
    (unless status
      (epackage-sources-list-upgrade verbose)
      (epackage-verbose-message "Upgrade sources list")))
  (epackage-sources-list-build verbose))

;;;###autoload
(defun epackage-cmd-sources-list-download (&optional verbose)
  "Download sources list.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (epackage-kill-buffer-sources-list)
  (unless (epackage-sources-list-p)
    (epackage-with-message verbose "Wait, downloading sources list"))
  (let ((status (epackage-sources-list-download verbose)))
    (unless status
      (epackage-message verbose "sources list alaready downloaded")))
  (epackage-sources-list-build verbose))

;;;###autoload
(defun epackage-cmd-download-package (package &optional verbose)
  "Download PACKAGE, but do not install it.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Download epackage: ")
         'interactive))
  (if (not (epackage-string-p package))
      (epackage-message "No package selected for download.")
    (if (epackage-package-downloaded-p package)
        (epackage-message "Ignore download. Already downloaded: %s" package)
      (let ((url (epackage-sources-list-info-url package)))
        (if (not url)
            (epackage-message
              "Abort. No URL to download package: %s" package)
          (epackage-download-package package verbose)
	  (epackage-download-package-actions package verbose))
        (when (and url
		   verbose)
          (let ((warnings (epackage-pkg-info-status-warnings package)))
            (if warnings
                (epackage-warn
                 "package status %s: %s"
                 package warnings))))))))

(defun epackage-recreate-package (package &optional verbose)
  "Re-create PACKAGE by deleting old and downloading new.
If optional VERBOSE is non-nil, display progress message.
No error checking are done for PACKAGE."
  (epackage-pkg-kill-buffer-force package verbose)
  (let ((dir (epackage-package-downloaded-p package)))
    (if dir
        (delete-directory dir 'recursive)))
  ;; FIXME: handle possibly changed configuration files
  (epackage-cmd-download-package package verbose))

;;;###autoload
(defun epackage-cmd-lint-package (package &optional verbose)
  "Lint, i.e. syntax check, PACKAGE.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Lint epackage: ")
         'interactive))
  (if (not (epackage-string-p package))
      (epackage-message "No packages selected for Lint.")
    (if (not (epackage-package-downloaded-p package))
        (epackage-message "Ignore Lint. Not downloaded: %s" package)
      (epackage-pkg-lint-package package verbose))))

;;;###autoload
(defun epackage-cmd-remove-package (package &optional verbose)
  "Physically remove PACKAGE and its configuration files from disk.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (list
      (if list
          (epackage-cmd-select-package "Remove epackage: " list)
        nil)
      'interactive)))
  (if (stringp package)
      (epackage-cmd-disable-package package verbose))
  (let ((dir (and (stringp package)
                  (epackage-package-downloaded-p package))))
    (if (not dir)
        (epackage-verbose-message
          "Remove ignored. Package not downloaded: %s"
          package)
      ;; If files are open and we delete a directory, clone it
      ;; again, the following happens:
      ;;
      ;;  "File info changed on disk.  Reread from disk into <file>? (y or n)"
      ;;
      ;; => We must kill all open buffers. FIXME: see if there is a variable
      ;; to avoid or to work around the above question and avoid
      ;; killing buffers.
      (epackage-with-message verbose (format "Remove %s" package)
        (epackage-config-delete-all package verbose)
        (epackage-pkg-kill-buffer-force package verbose)
        (epackage-verbose-message "Remove directory %s" dir)
        (delete-directory dir 'recursive))
      (run-hooks 'epackage--install-remove-hook)
      t)))

;;;###autoload
(defun epackage-cmd-upgrade-package (package &optional verbose)
  "Upgrade PACKAGE by downloading new code.
Install new configurations if package has been enabled.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (let ((list (epackage-status-downloaded-packages)))
     (list
      (if list
          (epackage-cmd-select-package "Upgrade epackage: " list)
        nil)
      'interactive)))
  (cond
   ((and (eq verbose 'interactive)
         (null package))
    (epackage-message "Package name is not set. Nothing to upgrade."))
   ((not (epackage-string-p package))
    (epackage-message "No package selected for upgrade."))
   ((not (epackage-package-downloaded-p package))
    (epackage-message "Package not downloaded: %s" package))
   ((not (epackage-git-master-p package))
    (epackage-message
     "Upgrade ignored. Locally modified. Branch is not \"master\" in %s"
     package))
   (t
    (epackage-upgrade-package-main package verbose))))

;;;###autoload
(defun epackage-cmd-upgrade-all-packages (&optional verbose)
  "Upgrade all downloaded packages.
Install new configurations if package has been enabled.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (let ((list (epackage-status-downloaded-packages))
        ;; Delay boot action until end.
        (epackage--install-action-list epackage--install-action-list)
        (boot (memq 'boot epackage--install-action-list)))
    (if boot
        (setq epackage--install-action-list
              (delq 'boot epackage--install-action-list)))
    (cond
     (list
      (epackage-with-message verbose "Wait, upgrading all packages"
        (dolist (elt list)
          (epackage-cmd-upgrade-package elt verbose)))
      (if boot
          (epackage-loader-file-generate-boot verbose))
      t)
     (t
      (epackage-verbose-message "No packages downloaded to upgrade")
      nil))))

;;;###autoload
(defun epackage-initialize (&optional verbose)
  "Inialize package.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  ;; Git, directories etc.
  (epackage-require-main verbose)
  ;; There are few checks that need this
  (let ((dir (epackage-sources-list-official-directory)))
    (unless (and
	     ;; sources list exists (has been installed)
	     (epackage-sources-list-p)
	     ;; and sources list git repository exists
	     (file-directory-p dir))
      (let ((epackage--initialize-flag t))
	(epackage-cmd-sources-list-download verbose))))
  (setq epackage--initialize-flag t)
  (unless (epackage-sources-list-p)
    (epackage-sources-list-build verbose))
  (run-hooks 'epackage--initialize-hook))

;;;###autoload
(defun epackage-version ()
  "Display `epackage--version-time'."
  (interactive)
  (epackage-princ epackage--version-time))

(defun epackage-documentation-buffer-version-defconst ()
  "Return version from current buffer.
Look for:  (defconst VARIABLE-NAME-VERSION* \"VALUE\"..."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^(defconst [^ ;\t\r\n]+-version.*[ \t]\"\\([^ \"\t\r\n]+\\)"
           nil t)
      (match-string-no-properties 1))))

(defun epackage-documentation-buffer-version-main ()
  "Return version from current buffer."
  (let ((version (lm-version)))
    (unless version
      (setq version (epackage-documentation-buffer-version-defconst)))
    version))

(defun epackage-documentation-buffer-main ()
  "Return documentation from current buffer.
Return:

    '((KEY VALUE) ....)

KEY is one of:

    'summary    One line information
    'version    Version number
    'modified   Date
    'created    Date
    'copyright  Copyright line
    'maintainer Emaail address(es) of maintainer."
  (let ((summary    (lm-summary))
        (commentary (lm-commentary))
        (maintainer (mapconcat #'concat (lm-maintainer) ", "))
        (created    (lm-creation-date))
        (modified   (lm-last-modified-date))
        (version    (epackage-documentation-buffer-version-main)))
    (list
     (list 'summary summary)
     (list 'version version)
     (list 'created created)
     (list 'modified modified)
     (list 'copyright)                  ;FIXME
     (list 'maintainer maintainer))))

(defun epackage-documentation-header-string ()
  "Return documentation header string from current buffer.
Summary, Version, Maintainer etc."
  (let* ((list       (epackage-documentation-buffer-main))
         (summary    (epackage-assoc 'summary list))
         (version    (epackage-assoc 'version list))
         (maintainer (epackage-assoc 'maintainer list)))
    (concat
     "epackage.el -- " (or summary "") "\n\n"
     "Version   : " (or version "") "\n"
     "Maintainer: " (or maintainer "") "\n"
     "\n")))

;; FIXME: Maybe use lm-commentary
(defun epackage-documentation-by-lisp-file (file buffer)
  "Display documentation of Emacs Lisp FILE in BUFFER."
  (let ((file (locate-library "epackage.el"))
        str)
    (finder-commentary "epackage.el")
    (setq str (with-current-buffer epackage--buffer-finder-commentary
                (buffer-string)))
    (with-current-buffer (setq buffer (get-buffer-create buffer))
      (erase-buffer)
      (insert str)
      (with-current-buffer (find-file-noselect file)
        (setq str (epackage-documentation-header-string)))
      (goto-char (point-min))
      (insert str))
  (kill-buffer epackage--buffer-finder-commentary)
  buffer))

;;;###autoload
(defun epackage-documentation ()
  "Display documentation."
  (interactive)
  (let ((buffer (get-buffer epackage--buffer-doc))
        (file "epackage.el"))
    (unless buffer
      (epackage-documentation-by-lisp-file
       (or (locate-library file)
           (epackage-error "Can't file from load-path: %s" file))
       epackage--buffer-doc))
    (display-buffer (get-buffer epackage--buffer-doc))))

(defsubst epackage-batch-ui-simple-p ()
  "Check if running inside `epackage-ui-simple'."
  ;; Running inside epackage-ui-simple?
  (when epackage--buffer-princ
    (let ((buffer (get-buffer epackage--buffer-ui-simple)))
      (eq epackage--buffer-princ
	  buffer))))

;;;###autoload
(defun epackage-ui-simple ()
  "User intrface. Simple."
  (interactive)
  (epackage-initialize)
  (let* ((buffer (get-buffer-create epackage--buffer-ui-simple))
	 (epackage--buffer-princ buffer))
    (switch-to-buffer buffer)
    (delete-other-windows)
    (goto-char (point-max))
    (epackage-batch-ui-menu-run)))

;;;###autoload
(defun epackage-manager () ;; FIXME
  "User intrface. A sketch. Not like the final interface at all."
  (interactive)
  (epackage-initialize)
  (call-interactively 'epackage-ui-simple))

;;;###autoload (autoload 'epackage-mode          "epackage" "" t)
;;;###autoload (autoload 'turn-on-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'tun-off-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'epackage-commentary    "epackage" "" t)

;; FIXME: Unfinished, this is at a sketch / planning phase.

;; (eval-and-compile
;;   (ti::macrof-minor-mode-wizard
;;    "epackage-" " Epkg" "z" "Epkg" 'Epackage "epackage--"
;;    "Emacs package manager

;; Mode description:

;; \\{epackage--mode-prefix-map}"

;;    "Epackage"
;;    nil
;;    "Number conversion mode"
;;    (list                                ;arg 10
;;     epackage--mode-easymenu-name
;;     "----"
;;     ["Package version"    epackage-version        t]
;;     ["Package commentary" epackage-commentary     t]
;;     ["Mode help"   epackage-mode-help   t]
;;     ["Mode off"    epackage-mode        t])
;;    (progn
;;      (define-key map "v"  'epackage-version)
;;      (define-key map "?"  'epackage-mode-help)
;;      (define-key map "Hm" 'epackage-mode-help)
;;      (define-key map "Hc" 'epackage-commentary)
;;      (define-key map "Hv" 'epackage-version))))

;;; .............................................. &functions-batch-ui ...

(put 'epackage-batch-macro 'lisp-indent-function 0)
(put 'epackage-batch-macro 'edebug-form-spec '(body))
(defmacro epackage-batch-macro (&rest body)
  "(dolist (elt command-line-args-left) BODY)."
  `(progn
     (epackage-initialize)
     (dolist (elt command-line-args-left)
       ,@body)))

(put 'epackage-batch-ignore-errors-macro 'lisp-indent-function 0)
(put 'epackage-batch-ignore-errors-macro 'edebug-form-spec '(body))
(defmacro epackage-batch-ignore-errors-macro (&rest body)
  "Like `epackage-batch-macro' bug ignore errors in BODY."
  `(progn
     (epackage-initialize)
     (dolist (elt command-line-args-left)
       (epackage-ignore-errors
         ,@body))))

(defun epackage-batch-list-package-summamry (list)
  "Display LIST of packages and they summary lines."
  (let (description)
    (dolist (package list)
      (setq description
            (epackage-sources-list-info-description package))
      (if description
          (message "%-25s %s" package description)
        (epackage-princ package)))))

(defun epackage-batch-ui-display-menu ()
  "Display `epackage--batch-ui-menu-string'."
  (interactive)
  (epackage-batch-separator-insert)
  (epackage-princ epackage--batch-ui-menu-string))

(defun epackage-batch-ui-display-version ()
  "Display `epackage--version-time' and `epackage--maintainer'."
  (interactive)
  (epackage-princ
   "== Version: %s <%s>"
   epackage--version-time
   epackage--maintainer))

(defsubst epackage-batch-ui-display-package-action-list ()
  "Display `epackage--download-action-list'."
  (epackage-princ
   "== Package activation list after download:%s"
   (if epackage--download-action-list
       (format " %s" epackage--download-action-list)
     "")))

;;;###autoload
(defun epackage-batch-ui-display-package-info ()
  "Display downloaded package's information file."
  (interactive)
  (let ((package (epackage-cmd-select-package "Info for package: ")))
    (epackage-cmd-package-check-macro
        package
        'interactive
        (format "PACKAGE name \"%s\" is invalid for display command"
                package)
    (cond
     ((epackage-package-downloaded-p package)
      (let ((file (epackage-file-name-package-info package)))
        (if (not file)
            (epackage-message "Broken epackage. Missing file: %s" file)
          (epackage-princ (epackage-file-content-as-string file)))))
     (t
      (epackage-message "Can't display info. Package not downloaded: %s"
                        package))))))

;;;###autoload
(defun epackage-batch-ui-display-package-documentation ()
  "Display downloaded extension's documentation."
  (interactive)
  (let ((package
         (epackage-cmd-select-package "Display package documentation: ")))
    (epackage-cmd-package-check-macro
        package
        'interactive
        (format "PACKAGE name \"%s\" is invalid for documentation command"
                package)
    (cond
     ((epackage-package-downloaded-p package)
      (let* ((file (epackage-pkg-info-documentation package))
             (path (format "%s/%s"
                           (epackage-directory-package-root package)
                           (or file "")))
             str)
        (if (not file)
            (epackage-message
              "Missing 'Commentary:' field in epackage info file")
          (if (setq str (lm-commentary path))
              (epackage-princ str)
            (epackage-message
              "No standard 'Commentary:' section found in %s"path)))))
     (t
      (epackage-message
        "Can't documentation . Package not downloaded: %s"
        package))))))

;;;###autoload
(defun epackage-batch-ui-loader-file-generate ()
  "Call `epackage-loader-file-generate-boot'."
  (interactive)
  (call-interactively 'epackage-loader-file-generate-boot))

;;;###autoload
(defun epackage-batch-ui-loader-file-byte-compile ()
  "Call `epackage-loader-file-byte-compile'."
  (interactive)
  (call-interactively 'epackage-loader-file-byte-compile))

;;;###autoload
(defun epackage-batch-ui-byte-compile-package ()
  "Call `epackage-cmd-byte-compile-package'."
  (interactive)
  (call-interactively 'epackage-cmd-byte-compile-package))

;;;###autoload
(defun epackage-batch-ui-autoload-package ()
  "Call `epackage-cmd-autoload-package'."
  (interactive)
  (call-interactively 'epackage-cmd-autoload-package))

;;;###autoload
(defun epackage-batch-ui-uninstall-package ()
  "Call `epackage-cmd-config-uninstall-package'."
  (interactive)
  (call-interactively 'epackage-cmd-config-uninstall-package))

;;;###autoload
(defun epackage-batch-ui-enable-package ()
  "Call `epackage-cmd-enable-package'."
  (interactive)
  (call-interactively 'epackage-cmd-enable-package))

;;;###autoload
(defun epackage-batch-ui-disable-package ()
  "Call `epackage-cmd-disable-package'."
  (interactive)
  (call-interactively 'epackage-cmd-disable-package))

;;;###autoload
(defun epackage-batch-ui-deactivate-package ()
  "Call `epackage-cmd-deactivate-package'."
  (interactive)
  (call-interactively 'epackage-cmd-deactivate-package))

;;;###autoload
(defun epackage-batch-ui-download-action-enable-toggle ()
  "Call `epackage-cmd-download-action-enable-toggle'."
  (interactive)
  (call-interactively 'epackage-cmd-download-action-enable-toggle)
  (epackage-batch-ui-display-package-action-list))

;;;###autoload
(defun epackage-batch-ui-download-action-activate-toggle ()
  "Call `epackage-cmd-download-action-activate-toggle'."
  (interactive)
  (call-interactively 'epackage-cmd-download-action-activate-toggle)
  (epackage-batch-ui-display-package-action-list))

;;;###autoload
(defun epackage-batch-ui-download-action-compile-toggle ()
  "Call `epackage-cmd-download-action-compile-toggle'."
  (interactive)
  (call-interactively 'epackage-cmd-download-action-compile-toggle))

;;;###autoload
(defun epackage-batch-ui-sources-list-upgrade ()
  "Call `epackage-cmd-sources-list-upgrade'."
  (interactive)
  (call-interactively 'epackage-cmd-sources-list-upgrade))

;;;###autoload
(defun epackage-batch-ui-download-package ()
  "Call `epackage-cmd-download-package'."
  (interactive)
  (call-interactively 'epackage-cmd-download-package))

;;;###autoload
(defun epackage-batch-ui-clean-package ()
  "Call `epackage-cmd-clean-package'."
  (interactive)
  (call-interactively 'epackage-cmd-clean-package))

;;;###autoload
(defun epackage-batch-ui-remove-package ()
  "Call `epackage-cmd-remove-package'."
  (interactive)
  (call-interactively 'epackage-cmd-remove-package))

;;;###autoload
(defun epackage-batch-ui-upgrade-package ()
  "Call `epackage-cmd-upgrade-package'."
  (interactive)
  (call-interactively 'epackage-cmd-upgrade-package))

;;;###autoload
(defun epackage-batch-ui-upgrade-all-packages ()
  "Call `epackage-cmd-upgrade-all-packages'."
  (interactive)
  (call-interactively 'epackage-cmd-upgrade-all-packages))

;;;###autoload
(defun epackage-batch-ui-list-downloaded-packages ()
  "List downloaded packages."
  (interactive)
  (let ((list (epackage-status-downloaded-packages)))
    (if (not list)
        (epackage-princ "No packages downloaded.")
      (epackage-princ "Downloaded packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-not-installed-packages ()
  "List downloaded packages."
  (interactive)
  (let ((list (epackage-status-not-installed-packages)))
    (if (not list)
        (epackage-princ "All downloaded packages are installed.")
      (epackage-princ "Not installed packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-installed-packages ()
  "List installed packages."
  (interactive)
  (let ((list (epackage-status-installed-packages)))
    (if (not list)
        (epackage-princ "No packages installed.")
      (epackage-princ "Installed packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-available-packages ()
  "Display available packages."
  (interactive)
  (let ((list (epackage-sources-list-info-pkg-list)))
    (if (not list)
        (epackage-princ "No sources list downloaded.")
      (epackage-princ "All available packages for download:")
      (epackage-batch-list-package-summamry list))))

;;; Command line batch commands
;;; emacs -Q --batch -f <command> <args>

(defun epackage-batch-devel-compose-package-dir ()
  "Run `epackage-devel-compose-package-dir' for first command line arg.
The argument must be full patch name to a *.el file."
  (let ((name (nth 0 command-line-args-left))
	(path (nth 1 command-line-args-left))
	dir
	file)
    (let ((i 0))
      (dolist (elt command-line-args-left)
	(epackage-message "ARG %d: %s" i elt)
	(setq i (1+ i))))
    (unless (stringp name)
      (epackage-error "PACKAGE NAME argument 1 is missing"))
    (unless (epackage-package-name-valid-p name)
      (epackage-error "Invalid PACKAGE NAME: %s" name))
    (unless (stringp path)
      (epackage-error "FILE argument 2 is missing"))
    (unless (string-match "^\\(.*/\\)\\(.+\\.el\\)" path)
      (epackage-error "FILE is not a full path name to *.el: %s" path))
    (setq dir (match-string 1 path)
	  file (match-string 2 path))
    (epackage-devel-compose-package-dir name dir)))

(defun epackage-batch-enable-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-enable-package elt 'verbose)))

(defun epackage-batch-disable-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-enable-package elt 'verbose)))

(defun epackage-batch-activate-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-activate-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-deactivate-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-deactivate-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-clean-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-clean-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-remove-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-remove-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-download-package ()
  "Run `epackage-cmd-download-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-download-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-upgrade-package ()
  "Run `epackage-cmd-upgrade-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-upgrade-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-upgrade-all-packages ()
  "Run `epackage-cmd-upgrade-all-packages'."
  (epackage-initialize)
  (epackage-cmd-upgrade-all-packages 'verbose))

(defun epackage-batch-ui-menu-selection (prompt)
  "Display UI menu PROMPT."
  (let* ((str (read-string prompt))
         (char (string-to-char str))
         (menu (assq char epackage--batch-ui-menu-actions))
         (choice (nth 1 menu)))
    (epackage-with-debug
      (epackage-princ "debug: str %s | char %s | menu %s"
               (length str)
               char
               menu))
    (or choice
        char)))

(defun epackage-batch-separator ()
  "princ separator string."
  (epackage-princ "\
== ==================================================================="))

(defun epackage-batch-separator-insert ()
  "Princ separator only if `epackage-batch-ui-simple-p'."
  (if (epackage-batch-ui-simple-p)
      (epackage-batch-separator)))

(defsubst epackage-batch-ui-menu-header ()
  "Display menu header."
  (epackage-princ "
== ===================================================================
== Epackage - Distributed Emacs Lisp Package System (DELPS)")
  (epackage-batch-ui-display-version)
  (epackage-princ "\
== ==================================================================="))

(defsubst epackage-recenter-bottom ()
  "Recenter buffer to bottom."
    (goto-char (point-max))
    ;; (when (> (point) (window-end nil t))
    ;;   (overlay-recenter (point))
    ;;   (recenter -3))))
    (let ((last-command 'ignore)
	  (recenter-positions '(bottom)))
      (recenter-top-bottom)))

(defun epackage-shring-window (&optional buffer-name)
  "Adjust visible `current-buffer' or BUFFER-NAME to minimum."
  (let* ((buffer-name "*Compile-Log*")
	 (buffer (get-buffer buffer-name))
	 current
	 win
	 winb)
    (when (and buffer
	       (window-live-p buffer))
      (setq current)
      (setq winb (window-buffer name))
      (setq win (get-buffer-window winb))
      (save-excursion
	(set-buffer winb)
	(select-window win)
	(goto-char (point-max))
	(unless (window-fixed-size-p)
	  (let ((size (window-height win))
		(min window-min-height)
		(diff (- size min)))
	    (if (> diff 0)
		(shrink-window diff))))))))

(defsubst epackage-batch-ui-menu-goto-point-max ()
  "Position point at `point-max'."
  (when (epackage-batch-ui-simple-p)
    (epackage-batch-ui-menu-compile-buffer-adjust)
    (epackage-recenter-bottom)))

;;;###autoload
(defun epackage-batch-ui-menu ()
  "Present an UI to run basic command."
  (let (epackage--buffer-princ)
    (epackage-batch-ui-menu-run)))

(defun epackage-batch-ui-menu-run ()
  "Present an UI to run basic command."
  (let ((epackage--install-action-list epackage--install-action-list)
        (debug-on-error t)
        (vc-handled-backends nil)
        (loop t)
        debug-ignored-errors
        choice)
    (epackage-initialize 'verbose)
    (setq epackage--debug nil)
    ;;  This is from command line, no enable action is needed for
    ;;  current Emacs
    (setq epackage--install-action-list
          (delq 'enable epackage--install-action-list))
    (epackage-batch-ui-menu-header)
    (while loop
      (epackage-batch-ui-menu-goto-point-max)
      (setq choice (epackage-batch-ui-menu-selection
		    epackage--batch-ui-menu-prompt))
      (epackage-with-debug
        (epackage-princ "debug: choice %s" choice))
      (cond
       ((null choice)
        (epackage-princ "** Unknown selection"))
       ((eq choice 'ignore)
        (epackage-princ "** Not implmented yet"))
       ((eq choice 'quit)
        (epackage-princ "** Exit")
        (setq loop nil))
       ((functionp choice)
        (call-interactively choice))
       ((eq choice ?\?)
	(epackage-batch-separator-insert)
        (epackage-princ epackage--batch-ui-menu-help))
       (t
        (epackage-princ "** Unknown menu selection: %s" choice))))
    (epackage-batch-ui-menu-goto-point-max)))

(defun epackage-run-action-list (package actions &optional verbose)
  "Run PACKAGE ACTIONS. See  `epackage--download-action-list'.
If optional VERBOSE is non-nil, display progress message."
  (let* ((actions epackage--download-action-list)
         (list (sort actions
                     (lambda (a b)
                       (string<
                        (symbol-name a)
                        (symbol-name b)))))
         (install-p (epackage-download-action-install-p))
         (enable-p (epackage-download-action-enable-p)))
    (dolist (elt list)
      (epackage-verbose-message "package action: %s" elt)
      ;; Development note: keep the cond-list in alphabetical order
      (cond
       ((eq elt 'activate)
        (let ((epackage--download-action-list epackage--download-action-list))
          ;; Don't generate boot loader... yet.
          (if enable-p
              (setq epackage--download-action-list nil))
          (epackage-cmd-activate-package package verbose)))
       ((eq elt 'autoload)
        (let ((epackage--download-action-list epackage--download-action-list))
          ;; Don't generate boot loader... yet.
          (if install-p
              (setq epackage--download-action-list nil))
          (epackage-cmd-autoload-package package verbose)))
       ((eq elt 'compile)
        (epackage-byte-compile-package-main package))
       ((eq elt 'enable)
        ;; Only now possibly generate boot loader
        (epackage-cmd-enable-package package verbose))
       ((eq elt 'lint)
        (epackage-pkg-lint-package package verbose))
       ((eq elt 'package-depends)
        (epackage-pkg-depends-satisfy package verbose))))))

(defun epackage-rerun-action-list (package &optional verbose)
  "Run PACKAGE actions as aready they are.
If optional VERBOSE is non-nil, display progress message.

The actions are evaluated based on the installation: If there
are byte compiled files, then byte compile. If there are autoload
files, then reinstall autoload files etc.

Return list of actions as in `epackage--download-action-list':
  '((action ...))."
  (let ((actions (epackage-package-status-actions package)))
    (when actions			; If not installed, nothing to do
      (epackage-run-action-list package actions verbose))))

;;;###autoload
(defalias 'epackage 'epackage-manager)

;;;###autoload
(defalias 'epackage-ui 'epackage-batch-ui-menu)

(provide   'epackage)

(add-hook  'epackage--mode-hook 'epackage-mode-define-keys)
(run-hooks 'epackage--load-hook)

;;; epackage.el ends here
