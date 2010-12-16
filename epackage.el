;;; epackage.el --- Distributed Emacs Lisp package system (DELPS)

;; This file is not part of Emacs

;; Copyright (C)    2009-2011 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto

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
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;; Depends:

;;      o   Emacs 22.1+ (released 2007). Designed only for Emacs.
;;          XEmacs has its own packaging system (pui-*).
;;          http://www.gnu.org/software/emacs
;;      o   git(1) Distributed Version Control System (DVCS). Any version.
;;          http://en.wikipedia.org/wiki/Git_(software)
;;      o   Depends only on standard Emacs. Does not use cl.

;;; Install:

;;  Put this file along your Emacs-Lisp `load-path' and add following
;;  into your ~/.emacs startup file.
;;
;;      ;; One big file to boot all installed packages
;;      ;; Automatically generated. Do not edit.
;;      (load "~/.emacs.d/epackage/00conf/epackage-loader" 'noerr)
;;
;;      ;;  M-x epackage to start package manager
;;      (autoload 'epackage "epackage" "" t)
;;
;;      (autoload 'epackage-loader-file-byte-compile    "epackage" "" t)
;;      (autoload 'epackage-loader-file-generate        "epackage" "" t)
;;      (autoload 'epackage-cmd-autoload-package        "epackage" "" t)
;;      (autoload 'epackage-cmd-enable-package          "epackage" "" t)
;;      (autoload 'epackage-cmd-disable-package         "epackage" "" t)
;;      (autoload 'epackage-cmd-activate-package        "epackage" "" t)
;;      (autoload 'epackage-cmd-deactivate-package      "epackage" "" t)
;;      (autoload 'epackage-cmd-clean-package           "epackage" "" t)
;;      (autoload 'epackage-cmd-remove-package          "epackage" "" t)
;;      (autoload 'epackage-cmd-upgrade-package         "epackage" "" t)
;;      (autoload 'epackage-cmd-upgrade-all-packages    "epackage" "" t)
;;      (autoload 'epackage-cmd-download-sources-list   "epackage" "" t)
;;      (autoload 'epackage-cmd-download-package        "epackage" "" t)
;;      (autoload 'epackage-initialize                  "epackage" "" t)
;;      (autoload 'epackage-version                     "epackage" "" t)
;;      (autoload 'epackage-documentation               "epackage" "" t)
;;
;;  In addition to full UI (M-x epackage), there is also a minimal
;;  command line UI:
;;
;;      emacs --batch -Q -l /path/to/epackage.el -f epackage-batch-ui-menu

;;; Commentary:

;;  Preface 2009
;;
;;      NOTE: 2010-12-08 This extension is in alpha design state;
;;      meaning that it is not in full use yet. The core elements are
;;      being planned and written. For testing, see available `M-x'
;;      `epackage-*' commands. There is also a rudimentary batch
;;      command line UI:
;;
;;          # Or run the provided Makefile: "make ui"
;;          emacs --batch -Q -l /path/to/epackage.el -f epackage-batch-ui-menu
;;
;;      ....expect full UI with nice menus, font-lock, mode command
;;      and Emacs buffers like in ELPA somewhere around spring 2011
;;      the earliest.
;;
;;      Emacs has been around for decades now. Many new version have
;;      come and gone. And yet there are wealth of useful extensions
;;      available e.g. at <http://emacswiki.org> which add new
;;      features not yet available in standard Emacs. The typical
;;      procedure to add a new extension to Emacs has been:
;;
;;      o   Find an extension at places like
;;          http://dir.gmane.org/gmane.emacs.sources or
;;          http://www.emacswiki.org
;;      o   Download and save the *.el file(s) along `load-path'
;;      o   Read the installation information. Usually embedded in comments
;;          at the beginning of *.el file(s).
;;      o   Modify the Emacs startup file `~/.emacs'
;;          to arrange loading the extension to one's liking.
;;
;;      That's quite a bit of work for each extension; reaching
;;      thousands out there. Many Linux distributions offer package
;;      managers to download and install programs. E.g. Debian has
;;      command *apt-get/aptitude* [1], Redhat uses *rpm* [2], Suse
;;      uses *yast* [3]. So why not make one for Emacs as well.
;;
;;      The DELPS has been designed built around two concepts: it
;;      borrows the Debian style package management and it uses
;;      version controlled packages.
;;
;;      Each Emacs extension is wrapped into epackage format which
;;      basically follows the Debian [4] packaging style where a separate
;;      control directory named `epackage/' is used for all the
;;      packaging details: activation, autoloads and installation etc.
;;      In addition, each epackage is imported in and deployed using
;;      Git Distributed Version Control System (DVCS). A specific
;;      "Yellow pages" file lists the available Git repositories where
;;      user can download packages. Once an epackage has been
;;      downloaded, subsequent downloads are very efficient because
;;      only deltas are transferred.
;;
;;      If you're an Emacs user, all these details do not concern you.
;;      From `M-x' `epackage' management view, select items to
;;      download, and activate them. There are several ways how to
;;      install packages. Select *autoload* install (no Emacs setup
;;      changes), *standard* install (= enabling), or *activation*
;;      install (Emacs environment is changed). Later you can upgrade
;;      packages. To get updates list of available packages, ask to
;;      "get" the sources list "Yellow pages" that lists available Git
;;      repositories.
;;
;;      If you're a developer who would like to make the extensions
;;      available for others as epackages, that will require
;;      familiarizing with the `git(1)'.
;;
;;      The epackage system can co-exist with any other installation,
;;      like ELPA [4], as usual. User's standard Emacs startup files, like
;;      `~/.emacs' are never modified.
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
;;  Epackage - the DVCS packaging system
;;
;;      The DELPS epackages are in the form of distributed[1] git[2]
;;      version control repositories. The traditional packaging
;;      methods, like ELPA[2], have previously relied on archives like
;;      *.tar.gz to hold the code. In contrast, the DVCS approach
;;      offers interesting features over the traditional archive
;;      distribution approach:
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
;;      Each Emacs extension is prepared for use with this system:
;;      upstream code is imported into a git repository, the epackage
;;      system is installe don top of upstream code in separate
;;      directory, the whole git repository is made available online
;;      and add information about its availability is recorded to a
;;      separate seources list yellow pages. The epackaging work can
;;      be done by anyone who wants to set up a repository. It doesn't
;;      necesarily need to be done by the original Emacs extension
;;      author (upstream) who may not be familiar with the `git(1)'
;;      program. For more information about the packaging, refer to
;;      section "The epackage system framework ".
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
;;      o   g, Get yellow page data. Update package sources list.
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
;;      o   x, execute (install, purge, remove).
;;
;;      Planned:
;;
;;      o   byte compile package.
;;      o   edit package's *info* file.
;;      o   email upstream, the package author (maintainer). You can
;;      o   send mail to person who is the maintainer of epackage
;;          for this utility. You can send requests to fix
;;          packaging or update contents of the 'info' file if some
;;          of the information in no up to date.
;;
;;      The package state is shows with following status indicators:
;;
;;      o   (A)ctivated. The package has been downloaded and code to
;;          immediately activate the package has been taken into use.
;;          This setting changes user's Emacs environment as defined
;;          by the packager. The changes typically include modifying hook
;;          to activate the package e.g. by file extension, adding
;;          key bindings to activate the package etc. You might want
;;          to use (v)iew command to see what exactly happens
;;      o   (E)enabled. One step down from Activated state. Only interactive
;;          functions and variables are provided in latent `autoload'
;;          state for user to call with `M-x' <function name>. User
;;          configuration is not modified in any way.If you want full
;;          control over package setup, set package to Enabled state
;;          and add further code to Emacs startup file "/.emacs to
;;          configure it
;;      o   (I)installed. This is synonym for Downloaded. Package has
;;          been fetched to local disk, but that is all. No setup
;;          whatsoever.
;;      o   (u)unmaintained. The package has been flagged as unmaintained.
;;      o   (b)uggy. The package contains remarks that it might be buggy
;;          if installed.
;;      o   (c)ompiled. Package has been byte compiled.
;;      o   (e)macs core. Package in included in latest core Emacs.
;;      o   (x)emacs core. Package is included in latest core XEmacs.
;;
;;      Building the initial list of available packages takes some time
;;      and this is done via open Internet connection. Install command
;;      also requires an open Internet connection.
;;
;;  The epackage system framework
;;
;;      Quick links for developrs:
;;
;;      o   https://github.com/jaalto/project--emacs-epackage-sources-list
;;      o   https://github.com/jaalto/project--emacs-epackage-template
;;
;;      To explain how do all the pieces in this system go together,
;;      lets take a look at the system overview. The system mirrors
;;      the style of Debian packaging management. There are two
;;      primary actors: (1) the epackage package maintainer and (2)
;;      the upstream. These two can be the same person or two separate
;;      persons.
;;
;;      o   A = An Emacs user who wants to install new software
;;      o   (Y)ellow pages = The sources list file that contains
;;          information about available epackages around the globe.
;;      o   E = The epackage. Maintained by a person who has found an
;;          interesting utility and wrapped it in epackage format. He
;;          is the maintainer of epackaged software. He keeps
;;          track of new releases and makes new epackages periodically
;;          available. If the initial packager looses interest,
;;          someone else can continue his work. He supplies the *url*
;;          to the yellow pages to notify about availability of epackage.
;;      o   U = Upstream. Person or team who wrote Emacs Lisp extension,
;;          the code or utility than enhances Emacs.
;;
;;      The moving parts communicate like in picture below. In order
;;      to find package, a yellow pages is consulted. It is seeded and
;;      update by all the epackage maintainer that wish to make
;;      epackages available. The user A does not need to know any
;;      details of this process; like in Debian, he installs an
;;      epackage that is made newly available or which has been
;;      updated and is upgradeable.
;;
;;      o   The location of Yellow Pages is fixed (%).
;;      o   The location of E's and U's can be anywhere (*).
;;      o   The E and U can be the same person (the upstream).
;;
;;                      %               *               *
;;          A           Y               E               U
;;          =============================================
;;          |           |               | keep eye on   |
;;          |  fetch    |               * ------------> |
;;          * --------> |               | <-----------  |
;;          | <-------- |               | epackage new  |
;;          |  upgrade  | keep epackage | releases      |
;;          |           | info in sync  |               |
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
;;          | <------------------------ |               |
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
;;      which defaults to `~/.emacs.d' or `~/elisp' respectively. The
;;      components below the root directory are organized as follows:
;;
;;          epackage/               Under epackage--root-directory
;;          |
;;          +-- 00coonf/
;;          |   epackage-loader.el     For user. One big boot file-
;;          |   epackage-load-path.el  Internal. Used during byte-compile.
;;          |   sources.lst            Internal. Package sources.
;;          |
;;          +-- 00install/         Extension "install" files
;;          |   *-<type>.el        autoloads, install, activate...
;;          |
;;          +--packages/           Version control repositories
;;             |
;;             +-- 00sources/      Yellow pages: list of available packages
;;             +-- package/        Downloaded package
;;             +-- ...
;;
;;  Epackage specification (draft; level 1)
;;
;;      The Git repository branches used are:
;;
;;      o   *master*, required. The published epackage.
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
;;      `patches' and `upstream'.
;;
;;          patches        o - o (modifications; merged to master)
;;                       /
;;          upstream    * ---- o
;;                       \      \ (merge)
;;          master        o ---- o - =>         contains epackage/ directory
;;
;;      The epackage method borrows concept from Debian where a
;;      separate control directory is used for package information.
;;      The directory name is `epackage/' and it is not configurable.
;;      The layout of an epackaged Emacs extension looks like:
;;
;;          <PACKAGE, Emacs extension root dir>
;;          | <files and possible directories>
;;          |
;;          +- .git/                  Version control branches (see above)
;;          |
;;          +-- epackage/
;;              info                  required: The information file
;;              PACKAGE-0loaddefs.el  optional: extracted ###autoload statements
;;              PACKAGE-autoloads.el  optional: autoload statements (manual)
;;              PACKAGE-compile.el    optional: Code to byte compile the extension
;;              PACKAGE-examples.el   optional: Custmization examples
;;              PACKAGE-install.el    required: Code to make the extension available
;;              PACKAGE-uninstall.el  optional: Code to remove the extension
;;              PACKAGE-xactivate.el  optional: Code to activate the extension
;;
;;      The names of the files have been chosen to sort
;;      alphabetically. All these configuration files are later
;;      combined in a single loader file. Loading a single file is
;;      faster than spending time in loading small file along
;;      `load-path'. The alphabetic order makes it possible to combine
;;      them safely together. Even on command line (for testing):
;;
;;              ls | egrep -vi '00|compile|examples|uninstall' | \
;;              xargs cat > PACKAGE-00.el
;;
;;     The *-0loaddefs.el
;;
;;      This file contains extracted ##autoload definitions. The file
;;      is usually automatically generated. The file does not modify
;;      user's environment. If extension does not contains any
;;      `###autoload' definitions, the manually crafted *-install.el
;;      file can be used as a substitute. In case of missing
;;      `##autoload' stanzas, you're encouraged to contact upstream
;;      with a possible patch. The "zero" at the start of the name is
;;      to help proper sorting ordering of files. Mnemonic: "if you
;;      load this file, you can start calling extension's features".
;;      The file ends in:
;;
;;          (provide 'PACKAGE-0loaddefs)
;;
;;     The *-autoloads.el
;;
;;      This file contains manually written `autoload' statements.
;;      This file acts as a backup if there is no `###auutoload'
;;      definitions. Its purpose it to publish propective functions
;;      (interactive or not) that might be called or used by the user.
;;      Mnemonic: "if you load this file, you can write lisp code to
;;      call the functions in the extension, or you can call
;;      extension's interactive functions via M-x". The file ends in:
;;
;;          (provide 'PACKAGE-autoloads)
;;
;;     The *-compile.el
;;
;;      This file contains Emacs Lisp command to byte compile the
;;      extension. The file is run at the root directory of the
;;      extention with `load-path' set to include all the relevant
;;      directories. Wvaluating the file must run all byte compilation
;;      commands. All the variables and function defined here must
;;      have `PACKAGE-*' prefix to keep the Emacs namespace clean. An
;;      exmaple for simple extension consisting of two files:
;;
;;          (dolist (file '("foo-lib.el" "foo.el"))
;;            (byte-compile-file file))
;;
;;      *Exception:* packages that only have a single *.el file do not
;;      need to define this file.
;;
;;     The *-examples.el
;;
;;      This file contans anything the upstream may have explained in
;;      the comments, or interesting snippets various users have found
;;      useful to customize the extentiosn. It provides a showcase, or
;;      scratchbook, to present anything that might be useful to be
;;      put into `~/.emacs' startup file. Mnemonic: "Look examples in
;;      this file for ideas how to take more out of the extension".
;;      This file is _not_ intenede to be loaded and it should _not_
;;      contain any `provide' statements. In fact it is recommended that
;;      any attempt to load this fle generates an error. The file starts
;;      with:
;;
;;          (error "PACKAGE-examples.el is not a config file. Study the examples.")
;;
;;     The *-install.el
;;
;;      This file is manually or automatically written. It publishes
;;      user variables and interactive `M-x' functions in *autoload*
;;      state. No modifications to user's Emacs setup is allowed. This
;;      file is only necessary if extension does not contain proper
;;      ###autoload statements. The *-install* in name refers to
;;      installation, or availability for that matter, of interactive
;;      functions. *Note:* try to avoid `require' or `load' commands
;;      as much as possible; or delay their calls to the point where
;;      user calls functions interactively. That helps keeping Emacs
;;      startup fast and lean. Mnemonic: "if you load this file, you
;;      can start calling extension's features". The file ends in:
;;
;;          (provide 'PACKAGE-install)
;;
;;     The *-uninstall.el
;;
;;      This file does the opposite of *-install.el and *-activate.el
;;      It runs commands to remove the extension as if it has never
;;      been loaded. Due to the nature of Emacs, it is not really
;;      practical to completely try to uninstall the package. The
;;      uninstallation usually covers undoing the changes to *-hook,
;;      *-functions and `auto-mode-alist' and the like variables. The
;;      actual symbols (defined functions and variables) are not
;;      removed. To shake extension completely, restart Emacs after
;;      uninstall of epackage. The file ends in:
;;
;;          (provide 'PACKAGE-uninstall)
;;
;;     The *-xactivate.el
;;
;;      This file makes the extension immediately active in user's
;;      environment. It can modify current environment by adding
;;      functions to hooks, adding minor or major modes or arranging
;;      key bindings so that when pressed, a feature is loaded and
;;      activated. It may also loop through `buffer-list' to activate
;;      features immediately in running Emacs. It is best that any
;;      custom settings, like variables and prefix keys, are defined
;;      in `~/.emacs' *before* this file gets loaded. As with
;;      *-install.el, try to avoid `require' or `load' commands and
;;      stick to `autoload'. Mnemonic: "If you load this file, the
;;      bells and whistles are turned on". The "x" at the start of the
;;      name is to help proper sorting ordering of all files. The file
;;      ends in:
;;
;;          (provide 'PACKAGE-xactivate)
;;
;;  The info file
;;
;;      A RFC 2822 (email) formatted file, which contains information
;;      about the extension. The header field names are case
;;      insensitive; but if you use the default *get.sh*, it expects
;;      the Vcs-* field to be case-sensitive. Continued lines must be
;;      indented with only 1 space. Required fields
;;      are marked with asterisk (*). In the long description
;;      part, new paragraphs are separated by a single dot(.)
;;      character on their own line. The layout of the `info' mirrors
;;      concepts of `control' file in Debian packaging system which is explained in
;;      <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version>.
;;
;;          *Package: <unique name, all lowercase>
;;          *Section: <data | extensions | files | languages | mail | tools | M-x finder-list-keywords>
;;          License: <GPL-[23]+ | BSD | Apache-2.0 | ... | None>
;;          *Depends: emacs (>= 20)
;;          Status: [ <keyword> ...]
;;          Compat: [ <epackage version> ]
;;          *Maintainer: First Last <first.last@example.com>
;;          *Email: First Last <first.last@example.com>
;;          Bugs: [ URL ]
;;          Vcs-Type:
;;          Vcs-Url:
;;          Vcs-Args:
;;          Vcs-Browser:
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
;;           .
;;           [<Longer description, next paragraph>]
;;
;;      An example:
;;
;;          Package: hide-lines
;;          Section: tools
;;          License: None
;;          Depends: emacs (>= 21)
;;          Status: unmaintained
;;          Compat:
;;          Maintainer:
;;          Email: Mark Hulme-Jones <ture@plig.net>
;;          Bugs:
;;          Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/hide-lines.el
;;          Vcs-Browser:
;;          Vcs-User:
;;          Vcs-Password:
;;          Homepage:
;;          Wiki: http://www.emacswiki.org/emacs/HideLines
;;          Commentary: hide-lines.el
;;          Description: Hide or preserve all matching lines in buffer
;;           Main command [C-u] M-x hide-lines to hide or show matching lines.
;;           With prefix argument, the corresponding lines are preserved while
;;           others are hidden.
;;           .
;;           Note 2010-12-03 the code hasn't been touched since 2004.
;;
;;  Details of the info file fields
;;
;;      Notes: Use one space to indent continued field. Limit maximum
;;      line length to 80 characters. In Emacs, see variable
;;      `fill-column' and set it to a little less, like 75.
;;
;;     Bugs
;;
;;      URL to report bugs. This can be an email address or a link to
;;      issue tracker of upstream project. Note: send packaging
;;      problems or update requests to the extension's epackage
;;      `Maintainer'.
;;
;;    Commentary
;;
;;      This field contains path, relative to epackage root directory,
;;      to single Emacs Lisp file which contains documentation
;;      suitable for `M-x' `finder-commentary'. In order to find
;;      documentation, this field must exist even for single Emacs
;;      Lisp file epackages. Extension developers should study
;;      *lm-maint.el* and function `lm-commentary'. The documentation
;;      read is encloded in the extension file between tags:
;;
;;          ;;; Commentary:
;;
;;          ;;; <documentation>
;;          ;;; <...>
;;
;;          ;;; Change Log:
;;
;;    Compat
;;
;;      The compatibility level used in this epackage. The Epackage
;;      format may change in time and this field indicates which the
;;      epackage layout version. If the value is missing or is empty,
;;      no specific compatibility level is required and latest is
;;      assumed. Usually an epackage maintainer should follow the
;;      latest format to prevent installation problems. See section
;;      "Epackage Compatibility Levels" for more information.
;;
;;     Conflicts
;;
;;      List of packages that must be removed before install can be
;;      done. This field follow guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;     Depends (required)
;;
;;      List of dependencies in all lowercase: Emacs flavor and
;;      external packages required. Do not list packages that are
;;      included in core Emacs; that would be unnecessary and slow
;;      down parsing. The Emacs flavor can have optional version
;;      information enclosed in parenthesis using comparison operators
;;      ">=", "<=" and logical "!". A between range is not defined.
;;      The logical *or* operator works only between Emacs flavors and
;;      is indicated with vertical bar "|".
;;
;;      In case an extension works only with certain version of Emacs,
;;      this information should be written to the end of
;;      `Description'. (which see). Old packages that are not updated
;;      to work for latest Emacs releases are candidate for removal
;;      from a epackage archive's yellow pages. Examples:
;;
;;          Depends: emacs, cedet, bytecomp
;;          Depends: emacs (>= 22.2) | xemacs (>= 20), cedet, bytecomp
;;
;;      To mark that package dows not work in XEmacs, use "!". The
;;      version parameter is ignored in logical *not*, parenhesis are
;;      still required:
;;
;;          Depends: emacs (>= 22.2), xemacs (!), cedet, bytecomp
;;
;;      _Limitations_: The *vertical* *bar*, OR-operator(|), is not
;;      general operator. It is only respected on the Emacs flavor
;;      part. Using OR-operator anywhere else causes treating the
;;      elments as required as if written "exension | extension" =>
;;      "extenstion, extenstion".
;;
;;      The *version* *information* is a no-op anewhere else than
;;      Emacs flavor check. This kind of fine grained package
;;      dependencies has never been in use with Emacs Lisp extenstion.
;;      There is no support for version numbers in Emacs Lisp commands
;;      `provide', `require', `load', `load-file' and `load-library'.
;;      Extenstions typically check the avilable features with
;;      `boundp' and `fboundp' to see if they have the required
;;      environment. So don't write:
;;
;;          Depends: emacs (>= 22.2), xemacs (!), cedet (>= 0.9)
;;                                                      |
;;                                       Will no tbe used
;;
;;      See also section "Development notes: depends".
;;
;;     Description (required)
;;
;;      The first line of this field is a concise description that
;;      fits on maximum line length of 80 characters in order to
;;      display "PACKAGE -- SHORT DESCRIPTION". The long description
;;      is explained in paragraphs that are separated from each other
;;      with a single (.) at its own line. The paragraphs are
;;      recommended to be intended by one space.
;;
;;     Email
;;
;;      The Upstream developer's name and email address(es). Multiple
;;      developers are separated by commas. The role can be expressed
;;      in RFC 2822 comment-parenthesis. An example:
;;
;;              Email: John Doe (Author) <jdoe@example.com>,
;;               Joe Average (Co-developer) <jave@example.com>
;;
;;     Homepage
;;
;;      URL to the project's homepage. It is recommended to use
;;      addresses that don't move; those of http://Freshmeat.net,
;;      http://www.Sourceforge.com, http://Launchpad.net,
;;      http://Github.com, http://Bitbucket.com etc. The Freshmeat is
;;      especially good because it provides project information in
;;      standardized manner. Through Freshmeat it is also possible to
;;      browse related software and subscribe to announcements.
;;      Freshmeat is also easy for the upstream developers to set up
;;      because it requires no heavy project management; only links.
;;
;;      In any case, the Homepage URL should not directly point to
;;      developer's volatile personal homepage if there is alternative
;;      choices. It is good idea to encourage "Garage" upstream
;;      developers to set up their software at some project hosting
;;      site because they include infrastructure for issue tracking.
;;      For more information, see
;;      <http://en.wikipedia.org/wiki/Comparison_of_open_source_software_hosting_facilities>.
;;
;;     License
;;
;;      If missing, the value is automatically assumed "GPL-2+". The valid
;;      License abbreviations should follow list defined at
;;      <http://wiki.debian.org/CopyrightFormat>. A special word "None"
;;      should be used if the software has no license information in any of
;;      the source files. Examples of valid license tokens:
;;
;;              GPL-2, GPL-2+, GPL-3, GPL-3+, BSD, Apache-2.0
;;
;;     Maintainer
;;
;;      This extension's epackage maintainer. Format is the same as in
;;      *Email* field. Contains the name and address of the person who
;;      made this extension available in epackage format. If the
;;      upstream is also the epackage mainainer, the content of this
;;      field is identical to *Email* field.
;;
;;     Package (required)
;;
;;      The name of the extension in all lowercase, satisfying regexp
;;      "[a-z][a-z0-9-]+". Usually base name of the extension file or
;;      the canonical known name in case of bigger packages like
;;      "gnus". An example "html-helper-mode.el" => package name is
;;      "html-helper-mode". In case of minor or major modes, always
;;      add *-mode even if file name does not explicitly say so. An
;;      example "python.el" => package name is "python-mode". No two
;;      packages can have the same name. Please notify upstream about
;;      the clash.
;;
;;     Recommends
;;
;;      List of packages that current extension can support or take
;;      advantage of. E.g this field would list package B if A can
;;      take advantage of package B, but it is not a requirement to
;;      install B for package A to work. This field is *not* used to
;;      announce related packages. That information can be mentioned in
;;      the end of `Description' in separate paragraph "SEE ALSO".
;;      This field follow guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html#s-binarydeps>
;;
;;     Section (required)
;;
;;      This field contains category for package. The valid keywords are
;;      those listed in `M-x' `finder-list-keywords'.
;;
;;     Status
;;
;;      This field lists information about the package. Each keyword
;;      has a unique meaning. the allowed list:
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
;;      The `core-*' values mark the package being included (or will
;;      be) in the latest [X]Emacs. The optional NN.N announces in
;;      which Emacs flavor the feature was included; e.g.
;;      *core-emacs-22.1*. Value `unmaintained' means that the
;;      original developer has vanished or abandoned the project and
;;      is no longer available for developing the package. Value
;;      `unsafe' means that the not all the symbols are name space
;;      clean (prefix-*), so some of the commands might clash with
;;      existing ones. The current release status of package can be
;;      indicated with term `stable' (no more actively developed, bugs
;;      shaken out), `unstable' (package is in active development) and
;;      `experimental' (no guarantees, not necessarily tested but this
;;      is the latest code). Value `broken' means that package is
;;      broken and does not work in some Emacs version (usually
;;      latest).
;;
;;     Vcs-Browser
;;
;;      The URL address to the version control browser of the repository.
;;      This field follow guidelines of
;;      <http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-vcs>
;;
;;     Vcs-Type
;;
;;      Version Control System type information. The value is the
;;      lowercase name of the version control program; cvs, svn, bzr,
;;      hg, git etc. A special value "http" can be used to signify
;;      direct HTTP download. This field follow guidelines of
;;      <http://www.debian.org/doc/developers-reference/best-pkging-practices.html#bpp-vcs>.
;;      An example of an Emacs extension hosted directly at a web page:
;;
;;          Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/vline.el
;;
;;     Vcs-Url
;;
;;      The Version Control System repository URL without any options.
;;      For CVS, this is the value of CVSROOT which includes also the
;;      protocol name. This field follow guidelines of
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
;;      <Vcs-Url> co -d <Package> <Vcs-Args>"
;;
;;          Vcs-Type: cvs
;;          Vcs-Url: :pserver:anonymous@example.com/reository/foo
;;          Vcs-Args: module
;;
;;     Vcs-User
;;
;;      The Version Control System repository's login name. In case the
;;      repository cannot be accessed simply by visiting the `Vcs-Url' (or
;;      in the case of CVS: pressing RETURN at login prompt), this is the
;;      login name.
;;
;;     Vcs-Password
;;
;;      The Version Control System repository's password. In some rare cases
;;      a generic password, like "anonymous" to access repository, is needed.
;;
;;     Wiki
;;
;;      This field points to extension page at
;;      <http://www.emacswiki.org>. If extension does not yet have a
;;      page yet, encourage upstream to create one.
;;
;;     X-*
;;
;;      Any other custom field can be inserted using `X-*' field
;;      notation. It is recommended that X-fields are listed at the bottom,
;;      Just before `Description:' field.
;;
;;          X-Comment: <comment here>
;;          X-Upstream-Homepage: <URL>
;;
;; Epackage Compatibility Levels
;;
;;      The latest epackage format is always described in section
;;      "Epackage specification" above. In here you can find list of
;;      older formats and changes.
;;
;;      o   2010-12-03 Draft spec. Compatibility level 1.
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
;;          ;; This command also upgrades the yellow pages file
;;          epackage-batch-ui-download-sources-list
;;
;; Development notes
;;
;;     XEmacs
;;
;;      This extension was written in Emacs 23, but it may work in
;;      Emacs 22 (2007) although that has not been tested. No support
;;      for older Emacs versions is on the chart. Enjoying real life,
;;      work and other Open Source projects where I participate in and
;;      which maintain, take their share. For those reasons I have to
;;      regret that I will not be having resources to port or support
;;      this utility to XEmacs. Please sned patches if you take the
;;      code to ride in XEmacs.
;;
;;     Depends
;;
;;      The *vertical* *bar*, OR-operator(|), is not really
;;      implemented. The pacages "emacs" and "xemacs" are treated
;;      specifically and the effect of "|" is actually the same as if
;;      it were written with comma:
;;
;;          Depends: emacs (>= 22) | xemacs (> 21.3)
;;          Depends: emacs (>= 22), xemacs (> 21.3)
;;
;;      Writing an algorithm for package depends clause that
;;      understands variety of operations (>=, <=, !, |) is
;;      challenging. Take for examples the Debian package depends
;;      guidelines described at
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>
;;      which ispired the syntax used. The Debian packaging system is
;;      centralized, so it knows all the available packages and their
;;      version numbers, and can therefore build the full depends
;;      lista and check if install is even possible. In contrast, the
;;      epackages are *distributed* and the version information or
;;      further depends can only be determined after the package has
;;      been downloaded and reading the "Depends:" field. Becasue of
;;      this, the distributed system:
;;
;;      o   Cannot know beforehand what epackages would be required for X
;;      o   Cannot know beforehand if it is posisble to even install
;;          package (to satisfy all depends).
;;      o   Cannot ask for specific version of epackage, because the
;;          version information is only available *after* the package
;;          has been downloaded.
;;
;;      In daily use these are not a problem in practise. If package X
;;      requires Y, the Y will be downloaded. If Y further requires Z,
;;      the Z will be downloaded etc. Somewhere at the chain the
;;      downloads stop. It is just that no progress indicators
;;      (item/count) can be presented to the user to tell how many
;;      more packages there is to load. The nature of Emacs Lisp
;;      packages is solo; there are only a few depends between
;;      packages.
;;
;;      Regarding the requirement for specific version of the package
;;      in form of:
;;
;;          Depends: cedet (>= 0.9)
;;                         |
;;                         No-op. Will not be used
;;
;;      Emacs extensions have never had any Perl like "use PACKAGE
;;      VERSION" statements, thus there is not much point of
;;      implementing this the `epackage.el'. The syntax is there in
;;      case somewhere in the future Emacs modifies the `require' and
;;      relevant `load' calls to accept optional version argument. For
;;      now, as it has always been, the extension developers ensure
;;      that the extensions will work together with the help of tests
;;      like `boundp', `fboundp' and `featurep'. If an extension
;;      breaks due to change in some other extension, it is best to
;;      notify the original developer and get the code updated.
;;      Compatibility problems between extensions are usually
;;      termporary. In case there is no longer upstream developer to
;;      fix things, the extension is best to be forgotten and removed
;;      from epackages. Or, if you have the time and skills, you can
;;      start maintaining an old extension and become the new
;;      upstream.
;;
;;     Version
;;
;;      Why is there no "Version:" field in the `info' file? There
;;      would be no need for it. The Git repository has tags that
;;      announce all version of the package. There is no need to
;;      manually keep in synch or update a separate "Version:" field
;;      in `info' file in repect to upstream versions in the Git
;;      repository. The "Version:" field may be introduced later if
;;      adds something that is essential.
;;
;; TODO
;;
;;      [Within groups, sorted by priority]
;;
;;      General
;;
;;      o   Run health check for downloaded Epackage
;;      o   Download problem, broken link:
;;          => Offer mailing the Yellow page maintainer
;;      o   What if user manually deletes directories? Left over config files?
;;      o   On upgrade, how to deal with updated files and byte compilation?
;;
;;      REPO
;;
;;      o  Check validity of "git tag -l" and upstream/* against the
;;         speficifation. Two dashes etc.
;;
;;      o   Better Fetch, pull conflict notifications. Now Git error.
;;
;;      o   What if epackage maintainer kills the repo and re-instantiates it
;;          from fresh? Symptoms: can't pull, because repos have diverged and
;;          do not have common objects. SOLUTION: offer deleting repo and
;;          downloading it again. Warn if there are any local modifications,
;;          the user might want ot have a backup (*.b). Can we do that? What
;;          if a backup already exists?
;;
;;      o   What to do if Yellow pages URL (repo) changes? See previous
;;          bullet for suggested fix.
;;
;;      o   Git tags (versions of packages), where is this information kept?
;;          Affects GUI.
;;
;;      o   What if user has made local customizations?
;;          Branch != master. => leave if alone and mark it
;;          "manual". User can deal with the merges and take full
;;          responsibility. We could theoretically still run 'git fetch'.
;;
;;      GUI
;;
;;      o   Write M-x epackage-manager
;;      o   Cache. Build it dynamically from packages and
;;          combine with package information (e.g. version).
;;      o   After download. Trying to install or activate package,
;;          check emacs compatibility and refuse to install if not met.
;;
;;      o   If user selects DETAIL view, collect
;;          information to another buffer dynamically (info, git tags,
;;          current git branch)
;;
;;      o   Rescan current information? (what is installed, what is not)
;;          => Keep cache? Or regenerate, or scan at startup every time?
;;
;;      Some day in the future:
;;
;;      o   Verify Compatibility Level of downloaded epackage
;;      o   Handle Conflicts field
;;      o   Edit yellow pages catalog?
;;          => Submit/update yellow pages catalog changes?
;;          => version controlled, patches? Interface to automatic email?
;;      o  The epackage/*-compile.el is run with `eval-current-buffer'.
;;         What about security considerations? Is there any need, because
;;         these are Git repositories and maintainers should be trusted
;;         => possible solution: require detached GPG signing of *-compile.el

;;; Change Log:

(eval-when-compile
  (autoload 'lm-version "lisp-mnt")
  (autoload 'lm-summary "lisp-mnt")
  (autoload 'lm-commentary "lisp-mnt")
  (autoload 'lm-creation-date "lisp-mnt")
  (autoload 'lm-last-modified-date "lisp-mnt")
  (autoload 'lm-maintainer "lisp-mnt")
  (autoload 'dired-make-relative-symlink "dired-x")
  (autoload 'mail-fetch-field "mail-utils")
  (autoload 'url-http-parse-response "url"))

;;; Code:

(defconst epackage-version-time "2010.1216.1050"
  "Version of last edit.")

(defconst epackage-maintainer "jari.aalto@cante.net"
  "Maintiner's email address.")

(eval-and-compile                       ;We need this at runtim
(defconst epackage-w32-p
  (or (memq system-type '(ms-dos windows-nt))
      (memq window-system '(win32 w32 mswindows)))
  "Non-nil under Windows, DOS operating system."))

(defgroup epackage nil
  "Distributed Emacs Lisp package system (DELPS)."
;  :link '(function-link view-mode)
;  :link '(custom-manual "(emacs)Misc File Ops")
  :group 'tools)

(defcustom epackage--download-action-list nil
  "*TYPE of actions to run after package download.
The order of TYPEs in list is not significant. The \"install\"
TYPE can be of of following. For more information about install
TYPEs, refer to \\[epackage-documentation].

    activate
    autoload
    enable
    compile

To install also dependant packages, add:

    package-depeds

To check package validity and colloect information in
`epackage--buffer-lint', add:

    lint

An example. The following would automatically compile and enable
package after download and download all dpends for the package:

  '(compile enable lint package-depends xboot).

Note: the symbol names have been named so that when sorted, the
actions can be run safely in order.

See also variable `epackage--depends-handling'."
  :type  '(list symbol)  ;; FIXME, list names of symbols
  :group 'epackage)

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

(defcustom epackage-install-download-hook nil
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
  "*Hook run after function `epackage-build-sources-list'."
  :type  'hook
  :group 'epackage)

(defcustom epackage--loader-file-boot-byte-compile-flag t
  "*Non-nil means to byte compile `epackage--loader-file-boot'.
When non-nil, After calling `epackage-loader-file-generate', file
returned by `epackage-file-name-loader-file' is byte compiled."
  :type  'boolean
  :group 'epackage)

(defcustom epackage--sources-list-url
  "git://github.com/jaalto/project--emacs-epackage-sources-list.git"
  "URL to the location of official available package list. The yellow pages.
This is the Git repository that contains the canonical list of
available packages.

The included text file contains information about package names
and their repository download URLs. Empty lines and comment on
their own lines started with character '#' are ignored. There
must be no leading whitespaces in front of PACKAGE-NAME.

  # Comment
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  ...

An example:

  foo git://example.com/repository/foo.git

This list is combined with user given list in
variable `epackage--sources-file-list'."
  :type  'string
  :group 'epackage)

(defcustom epackage--sources-file-list nil
  "*List of files that are in the form of `epackage--sources-list-url'.
In here you can list additional package repositories.

An example:

  '(\"~/.emacs.d/my/epackage-private-repo.lst\")

The files listed will be combined before `epackage--sources-list-url'
into a the main package sources list file whose path is returned
by function `epackage-file-name-sources-list-main'."
  :type  '(list string)
  :group 'epackage)

(defvar epackage--sources-list-regexp
  `,(concat "^\\(%s\\)\\>"
            "[ \t]+\\([^ \t\r\n]+\\)"
            ;;  In case there i no description, do not *require*
            ;;  a match
            "\\(?:[ \t]+\\([^ \t\r\n]+.+[^ \t\r\n]+\\)\\)?")
  "Regexp to match entries described in `epackage--sources-list-url'.
The %s marks the package name.")

(defcustom epackage--root-directory
  (let (ret)
    (dolist (elt (list
                  (if (featurep 'xemacs)
                      "~/.xemacs.d"
                    "~/.emacs.d")
                  "~/elisp"))
      (if (and elt
               (null ret)
               (file-directory-p elt))
          (setq ret elt)))
    (cond
     (ret
      ret)
     (t
      ;; No known package installation root directory
      (message
       (concat "Epackage: [ERROR] Can't determine location of lisp packages."
               "Please define `epackage--root-directory'.")))))
  "*Location of Lisp files. Typically ~/.emacs.d or ~/elisp.
Directory should not contain a trailing slash."
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
  "The name of local yellow pages repository.
Use `epackage-directory-loader' for full path name.")

(defconst epackage--sources-package-name "00sources"
  "The name of local yellow pages repository directory.
Copy of `epackage--sources-list-url'.")

(defconst epackage--directory-name-install "00install"
  "Install directory under `epackage--root-directory'.
This directory contains control files from packages.")

(defvar epackage--sources-file-name-official "epackage.lst"
  "Name of official yellow pages file that lists available packages.
Do not touch. See variable `epackage--sources-list-url'.")

(defvar epackage--package-control-directory "epackage"
  "Name of directory inside VCS controlled package.")

(defvar epackage--pkg-info-file-name "info"
  "Name of information file of epackage.
Do not touch. See variable `epackage--sources-list-url'.")

(defvar epackage--sources-file-name-main "sources.lst"
  "Name of the combined yellow pages file that lists available packages.
Do not touch. See variables `epackage--sources-list-url'
and `epackage--sources-file-list'.")

(defvar epackage--loader-file-boot "epackage-loader.el"
  "File that contains package enabling and activation code.
Use function `epackage-file-name-loader-file' for full path name.
Make fle with `epackage-loader-file-generate'.
See also variable `epackage--loader-file-boot-byte-compile-flag'.")

(defvar epackage--loader-file-load-path "epackage-load-path.el"
  "File that contains `load-path' definitions.
Not a user file. This is used internally during byte compiling
packages.")

(defconst epackage--directory-exclude-regexp
  (concat
   "/\\.\\.?$"
   "\\|"
   (regexp-opt
    '("/RCS"
      "/rcs"
      "/CVS"
      "/cvs"
      "/.svn"
      "/.git"
      "/.bzr"
      "/.hg"
      "/.darcs"
      "/.mtn"
      ))
   "$")
  "Regexp to exclude dirctory names.
See 'epackage-directory-recursive-list-default'.")

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
  '((activate  "-xactivate.el")
    (autoload  "-autoloads.el")
    (enable  "-install.el"  'required)
    (compile  "-compile.el")
    (info  "info" 'required)
    (loaddefs  "-0loaddefs.el")
    (uninstall  "-uninstall.el"))
  "File type and its mappings in `epackage--package-control-directory'.
Format is:
  '((TYPE  FILENAME [REQUIRED-FLAG]) ...)

Ff FILENAME sarts with '-', then the package name is prefixed to
the FILENAME. Say package name 'foo' is prefixed with '-install'
producing 'foo-install.el.")

(defvar epackage--buffer-doc "*Epackage documentation*"
  "Buffer displayed by `epackage-doscumentation'.")

(defvar epackage--buffer-info "*Epackage info*"
  "Buffer displayed by `epackage-pkg-info-display'.")

(defvar epackage--buffer-lint "*Epackage Lint*"
  "Buffer displayed by `epackage-pkg-lint-package'.")

;; FIXME Emacs 24.1 patch
(defconst epackage--buffer-finder-commentary "*Finder-package*"
  "Buffer name of call `finder-commentary'.")

(defvar epackage--buffer-emacs-messages "*Messages*"
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
  "Location of program git(1).")

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

(defconst epackage--batch-ui-menu-string "\
a       Install activate configuration; modifies Emacs environment
A       Deactivate. Uninstall activate configuration
b       Generate boot loader
B       Byte compile epackage
c       Clean install configuration files (whole uninstall)
d       Download epackage
e       Install standard (e)nable configuration from epackage
E       Uninstall standard enable configuration from epackage
g       Get sources list; update the yellow page data
i       Display (i)nfo file of epackage
I       Display documentation of extension.
l       List installed epackages
L       List downloaded epackages
n       List (n)ot installed epackages
o       Install aut(o)load configuration from epackage
p       List available (p)ackages in sources list
r       Remove; delete package physically from local disk
t       Ac(t)ion toggle: after every download, install (e)nable configuration
T       Ac(t)ion toggle: after every download, install (a)ctivate configuration
u       Upgrade epackage. Download new updates
U       Upgrade all epackages
y       B(y)te compile epackaged extension
Y       Action toggle: after every download, b(y)te compile epackage
?       Help.
q       Quit"
  "UI menu to run epackage from command line.")

(defconst epackage--batch-ui-menu-actions
  '((?a epackage-cmd-activate-package)
    (?b epackage-batch-ui-loader-file-generate)
    (?B epackage-batch-ui-byte-compile-package)
    (?A epackage-batch-ui-deactivate-package)
    (?c epackage-batch-ui-clean-package)
    (?d epackage-batch-ui-download-package)
    (?e epackage-batch-ui-enable-package)
    (?E epackage-barch-ui-disable-package)
    (?g epackage-batch-ui-download-sources-list)
    (?i epackage-batch-ui-display-package-info)
    (?I epackage-batch-ui-display-package-documentation)
    (?l epackage-batch-ui-list-installed-packages)
    (?L epackage-batch-ui-list-downloaded-packages)
    (?n epackage-batch-ui-list-not-installed-packages)
    (?o epackage-batch-ui-autoload-package)
    (?r epackage-batch-ui-remove-package)
    (?t epackage-batch-ui-download-action-enable-toggle)
    (?T epackage-batch-ui-download-action-activate-toggle)
    (?u epackage-batch-ui-upgrade-package)
    (?U epackage-batch-ui-upgrade-all-packages)
    (?p epackage-batch-ui-list-available-packages)
    (?q quit)
    (?Q quit)
    (?y epackage-batch-ui-byte-compile-package)
    (?Y epackage-batch-ui-download-action-compile-toggle))
  "UI menucommand and actions. Format: '((KEY FUNCTION) ...).

Use from command line:

  Emacs --batch -Q -l ./epackage.el -f epackage-batch-ui-menu")

(defconst epackage--batch-ui-menu-help "\
In a nutshell
-------------
To install some package: (d)ownload, (e)enable, (b)oot loader generate, (q)uit.

Packages management
-------------------
download        Download package to disk. No install whatsoever.

upgrade         Get updates for epackage.

info            Show downloaded epackage's information file.
                Use command \"List available (p)ackages\" prior download.

install         Several choices:
                * autoload. Install only minimal functions
                  that will be available in autoload state only.
                  If you want to configure everything manually in
                  ~/.emacs startup file, use this (for experts).
                * standard = enable only autoload code.
                  The opposite is disable.
                * activate = install file that provides hooks, bindings
                  and the like. Possibly modifies Emacs setup.
                  The opposite is deactivate.

clean           Delete all install configuration files. Package
                will not be available for later use. M-x calls
                are no longer available,

remove          Physically remove configuration files and package
                from download directory. The opposite of download.

Other actions
-------------
boot loader     Write boot loader that contains all packages'
                configurations in one file. Must be generated/updated
                after each package management change. This is intended to be
                loaded from ~/.emacs with

                (load \"~/.emacs.d/epackage/00conf/epackage-loader\" 'noerr)

get             Get package sources list Yellow Pages data. This updates
                the list of available packages."
  "UI menu help.")

(defmacro epackage-push (x place)
  "A close `push' CL library macro equivalent: (push X PLACE)."
  `(setq ,place (cons ,x ,place)))

(defmacro epackage-asscoc (key list)
  "Access of KEY in LIST and return its value.
An example:  '((a 1) (b 3))  => key \"a\". Returns 1."
  `(nth 1 (assoc ,key ,list)))

(defmacro epackage-fatal (format &rest args)
  "Call `error' with FORMAT and ARGS. Mark message with FATAL tag."
  `(error (concat "Epackage: [FATAL] " ,format) ,@args))

(defmacro epackage-error (format &rest args)
  "Call `error' with FORMAT and ARGS. mark message with ERROR tag."
  `(error (concat "Epackage: [ERROR] " ,format) ,@args))

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
(put 'epackage-with-byte-compile-buffer 'ebyte-compile-buffer-form-spec '(body))
(defmacro epackage-with-byte-compile-buffer (&rest body)
  "Run BODY if variable `epackage--byte-compile-buffer' is non-nil."
  `(if (get-buffer epackage--byte-compile-buffer-name)
       (with-current-buffer (get-buffer epackage--byte-compile-buffer-name)
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

(defsubst epackage-file-name-basename (dir)
  "Like `file-name-nondirectory' but always return last component of DIR.
An example:  /path/to/  => to"
    (when (string-match "^.+/\\([^/]+\\)/?$" dir)
      (match-string 1 dir)))

(defsubst epackage-file-name-directory-previous (dir)
  "Return previous directory by removing one component from DIR.
Return nil of there is nothing to remove .i.e. the result wold be \"/\"."
  (let ((path (file-name-as-directory dir)))
    (when (string-match "\\(.+\\)/[^/]+/?$" path)
      (match-string 1 dir))))

(defsubst epackage-string-p (string)
  "Return STRING of value is non-empty. Otherwise return nil."
  (and (stringp string)
       (not (string-match "^[ \t\r\n]*$" string))
       string))

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
          epackage--loader-file-boot))

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
  "Returnfile name under PACCKAGE directory with PATH added.
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

(defsubst epackage-eval-file (file &optional security)
  "Evaluate FILE with optionally checking SECURITY.
If SECURITY is non-nil, signal error if
- GPG signature is missing at location <FILE>.gpg
- GPG signature is invalid at location <FILE.gpg."
  (with-temp-buffer
    (insert-file-contents-literally file)
    ;; FIXME: Implement SECURITY
    (eval-buffer)))

(defun epackage-directory-list (dir &optional exclude)
  "Return all directories under DIR.
Optionally EXCLUDE matching directories."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (and (stringp exclude)
                      (not (string-match exclude elt))))
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

(defsubst epackage-directory-recursive-list-default (dir list)
  "Return all directories under DIR recursively to LIST.
Exclude directories than contain file .nosearch
or which match `epackage--directory-exclude-regexp'
and `epackage--directory-name'."
  (epackage-directory-recursive-list
   dir
   list
   (concat epackage--directory-exclude-regexp
           "\\|/" epackage--directory-name)))

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
  "Rturn PACKAGE filenme of TYPE in `epackage--directory-name-install'.
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

(defsubst epackage-file-name-activated-compose (package)
  "Return path to PACKAGE under activated directory."
  (format "%s/%s%s"
          (epackage-directory-root)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-xactivate.el" package))))

(defsubst epackage-file-name-enabled-compose (package)
  "Return path to PACKAGE under install directory."
  (format "%s/%s%s"
          (epackage-directory-root)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-install.el" package))))

(defsubst epackage-git-directory-p (dir)
  "Check if there is .git under DIR. Return DIR if so."
  (let ((path (concat (file-name-as-directory dir) ".git")))
    (if (file-directory-p path)
        dir)))

(defun epackage-package-enabled-p (package)
  "Return file if PACKAGE is enabled."
  (let ((file (epackage-file-name-enabled-compose package)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-activated-p (package)
  "Return file if PACKAGE is activated."
  (let ((file (epackage-file-name-activated-compose package)))
    (if (file-exists-p file)
        file)))

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
  "Return sources list build directory; the yellow pages.
Location of `epackage--sources-file-name-main'."
  (epackage-directory-conf))

(defsubst epackage-sources-list-official-directory ()
  "Return sources list repository directory; the yellow pages.
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
  "Signal error with MESSAGE if `epackage-initialize' has not been run."
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
  "Run BODY if `epackage--package-info-file' of PACKAGE exist.
Signal error if it doesn't. Variable `file' is bound during BODY.
Variable `info-file' is bound during macro."
  `(let ((info-file (epackage-file-name-package-info ,package)))
     (unless (file-exists-p info-file)
       (epackage-error "Info file does not exist: %s" info-file))
     (with-current-buffer (find-file-noselect info-file)
       ,@body)))

(put 'epackage-with-sources-list 'lisp-indent-function 0)
(put 'epackage-with-sources-list 'edebug-form-spec '(body))
(defmacro epackage-with-sources-list (&rest body)
  "Run BODY in package list buffer."
  `(progn
     (epackage-sources-list-verify)
     (with-current-buffer
         (find-file-noselect (epackage-file-name-sources-list-main))
       ,@body)))

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
           ,dir))
     (prog1
         (unless (epackage-git-command-ok-p
                  (epackage-git-command-process
                   ,@args))
           (epackage-git-error-handler)))
     (if ,verbose
         (epackage-message
           "Running 'git %s' in %s ...done"
           (mapconcat #'concat (list ,@args) " ")
           ,dir))))

(defsubst epackage-fetch-field (field)
  "Like `mail-fetch-field', but return value only if it exists.
If field is empty or does not exist, return nil."
  (let ((value (mail-fetch-field field)))
    (if (epackage-string-p value)
        value)))

(defun epackage-fetch-field-description ()
  "Return content of 'Description:' '(\"short desc\" \"long desc\").
Remove 1 space indentation and paragraph separators(.) characters."
  (let ((str (epackage-fetch-field "Description"))
        short
        long)
    (if (string-match "^\\(.+\\)$" str)
        (setq short (match-string 1 str)))
    ;;  FIXME. We suppose the caret(^) does not appear in reglar text,
    ;;  otherwise matching the continuing lines would present a more
    ;;  challenging regexp.
    (if (string-match "^\\( [^^]+\\)" str)
        (setq long (match-string 1 str)))
    ;; Remove one-space indentation
    (setq long (replace-regexp-in-string "^ " "" long))
    ;; Remove pragraph separators.
    (setq long (replace-regexp-in-string "^\\.[ \t]*$" "" long))
    (list short long)))

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
   (insert str)
   (epackage-depends-parse-buffer)))

(defsubst epackage-fetch-field-depends ()
  "Return preformatted content of 'Depends:' field.
See `epackage-depends-parse-collect' for returned value format."
  (let ((str (epackage-fetch-field "Depends")))
    (if (stringp str)
        (epackage-depends-parse-string str))))

(defsubst epackage-pkg-info-fetch-field (package field)
  "Read PACKAGE and raw information from FIELD.
Like `mail-fetch-field', but return value only if it exists.
If field is empty or does not exist, return nil."
  (epackage-with-package-info-file package
    (epackage-fetch-field field)))

(defsubst epackage-pkg-info-fetch-field-depends (package)
  "Read PACKAGE and information field 'Depends:' (preformatted).
Like `mail-fetch-field', but return value only if it exists.
See `epackage-depends-parse-collect' for returned value format."
  (epackage-with-package-info-file package
    (epackage-fetch-field-depends)))

(defsubst epackage-git-branch-list-master-p (list)
  "Return non-nil if current branch LIST indicates master as current branch."
  (string-match
   ;; At the beginning, or at end, or in the middle by spaces
   "\\(?:^\\| \\)\\*master\\(?: \\|$\\)"
   (mapconcat 'concat list " ")))

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

(defun epackage-git-command-branch-parse-1 ()
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
    (epackage-git-command-branch-parse-1)))

(defun epackage-git-command-branch-list (dir &optional verbose arg)
  "In DIR, run in optional VERBOSE mode 'git branch [ARG]'.
If optional VERBOSE is non-nil, display progress message.

Return:
    List of tag names."
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
    (epackage-with-git-command dir-before verbose
      "clone" url name)))

(defun epackage-git-master-p (package)
  "Return non-nil if PACKAGE's VCS branch is master."
  (let ((dir (epackage-directory-package-root package)))
    (when (file-directory-p dir)
      (let ((list (epackage-git-command-branch-list dir)))
        (epackage-git-branch-list-master-p list)))))

(defun epackage-upgrade-package (package &optional verbose)
  "Upgrade PACKAGE in VCS directory.
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
               "Branch name is not \"master\" in '%s'. "
               "Possibly changed manually or invalid package.")
            dir))
        (epackage-git-command-pull dir verbose)))))

(defun epackage-upgrade-sources-list (&optional verbose)
  "Update list of available packages; the yellow pages.
If optional VERBOSE is non-nil, display progress message."
  (let ((dir (epackage-sources-list-official-directory)))
    (unless (file-directory-p dir)
      (epackage-error
        (substitute-command-keys
         (format
          `,(concat "No such directory '%s'. "
                    "Run \\[epackage-initialize]")
          dir))))
    (epackage-git-command-pull dir verbose)))

(defun epackage-combine-files (file list &optional verbose)
  "Write to FILE a combined content of LIST of files.
If optional VERBOSE is non-nil, display progress message."
  (with-temp-buffer
    (dolist (elt list)
      (goto-char (point-max))
      (epackage-verbose-message "Combining sources list file %s" elt)
      (insert "###file: " file "\n")
      (insert-file-contents-literally elt))
    (epackage-with-message
        verbose (format "Write master sources list file %s" file)
      (unless (re-search-forward "^[^#\r\n]+://" nil t)
        (epackage-error
          "Can't see any Git repository URLs: %s" elt))
      (write-region (point-min) (point-max) file))))

(defun epackage-build-sources-list (&optional verbose)
  "Build list of available packages; the yellow pages.
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
             (list (epackage-file-name-sources-list-official))))
    (run-hooks 'epackage--build-sources-list-hook)))

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
         ret)
    ;; Check Eamcs flavor requirement
    (when (and flavor
               (setq version (nth 2 flavor)))
      (setq op (nth 1 flavor))
      (setq after (string< version emacs-version ))  ;; A < B   i.e   B > A
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
See variable's documentation for mor einformation."
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

(defun epackage-pkg-depends-satisfy (depends &optional verbose)
  "Resolve package DEPENDS by downloading more packages as needed.
If optional VERBOSE is non-nil, display progress message."
  (let ((missing (epackage-pkg-depends-resolve package verbose)))
    ;; This is recursive, since we're initially called through
    ;; epackage-run-action-list
    (dolist (elt missing)
      (epackage-push elt epackage--depends-satisfy-running)
      (epackage-cmd-download-package elt verbose))))

(defun epackage-run-action-list (package &optional verbose)
  "Run PACKAGE actions listed in `epackage--download-action-list'.
If optional VERBOSE is non-nil, display progress message."
  (let ((list (sort epackage--download-action-list
                    (lambda (a b)
                      (string<
                       (symbol-name a)
                       (symbol-name b))))))
    (dolist (elt list)
      (epackage-verbose-message "package action: %s" elt)
      ;; Development note: keep the list in alphabetical "run" order
      (cond
       ((eq elt 'activate)
        (epackage-cmd-activate-package package verbose))
       ((eq elt 'autoload)
        (epackage-cmd-autoload-package package verbose))
       ((eq elt 'compile)
        (epackage-byte-compile-package-main package))
       ((eq elt 'enable)
        (epackage-cmd-enable-package package verbose))
       ((eq elt 'lint)
        (epackage-pkg-lint-package package verbose))
       ((eq elt 'package-depends)
        (epackage-pkg-depends-satisfy package verbose))))))

(defsubst epackage-enable-file (from to &optional noerr verbose)
  "Enable by copying or by symlinking file FROM TO.
With optional NOERR, do not signall errors, display inly messages.
If optional VERBOSE is non-nil, display progress message.
See variable `epackage--symlink-support-flag'.

Return:
    non-nil    ok
    nil        nok"
  (cond
   ((file-exists-p from)
    (epackage-verbose-message "processing %s" to)
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

(defun epackage-config-install-action
  (type package &optional noerr verbose)
  "Run install of TYPE for PACKAGE.
With optional NOERR, do not signall errors, display inly messages.
If optional VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((from (epackage-directory-packages-control-file
               package type))
        (to (epackage-file-name-install-compose package type)))
    (epackage-enable-file from to noerr verbose)
    (run-hooks 'epackage--install-type-hook)))

(defun epackage-config-install-autoload (package &optional verbose)
  "Install PACKAGE autoload files.
If optional VERBOSE is non-nil, display progress message."
  (let ((status
         (or (epackage-config-install-action 'loaddefs package 'noerr)
             (epackage-config-install-action 'autoload package nil verbose))))
    (when status
      (run-hooks 'epackage--install-autoload-hook)
      status)))

(defun epackage-config-delete-action (type package &optional verbose)
  "Delete install configuration TYPE for PACKAGE.
If optional VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((file (epackage-file-name-install-compose package type)))
    (when (file-exists-p file)
      (epackage-verbose-message "Delete %s" file)
      (delete-file file)
      (run-hooks 'epackage--install-config-delete-type-hook))))

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
        (delete-file file)))
    (if list
        (run-hooks 'epackage--install-config-delete-all-hook))
    list))

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
     "Can't use `epackage--directory-name-install'.")
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
    (epackage-initialize-verify "Can't use `epackage--directory-name-pkg'")
    (dolist (elt (directory-files
                  dir
                  (not 'full-path)))
      (unless (string-match "^00\\|\\." elt)
        (add-to-list 'list elt)))
    (nreverse list)))

(defsubst epackage-status-installed-packages ()
  "Return list of packages in `epackage-directory-install'."
  (epackage-config-status-of-packages 'enable))

(defun epackage-status-not-installed-packages ()
  "Return list of packages in `epackage-directory-packages'.
Those that are not installed in `epackage-directory-install'."
  (let ((active (epackage-config-status-of-packages 'activate))
        (downloaded (epackage-status-downloaded-packages))
        list)
    (dolist (package downloaded)
      (unless (member package active)
        (epackage-push package list)))
    (nreverse list)))

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

(defun epackage-loader-file-insert-header ()
  "Insert header comments."
  (insert
    "\
;; Epackge boot file -- automatically generated
;;
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
  (let (list)
    (dolist (dir (epackage-directory-recursive-list-default path list))
      (insert (format
               "(add-to-list 'load-path \"%s\")\n"
               dir)))))

(defun epackage-loader-file-insert-path-list () ;; FIXME w32
  "Insert `load-path' commands to `current-buffer'."
  (let (name
        package
        list)
    (dolist (file (directory-files
                   (epackage-directory-install)
                   'full-path
                   "^.*-.*\\.el"
                   t))
      (setq name
            (file-name-sans-extension
             (file-name-nondirectory file)))
      ;; package-name-autoloads => package-name
      (setq package (replace-regexp-in-string  "-[^-]+$" "" name))
      (unless (member package list)
        (add-to-list 'list package)
        (epackage-loader-insert-file-path-list-by-path
         (epackage-directory-package-root package))))))

(defun epackage-loader-file-insert-install-code ()
  "Insert package installation code into `current-buffer'."
  ;; FIXME: Should only insert activate, not enable code if both exist
  (dolist (file (directory-files
                 (epackage-directory-install)
                 'full-path
                 "^.*-.*\\.el"
                 t))
    (goto-char (point-max))
    (if (file-exists-p file)
        (insert-file-contents-literally file))))

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
      (write-region (point-min) (point-max) file))))

(defun epackage-loader-file-generate-load-path-maybe (&optional verbose)
  "Generate `epackage-file-name-loader-load-path' file if not exists.
If optional VERBOSE is non-nil, display progress message."
  (let ((file (epackage-file-name-loader-load-path)))
    (unless (file-exists-p file)
      (epackage-loader-file-generate-load-path-main verbose))))

(defun epackage-byte-compile-default-maybe (dir-list &optional verbose)
  "If PACKAGE contains single *.el, byte compile it.
If optional VERBOSE is non-nil, display progress message."
  )

;; epackage-lisp-file-list

(defsubst epackage-loader-file-byte-compile-maybe (&optional verbose)
  "Check `epackage--byte-compile-loader-file' and byte compile.
If optional VERBOSE is non-nil, display progress message."
  (when epackage--loader-file-boot-byte-compile-flag
    (epackage-loader-file-byte-compile verbose)))

(defun epackage-byte-compile-package-guess (package &optional verbose)
  "Run byte compile on PACKAGE only if it contains one file.
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

(defun epackage-byte-compile-package-standard (package &optional verbose)
  "Run byte compile on PACKAGE with standard epacage compile file.
If optional VERBOSE is non-nil, display progress message.

Note: No error checking about existence of
`epackage-directory-packages-control-file' is done."
  (let ((load-path load-path)
        (file (epackage-directory-packages-control-file package 'compile))
        (dir (epackage-directory-package-root package))
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
  "Generate boot loader for all installed or activated packages.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (epackage-loader-file-generate-load-path-main)
  (let ((file (epackage-file-name-loader-boot)))
    (epackage-with-message verbose "Generating boot loader"
      (with-temp-buffer
        (epackage-loader-file-insert-main)
        (write-region (point-min) (point-max) file)
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))
      (epackage-loader-file-byte-compile-maybe verbose))))

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
    (let (case-fold-search
          list)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-z][a-z0-9-]+\\)[ \t]+[a-z]" nil t)
        (epackage-push (match-string-no-properties 1) list))
      (setq list (sort list (lambda (a b)
                              (string< a b))))
      list)))

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
    (epackage-error "Invalid `epackage--program-git' (%s)"
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

(defun epackage-require-main (&optional verbose)
  "Check requirements to run Epackage.
If optional VERBOSE is non-nil, display progress message."
  ;; FIXME, url.el not yet used.
  ;; (epackage-require-emacs verbose)
  (epackage-require-git verbose)
  (epackage-require-directories verbose))

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
        (write-region (point) (point-max) file)
        (kill-buffer (current-buffer))))))

(defun epackage-pkg-lint-info-buffer (&optional verbose)
  "Check validity of info in current buffer.
If optional VERBOSE is non-nil, display progress message."
  (let ((status t)
        field
        value
        regexp)
    (dolist (elt epackage--info-layout-mapping)
      (setq field  (nth 0 elt)
            regexp (nth 1 elt)
            value  (epackage-fetch-field field))
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
    status))

(defun epackage-pkg-lint-info-file (file &optional verbose)
  "Check validity of info FILE.
If optional VERBOSE is non-nil, display progress message."
  (with-current-buffer (find-file-noselect file)
    (let* ((dir (epackage-file-name-directory-previous
		 (file-name-directory file)))
	   (name (epackage-file-name-basename dir))
	   (package (epackage-fetch-field "Package")))
      (when (and verbose
	       (stringp package)
	       (not (string= package name)))
	(epackage-verbose-message
	 "[WARN] Lint - field Package does not match directory name: %s, %s"
	 package name))
    (epackage-pkg-lint-info-buffer verbose))))

(defun epackage-pkg-lint-git-branches (dir &optional verbose)
  "Check validity Git branches of package in DIR.
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
The base name of DIR is takes as the package name. An example:

  ~/.emacs.d/epackage/package/foo  => foo is package name.

Return:
    t  if valid."
  (let ((package (epackage-file-name-nondirectory dir))
        (status t)
        list
        name
        required
        path)
    (dolist (elt epackage--layout-mapping)
      (setq name     (nth 1 elt)
            required (nth 2 elt))
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
	    (epackage-verbose-message "[FATAL] Lint - Missing file: %s" info)
	    (epackage-push 'info list)))
	 (t
	  (epackage-push 'info list))))))
    list))

;;;###autoload
(defun epackage-pkg-lint-package (package &optional verbose)
  "Check validity of PACKAGE.
If optional VERBOSE is non-nil, display progress message.
With VERBOSE sisplay `epackage--buffer-lint'.

Return:
    See function `epackage-pkg-lint-directory'."
  (interactive
   (list (epackage-cmd-select-package "List package: ")
         'interactive))
  (let ((dir (epackage-directory-package-root package))
        point)
    (unless dir
      (epackage-error "Can't Lint. Package does not exist: %s" dir))
    (prog1
        (progn
          (with-current-buffer epackage--buffer-emacs-messages
            (setq point (point)))
          (epackage-pkg-lint-directory dir verbose))
      (when verbose
        (let ((buffer (get-buffer-create epackage--buffer-lint)))
          (with-current-buffer buffer
            (insert (format "-- %s\n" dir)))
          (with-current-buffer epackage--buffer-emacs-messages
            ;; Start reading Lint messages
            (goto-char point)
            (while (re-search-forward "^Epackage:.+Lint - \\(.+\n\\)" nil t)
              (append-to-buffer
               buffer (match-beginning 1) (match-end 1)))
          (display-buffer buffer)))))))

(defun epackage-download-sources-list (&optional verbose)
  "Download sources list file, the yellow pages.
If optional VERBOSE is non-nil, display progress message."
  (if (epackage-sources-list-p)
      (epackage-verbose-message "Sources list already exists.")
    (let ((dir (epackage-sources-list-official-directory)))
      (epackage-git-command-clone
       epackage--sources-list-url dir verbose))))

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
              "Run \\[epackage-cmd-download-sources-list]")))
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

(put 'epackage-mail-macro 'lisp-indent-function 1)
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

;;; User commands

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
  (epackage-download-action-lint 'lint)
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
              (format "*mail epackage %s maintainer*" package)
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
  "Autoload PACKAGE.
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
      (epackage-config-install-action 'enable package nil verbose)
      (run-hooks 'epackage--install-enable-hook))
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
      (epackage-config-install-action 'activate package nil verbose)
      (run-hooks 'epackage--install-activate-hook))
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
   (list (epackage-cmd-select-package "Disable epackage: ")
         'interactive))
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
(defun epackage-cmd-upgrade-all-packages (&optional verbose)
  "Upgrade all downloaded packages.
Install new configurations if package has been enabled.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (let ((list (epackage-status-downloaded-packages)))
    (if list
        (epackage-with-message verbose "Wait, upgrading all packages"
          (dolist (elt list)
            (epackage-cmd-upgrade-package elt verbose)))
      (epackage-verbose-message "No packages downloaded to upgrade"))))

;;;###autoload
(defun epackage-cmd-download-sources-list (&optional verbose)
  "Download or upgrade package list; the yellow pages of package repositories.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (if (epackage-sources-list-p)
      (epackage-with-message verbose "Upgrading package list"
        (epackage-upgrade-sources-list verbose))
    (epackage-with-message verbose "Wait, downloading package list"
      (epackage-download-sources-list))))

;;;###autoload
(defun epackage-cmd-build-sources-list (&optional verbose)
  "Build sources list file.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (if (epackage-sources-list-p)
      (epackage-with-message verbose "Building package sources list"
        (epackage-build-sources-list verbose))
    (epackage-with-message verbose "Initializing package sources list"
      (epackage-build-sources-list))))

;;;###autoload
(defun epackage-cmd-download-package (package &optional verbose)
  "Download PACKAGE, but do not install it.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Install epackage: ")
         'interactive))
  (if (not (epackage-string-p package))
      (epackage-message "No packages selected for install.")
    (if (epackage-package-downloaded-p package)
        (epackage-message "Skip, package already downloaded: %s" package)
      (epackage-download-package package verbose)
      (epackage-run-action-list package verbose))))

;;;###autoload
(defun epackage-cmd-remove-package (package &optional verbose)
  "Physically remove PACKAGE and its configuration files from disk.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Remove epackage: ")
         'interactive))
  (epackage-cmd-disable-package package verbose)
  (let ((dir (epackage-package-downloaded-p package)))
    (if (not dir)
        ;; FIXME: check verbose?
        (epackage-verbose-message
          "Remove ignored. Package not downloaded: %s"
          package)
      (epackage-config-delete-all package verbose)
      (epackage-verbose-message "Remove directory %s" dir)
      (delete-directory dir 'recursive)
      (run-hooks 'epackage--install-remove-hook))))

;;;###autoload
(defun epackage-cmd-upgrade-package (package &optional verbose)
  "Upgrade PACKAGE by downloading new code.
Install new configurations if package has been enabled.
If optional VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Upgrade epackage: ")
         'interactive))
  (cond
   ((not (epackage-string-p package))
    (epackage-message "No epackage selected for upgrade."))
   ((not (epackage-package-downloaded-p package))
    (epackage-message "Package not downloaded: %s" package))
   ((not (epackage-git-master-p package))
    (epackage-message
     "Upgrade ignored. Locally modified. Branch is not \"master\" in %s"
     (epackage-directory-package-root package)))
   (t
    (epackage-upgrade-package package verbose)
    ;; FIXME: Add post-processing
    ;; - New files in epackage/*
    ;; - Auto-install, auto-activate?
    ;; - obsolete 00control/* files ?
    )))

;;;###autoload
(defun epackage-initialize (&optional verbose)
  "Inialize package.
If optional VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (epackage-require-main verbose)
  (unless (epackage-sources-list-p)
    (epackage-cmd-download-sources-list verbose))
  (epackage-cmd-build-sources-list verbose)
  (setq epackage--initialize-flag t)
  (run-hooks 'epackage--initialize-hook))

;;;###autoload
(defun epackage-version ()
  "Display `epackage-version-time'."
  (interactive)
  (message epackage-version-time))

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
         (summary    (epackage-asscoc 'summary list))
         (version    (epackage-asscoc 'version list))
         (maintainer (epackage-asscoc 'maintainer list)))
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

;;;###autoload
(defun epackage-manager ()
  "Start User Interface."
  (epackage-initialize)
  (error "Not yet implemented. Estimate: late spring 2011.")) ;; FIXME

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
        (message package)))))

;;; Command line UI

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
          (message (epackage-file-content-as-string file)))))
     (t
      (epackage-message "Can't display info. Package not downloaded: %s"
                        package))))))

;;;###autoload
(defun epackage-batch-ui-display-package-documentation ()
  "Display downloaded extension's documentation."
  (interactive)
  (let ((package (epackage-cmd-select-package "Display package documentation: ")))
    (epackage-cmd-package-check-macro
        package
        'interactive
        (format "PACKAGE name \"%s\" is invalid for documentation command"
                package)
    (cond
     ((epackage-package-downloaded-p package)
      (let* ((file (epackage-pkg-info-field-commentary package))
             (path (format "%s/%s"
                           (epackage-directory-package-root package)
                           (or file "")))
             str)
        (if (not file)
            (epackage-message
              "Missing 'Commentary:' field in epackage info file")
          (if (setq str (lm-commentary path))
              (message str)
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
  "Call `epackage-cmd-autoload-package'."
  (interactive)
  (call-interactively 'epackage-cmd-byte-compile-package))

;;;###autoload
(defun epackage-batch-ui-autoload-package ()
  "Call `epackage-cmd-autoload-package'."
  (interactive)
  (call-interactively 'epackage-cmd-autoload-package))

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
  (call-interactively 'epackage-cmd-download-action-enable-toggle))

;;;###autoload
(defun epackage-batch-ui-download-action-activate-toggle ()
  "Call `epackage-cmd-download-action-activate-toggle'."
  (interactive)
  (call-interactively 'epackage-cmd-download-action-activate-toggle))

;;;###autoload
(defun epackage-batch-ui-download-action-compile-toggle ()
  "Call `epackage-cmd-download-action-compile-toggle'."
  (interactive)
  (call-interactively 'epackage-cmd-download-action-compile-toggle))

;;;###autoload
(defun epackage-batch-ui-download-sources-list ()
  "Call `epackage-cmd-download-sources-list'."
  (interactive)
  (call-interactively 'epackage-cmd-download-sources-list))

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
        (message "No packages downloaded.")
      (message "Downloaded packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-not-installed-packages ()
  "List downloaded packages."
  (interactive)
  (let ((list (epackage-status-not-installed-packages)))
    (if (not list)
        (message "All downloaded packages are installed.")
      (message "Not installed packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-installed-packages ()
  "List installed packages."
  (interactive)
  (let ((list (epackage-status-installed-packages)))
    (if (not list)
        (message "No packages installed.")
      (message "Installed packages:")
      (epackage-batch-list-package-summamry list))))

;;;###autoload
(defun epackage-batch-ui-list-available-packages ()
  "Display available packages."
  (interactive)
  (let ((list (epackage-sources-list-info-pkg-list)))
    (if (not list)
        (message "No yellow pages sources list downloaded.")
      (message "All available packages for download:")
      (epackage-batch-list-package-summamry list))))

;;; Command line batch commands
;;; emacs -Q --batch -f <command> <args>

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

(defun epackage-batch-ui-menu-selection ()
  "Display UI menu."
  (let* ((str (read-string "Choice: "))
         (char (string-to-char str))
         (menu (assq char epackage--batch-ui-menu-actions))
         (choice (nth 1 menu)))
    (epackage-with-debug
      (message "debug: str %s | char %s | menu %s"
               (length str)
               char
               menu))
    (or choice
        char)))

(defsubst epackage--batch-ui-menu-header ()
  "Display menu header."
  (message "\
===================================================
Epackage - Distributed Emacs Package System (DELPS)
Version: %s <%s>
===================================================
Package activation type after download:%s"
           epackage-version-time
           epackage-maintainer
           (if epackage--download-action-list
               (format " %s" epackage--download-action-list)
             "")))

;;;###autoload
(defun epackage-batch-ui-menu ()
  "Present an UI to run basic command."
  (let (debug-ignored-errors
        (debug-on-error t)
        (vc-handled-backends nil)
        (loop t)
        choice)
    (epackage-initialize 'verbose)
    (setq epackage--debug nil)
    (while loop
      (epackage--batch-ui-menu-header)
      (message epackage--batch-ui-menu-string)
      (setq choice (epackage-batch-ui-menu-selection))
      (epackage-with-debug
        (message "debug: choice %s" choice))
      (cond
       ((null choice)
        (message "** Unknown selection"))
       ((eq choice 'ignore)
        (message "** Not implmented yet"))
       ((eq choice 'quit)
        (message "** Exit")
        (setq loop nil))
       ((functionp choice)
        (call-interactively choice))
       ((eq choice ?\?)
        (message epackage--batch-ui-menu-help))
       (t
        (message "** Unknown menu selection: %s" choice))))))

;;;###autoload
(defalias 'epackage 'epackage-manager)

(add-hook  'epackage--mode-hook 'epackage-mode-define-keys)
(provide   'epackage)
(run-hooks 'epackage--load-hook)

;;; epackage.el ends her
