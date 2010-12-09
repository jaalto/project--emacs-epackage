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
;;      ;;  M-x epackage to start package manager
;;      (autoload 'epackage "epackage" "" t)
;;
;;      ;; One big file to boot all installed packages
;;      ;; Automatically generated. Do not edit.
;;      (load "~/.emacs.d/epackage/00conf/epackage-loader" 'noerr)
;;
;;  In addition to full UI (M-x epackage), there is also minimal
;;  command line UI:
;;
;;      emacs --batch -Q -l /path/to/epackage.el -f epackage-batch-ui-menu

;;; Commentary:

;;  Preface 2009
;;
;;	NOTE: 2010-12-08 This extension is in alpha design state;
;;	meaning that it is not in full use yet. The core elements are
;;	being planned and written. For testing, see available `M-x'
;;	`epackage-*' commands. There is also a rudimentary batch
;;	command line UI:
;;
;;	    # Or run the provided Makefile: "make ui"
;;	    emacs --batch -Q -l /path/to/epackage.el -f epackage-batch-ui-menu
;;
;;	....expect full UI with nice menus, font-lock, mode command
;;	and Emacs buffers like in ELPA somewhere around spring 2011
;;	the earliest.
;;
;;      Emacs has been around for decades now. Many new version have
;;      come and gone (in my days 18.59 ... 24.x), Still there are
;;      wealth of extensions available e.g. et <http://emacswiki.org>
;;      that add new features. The typical procedure to add a new
;;      extension to Emacs is:
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
;;      This utility is different from the existing ELPA[4] Emacs
;;      package manager. It has been built around two concepts: 1) it
;;      borrows the Debian style of package management and 2) it uses
;;      version controlled packages. This is a different approach than
;;      centralized ELPA Emacs package manager.
;;
;;      Each Emacs extension is wrapped into epackage format which
;;      basicly follows the Debian packaging style where a separat
;;      control directory named `epackage/' is used for all the
;;      packaging details: activation, autoloads and installation etc.
;;      In addition, each epackage is imported in and deployed using
;;      Git Distributed Version Control System (DVCS). A specific
;;      "Yellow pages" file lists the available Git repositories where
;;      user can download packages. Once an epackage has been
;;      downloaded, subsequent downloads are very efficient because
;;      only deltas are transferred. Another benefit of DVCS is its
;;      distributed nature: local modifications are possible; also all
;;      the software history and releases are included in one place --
;;      the Git repository. This opens some interesting prospects and
;;      freedom to maintain and deploy epackages.
;;
;;      If you're an Emacs user, all these details do not concern you.
;;      From `M-x' `epackage' package management view, select packages
;;      to download, and activate them. There are several ways how to
;;      install the packages. Select autoload install (no Emacs setup
;;      changes), standard install (= enabling), or activation install
;;      (Emacs environment is changed). Later you can upgrade package
;;      or packages. A new epackage list, the yellow pages, that lists
;;      available Git repositories, must be refreshed from time to
;;      time in order to see new available packages.
;;
;;      If you're an Emacs extension developer who would like to make
;;      the extension available for other to download through
;;      epackage, that will require familiarizing with the `git(1)'.
;;
;;      The epackage system can co-exist with any other installation,
;;      like ELPA, as usual. User's standard Emacs startup files, like
;;      `~/.emacs' are never modified.
;;
;;	[1] http://en.wikipedia.org/wiki/Advanced_Packaging_Tool
;;
;;	[2] http://en.wikipedia.org/wiki/RPM_Package_Manager
;;
;;	[3] http://en.wikipedia.org/wiki/YaST See also
;;	http://en.wikipedia.org/wiki/Yellowdog_Updater,_Modified
;;
;;	[4] http://www.emacswiki.org/emacs/ELPA
;;
;;  Epackage - the DVCS packaging system
;;
;;      In this system packages are available in a form of
;;      distributed[1] git[2] version control repositories. The
;;      traditional packaging methods (like ELPA[2]) have previously
;;      relied on archives like *.tar.gz to hold the code. In contrast
;;      the DVCS offers important features over monolithic archive
;;      approach:
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
;;	o   Encourages social collaboration; more easier interacting
;;	    with the upstream e.g. through http://github.com
;;	    push/pull.
;;
;;      Each Emacs extension need to be prepared for use with this
;;      system: import it into git, make repository must available
;;      online and add information about the Git repository to
;;      epackage sources list, the yellow pages. This job can be done
;;      by anyone who wants to set up a repository. It doesn't need to
;;      be done by the original Emacs extension author (upstream) who
;;      may not be familiar with the `git(1)' program. For more
;;      information about the packaging, refer to section "The
;;      epackage system framework ".
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
;;      o   B, Byte compile package.
;;      o   _c_, Clean package's configuration files (whole uninstall).
;;      o   _d_, Download package.
;;      o   D, run `dired' on package installation directory.
;;      o   e, edit package's *info* file.
;;      o   E, email upstream, the package author (maintainer). You can
;;          as for new wish list features, report bugs etc.
;;      o   g, Get yellow page data. Update package sources list.
;;      o   _i_, Install standard configuration for package.
;;      o   _I_, Uninstall standard configuration for package.
;;      o   l<key>, list command: available, installed, downloaded, enabled,
;;          activated, autoloaded, not-installed.
;;      o   m, mark package (for command install or remove).
;;      o   M, send mail to person who is the maintainer of epackage
;;          for this utility. You can send requests to fix
;;          packaging or update contents of the 'info' file if some
;;          of the information in no up to date.
;;      o   _o_, Install autoload configuration for package.
;;      o   _r_, Remove; delete package physically from local disk.
;;      o   s<key>, sort command. Change listing by several criterias.
;;      o   u, upgrade package to newer version.
;;      o   U, upgrade all packages
;;          TODO: unmark (install, purge, remove) ?
;;      o   v<key>, view command. E.g (a)activation file, (i)info file.
;;      o   q, quit. Run `bury-buffer'.
;;      o   x, execute (install, purge, remove).
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
;;      To epxlain how do all the pieces in this system go together,
;;      lets take a look at the system overview. The system mirrors
;;      the style of Debian packaging management. There are two
;;      primary actors: (1) the epackage package maintainer and (2)
;;      the upstream. These two can be the same person or two separate
;;      persons.
;;
;;      o   A = An Emacs user who wants to install new software
;;      o   (Y)ellow pages = The sources list file that contains
;;          information about available epakages around the globe.
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
;;
;;  User's local epackage system layout
;;
;;      The packages are installed under root `epackage--root-directory',
;;      which defaults to `~/.emacs.d' or `~/elisp' respectively. The
;;      root directory is organized as follows:
;;
;;          epackage/               Under epackage--root-directory
;;          |
;;          +-- 00coonf/
;;          |   epackage-loader.el  One big boot file
;;          |
;;          +-- 00install/
;;          |   <package>-activate.el
;;          |   <package>-install.el
;;          |   ...
;;          |
;;          +--packages/           Version control repositories.
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
;;          patches        o - o (medications; merged to master)
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
;;              cat PACKAGE-* | egrep -v '00|uninst|compile' > PACKAGE-00.el
;;
;;     The *-0loaddefs.el
;;
;;      This file contains extracted ##autoload definitions. The file
;;      is automatically generated. The file does not modify user's
;;      environment. If extension does not contains any `###autoload'
;;      definitions, the manually crafted *-install.el file can be
;;      used as a substitute. In case of missing `##autoload' stanzas,
;;      you're encouraged to contact upstream with a possible patch.
;;      The "zero" at the start of the name is to help proper sorting
;;      ordering of files. Mnemonic: "if you load this file, you
;;      can start calling extension's features". The file ends in:
;;
;;          (provide 'PACKAGE-0loaddefs)
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
;;      indented; suggested indentation is 1 space. Required fields
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
;;          Maintainer: [ <epackage maintainer email> ]
;;          *Email: [ First Last <firts.last@example.com ]
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
;;          Description: Hide or preserve all matching lines in buffer
;;           Main command [C-u] M-x hide-lines to hide or show matching lines.
;;           With prefix argument, the corresponding lines are preserved while
;;           others are hidden.
;;           .
;;           Note 2010-12-03 the code hasn't been touched since 2004.
;;
;;  Details of the info file fields
;;
;;      Recommendations: Use one space to indent continued field.
;;      Limit maximum line length to 80 characters. See variable
;;      `fill-column'
;;
;;     Bugs
;;
;;      URL to report bugs. This can be an email address or a link to
;;      issue tracker of upstream project. Note: send packaging
;;      problems or update requests to the extension's epackage
;;      `Maintainer'.
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
;;      List of dependencies: Emacs flavor and additional packages
;;      required. The version information is enclosed in parentheses
;;      with comparison operators ">=" and "<=". A between range is
;;      not defined. This field follows guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;      In case an extension works only with certain version of Emacs,
;;      this information should be written to the end of
;;      `Description'. (which see). Old packages that are not updated
;;      to work for latest Emacs releases are candidate for removal
;;      from a epackage archive's yellow pages. An example how to use
;;      the field:
;;
;;              Depends: emacs (>= 22.2) | xemacs (>= 20)
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
;;      The Upstream developer's email address(es). Multiple
;;      developers are separated by commas. The role can be expressed
;;      in RFC 2822 comment-parenthesis. An example:
;;
;;              Email: John doe (Author) <jdoe@example.com>,
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
;;      This extension's epackage maintainer. The person who made this
;;      extension available in epackage format. If this field is
;;      missing, then the upstream (`Email') is assumed to be the
;;      packager. It is always desirable that upstream, who develops
;;      the extension also provides the software in epackage format.
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
;;	Several FUNCTIONS can be accessed from command line in a
;;	manner of:
;;
;;	    emacs --batch -Q -l /path/to/epackage.el -f FUNCTION
;;
;;	The functions and their command line arguments are:
;;
;;	    ;; Interactive, menu driven
;;	    epackage-batch-ui-menu
;;
;;	    epackage-batch-ui-upgrade-all-packages
;;	    epackage-batch-upgrade-package PACKAGE ...
;;	    epackage-batch-download-package PACKAGE ...
;;	    epackage-batch-remove-package PACKAGE ...
;;	    epackage-batch-clean-package PACKAGE ...
;;	    epackage-batch-activate-package PACKAGE ...
;;	    epackage-batch-deactivate-package PACKAGE ...
;;	    epackage-batch-enable-package PACKAGE ...
;;	    epackage-batch-disable-package PACKAGE ...
;;	    epackage-batch-ui-list-installed-packages
;;	    epackage-batch-ui-list-not-installed-packages
;;	    epackage-batch-ui-list-downloaded-packages
;;	    epackage-batch-ui-loader-file-generate
;;	    epackage-batch-ui-loader-file-byte-compile
;;
;;	    ;; This command also upgrades the yellow pages file
;;	    epackage-batch-ui-download-sources-list
;;
;; TODO
;;
;;	[Within groups, sorted by priority. ">" under work]
;;
;;	General
;;
;;	o   > Add *-hook variables to command actions.
;;      o   > Run health check for downloaded Epackage
;;      o   > auto-byte-compile feature on package install (cnfigurable)
;;      o   > Verify sources list file: No duplicate of same packages.
;;      o   > Standard install by default enables (installs *-install).
;;          Make this configurable what to do by default.
;;      o   > Dynamically search all *.el and *elc. When byte compiled,
;;          symlink those files as well.
;;      o   > Download problem, broken link:
;;	    => Offer mailing the Yellow pages maintainer about broken link
;;      o   What if user manually deletes directories? Left over config files?
;;
;;	REPO
;;
;;      o   Fetch, pull conflicts?
;;
;;      o   What if epackage maintainer kills the repo and re-instantiates it
;;          from fresh? Symptoms: can't pull, because repos have diverged and
;;          do not have common objects. SOLUTION: offer deleting repo and
;;          downloading it again. Warn if there are any local modifications,
;;          the user might want ot have a backup (*.b). Can we do that? What
;;          if a backup already exists?
;;
;;      o   What to do if Yellow pages URL (repo) changes and you have
;;          the old one installed? How to cope with the change? The Git
;;          repository may need to be destroyed and downloaded again to
;;          be sure (not necessarily derived from old one).
;;
;;      o   re-fetch repository (destroy, re-download).
;;      o   Git tags, where is this information kept? Affects GUI.
;;
;;      o   What if user has made local customizations?
;;          Branch != master. Thought: we leave if alone and mark it
;;          "manual". User can deal with the merges and take full
;;          responsibility. We can still run 'git fetch'.
;;	GUI
;;
;;	o   Write M-x epackage-manager
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
;;	Some day in the future:
;;
;;      o   Verify Compatibility Level of downloaded epackage
;;	o   Implement Depends, Conflicts checks.
;;      o   Edit yellow pages catalog?
;;          => Submit/update yellow pages catalog changes?
;;          => version controlled, patches? Interface to automatic email?

;;; Change Log:

(eval-when-compile
  (autoload 'lm-summary "lisp-mnt")
  (autoload 'lm-maintainer "lisp-mnt")
  (autoload 'dired-make-relative-symlink "dired-x")
  (autoload 'url-http-parse-response "url"))

;;; Code:

(defconst epackage-version-time "2010.1209.0952"
  "Version of last edit.")

(defconst epackage-maintainer "jari.aalto@cante.net"
  "Maintiner's email address.")

(eval-and-compile                       ;We need this at runtim
(defconst epackage-w32-p
  (or (memq system-type '(ms-dos windows-nt))
      (memq window-system '(win32 w32 mswindows)))
  "Non-nil under Windows, DOS operating system."))

(defgroup Epackage nil
  "Distributed Emacs Lisp package system (DELPS)."
;  :link '(function-link view-mode)
;  :link '(custom-manual "(emacs)Misc File Ops")
  :group 'tools)

(defcustom epackage--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'Epackage)

(defcustom epackage--loader-file-byte-compile-flag t
  "*Non-nil meand to byte compile `epackage--loader-file'.
When non-nil, After calling `epackage-loader-file-generate', file
returned by `epackage-file-name-loader-file' is byte compiled."
  :type  'boolean
  :group 'Epackage)

(defcustom epackage--sources-list-url
  "git://github.com/jaalto/project--emacs-epackage-sources-list.git"
  "URL to the location of available package list. The yellow pages.
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

  foo git://example.com/repository/foo.git")

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
  "*Location of lisp files. Typically ~/.emacs.d or ~/elisp.
Directory should not contain a trailing slash."
  :type  'directory
  :group 'Epackage)

(defvar epackage--symlink-support-flag
  (if epackage-w32-p
      nil
    t)
  "If non-nil, symlinks are supported.
The value must be nil under Windows Operating System.")

(defvar epackage--directory-name "epackage"
  "Name of package directory under `epackage--root-directory'.
Use function `epackage-directory' for full path name.")

(defconst epackage--directory-name-pkg "packages"
  "directory under `epackage--root-directory' where to download.
Use function `epackage-file-name-package-compose' for full path name.")

(defconst epackage--directory-name-conf "00conf"
  "The name of local yellow pages repository.
Use `epackage-file-name-loader-directory' for full path name.")

(defconst epackage--sources-package-name "00sources"
  "The name of local yellow pages repository.
Copy of `epackage--sources-list-url'.")

(defconst epackage--directory-name-install "00install"
  "Install directory under `epackage--root-directory'.
This directory contains control files from packages.")

(defvar epackage--sources-file-name "epackage.lst"
  "Name of yellow pages file that lists available packages.
See variable `epackage--sources-list-url'.")

(defvar epackage--loader-file "epackage-loader.el"
  "file that contains package enabling and activation code.
Use function `epackage-file-name-loader-file' for full path name.
Make fle with `epackage-loader-file-generate'.
See also variable `epackage--loader-file-byte-compile-flag'.")

(defvar epackage--package-control-directory "epackage"
  "Name of directory inside VCS controlled package.")

(defconst epackage--directory-exclude-regexp
  (concat
   "/\\.\\.?$"
   "\\|/RCS$"
   "\\|/rcs$"
   "\\|/CVS$"
   "\\|/cvs$"
   "\\|/\\.\\(svn\\|git\\|bzr\\|hg\\|mtn\\|darcs\\)$"
   "\\|/"
   epackage--directory-name
   "$")
  "Regexp to exclude dirctory names.")

(defconst epackage--layout-mapping
  '((activate  "-xactivate.el")
    (autoload  "-autoloads.el")
    (enable  "-install.el"  'required)
    (info  "info" 'required)
    (loaddefs  "-0loaddefs.el")
    (uninstall  "-uninstall.el"))
  "File name mappings under epackage/ directory.
Format is:
  '((TYPE . FILENAME [REQUIRED-FLAG]) ...)

Ff FILENAME sarts with '-', then the package name is prefixed to
the FILENAME. Say package name 'foo' is prefixed with '-install'
producing 'foo-install.el.")

(defvar epackage--doc-buffer "*Epackage documentation*"
  "Buffer displayed by `epackage-doscumentation'.")

(defvar epackage--finder-commentary-buffer "*Finder-package*"
  "Buffer name of call `finder-commentary'.")

(defvar epackage--initialize-flag nil
  "Set to t, when epackage has been started. do not touch.")

(defvar epackage--program-git nil
  "Location of program git(1).")

(defvar epackage--process-output "*Epackage process*"
  "Output of `epackage--program-git'.")

(defvar epackage--debug t
  "If non-nil, activate debug.")

(defconst epackage--batch-ui-menu-string "\
a       Install activate configuration; modifies Emacs environment.
A       Deactivate. Uninstall activate configuration
b       Generate boot loader.
c       Clean package's configuration files (whole uninstall).
d       Download package.
g       Get yellow page data. Update package sources list.
i       Install standard configuration for package.
I       Uninstall standard configuration for package.
l       List installed packages.
L       List downloaded packages.
n       List not installed packages.
o       Install autoload configuration for package.
p       List available packages in yellow pages.
r       Remove; delete package physically from local disk.
u       Upgrade package. Download new updates.
U       Upgrade all packages.
?	Help.
q       Quit."
  "UI menu to run epackage from command line.")

(defconst epackage--batch-ui-menu-actions
  '((?a epackage-cmd-activate-package)
    (?b epackage-batch-ui-loader-file-generate)
;;    (?B epackage-batch-ui-loader-file-byte-compile) ;; FIXME, byte cmpile package
    (?A epackage-batch-ui-deactivate-package)
    (?c epackage-batch-ui-clean-package)
    (?d epackage-batch-ui-download-package)
    (?g epackage-batch-ui-download-sources-list)
    (?i epackage-batch-ui-cmd-enable-package)
    (?I epackage-barch-ui-disable-package)
    (?l epackage-batch-ui-list-installed-packages)
    (?L epackage-batch-ui-list-downloaded-packages)
    (?n epackage-batch-ui-list-not-installed-packages)
    (?o epackage-batch-ui-autoload-package)
    (?r epackage-batch-ui-remove-package)
    (?u epackage-batch-ui-upgrade-package)
    (?U epackage-batch-ui-upgrade-all-packages)
    (?p epackage-batch-ui-list-available-packages)
    (?q quit)
    (?Q quit))
  "UI menucommand and actions. Format: '((KEY FUNCTION) ...).

Use from command line:

  emacs --batch -Q -l ./epackage.el -f epackage-batch-ui-menu")

(defconst epackage--batch-ui-menu-help "\
Packages management
-------------------
download	Download package to disk. No install whatsoever.

upgrade		Get new updates for package(s).

install		Several choices:
		* autoload. Install only minimal functions
		  that will be available in autoload state only.
		  If you want to configure everything manually in
		  ~/.emacs startup file, use this (for experts).
		* install standard = enable only autoload code.
		  The opposite is uninstall = disable.
		* activate = install hooks, bindings and the like.
		  Posisbly modifies Emacs setup.
		  The opposite is deactivate.

clean		Delete all install configuration files. Package
		will not be available for later use. M-x etc.
		calls are not there any longer.

remove		Physically remove configuration files and pakkage
		from download directory

Other actions
-------------
generate	Write a boot loader that contains all packages'
		configurations in one file. This is intended to be
		loaded from ~/.emacs. Must be generated/updated
		after each package management change.

get		Get Yellow pages data. This updated package sources
		list file to know about new available packages."
  "UI menu help.")


(defsubst epackage-file-name-basename (dir)
  "Like `file-name-nondirectory' but always return last component
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

(defsubst epackage-directory ()
  "Return root directory."
  (format "%s%s"
          (expand-file-name
           (file-name-as-directory epackage--root-directory))
          (if (stringp epackage--directory-name)
              epackage--directory-name
            (epackage-error
              "epackage--directory-name is not a string"))))

(defsubst epackage-file-name-loader-directory ()
  "Location of `epackage--directory-name-conf'."
  (format "%s/%s"
          (epackage-directory)
          epackage--directory-name-conf))

(defsubst epackage-file-name-compose (name)
  "Return path to NAME in epackage directory."
  (format "%s/%s"
          (epackage-directory)
          name))

(defsubst epackage-file-name-loader-file ()
  "Return path to boot loader file."
  (format "%s/%s"
          (epackage-file-name-loader-directory)
          epackage--loader-file))

(defsubst epackage-file-name-package-compose (package)
  "Return VCS directory for PACKAGE."
  (format "%s/%s%s"
          (epackage-directory)
          epackage--directory-name-pkg
          (if (string= "" package)
              ""
            (concat "/" package))))

(defsubst epackage-file-name-pkg-directory ()
  "Return VCS directory"
  (epackage-file-name-package-compose ""))

(defsubst epackage-file-name-install-directory ()
  "Return link directory"
  (format "%s/%s"
          (epackage-directory)
          epackage--directory-name-install))

(defsubst epackage-file-name-vcs-package-control-directory (package)
  "Return control directory of PACKAGE"
  (let ((root (epackage-file-name-package-compose package)))
    (format "%s/%s" root epackage--package-control-directory)))

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

(put 'epackage-ignore-errors 'lisp-indent-function 0)
(put 'epackage-ignore-errors 'edebug-form-spec '(body))
(defmacro epackage-ignore-errors (&rest body)
  "A close `ignore-errors' CL library macro equivalent."
  `(condition-case error
       (progn
         ,@body)
     (error)))                          ;variable test, not a function call

(put 'epackage-push 'lisp-indent-function 0)
(put 'epackage-push 'edebug-form-spec '(body))
(defmacro epackage-push (x place)
  "A close `push' CL library macro equivalent: (push X PLACE)."
  `(setq ,place (cons ,x ,place)))

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

(defmacro epackage-fatal (format &rest args)
  "Call `error' with ARGS. Mark message with FATAL tag."
  `(error (concat "Epackage: [FATAL] " ,format) ,@args))

(put 'epackage-error 'lisp-indent-function 0)
(put 'epackage-error 'edebug-form-spec '(body))
(defmacro epackage-error (format &rest args)
  "Call `error' with ARGS. mark message with ERROR tag."
  `(error (concat "Epackage: [ERROR] " ,format) ,@args))

(put 'epackage-fatal 'lisp-indent-function 0)
(put 'epackage-fatal 'edebug-form-spec '(body))
(defmacro epackage-warn (format &rest args)
  "Call `error' with ARGS. Mark message with WARN tag."
  `(message (concat "Epackage: [WARN] " ,format) ,@args))

(put 'epackage-message 'lisp-indent-function 0)
(put 'epackage-message 'edebug-form-spec '(body))
(defmacro epackage-message (format &rest args)
  `(message (concat "Epackage: " ,format) ,@args))

(put 'epackage-with-verbose 'lisp-indent-function 0)
(put 'epackage-with-verbose 'edebug-form-spec '(body))
(defmacro epackage-with-verbose (&rest body)
  "If variable `verbose' is non-nil, run BODY."
  `(when verbose
     ,@body))

(put 'epackage-verbose-message 'lisp-indent-function 0)
(put 'epackage-verbose-message 'edebug-form-spec '(body))
(defmacro epackage-verbose-message (&rest args)
  "If variable `verbose' is non-nil, call `message' with ARGS."
  `(epackage-with-verbose
     (epackage-message ,@args)))

(put 'epackage-with-message 'lisp-indent-function 2)
(put 'epackage-with-message 'edebug-form-spec '(body))
(defmacro epackage-with-message (verbose message &rest body)
  "if VERBOSE, display MESSAGE before and after (\"..done\") BODY."
  `(progn
     (if ,verbose
         (epackage-message "%s" (concat ,message "...")))
     (prog1
         ,@body
       (if ,verbose
           (epackage-message "%s" (concat ,message "...done"))))))

(defun epackage-file-name-pkg-directory-control-file (package type)
  "Return PACKAGE control file of TYPE.
The TYPE is car of list `epackage--layout-mapping'."
  (let ((dir (epackage-file-name-vcs-package-control-directory package))
        (file (nth 1 (assq type epackage--layout-mapping))))
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
  (let ((dir (epackage-file-name-install-directory))
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
          (epackage-directory)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-xactivate.el" package))))

(defsubst epackage-file-name-enabled-compose (package)
  "Return path to PACKAGE under install directory."
  (format "%s/%s%s"
          (epackage-directory)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-install.el" package))))

(defsubst epackage-git-directory-p (dir)
  "Return DIR if directory contains .git/"
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
  "Check if package has been downloaded."
  (unless (epackage-string-p package)
    (epackage-error "arg 'package' is not a string."))
  (let ((dir (epackage-file-name-package-compose package)))
    (if (file-directory-p dir)
        dir)))

(defsubst epackage-sources-list-directory ()
  "Return sources list, the yellow pages, directory."
  (epackage-file-name-package-compose epackage--sources-package-name))

(defsubst epackage-file-name-sources-list ()
  "Return path to `epackage--sources-file-name'."
  (format "%s/%s"
          (epackage-sources-list-directory)
          epackage--sources-file-name))

(defsubst epackage-directory-p (dir)
  "Check if directory contains subdir `epackage--directory-name'."
  (file-directory-p
   (concat
    (file-name-as-directory dir)
    epackage--directory-name)))

(defsubst epackage-sources-list-p ()
  "Check existence of `epackage--sources-file-name'."
  (let ((file (epackage-file-name-sources-list)))
    (if (file-exists-p file)
	file)))

(defsubst epackage-sources-list-verify ()
  "Signal error if `epackage--sources-file-name' does not exist."
  (or (epackage-sources-list-p)
      (epackage-error "Missing file %s. Run epackage-initialize"
		      (epackage-file-name-sources-list))))

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

(defun epackage-error-no-directory (dir &optional message)
  "Signal error with optional MESSAGE if DIR does not exist."
  (unless (file-directory-p dir)
    (epackage-error-initialize
     (or message
         (format "No such directory %s" dir)))))

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

(put 'epackage-with-sources-list 'lisp-indent-function 0)
(put 'epackage-with-sources-list 'edebug-form-spec '(body))
(defmacro epackage-with-sources-list (&rest body)
  "Run BODY in package list buffer."
  `(progn
     (epackage-sources-list-verify)
     (with-current-buffer
         (find-file-noselect (epackage-file-name-sources-list))
       ,@body)))

(defsubst epackage-git-error-handler (&optional command)
  "On Git error, show proces buffer and signal error."
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
  "Run git COMMAND with output to `epackage--process-output'."
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
  "Run git command in DIR with ARGS.
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

(defsubst epackage-git-branch-list-master-p (list)
  "Return non-nil if current branch LIST indicates master as current branch."
  (string-match
   ;; At the beginning, or at end, or in the middle by spaces
   "\\(?:^\\| \\)\\*master\\(?: \\|$\\)"
   (mapconcat 'concat list " ")))

(defun epackage-git-command-tag-list (dir &optional verbose)
  "Run 'git tag -l' in DIR.
If VERBOSE is non-nil, display progress message.

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
  "Run 'git branch ARG' in DIR.
If VERBOSE is non-nil, display progress message.

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
If VERBOSE is non-nil, display progress message."
  (epackage-git-branch-list-current-branch
   (epackage-git-command-branch-list dir verbose)))

(defun epackage-git-command-pull (dir &optional verbose)
  "Run 'git pull' in DIR.
If VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "pull"))

(defun epackage-git-command-fetch (dir &optional verbose)
  "Run 'git fetch' in DIR.
If VERBOSE is non-nil, display progress message."
  (epackage-with-git-command dir verbose
    "fetch"))

(defun epackage-git-command-clone (url dir &optional verbose)
  "Run 'git clone URL DIR' in VCS package directory vault.
If VERBOSE is non-nil, display progress message."
  (let ((name (epackage-file-name-basename dir))
	(dir-before (epackage-file-name-directory-previous dir)))
    (epackage-with-git-command dir-before verbose
      "clone" url name)))

(defun epackage-git-master-p (package)
  "Return non-nil if PACKAGE's VCS branch is master."
  (let ((dir (epackage-file-name-package-compose package)))
    (when (file-directory-p dir)
      (let ((list (epackage-git-command-branch-list dir)))
        (epackage-git-branch-list-master-p list)))))

(defun epackage-upgrade-package (package &optional verbose)
  "Upgrade PACKAGE in VCS directory.
If VERBOSE is non-nil, display progress message."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (epackage-error "No download URL for package '%s'" package))
    (let ((dir (epackage-file-name-package-compose package)))
      (epackage-with-verbose
        (epackage-message "Upgrading package: %s..." package))
      (unless (epackage-git-master-p package)
        (epackage-fatal
          `,(concat
             "Can't upgrade. "
             "Branch name is not \"master\" in '%s'. "
             "Possibly changed manually or invalid package.")
          dir))
      (epackage-git-command-pull dir verbose)
      (epackage-with-verbose
        (epackage-message "Upgrading package: %s...done" package)))))

(defun epackage-upgrade-sources-list (&optional verbose)
  "Update list of available packages; the yellow pages.
If VERBOSE is non-nil, display progress message."
  (let ((dir (epackage-sources-list-directory)))
    (unless (file-directory-p dir)
      (epackage-error
        (substitute-command-keys
         (format
          `,(concat "No such directory '%s'. "
                    "Run \\[epackage-initialize]")
          dir))))
    (epackage-git-command-pull dir verbose)))

(defun epackage-download-package (package &optional verbose)
  "Download PACKAGE to VCS directory.
If VERBOSE is non-nil, display progress message."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (epackage-error "No Git URL for package '%s'" package))
    (let ((dir (epackage-file-name-package-compose package)))
      (epackage-git-command-clone url dir))))

(defsubst epackage-enable-file (from to &optional noerr verbose)
  "Enable by copying or by symlinking file FROM TO.
With NOERR, do not signall errors, display inly messages.
If VERBOSE is non-nil, display progress message.

Return:
    non-nil    ok
    nil        nok"
  (cond
   ((file-exists-p from)
    (epackage-verbose-message "processing %s" to)
    (cond
     (epackage--symlink-support-flag
      (dired-make-relative-symlink from to 'overwrite))
     (t
      (copy-file from to 'overwrite 'keep-time)))
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
With NOERR, do not signall errors, display inly messages.
If VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((from (epackage-file-name-pkg-directory-control-file
               package type))
        (to (epackage-file-name-install-compose package type)))
    (epackage-enable-file from to noerr verbose)))

(defun epackage-config-install-autoload (package &optional verbose)
  "Install PACKAGE autoload files.
If VERBOSE is non-nil, display progress message."
  (or (epackage-config-install-action 'loaddefs package 'noerr)
      (epackage-config-install-action 'autoload package nil verbose)))

(defun epackage-config-delete-action (type package &optional verbose)
  "Delete install configuration TYPE for PACKAGE.
If VERBOSE is non-nil, display progress message.
TYPE is car of `epackage--layout-mapping'."
  (let ((file (epackage-file-name-install-compose package type)))
    (when (file-exists-p file)
      (epackage-with-verbose
        (epackage-message "Delete %s" file))
      (delete-file file))))

(defun epackage-config-delete-all (package &optional verbose)
  "Delete all install configuration files for PACKAGE
If VERBOSE is non-nil, display progress message."
  (let ((dir (epackage-file-name-install-directory)))
    (epackage-error-no-directory dir)
    (dolist (file (directory-files
                   dir
                   'full-path
                   (format "^%s-" package)
                   t))
      (when (file-exists-p file)
        (epackage-with-verbose
          (epackage-message "Delete %s" file))
          (delete-file file)))))

(defun epackage-directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (not (string-match
                       epackage--directory-exclude-regexp
                       elt)))
        (epackage-push elt list)))
    list))

(defun epackage-directory-recursive-list (dir list)
  "Return all directories under DIR recursively to LIST.
Exclude directories than contain file .nosearch
or whose name match `epackage--directory-name'."
  (let ((dirs (epackage-directory-list dir)))
    (epackage-push dir list)
    (dolist (elt dirs)
      (cond
       ((file-exists-p (concat elt "/.nosearch")))
       (t
        (epackage-push elt list)
        (epackage-directory-recursive-list elt list))))
    list))

(defun epackage-config-status-of-packages (type)
  "Return packages of TYPE of `epackage--layout-mapping'."
  (let* ((dir      (epackage-file-name-install-directory))
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
          (add-to-list list (match-string 1 elt))))
    (nreverse list)))

(defsubst epackage-status-enabled-packages ()
  "Return list of packages in `epackage-file-name-install-directory'."
  (epackage-config-status-of-packages 'enable))

(defsubst epackage-status-activated-packages ()
  "Return list of packages in `epackage-file-name-install-directory'."
  (epackage-config-status-of-packages 'activate))

(defun epackage-status-downloaded-packages ()
  "Return list of packages in `epackage--directory-name-pkg'."
  (let ((dir (epackage-file-name-pkg-directory))
        list)
    (epackage-initialize-verify "Can't use `epackage--directory-name-pkg'")
    (dolist (elt (directory-files
                  dir
                  (not 'full-path)))
      (unless (string-match "^00\\|\\." elt)
        (add-to-list 'list elt)))
    (nreverse list)))

(defsubst epackage-status-installed-packages ()
  "Return list of packages in `epackage-file-name-install-directory'."
  (epackage-config-status-of-packages 'enable))

(defun epackage-status-not-installed-packages ()
  "Return list of packages in `epackage-file-name-pkg-directory'.
Those that are not installed in `epackage-file-name-install-directory'."
  (let ((active (epackage-config-status-of-packages 'activate))
        (downloaded (epackage-status-downloaded-packages))
        list)
    (dolist (package downloaded)
      (unless (member package active)
        (epackage-push package list)))
    (nreverse list)))

(defun epackage-loader-file-insert-header ()
  "Insert header comments."
  (insert
   (format
    "\
;; Epackge boot file -- automatically generated
;;
;; Do not modify. Changes done here will be lost.
;; Add following to your ~/.emacs to use this file:
;;   (load-file \"%s\")

"
    (file-name-sans-extension
     (epackage-file-name-loader-file)))))

(defsubst epackage-loader-file-insert-footer ()
  "Insert Footer."
  (insert
   (format "\
\(provide '%s)

;; End of file
"
           (file-name-sans-extension
            (file-name-nondirectory
             (epackage-file-name-loader-file))))))

(defun epackage-loader-insert-file-path-list-by-path (path)
  "Insert `load-path' definitions to `current-buffer' from PATH."
  (let (list)
    (dolist (dir (epackage-directory-recursive-list path list))
      (insert (format
               "(add-to-list 'load-path \"%s\")\n"
               dir)))))

(defun epackage-loader-file-insert-path-list () ;; FIXME w32
  "Insert `load-path' commands to `current-buffer'."
  (let (name
        package
        list)
    (dolist (file (directory-files
                   (epackage-file-name-install-directory)
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
         (epackage-file-name-package-compose package))))))

(defun epackage-loader-file-insert-install-code ()
  "Insert package installation code into `current-buffer'."
  ;; FIXME: Should only insert activate, not enable code if both exist
  (dolist (file (directory-files
                 (epackage-file-name-install-directory)
                 'full-path
                 "^.*-.*\\.el"
                 t))
    (goto-char (point-max))
    (if (file-exists-p file)
        (insert-file-contents-literally file))))

(defsubst epackage-loader-file-insert-main ()
  "Insert Epackage loader boot commands to current point."
  (epackage-loader-file-insert-header)
  (epackage-loader-file-insert-path-list)
  (epackage-loader-file-insert-install-code)
  (epackage-loader-file-insert-footer))

(defun epackage-loader-file-byte-compile (&optional verbose)
  "Byte compile `epackage-file-name-loader-file'.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (let ((file (epackage-file-name-loader-file)))
    (cond
     ((file-exists-p file)
      (byte-compile-file file))
     (verbose
      (epackage-message "No boot loader file generated to byte compile.")))))

(defsubst epackage-loader-file-byte-compile-maybe (&optional verbose)
  "Check `epackage--byte-compile-loader-file' and byte compile.
If VERBOSE is non-nil, display progress message."
  (when epackage--loader-file-byte-compile-flag
    (epackage-loader-file-byte-compile verbose)))

(defun epackage-loader-file-generate (&optional verbose)
  "Generate main loader for all installed or activated packages.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (let ((file (epackage-file-name-loader-file)))
    (epackage-with-message verbose "Generating boot loader"
      (with-current-buffer (find-file-noselect file)
        (delete-region (point-min) (point-max))
        (epackage-loader-file-insert-main)
        (write-region (point-min)
                      (point-max)
                      (epackage-file-name-loader-file))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))
      (epackage-loader-file-byte-compile-maybe verbose))))

(defun epackage-sources-list-info-parse-line (package)
  "Return list of fields described in `epackage--sources-list-url'.
Point must be at the beginning of line."
  (if (looking-at
       (format
        `,(concat "^\\(%s\\)\\>"
                  "[ \t]+\\([^ \t\r\n]+\\)"
                  "[ \t]*\\([^ \t\r\n]*\\)")
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
    (let ((re
           (format
            `,(concat "^\\(%s\\)\\>"
                      "[ \t]+\\([^ \t\r\n]+\\)"
                      "[ \t]*\\([^ \t\r\n]*.*[^ \t\r\n]\\)")
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
If VERBOSE is non-nil, display progress message."
  (unless (fboundp 'url-retrieve-synchronously)
    (epackage-error
      `,(concat
         "this Emacs does not define "
         "`url-retrieve-synchronously' from url.el"))))

(defun epackage-require-git (&optional verbose)
  "Require Git program.
If VERBOSE is non-nil, display progress message."
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
If VERBOSE is non-nil, display progress message."
  (dolist (dir (list
                (epackage-directory)
                (epackage-file-name-pkg-directory)
                (epackage-file-name-install-directory)
                (epackage-file-name-loader-directory)))
    (unless (file-directory-p dir)
      (epackage-with-verbose
        (epackage-message "Making directory %s ..." dir))
      (make-directory dir))))

(defun epackage-require-main (&optional verbose)
  "Check requirements to run Epackage.
If VERBOSE is non-nil, display progress message."
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

(defun epackage-url-retrieve-main (url file)
  "Download URL and save to a FILE."
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (epackage-error "Can't access url: %s" url))
    (with-current-buffer buffer
      (epackage-url-http-parse-respons-error url)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (epackage-with-binary
        (write-region (point) (point-max) file)
        (kill-buffer (current-buffer))))))

(defun epackage-pkg-lint-git-branches (dir)
  "Check validity Git branches of package in DIR.
If valid, return list of required branches."
  (let ((list (epackage-git-command-branch-list
               dir (not 'verbose) "-a"))
        branches
        status)
    (dolist (elt list)
      ;; * master
      ;; remotes/origin/upstream
      (when (string-match "^\\(\\*?master\\|\\(?:.+/\\)upstream\\)$" elt)
        (epackage-push elt branches)))
    (if (eq (length branches) 2)
        branches)))

(defun epackage-pkg-lint-dir-structure (dir)
  "Check validity directories of package in DIR.
The base name of DIR is the package name. An example:

  ~/.emacs.d/epackage/package/foo  => foo is package name.

If valid, return list of required or optional files."
  (let ((package (epackage-file-name-nondirectory dir))
        invalid
        list)
    (dolist (elt epackage--layout-mapping)
      (unless invalid
        (let* ((name (nth 1 elt))
               (required (nth 2 elt))
               (path (format "%s%s/%s%s"
                             (file-name-as-directory dir)
                             epackage--directory-name
                             package
                             name)))
          (cond
           (required
            (if (file-exists-p path)
                (epackage-push path list)
              (setq invalid path)))
           (t
            (if (file-exists-p path)
                (epackage-push path list)))))))
    list))

(defun epackage-pkg-lint-main (dir)
  "Check validity of package in DIR.
If invalid, return list of problems:
  'dir      Missing `epackage--directory-name'
  'files    Missing required `epackage--layout-mapping'.
  'git      Missing required Git branches: upstream, master."
  (let (list)
    (if (not (epackage-directory-p dir))
        (epackage-push 'dir list)
      (unless (epackage-pkg-lint-git-branches dir)
        (epackage-push 'git list))
      (unless (epackage-pkg-lint-dir-structure dir)
        (epackage-push 'files list)))
    list))

(defun epackage-download-sources-list (&optional verbose)
  "Download sources list file, the yellow pages."
  (if (epackage-sources-list-p)
      (epackage-with-verbose
        (epackage-message "Sources list already exists."))
    (let ((dir (epackage-sources-list-directory)))
      (epackage-git-command-clone
       epackage--sources-list-url dir verbose))))

(defun epackage-cmd-select-package (&optional message)
  "Interactively select package with optional MESSAGE.
Return package name or nil."
  (let (package)
    (if (not (epackage-sources-list-p))
        (epackage-message "%s"
                          (substitute-command-keys
                           `,(concat
                              "Can't build package list. "
                              "Run \\[epackage-cmd-download-sources-list]")))
      (setq package
            (completing-read
             (if message
                 message
               "Select epackage: ")
             (epackage-sources-list-info-pkg-list)
             (not 'predicate)
             'require-match))
      (if (epackage-string-p package)
          package))))

(put 'epackage-cmd-package-check-macro 'lisp-indent-function 3)
(put 'epackage-cmd-package-check-macro 'edebug-form-spec '(body))
(defmacro epackage-cmd-package-check-macro
  (package verbose message &rest body)
  "Check PACKAGE, VERBOSE. If nok, display/signal error MESSAGE. If ok, run BODY."
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

;;;###autoload
(defun epackage-cmd-autoload-package (package &optional verbose)
  "Autoload PACKAGE.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Autoload epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (format "PACKAGE name \"%s\" is invalid for autoload command"
	      package)
    (epackage-config-install-autoload package verbose)))

;;;###autoload
(defun epackage-cmd-enable-package (package &optional verbose)
  "Enable PACKAGE.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Enable epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (format "package name \"%s\" is invalid for enable command"
	      package)
    (epackage-config-install-autoload package verbose)
    (epackage-config-install-action 'enable package nil verbose)))

;;;###autoload
(defun epackage-cmd-disable-package (package &optional verbose)
  "Disable PACKAGE.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Disable epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (format "package name \"%s\" is invalid for disable command"
	      package)
    (let ((file (epackage-file-name-install-compose package 'enable)))
      (when (file-exists-p file)
	(epackage-with-verbose
	  (epackage-message "Delete %s" file))
	(delete-file file)))))

;;;###autoload
(defun epackage-cmd-activate-package (package &optional verbose)
  "Activate PACKAGE autoload files.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Activate epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (epackage-message "package name \"%s\" is invalid for activate command"
			package)
    (epackage-config-install-autoload package verbose)
    (epackage-config-install-action 'activate package nil verbose)))

;;;###autoload
(defun epackage-cmd-deactivate-package (package &optional verbose)
  "Deactivate PACKAGE.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Deactivate epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (epackage-error "package name \"%s\" is invalid for deactivate command"
		      package)
    (let ((file (epackage-file-name-install-compose package 'activate)))
      (when (file-exists-p file)
	(epackage-with-verbose
	  (epackage-message "Delete %s" file))
	(delete-file file)))))

;;;###autoload
(defun epackage-cmd-clean-package (package &optional verbose)
  "Clean all install configuration files of PACKAGE.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Disable epackage: ")
         'interactive))
  (epackage-cmd-package-check-macro package verbose
      (epackage-error "package name \"%s\" is invalid for clean command"
		      package)
    (epackage-config-delete-all package verbose)))

;;;###autoload
(defun epackage-cmd-remove-package (package &optional verbose)
  "Physically remove PACKAGE and its configuration files from disk.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list (epackage-cmd-select-package "Remove epackage: ")
         'interactive))
  (epackage-cmd-disable-package package verbose)
  (let ((dir (epackage-package-downloaded-p package)))
    (if (not dir)
        (epackage-with-verbose
          (epackage-message "Remove ignored. Package not installed: %s"
                            package))
      (epackage-config-delete-all package verbose)
      (epackage-with-verbose
        (epackage-message "Delete directory %s" dir))
      (delete-directory dir 'recursive))))

;;;###autoload
(defun epackage-cmd-upgrade-package (package &optional verbose)
  "Downloads updates for existing PACKAGE.
Install new configurations if package has been enabled.
If VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Upgrade epackage: ")
         'interactive))
  (cond
   ((not (epackage-string-p package))
    (epackage-message "No epackage selected for upgrade."))
   ((not (epackage-package-downloaded-p package))
    (epackage-message "Epackage not downloaded"))
   ((not (epackage-git-master-p package))
    (epackage-message
     "Abort. Package is manually modified. Branch is not 'master' in %s"
     (epackage-file-name-package-compose package)))
   (t
    (epackage-upgrade-package package verbose)
    ;; FIXME: Add post-processing
    ;; - New files in epackage/*
    ;; - Auto-install, auto-activate?
    ;; - obsolete 00control/* files ?
    )))

;;;###autoload
(defun epackage-cmd-upgrade-all-packages (&optional verbose)
  "Downloads updates for all packages.
Install new configurations if package has been enabled.
If VERBOSE is non-nil, display progress messages."
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
If VERBOSE is non-nil, display progress messages."
  (interactive
   (list 'interactive))
  (if (epackage-sources-list-p)
      (epackage-with-message verbose "Upgrading package list"
        (epackage-upgrade-sources-list verbose))
    (epackage-with-message verbose "Wait, downloading package list"
      (epackage-download-sources-list))))

;;;###autoload
(defun epackage-cmd-download-package (package &optional verbose)
  "Download PACKAGE, but do not install it.
If VERBOSE is non-nil, display progress messages."
  (interactive
   (list (epackage-cmd-select-package "Install epackage: ")
         'interactive))
  (if (not (epackage-string-p package))
      (epackage-message "No packages selected for install.")
    (if (epackage-package-downloaded-p package)
        (epackage-message "Skip, already downloaded: %s" package)
      (epackage-download-package package verbose))))

;;;###autoload
(defun epackage-initialize (&optional verbose)
  "Inialize package.
If VERBOSE is non-nil, display progress message."
  (interactive
   (list 'interactive))
  (epackage-require-main verbose)
  (unless (epackage-sources-list-p)
    (epackage-cmd-download-sources-list verbose))
  (setq epackage--initialize-flag t))

;;;###autoload
(defun epackage-manager ()
  "Start User Interface."
  (epackage-initialize))

;;;###autoload
(defun epackage-version ()
  "Display `epackage-version-time'."
  (interactive)
  (message epackage-version-time))

(defun epackage-documentation-header-string ()
  "Make documentation header string.
Summary, Version, Maintainer etc."
  (with-current-buffer (find-file-noselect (locate-library "epackage.el"))
    (let ((summary (lm-summary))
          (maintainer (car-safe (lm-maintainer)))
          (version epackage-version-time))
      (concat
       "epackage.el -- " summary "\n\n"
       "Version   : " version "\n"
       "Maintainer: " (or maintainer "") "\n"
       "\n"))))

;;;###autoload
(defun epackage-documentation ()
  "Display documentation."
  (interactive)
  (let ((buffer (get-buffer epackage--doc-buffer)))
    (unless buffer
      ;; See also lm-commentary
      (finder-commentary "epackage.el")
      (with-current-buffer epackage--finder-commentary-buffer
        (let ((str (buffer-string))
              (buffer (get-buffer-create epackage--doc-buffer)))
          (with-current-buffer buffer
            (insert str)
            (goto-char (point-min))
            (insert (epackage-documentation-header-string)))))
      (kill-buffer epackage--finder-commentary-buffer))
    (display-buffer buffer)))

(defun epackage-manager ()
  "Start User Interface."
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

;;;###autoload
(defun epackage-batch-ui-loader-file-generate ()
  "Call `epackage-loader-file-generate'."
  (interactive)
  (call-interactively 'epackage-loader-file-generate))

;;;###autoload
(defun epackage-batch-ui-loader-file-byte-compile ()
  "Call `epackage-loader-file-byte-compile'."
  (interactive)
  (call-interactively 'epackage-loader-file-byte-compile))

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
(defun epackage-batch-ui-download-sources-list ()
  "Call `epackage-cmd-download-sources-list'."
  (interactive)
  (call-interactively 'epackage-cmd-download-sources-list))

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
(defun epackage-batch-ui-download-sources-list ()
  "Call `epackage-batch-ui-download-sources-list'."
  (interactive)
  (call-interactively 'epackage-cmd-download-sources-list))

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

;;;###autoload
(defun epackage-batch-enable-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-enable-package elt 'verbose)))

;;;###autoload
(defun epackage-batch-disable-package ()
  "Run `epackage-cmd-enable-package' for command line args."
  (epackage-batch-ignore-errors-macro
   (epackage-cmd-enable-package elt 'verbose)))

;;;###autoload
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
===================================================
Version: %s
Contact: %s"
	   epackage-version-time
	   epackage-maintainer))

;;;###autoload
(defun epackage-batch-ui-menu ()
  "Present an UI to run basic command."
  (epackage-initialize 'verbose)
  (let ((vc-handled-backends nil)
	(loop t)
        choice)
    (setq debug-on-error t)
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
(defalias 'epackage-manager 'epackage)

(add-hook  'epackage--mode-hook 'epackage-mode-define-keys)
(provide   'epackage)
(run-hooks 'epackage--load-hook)

;;; epackage.el ends here
