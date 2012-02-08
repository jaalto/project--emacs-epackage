#   Copyright
#
#	Copyright (C) 2010-2012 Jari Aalto <jari.aalto@cante.net>
#
#   License
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program. If not, see <http://www.gnu.org/licenses/>.

PACKAGE		= epackage
SRC		= $(PACKAGE).el

#  We can't use variable EMACS, because under Emacs M-x shell, it
#  would be set to 't' and override anything here.

BIN		= emacs
LISP		= --eval '(progn (setq vc-handled-backends nil))'
PKG		= -l $(SRC)
COMPILE		= --batch --no-init-file --quick --funcall batch-byte-compile
INVOKE		= $(BIN) -Q -nw --batch

all: help

help:
	@echo "Select make <target>:"
	@echo "---------------------"
	@grep '^# .*-' Makefile | sed 's,# ,,' | sort

# clean - Delete files that can be generated
clean:
	rm -f *.elc *[#~] *.bak

$(PACKAGE).elc: $(SRC)
	$(BIN) $(COMPILE) $(SRC)

# build - Byte compile *.el file
build: $(PACKAGE).elc

# doc - Build plain text and HTML documentation from *.el with external utilities
doc:
	$(MAKE) -C doc

# install - There is no install. Manually copy *.el file to Emacs load-path
install:
	@echo "There is no install. Manually copy *.el file to Emacs load-path"


# ui - Start command line package manager User Interface
ui:
	$(INVOKE) $(LISP) $(PKG) -f epackage-batch-ui-menu

# update - Update Source List file (available packages)
update:
	$(INVOKE) -f epackage-batch-ui-sources-list-upgrade

# examples - Show command line examples
examples:
	# Multiple package commands:
	@echo $(INVOKE) $(PKG) -f epackage-download-package ...
	@echo $(INVOKE) $(PKG) -f epackage-enable-package ...
	@echo $(INVOKE) $(PKG) -f epackage-disable-package ...
	@echo $(INVOKE) $(PKG) -f epackage-remove-package ...

.PHONY: doc ui examples

# End of file
