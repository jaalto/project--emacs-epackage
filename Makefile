#   Copyright
#
#	Copyright (C) 2010-2011 Jari Aalto <jari.aalto@cante.net>
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
#
#    Depends
#
#	http://freshmeat.net/projects/perl-text2html
#	http://freshmeat.net/projects/perl-ripdoc

PACKAGE		= epackage
SRC		= $(PACKAGE).el
BIN		= emacs

all: help

help:
	@echo "Select make <target>:"
	@echo "---------------------"
	@grep '^# .*-' Makefile | sed 's,# ,,' | sort

$(PACKAGE).elc: $(SRC)
	$(BIN) --batch -Q -q -f batch-byte-compile $(SRC)

# build - Byte compile *.el file
build: $(PACKAGE).elc

# doc - Build plain text and HTML documentation from *.el with external utilities
doc:
	$(MAKE) -C doc

# install - There is no install. Manually copy *.el file to Emacs load-path
install:

# ui - Start command line package manager User Interface
ui:
	emacs -Q -nw --batch -l $(SRC) -f epackage-batch-ui-menu

.PHONY: doc ui

# End of file
