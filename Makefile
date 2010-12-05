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

all: build doc

build:
	$${EMACS_BIN:-emacs} --batch -q -f byte-compile $(SRC)

doc:
	$(MAKE) -C doc

# End of file
