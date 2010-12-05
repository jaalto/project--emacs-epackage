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
SRC		= ../$(PACKAGE).el
TXT		= index.txt
HTML		= index.html
DOCDIR		= doc
LICENSE		= disclaimer.txt

all: doc

$(TXT): $(SRC)
	ripdoc $(SRC) > $(TXT)

$(HTML): $(TXT) $(LICENSE)
	t2html --author "Jari Aalto" \
	--title "Epackage - Distributed Emacs Lisp Package System (DELPS)" \
	--css-code-bg \
	--css-file=index.css \
	--disclaimer-file=$(LICENSE) \
	--out \
	$(TXT)

doc: $(HTML)

# End of file
