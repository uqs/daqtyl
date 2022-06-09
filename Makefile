SCAD=	right.scad left.scad right-plate.scad left-plate.scad right-palm-rest.scad left-palm-rest.scad
SCAD+=	encoder-test.scad trackball-test.scad thumb-test.scad
SCAD+=	all-test.scad left-test.scad right-test.scad
SCAD+=	usb_holder.scad
STL=	${SCAD:.scad=.stl}

.PATH: ${.CURDIR}/things ${.CURDIR}/src
.OBJDIR: things
.SUFFIXES: .stl .scad

.scad.stl:
	openscad --export-format binstl -o ${.TARGET} ${.IMPSRC}

all: ${STL}
