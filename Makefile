SCAD=	right.scad left.scad right-plate.scad left-plate.scad right-palm-rest.scad left-palm-rest.scad encoder-test.scad trackball-test.scad
STL=	${SCAD:.scad=.stl}

.PATH: things
.OBJDIR: things
.SUFFIXES: .stl .scad

.scad.stl:
	openscad --export-format binstl -o ${.TARGET} ${.IMPSRC}

all: ${STL}
