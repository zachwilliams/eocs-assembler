all: html pdf
	@echo Done

html:
	scribble --htmls ++style quiet.css assembler.scrbl

pdf:
	scribble --pdf assembler.scrbl
	mv assembler.pdf eocs-assembler.pdf

