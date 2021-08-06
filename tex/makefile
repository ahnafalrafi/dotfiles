DOC=document.tex

all:
	@latexmk -quiet -bibtex -f -pdf -use-make $(DOC)

.PHONY: clean
# -bibtex also removes the .bbl files (http://tex.stackexchange.com/a/83384/79184).
clean:
	latexmk -c -bibtex

.PHONY: watch
watch:
	@while true;          \
	  do                  \
	  inotifywait $(DOC); \
	  sleep 0.01;         \
	  make all;           \
	  done
