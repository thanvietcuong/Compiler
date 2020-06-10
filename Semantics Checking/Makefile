CFLAGS=-g
export CFLAGS

all: checker-pa4.exe

checker-pa4.exe: clean
	rm -f checker-pa4.mli checker-pa4.exe checker-pa4.cmi checker-pa4.cmx
	ocamlopt -o checker-pa4.exe checker-pa4.ml

clean:
	rm -f checker-pa4.mli checker-pa4.exe checker-pa4.cmi checker-pa4.cmx
	rm -f *.cl-*

test:
	@echo "testing file: " $(file)
	cool --parse $(file)
	./checker-pa4.exe $(file)-ast

cool:
	@echo "cool type: " $(file)
	cool --parse $(file)
	cool --type $(file)-ast --out ref 

cool-class-map:
	@echo "cool class map: " $(file)
	cool --parse $(file)
	cool --class-map $(file)-ast --out ref 

cool-imp-map:
	@echo "cool imp map: " $(file)
	cool --parse $(file)
	cool --imp-map $(file)-ast --out ref 

diff:
	@echo "comparing results for: " $(file)
	cool --parse $(file)
	cool --class-map $(file)-ast --out ref
	./checker-pa4.exe $(file)-ast
	diff -b -B -w ref.cl-type $(file)-type


diff-all:
	for file in *.cl ; do \
		echo "---------------------------" ;\
		f="$$(basename -- $$file)";\
		echo "comparing results for: $$f" ;\
		./cool --parse $$f ;\
		./cool --type $$f-ast --out ref ;\
		./checker-pa4.exe $$f-ast ;\
		diff -b -B -w ref.cl-type $$f-type ;\
	done \
