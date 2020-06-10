run: interpreter

interpreter: clean
	ocamlopt -o interpreter.exe interpreter.ml
	chmod +x interpreter.exe

clean:
	rm -f *.o *.cmx *.cmi *.exe *.cl-type

test:
	@echo "testing file: " $(file)
	cool --type $(file) 
	./interpreter.exe $(file)-type
