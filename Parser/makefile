parser: parser.y lexer.l
		flex lexer.l
		bison -d parser.y
		gcc parser.tab.c symtab.c stack.c lex.yy.c -o parser

.PHONY: clean
clean:
	rm -f $(ODIR)/*.o parser res.txt lex.yy.c parser.tab.h parser.tab.c results/*

run:
	./parser < ./../"Test Codes"/array_matrix.c > results/array_matrix.txt
	./parser < ./../"Test Codes"/array.c > results/array.txt
	./parser < ./../"Test Codes"/expressions.c > results/expressions.txt
	./parser < ./../"Test Codes"/function.c > results/function.txt
	./parser < ./../"Test Codes"/if_loops.c > results/if_loops.txt
	./parser < ./../"Test Codes"/matrix.c > results/matrix.txt
	./parser < ./../"Test Codes"/nested_expressions.c > results/nested_expressions.txt
	./parser < ./../"Test Codes"/nested_functions.c > results/nested_functions.txt
	./parser < ./../"Test Codes"/nested_if.c > results/nested_if.txt
	./parser < ./../"Test Codes"/nested_loops.c > results/nested_loops.txt
	./parser < ./../"Test Codes"/pointer_func.c > results/pointer_func.txt
	./parser < ./../"Test Codes"/pointers_array.c > results/pointers_array.txt
	./parser < ./../"Test Codes"/pointers.c > results/pointers.txt
	./parser < ./../"Test Codes"/recursion.c > results/recursion.txt

test: parser.y lexer.l
	rm -f $(ODIR)/*.o parser res.txt lex.yy.c parser.tab.h parser.tab.c
	flex lexer.l
	bison -d parser.y
	gcc parser.tab.c symtab.c stack.c lex.yy.c -o parser
	./parser < ./../"Test Codes"/test.c > res.txt