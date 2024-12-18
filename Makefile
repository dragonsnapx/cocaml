all:
	dune test & llc test/test.ll & clang test/test.s -o test/test_ex

run:
	./test/test_ex

clean:
	rm test/test.ll test/test.s test/test_ex