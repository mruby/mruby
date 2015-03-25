gen: gen.cpp
	clang++ -std=c++11 -g3 -O0 gen.cpp $(shell llvm-config --cppflags --cxxflags --libs --ldflags --cflags) -pthread -lcurses -ldl -o gen -lz
