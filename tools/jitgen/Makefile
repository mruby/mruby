all: gen scale_geps

gen: gen.cpp
	clang++ -std=c++11 -g3 -O0 gen.cpp $(shell llvm-config --cppflags --cxxflags --libs --ldflags --cflags) -pthread -lcurses -ldl -o gen -lz

scale_geps: scale_geps.cpp
	clang++ -std=c++11 -g3 -O0 scale_geps.cpp $(shell llvm-config --cppflags --cxxflags --libs --ldflags --cflags) -pthread -lcurses -ldl -o scale_geps -lz

