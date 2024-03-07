#/usr/bin/env bash
port=8000
build_dir=../../build

which emcc
ret=$?
if [ $ret -ne 0  ]; then
  echo "emcc compiler could not be found.\nInstall Emscripten or source emsdk_set_env.sh file. Exiting."
  exit $ret
fi

for f in hello_world fib; do
  echo -n "Compiling $f.rb to $f.wasm..."
  ../../bin/mrbc -Btest_symbol $f.rb
  emcc -D CFILE="<$f.c>" -I../../include -I. -o $f.bc stub.c
  emcc -o $f.html $f.bc ../../build/emcc-wasm/lib/libmruby.bc
  echo "done"
done

echo ""
echo "Launch a minimal Web server:"
echo "ruby -run -e httpd . -p $port"
echo ""
echo "and navigate to:"
for f in hello_world fib; do
 echo "http://localhost:8000/$f.html"
done
