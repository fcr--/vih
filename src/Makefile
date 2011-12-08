all:: main

graphics:
	ghc --make TestTerminal.hs

buffers:
	ghc --make BufferManager.hs

testBuffers:
	ghc --make TestBufferManager.hs -o .ptestBuffer

main:
	ghc --make PSvi.hs -o vih

clean:
	rm -f ptest*
	rm -f *.hi *.o

install:
	mkdir ~/.vih/
	cp *.peg *.attr ~/.vih/
