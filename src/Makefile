all:: main

graphics:
	ghc --make TestTerminal.hs

buffers:
	ghc --make BufferManager.hs

testBuffers:
	ghc --make TestBufferManager.hs -o .ptestBuffer

main:
	ghc -O --make PSvi.hs -o vih

clean:
	rm -f ptest* vih
	rm -f *.hi *.o
	rm -f ./Peg/*.hi ./Peg/*.o

install:
	mkdir -p ~/.vih/ ~/bin
	cp run.ps init.ps *.peg *.attr ~/.vih/
	rm -f ~/bin/vih
	ln vih ~/bin
