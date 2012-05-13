all:: vih

SRC=Buffer.hs PSvi.hs Terminal.hs TestBufferManager.hs TestTerminal.hs \
    BufferManager.hs ReadHighlighting.hs TerminalInterfaces.hs \
    TestHighlighting.hs Peg/Peg.hs Peg/PegParser.hs

graphics:
	ghc --make TestTerminal.hs

buffers:
	ghc --make BufferManager.hs

testBuffers:
	ghc --make TestBufferManager.hs -o .ptestBuffer

vih: $(SRC)
	ghc -O --make PSvi.hs -o vih

clean:
	rm -f ptest* vih
	rm -f *.hi *.o
	rm -f ./Peg/*.hi ./Peg/*.o

install: all
	mkdir -p ~/.vih/ ~/bin
	cp run.ps init.ps *.peg *.attr ~/.vih/
	rm -f ~/bin/vih
	ln vih ~/bin

.PHONY: install
