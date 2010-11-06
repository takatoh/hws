.PHONY: build clean

build: hws.exe

hws.exe: hws.hs
	ghc -o hws.exe --make hws.hs


clean:
	del *.hi *.o *.exe

