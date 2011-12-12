#!/usr/bin/env bash

default: tc

tc : TackParser.hs TackLexer.hs Token.hs PrettyPrint.hs AST.hs SrcPos.hs TackPPrinter.hs Scope.hs SymbolTable.hs SymbolTypes.hs  GetTackType.hs IR.hs IRTranslator.hs ASM.hs CodeGen.hs
	ghc --make  -o tc Main.hs

TackLexer.hs: TackLexer.x 
	alex TackLexer.x

TackParser.hs : TackParser.y
	happy -a TackParser.y


clean:
	rm -f tackparser.hs tacklexer.hs
	rm -f *.hi
	rm -f *.o
	rm -f tc
	rm result/*

trans   : tc
	./tc < test/001.tack > result/001.s
	./tc < test/002.tack > result/002.s
	./tc < test/003.tack > result/003.s
	./tc < test/004.tack > result/004.s
	./tc < test/005.tack > result/005.s
	./tc < test/006.tack > result/006.s
	./tc < test/007.tack > result/007.s
	./tc < test/008.tack > result/008.s
	./tc < test/009.tack > result/009.s
	./tc < test/010.tack > result/010.s
	./tc < test/011.tack > result/011.s
	./tc < test/018.tack > result/018.s
	./tc < test/019.tack > result/019.s
	./tc < test/020.tack > result/020.s
	./tc < test/022.tack > result/022.s


run:
	gcc -m64 -masm=intel -o result/001.exe result/001.s x64runtime.c
	result/001.exe
	gcc -m64 -masm=intel -o result/002.exe result/002.s x64runtime.c
	result/002.exe
	gcc -m64 -masm=intel -o result/003.exe result/003.s x64runtime.c
	result/003.exe
	gcc -m64 -masm=intel -o result/004.exe result/004.s x64runtime.c
	result/004.exe
	gcc -m64 -masm=intel -o result/005.exe result/005.s x64runtime.c
	result/005.exe
	gcc -m64 -masm=intel -o result/006.exe result/006.s x64runtime.c
	result/006.exe
	gcc -m64 -masm=intel -o result/007.exe result/007.s x64runtime.c
	result/007.exe
	gcc -m64 -masm=intel -o result/008.exe result/008.s x64runtime.c
	result/008.exe
	gcc -m64 -masm=intel -o result/009.exe result/009.s x64runtime.c
	result/009.exe
	gcc -m64 -masm=intel -o result/010.exe result/010.s x64runtime.c
	result/010.exe
	gcc -m64 -masm=intel -o result/011.exe result/011.s x64runtime.c
	result/011.exe
	gcc -m64 -masm=intel -o result/018.exe result/018.s x64runtime.c
	result/018.exe
	gcc -m64 -masm=intel -o result/019.exe result/019.s x64runtime.c
	result/019.exe
	gcc -m64 -masm=intel -o result/020.exe result/020.s x64runtime.c
	result/020.exe
	gcc -m64 -masm=intel -o result/022.exe result/022.s x64runtime.c
	result/022.exe







