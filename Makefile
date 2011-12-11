#!/usr/bin/env bash

default: ir_translator

ir_translator : TackParser.hs TackLexer.hs Token.hs PrettyPrint.hs AST.hs SrcPos.hs TackPPrinter.hs Scope.hs SymbolTable.hs SymbolTypes.hs  GetTackType.hs IR.hs IRTranslator.hs ASM.hs CodeGen.hs
	ghc --make  -optl"-Wl,-read_only_relocs,suppress" -o ir_translator Main.hs

TackLexer.hs: TackLexer.x 
	alex TackLexer.x

TackParser.hs : TackParser.y
	happy -a TackParser.y


clean:
	rm -f tackparser.hs tacklexer.hs
	rm -f *.hi
	rm -f *.o
	rm -f ir_translator
	rm result/*

trans   : ir_translator
	./ir_translator < test/001.tack > result/001.ir
	./ir_translator < test/002.tack > result/002.ir
	./ir_translator < test/003.tack > result/003.ir
	./ir_translator < test/004.tack > result/004.ir
	./ir_translator < test/005.tack > result/005.ir
	./ir_translator < test/006.tack > result/006.ir
	./ir_translator < test/007.tack > result/007.ir
	./ir_translator < test/008.tack > result/008.ir
	./ir_translator < test/009.tack > result/009.ir
	./ir_translator < test/010.tack > result/010.ir
	./ir_translator < test/011.tack > result/011.ir
	./ir_translator < test/018.tack > result/018.ir
	./ir_translator < test/019.tack > result/019.ir
	./ir_translator < test/020.tack > result/020.ir
	./ir_translator < test/022.tack > result/022.ir


run:
	java -ea -jar IRInterpreter.jar result/001.ir
	java -ea -jar IRInterpreter.jar result/002.ir
	java -ea -jar IRInterpreter.jar result/003.ir
	java -ea -jar IRInterpreter.jar result/004.ir
	java -ea -jar IRInterpreter.jar result/005.ir
	java -ea -jar IRInterpreter.jar result/006.ir
	java -ea -jar IRInterpreter.jar result/007.ir
	java -ea -jar IRInterpreter.jar result/008.ir
	java -ea -jar IRInterpreter.jar result/009.ir
	java -ea -jar IRInterpreter.jar result/010.ir
	java -ea -jar IRInterpreter.jar result/011.ir
	java -ea -jar IRInterpreter.jar result/018.ir
	java -ea -jar IRInterpreter.jar result/019.ir
	java -ea -jar IRInterpreter.jar result/020.ir
	java -ea -jar IRInterpreter.jar result/022.ir



