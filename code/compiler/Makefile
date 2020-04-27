#!/bin/bash
# Before executing set env to your installation of fsharp
fsharp_dir="${FSHARP_DIR}"

buildInteractive:
	mkdir -p src/generated
	@echo ""
	@echo " -- PARSER: --"
	@echo ""
	fsyacc --module Parser src/Parser.fsy -o $(shell pwd)/src/generated/Parser.fs
	@echo ""
	@echo " -- LEXER: --"
	@echo ""
	fslex --unicode src/LexerInteractive.fsl -o $(shell pwd)/src/generated/LexerInteractive.fs
	@echo ""

run:
	make buildInteractive
	@echo ""
	@echo " -- STARTING INTERACTIVE --"
	@echo ""
	fsharpi -r ~/$(fsharp_dir)/FsLexYacc.Runtime.dll --consolecolors --lib:$(shell pwd)/src/generated/ \
		src/AbstractSyntax.fs src/Transpiler.fs Parser.fs LexerInteractive.fs src/Interpreter.fs src/Test.fs src/Compiler.fs src/Program.fs

setup:
	# This setup is based on this guide: https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858#5-link-the-runtime-dll-to-your-fsharp-folder
	@echo 'Setting up....'
	@echo -e 'Note, you must have mono installed (not using homebrew): http://www.mono-project.com/download/#download-mac\n'

	# Downloading nuget.exe to ~/fsharp
	mkdir ~/$(fsharp_dir)
	export FSHARP_DIR=$(fsharp_dir)
	sudo curl -o ~/$(fsharp_dir)/nuget.exe https://dist.nuget.org/win-x86-commandline/latest/nuget.exe

	# Installing FsLexYacc in $fsharp_dir
	cd ~/$(fsharp_dir)
	mono nuget.exe install FsLexYacc

	# Setting up fslex and fyacc as executable commands
	cd /usr/local/bin
	@echo -e '#!/bin/bash\nmono /Users/'${USER}'/$(fsharp_dir)/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe $*' > fslex
	@echo -e '#!/bin/bash\nmono /Users/'${USER}'/$(fsharp_dir)/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe $*' > fsyacc
	chmod a+x fslex fsyacc

	# Linking the Runtime to your ~/fsharp folder, so you can reference it with 'fsharpi -r $(fsharp_dir)FsLexYacc.Runtime.dll Absyn.fs...'
	ln -s ~/$(fsharp_dir)/FsLexYacc.10.0.0/build/fsyacc/net46/FsLexYacc.Runtime.dll ~/$(fsharp_dir)

	@echo -e "\n... ok, hope I'm done"
	@echo "Now you can reference the Runtime like: 'fsharpi -r ~/$(fsharp_dir)/FsLexYacc.Runtime.dll Absyn.fs...'"
	@echo "... happy coding, bye"

test:
	make test/eval
	make test/reduce

test/eval:
	bin/test.sh -i

test/reduce:
	bin/test.sh -r

LexerGen=bin/Lexer.fs
ParserGen=bin/Parser.fs
AbstractSyntaxLib=bin/AbstractSyntax.dll
ParserLib=bin/Parser.dll
LexerLib=bin/Lexer.dll
InterpreterLib=bin/Interpreter.dll
CompilerLib=bin/Compiler.dll
fsharpc=fsharpc -r ~/$(fsharp_dir)/FsLexYacc.Runtime.dll
mkbundle=mkbundle --sdk /Library/Frameworks/Mono.framework/Versions/Current
JaLiExe=bin/JaLi.exe

clean:
	rm -f bin/*.dll bin/*.fs bin/Parser.fsyacc.output bin/Parser.fsi bin/JaLi.exe bin/jali

all: bin/jali

bin/jali: $(JaLiExe)
	$(mkbundle) -L bin/ -L lib/ -L ~/$(fsharp_dir) bin/JaLi.exe ~/$(fsharp_dir)/FsLexYacc.Runtime.dll bin/FSharp.Core.dll bin/AbstractSyntax.dll bin/Parser.dll bin/Lexer.dll bin/Interpreter.dll bin/Compiler.dll -o bin/jali

$(LexerGen): src/Lexer.fsl
	fslex src/Lexer.fsl -o $(LexerGen)

$(ParserGen): src/Parser.fsy
	fsyacc -v --module Parser src/Parser.fsy -o $(ParserGen)

$(AbstractSyntaxLib): src/AbstractSyntax.fs
	$(fsharpc) -a src/AbstractSyntax.fs -o $(AbstractSyntaxLib)

$(ParserLib): $(ParserGen) $(AbstractSyntaxLib)
	$(fsharpc) -a $(ParserGen) -r $(AbstractSyntaxLib) -o $(ParserLib)

$(LexerLib): $(LexerGen) $(AbstractSyntaxLib) $(ParserLib)
	$(fsharpc) -a $(LexerGen) -r $(AbstractSyntaxLib) -r $(ParserLib) -o $(LexerLib)

$(InterpreterLib): src/Interpreter.fs $(AbstractSyntaxLib)
	$(fsharpc) -a src/Interpreter.fs -r $(AbstractSyntaxLib) -o $(InterpreterLib)

$(CompilerLib): src/Compiler.fs $(AbstractSyntaxLib) $(InterpreterLib)
	$(fsharpc) -a src/Compiler.fs -r $(AbstractSyntaxLib) -r $(InterpreterLib) -o $(CompilerLib)

$(JaLiExe): src/JaLi.fsx $(AbstractSyntaxLib) $(ParserLib) $(LexerLib) $(CompilerLib) $(InterpreterLib)
	$(fsharpc) src/JaLi.fsx -o $(JaLiExe) -r $(AbstractSyntaxLib) -r $(ParserLib) -r $(LexerLib) -r $(CompilerLib) -r $(InterpreterLib) -o $(JaLiExe)
