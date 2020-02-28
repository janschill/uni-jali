#!/bin/bash

build: 
	@echo ""
	@echo " -- PARSER: --"
	@echo ""
	fsyacc --module Parser Parser.fsy
	@echo ""
	@echo " -- LEXER: --"
	@echo ""
	fslex --unicode Lexer.fsl
	@echo ""

run: 
	make build
	@echo ""
	@echo " -- STARTING INTERACTIVE --"
	@echo ""
	fsharpi -r ~/fsharp/FsLexYacc.Runtime.dll AbstractSyntax.fs Parser.fs Lexer.fs Program.fs

setup:
	# This setup is based on this guide: https://gist.github.com/AndreasHassing/16567f299b77b0090d94441115a5d031/ae1db7572fd877df733213120800084fbafe9858#5-link-the-runtime-dll-to-your-fsharp-folder
	@echo 'Setting up....'
	@echo -e 'Note, you must have mono installed (not using homebrew): http://www.mono-project.com/download/#download-mac\n'

	# Downloading nuget.exe to ~/fsharp
	mkdir ~/.fsharp
	sudo curl -o ~/fsharp/nuget.exe https://dist.nuget.org/win-x86-commandline/latest/nuget.exe

	# Installing FsLexYacc in ~/fsharp
	cd ~/fsharp
	mono nuget.exe install FsLexYacc

	# Setting up fslex and fyacc as executable commands
	cd /usr/local/bin
	@echo -e '#!/bin/bash\nmono /Users/'${USER}'/fsharp/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe $*' > fslex
	@echo -e '#!/bin/bash\nmono /Users/'${USER}'/fsharp/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe $*' > fsyacc
	chmod a+x fslex fsyacc

	# Linking the Runtime to your ~/fsharp folder, so you can reference it with 'fsharpi -r ~/fsharp/FsLexYacc.Runtime.dll Absyn.fs...'
	ln -s ~/fsharp/FsLexYacc.10.0.0/build/fsyacc/net46/FsLexYacc.Runtime.dll ~/fsharp

	@echo -e "\n... ok, hope i'm done"
	@echo "Now you can reference the Runtime like: 'fsharpi -r ~/fsharp/FsLexYacc.Runtime.dll Absyn.fs...'"
	@echo "... happy coding, bye"


