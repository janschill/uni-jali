echo ""
echo " -- PARSER: --"
echo ""
fsyacc --module Parser Parser.fsy
echo ""
echo " -- LEXER: --"
echo ""
fslex --unicode Lexer.fsl
echo ""

if [ "$1" == "-i" ]
then
    echo ""
    echo " -- STARTING INTERACTIVE --"
    echo ""
    fsharpi -r ~/fsharp/FsLexYacc.Runtime.dll AbstractSyntax.fs Parser.fs Lexer.fs Program.fs
fi
