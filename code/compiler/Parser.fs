// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

  open AbstractSyntax;

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | LPAR
  | RPAR
  | IF
  | ELSEIF
  | ELSE
  | EQL
  | NOT
  | EOF
  | ASSIGN
  | LET
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | NAME of (string)
  | CONSTBOOL of (bool)
  | CONSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_IF
    | TOKEN_ELSEIF
    | TOKEN_ELSE
    | TOKEN_EQL
    | TOKEN_NOT
    | TOKEN_EOF
    | TOKEN_ASSIGN
    | TOKEN_LET
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_NAME
    | TOKEN_CONSTBOOL
    | TOKEN_CONSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | LPAR  -> 0 
  | RPAR  -> 1 
  | IF  -> 2 
  | ELSEIF  -> 3 
  | ELSE  -> 4 
  | EQL  -> 5 
  | NOT  -> 6 
  | EOF  -> 7 
  | ASSIGN  -> 8 
  | LET  -> 9 
  | PLUS  -> 10 
  | MINUS  -> 11 
  | TIMES  -> 12 
  | DIV  -> 13 
  | MOD  -> 14 
  | NAME _ -> 15 
  | CONSTBOOL _ -> 16 
  | CONSTINT _ -> 17 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_LPAR 
  | 1 -> TOKEN_RPAR 
  | 2 -> TOKEN_IF 
  | 3 -> TOKEN_ELSEIF 
  | 4 -> TOKEN_ELSE 
  | 5 -> TOKEN_EQL 
  | 6 -> TOKEN_NOT 
  | 7 -> TOKEN_EOF 
  | 8 -> TOKEN_ASSIGN 
  | 9 -> TOKEN_LET 
  | 10 -> TOKEN_PLUS 
  | 11 -> TOKEN_MINUS 
  | 12 -> TOKEN_TIMES 
  | 13 -> TOKEN_DIV 
  | 14 -> TOKEN_MOD 
  | 15 -> TOKEN_NAME 
  | 16 -> TOKEN_CONSTBOOL 
  | 17 -> TOKEN_CONSTINT 
  | 20 -> TOKEN_end_of_input
  | 18 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_Expr 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_AtExpr 
    | 9 -> NONTERM_AtExpr 
    | 10 -> NONTERM_AtExpr 
    | 11 -> NONTERM_Const 
    | 12 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 20 
let _fsyacc_tagOfErrorTerminal = 18

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | IF  -> "IF" 
  | ELSEIF  -> "ELSEIF" 
  | ELSE  -> "ELSE" 
  | EQL  -> "EQL" 
  | NOT  -> "NOT" 
  | EOF  -> "EOF" 
  | ASSIGN  -> "ASSIGN" 
  | LET  -> "LET" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | NAME _ -> "NAME" 
  | CONSTBOOL _ -> "CONSTBOOL" 
  | CONSTINT _ -> "CONSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | ELSEIF  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | EQL  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CONSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CONSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 8us; 65535us; 0us; 2us; 12us; 5us; 13us; 6us; 14us; 7us; 15us; 8us; 16us; 9us; 20us; 10us; 21us; 11us; 8us; 65535us; 0us; 4us; 12us; 4us; 13us; 4us; 14us; 4us; 15us; 4us; 16us; 4us; 20us; 4us; 21us; 4us; 8us; 65535us; 0us; 17us; 12us; 17us; 13us; 17us; 14us; 17us; 15us; 17us; 16us; 17us; 20us; 17us; 21us; 17us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 12us; 21us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 6us; 1us; 3us; 4us; 5us; 6us; 7us; 1us; 1us; 1us; 2us; 6us; 3us; 3us; 4us; 5us; 6us; 7us; 6us; 3us; 4us; 4us; 5us; 6us; 7us; 6us; 3us; 4us; 5us; 5us; 6us; 7us; 6us; 3us; 4us; 5us; 6us; 6us; 7us; 6us; 3us; 4us; 5us; 6us; 7us; 7us; 6us; 3us; 4us; 5us; 6us; 7us; 9us; 6us; 3us; 4us; 5us; 6us; 7us; 10us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 12us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 11us; 13us; 15us; 22us; 29us; 36us; 43us; 50us; 57us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 78us; 80us; 82us; 84us; 86us; 88us; |]
let _fsyacc_action_rows = 25
let _fsyacc_actionTableElements = [|4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 0us; 49152us; 6us; 32768us; 7us; 3us; 10us; 12us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 0us; 16385us; 0us; 16386us; 3us; 16387us; 12us; 14us; 13us; 15us; 14us; 16us; 3us; 16388us; 12us; 14us; 13us; 15us; 14us; 16us; 0us; 16389us; 0us; 16390us; 0us; 16391us; 5us; 16393us; 10us; 12us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 6us; 32768us; 1us; 22us; 10us; 12us; 11us; 13us; 12us; 14us; 13us; 15us; 14us; 16us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 0us; 16392us; 1us; 32768us; 15us; 19us; 1us; 32768us; 8us; 20us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 4us; 32768us; 0us; 21us; 9us; 18us; 16us; 24us; 17us; 23us; 0us; 16394us; 0us; 16395us; 0us; 16396us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 13us; 14us; 15us; 19us; 23us; 24us; 25us; 26us; 32us; 39us; 44us; 49us; 54us; 59us; 64us; 65us; 67us; 69us; 74us; 79us; 80us; 81us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 3us; 3us; 3us; 3us; 1us; 4us; 3us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16392us; 65535us; 65535us; 65535us; 65535us; 16394us; 16395us; 16396us; |]
let _fsyacc_reductions ()  =    [| 
# 183 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 192 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                    _1 
                   )
# 30 "Parser.fsy"
                 : AbstractSyntax.expr));
# 203 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                           _1                 
                   )
# 34 "Parser.fsy"
                 : AbstractSyntax.expr));
# 214 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                           Prim("+", _1, _3)  
                   )
# 35 "Parser.fsy"
                 : AbstractSyntax.expr));
# 226 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                                           Prim("-", _1, _3)  
                   )
# 36 "Parser.fsy"
                 : AbstractSyntax.expr));
# 238 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                           Prim("*", _1, _3)  
                   )
# 37 "Parser.fsy"
                 : AbstractSyntax.expr));
# 250 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                           Prim("/", _1, _3)  
                   )
# 38 "Parser.fsy"
                 : AbstractSyntax.expr));
# 262 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                           Prim("%", _1, _3)  
                   )
# 39 "Parser.fsy"
                 : AbstractSyntax.expr));
# 274 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Const)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                _1          
                   )
# 43 "Parser.fsy"
                 : AbstractSyntax.expr));
# 285 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                Let(_2, _4) 
                   )
# 44 "Parser.fsy"
                 : AbstractSyntax.expr));
# 297 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractSyntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                _2          
                   )
# 45 "Parser.fsy"
                 : AbstractSyntax.expr));
# 308 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                           ConstantInteger _1 
                   )
# 48 "Parser.fsy"
                 : 'Const));
# 319 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                           ConstantBoolean _1 
                   )
# 49 "Parser.fsy"
                 : 'Const));
|]
# 331 "Parser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 21;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : AbstractSyntax.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
