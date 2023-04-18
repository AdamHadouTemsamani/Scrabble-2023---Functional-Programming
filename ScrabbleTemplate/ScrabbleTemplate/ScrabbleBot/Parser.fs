// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
            
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = asciiLetter 
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric" 

    let spaces         = many whitespaceChar
    let spaces1        = many1 (satisfy System.Char.IsWhiteSpace <?> "space1")

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 

    let pid = pletter <|> pchar '_' .>>. many palphanumeric |>> fun (m,ms)-> m::ms |> System.String.Concat
    
    let unop op a = op >*>. a
    let binop op p1 p2 =  p1 .>*> op .>*>. p2 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let BoolParse, bref = createParserForwardedToRef<bExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let DivParse =  binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    do pref := choice [MulParse;DivParse;ModParse; AtomParse]

    let VParse = pid |>> V <?> "V"
    let PvParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NParse   = pint32 |>> N <?> "Int"
    let NegParse   = unop (pchar '-' ) AtomParse |>> (fun x -> (N -1,x))|>> Mul <?> "Mul"
    let CTIParser   = pCharToInt >*>. parenthesise CharParse|>> CharToInt <?> "CharToInt"
    
    let ParParse = parenthesise TermParse
    do aref := choice [CTIParser;PvParse;NegParse;VParse;NParse;ParParse]

    let AexpParse = TermParse 

    let pcharMid = pchar ''' >>. anyChar .>> pchar '''
    let CParse   = pcharMid |>> C <?> "C"
    let TUParse   =  unop pToUpper CharParse |>> ToUpper <?> "ToUpper"
    let TLParse   = unop pToLower CharParse |>> ToLower <?> "ToLower"
    let ITCParser   = pIntToChar >*>. parenthesise AexpParse  |>> IntToChar <?> "IntToChar"
    let CVParser   = pCharValue >*>. parenthesise AexpParse |>> CV <?> "CV"
    
    let CParParse = parenthesise CharParse
    do cref := choice [ITCParser;CVParser;TLParse;TUParse;CParse;CParParse;CharParse]
    
    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
        placedTiles   : Map<coord, char * uint>
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0)
                                                 defaultSquare = Map.empty
                                                 squares = fun _ -> Success (Some Map.empty)
                                                 placedTiles = Map.empty}
