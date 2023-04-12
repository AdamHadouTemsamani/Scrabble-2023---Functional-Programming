// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open System
    open StateMonad

    let add a b = a >>= fun x -> b >>= fun y -> ret (x + y)
    let sub a b = a >>= fun x -> b >>= fun y -> ret (x - y)
    let mul a b = a >>= fun x -> b >>= fun y -> ret (x * y)
    let div a b = a >>= fun x -> b >>= fun y -> if y = 0 then fail DivisionByZero else ret (x / y)
    let modu a b = a >>= fun x -> b >>= fun y -> if y = 0 then fail DivisionByZero else ret (x % y)
    
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b = a >>= fun x -> b >>= fun y -> ret (f x y)
    let isVowel c = List.contains (Char.ToUpper c) ['A';'E';'I';'O';'U';'Æ';'Ø';'Å']
    let rec arithEval a : SM<int> =
        match a with
         |N n -> ret n
         |V v -> lookup v
         |WL -> wordLength
         |PV exp -> arithEval exp >>= pointValue 
         |Add (a,b) -> add (arithEval a) (arithEval b)
         |Sub (a,b) -> arithEval a |> sub <| arithEval b
         |Mul (a,b) -> arithEval a |> mul <| arithEval b
         |Div (a,b) -> arithEval a |> div <| arithEval b
         |Mod (a,b) -> arithEval a |> modu <| arithEval b
         |CharToInt c -> charEval c >>= fun x -> int x |> ret

    and charEval c : SM<char> =
        match c with
         |C c -> ret c
         |ToUpper exp -> charEval exp >>= fun x -> Char.ToUpper x |> ret
         |ToLower exp -> charEval exp >>= fun x -> Char.ToLower x |> ret
         |CV exp -> arithEval exp >>= fun x -> characterValue x
         |IntToChar a -> arithEval a >>= fun x -> char x |> ret

    and boolEval b : SM<bool> =
        match b with
         |TT -> ret true
         |FF -> ret false
         |AEq (expA,expB) -> binop (=) (arithEval expA) (arithEval expB)
         |ALt (expA,expB) -> binop (<) (arithEval expA) (arithEval expB)
         |Not exp -> boolEval exp >>= fun x -> not x |> ret
         |Conj (expA,expB) ->binop (&&) (boolEval expA) (boolEval expB)
         |IsDigit exp -> charEval exp >>= fun x -> Char.IsDigit x |> ret
         |IsLetter exp -> charEval exp >>= fun x -> Char.IsLetter x |> ret
         |IsVowel exp -> charEval exp >>= fun x -> isVowel x |> ret


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        |Declare name -> declare name
        |Ass (name,a) -> arithEval a >>= fun x -> update name x
        |Skip -> ret ()
        |Seq (stA,stB) -> stmntEval stA >>>= stmntEval stB
        |ITE (bEx,stA,stB) -> boolEval bEx >>= fun x -> if x then stmntEval stA else stmntEval stB
        |While (bEx,s) ->  boolEval bEx >>=
            (fun b ->
                if b
                then
                    stmntEval(While (bEx, s))
                else
                    ret ()) 

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"