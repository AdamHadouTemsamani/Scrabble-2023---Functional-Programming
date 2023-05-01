namespace YourClientName

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        //player turn? - uint32
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h } //keep track of player turn here
    //let newMkState som tager højder for kun de nødvendige ting - word, playerId, playerTurn, board, numberofplayer(hvis vi gerne vil tage højde for mere end 2)

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    let updateBoard (st:state) addedTiles =
        let updated = List.fold (fun newBoard newTile -> Map.add (fst newTile) (snd newTile) newBoard) st.board.placedTiles addedTiles
        {st with board = {st.board with placedTiles = updated}}
        
    let updateHand (st:state) placedTiles receivedTiles =
        let reduced = List.fold (fun newHand tile -> MultiSet.remove (fst (snd tile)) 1u newHand) st.hand placedTiles
        let updated = List.fold (fun newHand tile ->MultiSet.add (fst tile) (snd tile) newHand) reduced receivedTiles
        {st with hand = updated}
    

module Scrabble =
    open System.Threading
    
    type dir =
        |RIGHT
        |DOWN
        
    let nextCoord (x, y) dir =
        match dir with
            |RIGHT -> (x+1,y)
            |DOWN -> (x, y+1)
            
    let prevCoord (x,y) dir =
        match dir with
            |RIGHT -> (x-1,y)
            |DOWN -> (x, y-1)
    
    let bestWord l1 l2 = if List.length l1 > List.length l2 then l1 else l2
  
    let mkWordFromCoord startChoord dir startDict startHand tiles tilesOnBoard =
        let rec aux longestWord acc coord dict hand =
            match Map.tryFind coord tilesOnBoard with
            |None ->
                MultiSet.fold (fun best key _ ->
                        let handTile = Map.find key tiles |> Set.toList |> List.item 0 |> fst
                        let pointValue = Map.find key tiles |> Set.toList |> List.item 0 |> snd
                        let tilePlacement = (coord,(key,(handTile,pointValue)))
                        let newHand = MultiSet.remove key 1u hand
                        match Dictionary.step handTile dict with
                        |None -> best
                        |Some (b,d) ->
                            let currentWord = tilePlacement :: acc
                            let newBest = if b then bestWord best currentWord else best
                            aux newBest currentWord (nextCoord coord dir) d newHand
                        ) longestWord hand 
            |Some (_,(cv, _)) ->
                match Map.tryFind (prevCoord coord dir) tilesOnBoard with
                |None ->
                    match Dictionary.step cv dict with
                    |None -> longestWord
                    |Some (_,d) -> aux longestWord acc (nextCoord coord dir) d hand
                |Some (_,(_,_)) -> longestWord
        aux [] [] startChoord startDict startHand
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) = //shouldPlay bool inkluderet her? - if statement om det hele? hvis det er spiller tur, ellers så recv stream?
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            let move =
                if Map.isEmpty st.board.placedTiles
                then
                    let moveRight = mkWordFromCoord (0,0) RIGHT st.dict st.hand pieces st.board.placedTiles 
                    let moveDown = mkWordFromCoord (0,0) DOWN st.dict st.hand pieces st.board.placedTiles 
                    bestWord moveRight moveDown
                else
                    let moveRight = Map.fold (fun acc key _ -> mkWordFromCoord key RIGHT st.dict st.hand pieces st.board.placedTiles |> bestWord acc) [] st.board.placedTiles
                    let moveDown = Map.fold (fun acc key _ -> mkWordFromCoord key DOWN st.dict st.hand pieces st.board.placedTiles |> bestWord acc) [] st.board.placedTiles
                    bestWord moveRight moveDown
                    
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move) //

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(placedTiles, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.updateBoard st placedTiles// This state needs to be updated
                let st' = State.updateHand st' placedTiles newPieces
                //let st' = mkState alle parameter (nyt board)
                aux st'
            | RCM (CMPlayed (pid, placedTiles, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.updateBoard st placedTiles // This state needs to be updated
                //let st' = mkState alle parameter (nyt board)
                aux st'
            | RCM (CMPlayFailed (pid, attemptedMove)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st' 
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        