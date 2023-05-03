namespace DaiBakaBot

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
    
    type dir =
        |RIGHT
        |DOWN
    
    type anchor = coord * dir
    
    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        amountOfPlayers: uint32
        hand          : MultiSet.MultiSet<uint32>
        turnNumber    : uint32
        anchorpoints  : Set<anchor>
    }

    let mkState b d pn aop h tn= {board = b;
                                  dict = d;
                                  playerNumber = pn;
                                  amountOfPlayers = aop;
                                  hand = h;
                                  turnNumber = tn
                                  anchorpoints = Set.empty}
    

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let amountOfPlayers st = st.amountOfPlayers
    let hand st          = st.hand
    let turnNumber st = st.turnNumber
    
    let nextCoord (x, y) dir =
        match dir with
            |RIGHT -> (x+1,y)
            |DOWN -> (x, y+1)
            
    let prevCoord (x,y) dir =
        match dir with
            |RIGHT -> (x-1,y)
            |DOWN -> (x, y-1)
    
    let updateAnchorPoints st =
        let rec back coord set dir =
            function
            | 0u -> set
            | n -> back (prevCoord coord dir) (Set.add (coord,dir) set) dir (n-1u)
        
        let tempSet = Map.fold (fun acc key _ -> back key acc DOWN (MultiSet.size st.hand)) Set.empty st.board.placedTiles
        Map.fold (fun acc key _ -> back key acc RIGHT (MultiSet.size st.hand)) tempSet st.board.placedTiles
        
        
       
    
    let updateBoard addedTiles (st:state) =
        let updated = List.fold (fun newBoard newTile -> Map.add (fst newTile) (snd newTile) newBoard) st.board.placedTiles addedTiles
        {st with board = {st.board with placedTiles = updated}}
        
    let updateHand placedTiles receivedTiles (st:state)=
        let reduced = List.fold (fun newHand tile -> MultiSet.removeSingle (fst (snd tile)) newHand) st.hand placedTiles
        let updated = List.fold (fun newHand tile ->MultiSet.add (fst tile) (snd tile) newHand) reduced receivedTiles
        {st with hand = updated}
    
    let updateState st playedTiles receivedTiles newTurnNumber =
        {st with turnNumber = newTurnNumber} |> updateHand playedTiles receivedTiles |> updateBoard playedTiles

module Scrabble =
    open System.Threading
    open State
        
        
    let checkIfCoordNotSurrounded coord dir tilesOnBoard =
        match dir with
                |RIGHT -> match Map.tryFind (prevCoord coord DOWN) tilesOnBoard with
                          |None -> match Map.tryFind (nextCoord coord DOWN) tilesOnBoard with
                                   |None -> true
                                   |Some (_,(_,_)) -> false
                          |Some (_,(_,_)) -> false
                |DOWN -> match Map.tryFind (prevCoord coord RIGHT) tilesOnBoard with
                          |None -> match Map.tryFind (nextCoord coord RIGHT) tilesOnBoard with
                                   |None -> true
                                   |Some (_,(_,_)) -> false
                          |Some (_,(_,_)) -> false
    
    let tileOnNextCoord coord dir tilesOnBoard =
        match Map.tryFind (nextCoord coord dir ) tilesOnBoard with
        |None -> false
        |Some (_,(_,_)) -> true
    
    let bestWord l1 l2 = if List.length l1 > List.length l2 then l1 else l2
  
    let mkWordFromCoord startCoord dir startDict startHand tiles tilesOnBoard =
        let rec aux longestWord acc coord dict hand =
            match Map.tryFind coord tilesOnBoard with
            |None ->
                if checkIfCoordNotSurrounded coord dir tilesOnBoard then 
                    MultiSet.fold (fun best key _ ->
                            let handTile = Map.find key tiles |> Set.toList |> List.item 0 |> fst
                            let pointValue = Map.find key tiles |> Set.toList |> List.item 0 |> snd
                            let tilePlacement = (coord,(key,(handTile,pointValue)))
                            let newHand = MultiSet.removeSingle key hand
                            match Dictionary.step handTile dict with
                            |None -> best
                            |Some (b,d) ->
                                let currentWord = tilePlacement :: acc
                                let newBest = if b && not (tileOnNextCoord coord dir tilesOnBoard)
                                              then bestWord best currentWord
                                              else best
                                aux newBest currentWord (nextCoord coord dir) d newHand
                            ) longestWord hand
                else
                    longestWord
            |Some (_,(cv, _)) ->
                match Map.tryFind (prevCoord coord dir) tilesOnBoard with
                |None ->
                    match Dictionary.step cv dict with
                    |None -> longestWord
                    |Some (_,d) -> aux longestWord acc (nextCoord coord dir) d hand
                |Some (_,(_,_)) -> longestWord
        
        aux [] [] startCoord startDict startHand
         
    
    let playGame cstream pieces (st : state) =

        let rec aux (st : state) = 
            if (st.turnNumber % st.amountOfPlayers) + 1u = st.playerNumber then 
                Print.printHand pieces (hand st)

                let move =
                    if Map.isEmpty st.board.placedTiles
                    then
                        mkWordFromCoord (0,0) RIGHT st.dict st.hand pieces st.board.placedTiles
                    else
                        updateAnchorPoints st |>
                        Set.fold (fun acc (coord,dir) -> mkWordFromCoord coord dir st.dict st.hand pieces st.board.placedTiles |> bestWord acc) []
                        
                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move) //

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(placedTiles, points, newPieces)) ->
                (* Successful play by you. Update your state (removeSingle old tiles, add the new ones, change turn, etc) *)
                let st' = updateState st placedTiles newPieces (st.turnNumber + 1u)
                
                aux st'
            | RCM (CMPlayed (pid, placedTiles, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = {st with turnNumber = st.turnNumber + 1u} |> updateBoard placedTiles
                aux st'
            | RCM (CMPlayFailed (pid, attemptedMove)) ->
                (* Failed play. Update your state *)
                let st' = {st with turnNumber = st.turnNumber + 1u}
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers handSet 0u)
        