module Dictionary 
    
    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    
    let empty () = Leaf false
   
    let rec insert (s: string) dict =
       match dict with
        | Leaf _ when (String.length s) = 0 ->
            Leaf true
        | Leaf b ->
            Node(b, Map.add s.[0] (insert s.[1..] (empty())) Map.empty)
        | Node(_,m) when String.length s = 0 ->
            Node(true, m)
        | Node (b,m) ->
            match Map.tryFind s.[0] m with
            | Some childNode ->
                let newNode = insert s.[1..] childNode
                Node(b, Map.add s.[0] newNode m)
            | None ->
                let newNode = insert s.[1..] (empty())
                Node(b, Map.add s.[0] newNode m)
   
    let step (c: char) (dict: Dict) : (bool * Dict) option =
        match dict with
        | Leaf _ -> None
        | Node (_,m) ->
            match Map.tryFind c m with
            | Some childNode ->
                match childNode with
                | Leaf b -> Some (b,childNode)
                | Node (b,_) -> Some (b,childNode)
            | None -> None
      
    let lookup (s: string) (dict: Dict) : bool =
        let innerLookup acc element =
            match acc with
            | Some (_,child) -> step element child
            | None -> None
        let b = List.fold innerLookup (Some (false, dict)) (Seq.toList s)
        match b with
           | Some (b,_) -> b
           | None -> false
