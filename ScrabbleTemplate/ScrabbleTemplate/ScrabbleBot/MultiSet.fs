// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a:comparison> = {contents : Map<'a,uint32>; setSize : uint32}

    let empty = {contents =  Map.empty;setSize = 0u}
    let isEmpty s =  s.setSize = 0u
    let size s  = s.setSize
    let contains a s = Map.containsKey a s.contents
    let numItems a s = Map.tryFind a s.contents |> Option.defaultValue 0u
    let add a n s ={contents = Map.add a (Map.tryFind a s.contents |> Option.defaultValue 0u |> (+) n)  s.contents; setSize = (n+ s.setSize)} 
    
    let addSingle a s = add a 1u s
    let remove a n s =
            match n with
            |_ when contains a s = false -> s
            |n when Map.find a s.contents <= n -> {contents = Map.remove a s.contents; setSize=(s.setSize - Map.find a s.contents)}
            |n -> {contents = Map.add a (Map.find a s.contents - n) s.contents; setSize = s.setSize - n}
    let removeSingle a s = remove a 1u s
    
    let fold f acc s = Map.fold f acc s.contents
        
    let foldBack f s acc = Map.foldBack f s.contents acc