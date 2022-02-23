module MultiSet

type MultiSet<'a when 'a: comparison> = S of Map<'a, uint32>

let empty = S(Map.empty)

let size (S (s)) =
    Map.fold (fun acc _ value -> acc + value) 0u s

let isEmpty s = (size s) = 0u
let contains a (S (s)) = Map.containsKey a s

let numItems a (S (s)) =
    Map.tryFind a s |> Option.defaultValue 0u

let add a n (S (s)) =
    S(Map.add a ((numItems a (S(s))) + n) s)

let addSingle a s = add a 1u s

let remove a n (S (s)) =
    let sz = numItems a (S(s))

    if n < sz then
        S(Map.add a (sz - n) s)
    else
        S(Map.remove a s)

let removeSingle a s = remove a 1u s
let fold f acc (S (s)) = Map.fold f acc s
let foldBack f (S (s)) acc = Map.foldBack f s acc
