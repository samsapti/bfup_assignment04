module Dictionary

type Dict = D of Set<string>
let empty () = D(Set.empty)
let insert s (D (dict)) = D(Set.add s dict)
let lookup s (D (dict)) = Set.contains s dict
