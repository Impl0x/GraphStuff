[<AutoOpen>]
module GraphStuff.BST

open System

type Tree =
| Tree of (int * Tree * Tree)
| Empty

let levelOrder tree =
    let rec loop queue =
       seq {
            match queue with 
            | [] -> ()
            | (Empty :: tl) -> yield! loop tl
            | (Tree (x, l, r) :: tl) -> 
                yield x
                yield! loop (tl @ [l; r])
        }
    loop [tree]

let rec search (tree : Tree) (value : int) =
    match tree with
    | Empty -> "Value Not Found!" :> obj
    | Tree (x, left, right) -> 
        match value with
        | v when v < x ->  search left v
        | v when v > x -> search right v
        | v when v = x -> x :> obj

let tree =
    Tree (5, 
        Tree(3,
            Tree (1,
                Tree (0, Empty, Empty),
                Tree (2, Empty, Empty)),
            Tree (4, Empty, Empty)),
        Tree (7,
            Tree (6, Empty, Empty),
            Tree (9,
                Tree (8, Empty, Empty),
                Tree (10, Empty, Empty))))

let run () =
    failwith "NOT FULLY IMPLEMETED!"
    levelOrder tree |> Seq.iter (fun x -> printf "%A" x)
    printf "\n"

    let mutable searchValue = 4
    let searchResult = downcast (search tree searchValue)
    printf "Searched for %i: %A\n" searchValue searchResult

    searchValue <- 11
    let searchResult = downcast (search tree searchValue)
    printf "Searched for %i: %A\n" searchValue searchResult

    Console.ReadKey true |> ignore