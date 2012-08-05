module GraphStuff.Tree

open System

type Tree<'a> =
    | Tree of ('a * Tree<'a> * Tree<'a>)
    | Empty

let rec preOrder tree = 
    seq {
        match tree with 
        | Empty -> ()
        | Tree (x, left, right) -> 
            yield x
            yield! preOrder left
            yield! preOrder right
    }

let rec inOrder tree =
    seq {
        match tree with
        | Empty -> ()
        | Tree (x, left, right) ->
            yield! inOrder left
            yield x
            yield! inOrder right
    }

let rec postOrder tree =
    seq {
        match tree with
        | Empty -> ()
        | Tree (x, left, right) ->
            yield! postOrder left
            yield! postOrder right
            yield x
    }

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

let tree =
    Tree ("F",
        Tree ("B",
            Tree ("A", Empty, Empty),
            Tree ("D",
                Tree ("C", Empty, Empty),
                Tree ("E", Empty, Empty))),
        Tree ("G", 
            Tree ("I", 
                Tree ("H", Empty, Empty),
                Empty),
            Empty))

let disp x = printf "%A " x

let run () =
    printf "Pre Order: "
    preOrder tree |> Seq.iter disp
    printf "\n"

    printf "In Order: "
    inOrder tree |> Seq.iter disp
    printf "\n"

    printf "Post Order: "
    postOrder tree |> Seq.iter disp
    printf "\n"

    printf "Level Order: "
    levelOrder tree |> Seq.iter disp
    printf "\n"

    Console.ReadKey (true) |> ignore