module AVLTree

module Tree =
    type Tree<'a> =
        | Node of int * 'a Tree * 'a * 'a Tree
        | Nil
 
    let height = function
        | Node(h, _, _, _) -> h
        | Nil -> 0
 
    let make l x r =
        let h = 1 + max (height l) (height r)
        Node(h, l, x ,r)
 
    let rotRight = function
        | Node(_, Node(_, ll, lx, lr), x, r) ->
            let r' = make lr x r
            make ll lx r'
        | node -> node
 
    let rotLeft = function
        | Node(_, l, x, Node(_, rl, rx, rr)) ->
            let l' = make l x rl
            make l' rx rr
        | node -> node
 
    let doubleRotLeft = function
        | Node(h, l, x, r) ->
            let r' = rotRight r
            let node' = make l x r'
            rotLeft node'
        | node -> node
 
    let doubleRotRight = function
        | Node(h, l, x, r) ->
            let l' = rotLeft l
            let node' = make l' x r
            rotRight node'
        | node -> node
 
    let balanceFactor = function
        | Nil -> 0
        | Node(_, l, _, r) -> (height l) - (height r)
 
    let balance = function
        | Node(h, l, x, r) as node when balanceFactor node >= 2 ->
            if balanceFactor l >= 1 then rotRight node
            else doubleRotRight node
        | Node(h, l, x, r) as node when balanceFactor node <= -2 ->
            if balanceFactor r <= -1 then rotLeft node
            else doubleRotLeft node 
        | node -> node
 
    let rec insert v = function
        | Nil -> Node(1, Nil, v, Nil)
        | Node(_, l, x, r) as node ->
            if v = x then node
            else
                let l', r' = if v < x then insert v l, r else l, insert v r
                let node' = make l' x r'
                balance <| node'
 
    let rec contains v = function
        | Nil -> false
        | Node(_, l, x, r) ->
            if v = x then true
            else
                if v < x then contains v l
                else contains v r

open Tree
type AvlTree<'a when 'a : comparison> (tree : 'a Tree) =
    member this.Height = height tree
 
    member this.Left =
        match tree with
        | Node(_, l, _, _) -> Some <| AvlTree<'a> (l)
        | Nil -> None
 
    member this.Right =
        match tree with
        | Node(_, _, _, r) -> Some <| AvlTree<'a> (r)
        | Nil -> None
 
    member this.Value =
        match tree with
        | Node(_, _, x, _) -> Some x
        | Nil -> None

    member this.Insert x = AvlTree<'a> (insert x tree)
 
    member this.Contains v = tree |> contains v

let (|AvlTree|_|) (tree : AvlTree<'a>) =
    if tree.Value = None then None
    else Some (tree.Height, tree.Left, tree.Value, tree.Right)
 
module AvlTree =
    [<GeneralizableValue>]
    let empty<'a when 'a : comparison> : AvlTree<'a> = new AvlTree<'a>(Nil)

    let ofList (list : 'a list when 'a : comparison) =
        let rec build list (accum : AvlTree<'a>) =
            match list with
            | [] -> accum
            | hd::tl -> build tl (accum.Insert hd)
        build list empty<'a>

    let levelOrder tree =
        let rec getElements tree =
            seq {
                match tree with
                | AvlTree (h, l, x, r) ->
                    yield (Option.get x, h)
                    yield! getElements <| Option.get l
                    yield! getElements <| Option.get r
                | _ -> ()
            }
        getElements tree 
        |> Seq.sortBy (snd >> ((*) -1))
        |> Seq.groupBy snd
        |> Seq.map (snd >> List.ofSeq >> List.map fst)
        |> List.ofSeq