module AVLTree

module Tree =
    type Tree<'a> =
        | Node of int * 'a Tree * 'a * 'a Tree
        | Nil
 
    /// Gets the height of a given tree
    let height = function
        | Node (h, _, _, _) -> h
        | Nil -> 0
    
    /// Makes a new tree with given left and right subtrees and a root value
    let make l x r =
        let h = 1 + max (height l) (height r)
        Node (h, l, x ,r)
 
    /// Rotates a tree to the right
    let rotRight = function
        | Node (_, Node (_, ll, lx, lr), x, r) ->
            let r' = make lr x r
            make ll lx r'
        | node -> node
 
    /// Rotates a tree to the left
    let rotLeft = function
        | Node (_, l, x, Node (_, rl, rx, rr)) ->
            let l' = make l x rl
            make l' rx rr
        | node -> node

    /// Rotates a tree to the right twice
    let doubleRotRight = function
        | Node (h, l, x, r) ->
            let l' = rotLeft l
            let node' = make l' x r
            rotRight node'
        | node -> node
 
    /// Rotates a tree to the left twice
    let doubleRotLeft = function
        | Node (h, l, x, r) ->
            let r' = rotRight r
            let node' = make l x r'
            rotLeft node'
        | node -> node
 
    /// Gets the balance factor of a given tree
    let balanceFactor = function
        | Nil -> 0
        | Node (_, l, _, r) -> (height l) - (height r)
 
    /// Balances a given tree
    let balance = function
        | Node (h, l, x, r) as node when balanceFactor node >= 2 ->
            if balanceFactor l >= 1 then rotRight node
            else doubleRotRight node
        | Node (h, l, x, r) as node when balanceFactor node <= -2 ->
            if balanceFactor r <= -1 then rotLeft node
            else doubleRotLeft node 
        | node -> node
 
    /// Inserts a value in a given tree
    let rec insert v = function
        | Nil -> Node (1, Nil, v, Nil)
        | Node (_, l, x, r) as node ->
            if v = x then node
            else
                let l', r' = if v < x then insert v l, r else l, insert v r
                let node' = make l' x r'
                balance <| node'
 
    /// Tests if a value is contained in a given tree
    let rec contains v = function
        | Nil -> false
        | Node (_, l, x, r) ->
            if v = x then true
            else
                if v < x then contains v l
                else contains v r

open Tree
type AvlTree<'a when 'a : comparison> (tree : 'a Tree) =
    /// Gets the height of this tree
    member this.Height = height tree
 
    /// Gets the left subtree of this tree
    member this.Left =
        match tree with
        | Node (_, l, _, _) -> Some <| AvlTree<'a> (l)
        | Nil -> None
 
    /// Gets the right subtree of this tree
    member this.Right =
        match tree with
        | Node (_, _, _, r) -> Some <| AvlTree<'a> (r)
        | Nil -> None
    
    /// Gets the root value of this tree
    member this.Value =
        match tree with
        | Node (_, _, x, _) -> Some x
        | Nil -> None

    /// Inserts a given value into this tree
    member this.Insert x = AvlTree<'a> (insert x tree)
 
    /// Returns whether a value is contained within this tree
    member this.Contains v = tree |> contains v

let (|AvlTree|_|) (tree : AvlTree<'a>) =
    if tree.Value = None then None
    else Some (tree.Height, tree.Left, tree.Value, tree.Right)
 
module AvlTree =
    /// An empty AVL tree
    let [<GeneralizableValue>] empty<'a when 'a : comparison> : AvlTree<'a> = new AvlTree<'a>(Nil)

    /// Makes an AVL tree from a given list
    let ofList (list : 'a list when 'a : comparison) =
        let rec build list (accum : AvlTree<'a>) =
            match list with
            | [] -> accum
            | hd::tl -> build tl (accum.Insert hd)
        build list empty<'a>

    /// Returns the pre-order representation of a given tree
    let rec preOrder tree =
        seq {
            match tree with
            | AvlTree (_, l, x, r) ->
                yield x
                yield! preOrder <| Option.get l
                yield! preOrder <| Option.get r
            | _ -> ()
        }

    /// Returns the in-order representation of a given tree
    let rec inOrder tree =
        seq {
            match tree with
            | AvlTree (_, l, x, r) ->
                yield! inOrder <| Option.get l
                yield x
                yield! inOrder <| Option.get r
            | _ -> ()
        }

    /// Returns the post-order representation of a given tree
    let rec postOrder tree =
        seq {
            match tree with
            | AvlTree (_, l, x, r) ->
                yield! postOrder <| Option.get l
                yield! postOrder <| Option.get r
                yield x
            | _ -> ()
        }

    /// Returns the level-order representation of a given tree
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
        |> Seq.map (snd >> Seq.map fst)