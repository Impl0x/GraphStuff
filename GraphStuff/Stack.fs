module GraphStuff.Stack

open System

type StackNode<'a> =
| StackNode of ('a * StackNode<'a>)
| Empty

type Stack (head : 'a, tail : StackNode<'a>) =
    let mutable head = head
    let mutable tail = tail
    let mutable stack = StackNode (head, tail)

    member this.Head
        with get () = head
        and set value = head <- value

    member private this.Tail 
        with get () = tail
        and set value = tail <- value

    member private this.Stack
        with get () = stack
        and set value = stack <- value


    member this.Count =
        let rec count stack =
            match stack with
            | StackNode (_, Empty) -> 1
            | StackNode (_, tl) -> 1 + count tl
            | Empty -> 0 //Its not possible to reach this case but I like squelching compiler warnings.
        count this.Stack

    member this.Push value =
        let tl = StackNode (this.Head, this.Tail)
        this.Head <- value
        this.Tail <- tl
        this.Stack <- StackNode (this.Head, this.Tail)

    member this.Pop () = 
        match this.Stack with
        | Empty -> failwith "Stack is empty."
        | StackNode (hd, tl) ->
            this.Stack <- tl
            hd

    member this.Peek () =
        match this.Stack with
        | Empty -> failwith "stack is empty."
        | StackNode (hd, _) -> hd
        

let run () =
    let dispStack (s : Stack) =
        for i in List.rev [1 .. s.Count] do 
            if i <> 1 then printf "%A -> " (s.Pop ())
            else printf "%A\n" (s.Pop ())

    let stack = Stack (1, StackNode (2, StackNode (3, StackNode (4, StackNode (5, Empty)))))

    dispStack stack
    printf "\n"

    for i in [1 .. 100] do
        stack.Push i
    dispStack stack

    Console.ReadKey true |> ignore