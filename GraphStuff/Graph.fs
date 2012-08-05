module GraphStuff.Graph

open System
open System.Collections.Generic

/// Represents the "cost" of traversing an edge that spans between two nodes.
type Cost = float

/// A single node in a node graph.
type Node (name : 'a) =
    let neighbors = Dictionary<Node, Cost> ()

    member this.Name = name

    /// List containing all of the neighbors (other nodes that are connected by an edge) to this node.
    member this.Neighbors = List.ofSeq neighbors.Keys

    /// List of (Node * Cost) pairs in which all the neighbors to this node,
    /// as well as the respective cost to travel to them from this node, are stored.
    member this.NeighborCostPairs = [for i in neighbors do yield (i.Key, i.Value)]

    /// Checks to see if a given Node is neighbors with this Node.
    member this.IsNeighborsWith (node : Node) = neighbors.ContainsKey (node)

    /// Finds the cost of travelling from this Node to the given Node.
    member this.CostOfTravel (target : Node) =
        let _, cost = neighbors.TryGetValue target
        cost

    /// Adds a one-way edge between this Node and another Node.
    member this.AddNeighbor (newNeighbor : Node, cost : Cost) = neighbors.Add(newNeighbor, cost)

    /// Checks to see if a given node is contained within the list of neighbors to this node
    /// and removes it from the list if it is.
    member this.RemoveNeighbor (node : Node) = 
        if this.IsNeighborsWith node then neighbors.Remove node |> ignore

    override this.ToString () = "Node (" + this.Name.ToString () + ")"

/// Graph of Nodes.
/// Due to the level of interconnectivity between Nodes, all operations on Nodes should be
/// performed using the functions contained by this class.
type Graph (nodeList : Node list) =
    let nodes = List<Node> ()
    do for node in nodeList do nodes.Add node

    /// List of all the Nodes on this Graph.
    member this.Nodes = nodes.AsReadOnly ()

    /// Adds a new Node to the graph
    member this.AddNode (node : Node) = nodes.Add node

    /// Removes a given node from this Graph.
    member this.RemoveNode (node : Node) =
        if this.ContainsNode node then
            for i in node.Neighbors do i.RemoveNeighbor node
            nodes.Remove node |> ignore

    /// Checks to see if this Graph contains a given Node.
    member this.ContainsNode (node : Node) = nodes.Contains node

    /// Adds a one-way edge with a given cost between two Nodes.
    member this.OneWayPath (startNode : Node, endNode : Node, cost : Cost) =
        if this.ContainsNode startNode && this.ContainsNode endNode then
            startNode.AddNeighbor (endNode, cost)

    /// Adds a two-wat edge between two nodes
    member this.TwoWayPath (a : Node, b : Node, cost : Cost) =
        if this.ContainsNode a && this.ContainsNode b then
            a.AddNeighbor (b, cost)
            b.AddNeighbor (a, cost)

let dijkstra (graph : Graph, startNode : Node, endNode : Node) =
    let infinity = Double.PositiveInfinity

    /// Set of all unvisited nodes in the graph
    let unvisited = HashSet<Node> ()

    let nodeDistances = Dictionary<Node, float> ()

    for node in graph.Nodes do
        if node = startNode then nodeDistances.Add (node, 0.0)
        else nodeDistances.Add (node, infinity)
        unvisited.Add (node) |> ignore

    /// Retrieves the distance to a given node from the nodeDistances dictionary.
    let getStoredDist node = 
        match nodeDistances.TryGetValue node with
        | true, x -> Some x
        | false, _ -> None

    /// Gets the node with the lowest tentative distance of all nodes on the graph.
    let minDist () = unvisited |> Seq.minBy (getStoredDist)

    /// Evaluates the tentative distances of each of the current node's neighbors and updates nodeDistances if necessary.
    let evalNeighbors (currentNode : Node) =
        printf "Evaluating node %s\n" (currentNode.ToString ())
        let currentNeighbors = HashSet<Node> ()
        for node in currentNode.Neighbors do 
            currentNeighbors.Add(node) |> ignore

        let validNeighbors = new HashSet<Node> ()
        for node in unvisited do validNeighbors.Add node |> ignore
        validNeighbors.IntersectWith currentNeighbors

        for neighbor in validNeighbors do
            let neighborDist = currentNode.CostOfTravel neighbor
            if ((getStoredDist currentNode).Value + neighborDist) < (getStoredDist (neighbor)).Value then 
                nodeDistances.Remove (neighbor) |> ignore
                nodeDistances.Add (neighbor, neighborDist + (getStoredDist currentNode).Value)
        unvisited.Remove currentNode |> ignore

    let rec loop (currentNode : Node) =
        evalNeighbors currentNode
        if (unvisited.Contains endNode) = false then (getStoredDist endNode)
        elif (getStoredDist (minDist ())).Value = infinity then None
        else
            loop (minDist ())

    loop startNode

let run () =
    let nodeList = List<Node> ()
    let graph = Graph []
    for i in [1 .. 6] do graph.AddNode (Node i)
    graph.TwoWayPath (graph.Nodes.Item 0, graph.Nodes.Item 1, 7.0)
    graph.TwoWayPath (graph.Nodes.Item 0, graph.Nodes.Item 2, 9.0)
    graph.TwoWayPath (graph.Nodes.Item 0, graph.Nodes.Item 5, 14.0)
    graph.TwoWayPath (graph.Nodes.Item 1, graph.Nodes.Item 2, 10.0)
    graph.TwoWayPath (graph.Nodes.Item 1, graph.Nodes.Item 3, 15.0)
    graph.TwoWayPath (graph.Nodes.Item 2, graph.Nodes.Item 3, 11.0)
    graph.TwoWayPath (graph.Nodes.Item 2, graph.Nodes.Item 5, 2.0)
    graph.TwoWayPath (graph.Nodes.Item 3, graph.Nodes.Item 4, 6.0)
    graph.TwoWayPath (graph.Nodes.Item 4, graph.Nodes.Item 5, 9.0)

    let a = dijkstra (graph, graph.Nodes.Item 0, graph.Nodes.Item 4)
    printf "Cost: %f" a.Value
    Console.ReadKey true |> ignore