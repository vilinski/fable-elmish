(**
 - title: Counter
 - tagline: The famous Increment/Decrement ported from Elm
*)


#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#r "../node_modules/fable-react/Fable.React.dll"
#r "../node_modules/fable-elmish/Fable.Elmish.dll"
#r "../node_modules/fable-elmish-react/Fable.Elmish.React.dll"
#r "../node_modules/fable-elmish-debugger/Fable.Elmish.Debugger.dll"

open Fable.Core
open Fable.Import
open Elmish

/// Remote data
module RemoteData =
  /// Extended `Result` alike data structure for remote data
  type RemoteData<'error,'content> =
    | NotAsked
    | Loading
    | Failure of 'error
    | Success of 'content

  /// transforms content in case of fetch success
  let map f = function
    | NotAsked -> NotAsked
    | Loading -> Loading
    | Failure e -> Failure e
    | Success c -> Success (f c)

  /// transforms content in case of fetch failure
  let mapError f = function
    | NotAsked -> NotAsked
    | Loading -> Loading
    | Failure e -> Failure (f e)
    | Success c -> Success c

/// Types holding the recursive nested elements
module Tree =
  module R = Fable.Helpers.React
  open R.Props
  open System
  open Fable.Core.JsInterop
  open Fable.Helpers.React.Props
  open RemoteData

  /// Node id
  type NodeId = System.Guid
  /// create new id
  let newId() = System.Guid.NewGuid()
  /// tree messages
  type TreeMsg<'a, 'amsg> =
    | NoOp
    | Expand
    | Collapse
    | FetchChildren
    | FetchChildrenDone of RemoteData<string, 'a array>
    | ChildAt of NodeId * TreeMsg<'a, 'amsg>
    | Content of 'amsg
  /// a node containing some data, can have multiple child nodes
  and 'a Node =
    { /// content of the node
      content : 'a
      /// lazy loaded children nodes
      children: RemoteData<string, 'a Node array>
      /// indicated whether the node is expanded
      expanded: bool
      /// node id used to propagate message
      id: NodeId
    }

  /// Nested elements structure.
  type Tree<'a, 'amsg> =
    { /// The TEA view function for the content type
      view : 'a -> 'amsg Dispatch -> React.ReactElement
      /// The TEA update function for the
      update : 'amsg -> 'a -> ('a * Cmd<'amsg>)
      /// Function to lazy load children elements.
      /// Should produce a message, adding them to the structure
      fetch : 'a -> Cmd<TreeMsg<'a,'amsg>>
      /// The root node containing single element
      root: 'a Node
    }
  let toChildAt childNodeId msg = ChildAt(childNodeId, msg)
  let rec private viewNode (dispatch: TreeMsg<'a,'amsg> -> unit) viewContent node =
    let viewNodes dispatch viewContent = function
      | NotAsked -> []
      | Loading -> [ unbox "loading.." ]
      | Failure error -> [ unbox error ]
      | Success nodes ->
        nodes
        |> Seq.map (fun childNode ->
           let childDispatch = (toChildAt childNode.id) >> dispatch
           R.li []
                [ viewNode childDispatch viewContent childNode ])
        |> Seq.toList
        |> R.ul []
        |> List.singleton

    let onClick msg =
      OnClick <| fun _ -> msg |> dispatch
    let onClickMsg =
      match node.expanded with
      | true -> Collapse
      | false -> Expand
    // make visible whether the element is already fetched
    let sign =
      match node.expanded, node.children with
      | true, Success _ -> "▼"
      | false, Success _ -> "►"
      | true, _ -> "▽"
      | false, _ -> "▷"
      |> unbox

    if not node.expanded then []
    else viewNodes dispatch viewContent node.children
    |> List.append
      [ R.button [onClick onClickMsg] [sign]
        viewContent node.content (Content >> dispatch)
      ]
    |> R.div []

  let view (model: Tree<'a, 'amsg>) (dispatch: TreeMsg<'a,'amsg> -> unit) =
    viewNode dispatch model.view model.root

  let rec private updateNode msg node tree =
    match msg with
    | NoOp -> node, []
    | Expand ->
      match node.children with
      | NotAsked ->
        { node with expanded = true; children = Loading }, tree.fetch node.content
      | _ ->
        { node with expanded = true }, []
    | Collapse -> { node with expanded = false } , []
    | FetchChildren -> node , tree.fetch node.content
    | FetchChildrenDone remoteList ->
        let children =
          remoteList
          |> RemoteData.map (fun arr ->
              arr
              |> Array.map (fun content ->
                { content = content
                  children = NotAsked
                  expanded = false
                  id = Guid.NewGuid()}))
        { node with children = children }, []
    | Content amsg ->
      let (content1: 'a, cmd) = tree.update amsg node.content
      { node with content = content1 }, Cmd.map Content cmd
    | ChildAt (nodeid, msg) ->
      match node.children with
      | Success children ->
        let tuples =
          children
          |> Array.map (fun n ->
            if n.id <> nodeid
            then n, []
            else updateNode msg n tree
          )
        let children1 = tuples |> Array.map fst
        let cmd = tuples |> Seq.collect snd |> Seq.toList
        { node with children = Success children1 }, Cmd.map (toChildAt nodeid) cmd
      | _ -> node, []

  let update (msg: TreeMsg<'a, 'amsg>) (tree: Tree<'a,'amsg>) =
    let (root1, cmd) = updateNode msg tree.root tree
    { tree with root = root1 }, cmd

/// counter component
module C =
  open System
  open RemoteData
  open Tree
  // rendering views with React
  module R = Fable.Helpers.React
  open Fable.Core.JsInterop
  open Fable.Helpers.React.Props
  open Fable.PowerPack

  type Msg =
    | Increment
    | Decrement

  let view count dispatch =
    let onClick msg =
      OnClick <| fun _ -> msg |> dispatch

    R.div []
      [ R.button [ onClick Decrement ] [ unbox "-" ]
        R.div [] [ unbox (string count) ]
        R.button [ onClick Increment ] [ unbox "+" ] ]

  let update (msg:Msg) count =
    match msg with
    | Increment -> count + 1, []
    | Decrement -> count - 1, []

  let fetch i =
    Cmd.ofPromise
      (fun i -> promise { return [| i + 1; i + 2; i + 3 |] |> Array.map ((*) 10) })
      i
      (Success >> FetchChildrenDone) (string >> Failure >> FetchChildrenDone)

  let root =
    { content = 1
      children = NotAsked
      expanded = false
      id = Guid.NewGuid()
    }
  let init() =
    { view = view
      update = update
      fetch = fetch
      root = root
    }, []

// MODEL

type Msg =
  | Increment
  | Decrement
let init () = 0, []

// UPDATE

let update (msg:Msg) count =
  match msg with
  | Increment -> count + 1, []
  | Decrement -> count - 1, []

// rendering views with React
module R = Fable.Helpers.React
open Fable.Core.JsInterop
open Fable.Helpers.React.Props

let view count dispatch =
  let onClick msg =
    OnClick <| fun _ -> msg |> dispatch

  R.div []
    [ R.button [ onClick Decrement ] [ unbox "-" ]
      R.div [] [ unbox (string count) ]
      R.button [ onClick Increment ] [ unbox "+" ] ]

open Elmish.React
open Elmish.Debug

// App
Program.mkProgram C.init Tree.update Tree.view
|> Program.withReact "elmish-app"
|> Program.withDebugger
|> Program.withConsoleTrace
|> Program.run
