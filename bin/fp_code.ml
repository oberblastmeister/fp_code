open Header
(*
   Let's start by implementing a really bad map using a functor.
   It just needs to be a list of key-value pairs.
   The type of the key and the equality function for that type
   will be in a module provide as an argument to the functor.
   The type of the value will be
*)

module type EqType = sig
  (*
     In OCaml we usually name the "main" type in a module `t`.
     Here, `t` is a type that has an equality function.
     Defining it this way means that modules `Int` and `String`
  *)
  type t

  val equal : t -> t -> bool
end

module type ShowType = sig
  type t

  val show : t -> string
end

module Map = struct
  module type Signature = sig
    (*
      If we were writing idiomatic OCaml code, we
      would name this `'a t`.
    *)
    type 'a map

    module Key : EqType

    val empty : 'a map
    val add : Key.t -> 'a -> 'a map -> 'a map
    val remove : Key.t -> 'a map -> 'a map
    val find : Key.t -> 'a map -> 'a option
  end

  module Make (Eq : EqType) : Signature with type Key.t = Eq.t = struct
    module Key = Eq

    type 'a map = (Key.t * 'a) list

    let empty = []
    let add _ _ _ = failwith "implement me!"
    let remove _ _ = failwith "implement me!" ( = )
    let find _ = failwith "implement me!"
  end
end

(*
   This is ok so far, but we want some more functions to
   operate on our map. However, instead of adding them directly,
   let's abstract our implementations of these functions so they
   work on any container with a `fold` function. To do this,
   we'll make a functor that takes the container and `fold` as
   an argument.
*)

module type Foldable = sig
  type 'a container

  (*
     We define the element as a separate type so we can
     have a more complicated element type than just the `'a`
     that's passed to `container`
  *)
  type 'a element

  val fold : ('b -> 'a element -> 'b) -> 'b -> 'a container -> 'b
end

module Container_funcs = struct
  module type Signature = sig
    type 'a container
    type 'a element

    val fold : ('b -> 'a element -> 'b) -> 'b -> 'a container -> 'b

    (* returns the number of elements in the container*)
    val length : 'a container -> int

    (* returns the number of elements in the container satisfying a predicate *)
    val count : ('a element -> bool) -> 'a container -> int

    (* returns the first element that satisfies the predicate, `None` if there aren't any *)
    val find : ('a element -> bool) -> 'a container -> 'a element option

    (* returns the maximum element in the container using the provided comparison function, `None` if there aren't any *)
    val max_element : ('a element -> 'a element -> int) -> 'a container -> 'a element option
  end

  module Make (Fold : Foldable) :
    Signature
      with type 'a container := 'a Fold.container
       and type 'b element := 'b Fold.element = struct
    let fold _ = failwith "implement me!"
    let length _ = failwith "implement me!"
    let count _ = failwith "implement me!"
    let find _ = failwith "implement me!"
    let max_element _ = failwith "implement me!"
  end
end

(*
Now let's augment our Map module with these functions by
`include`ing the result of applying a fold function to
`Container_funcs.Make`.
*)

module Map2 = struct
  module type Signature = sig
    include Map.Signature

    include
      Container_funcs.Signature
        with type 'a container := 'a map
        (* Notice how the type of our elements is a tuple of the key and the value *)
         and type 'a element := Key.t * 'a
  end

  module Make (Eq : EqType) : Signature with type Key.t = Eq.t = struct
    include Map.Make (Eq)

    include Container_funcs.Make (struct
      type 'a container = 'a map
      type 'a element = Eq.t * 'a

      let fold _ = failwith "implement me!"
    end)
  end
end

(*
Now let's use abstraction to use our map implementation
as a set. A set is basically the same as a map except that
we don't care about the values. So, we can just put anything in
as the value, such as `()` or `12`, and as long as we don't expose
this value in the signature of the module we're fine.
*)

module Set = struct
  module type Signature = sig
    type set

    module Key : EqType

    val empty : set
    val add : Key.t -> set -> set
    val mem : Key.t -> set -> bool

    include
      Container_funcs.Signature
      (* Notice how both of these ignore the `'a`; our `set` type isn't polymorphic *)
        with type 'a container := set
         and type 'a element := Key.t

    (*
      If this set implementation was something half-decent like a
      binary search tree, we could have a faster implementation of
      `max_element` than the one provided by `Container_funcs` as long as the
      comparison function is the same as the one used for the tree itself.
      Let's call this new function maximum.
      But alas, we didn't implement a binary search tree.
    *)
    (* This function won't actually exist, so we'll comment out its signature *)
    (* val maximum : t -> Key.t option *)
  end

  module Make (Eq : EqType) : Signature with type Key.t = Eq.t = struct
    module Key = Eq

    type set = Key.t list

    let empty = []
    let add _ = failwith "implement me!"
    let mem _ = failwith "implement me!"

    include Container_funcs.Make (struct
      type 'a container = set
      type 'a element = Eq.t

      let fold _ = failwith "implement me!"
    end)

    (* let maximum = ... *)
  end
end

(*
  This is an interface for a builder like the one from the presentation.
  Anything that we put in for this signature should have a fast append,
  but of course we can't guarentee that in the type system.
  Let's 
*)
module type Builder = sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
end

(*
   Let's implement a DList like 
*)
module DList : Builder = struct
  type 'a t = 'a list -> 'a list

  let empty _ = failwith ""
  let singleton _ = failwith ""
  let append _ = failwith ""
  let to_list _ = failwith ""
end

module Tree : Builder = struct
  type 'a t = Empty | Singleton of 'a | Append of 'a t * 'a t

  let empty = Empty
  let singleton _ = failwith ""
  let append _ = failwith ""

  (* try to make this fail recursive *)
  let to_list _ = failwith ""
end

(*
  Implement string interning using generative functors.
  Different string interners should have different symbol types so that ids do not clash.
*)
module SymbolTable = struct
  module type Signature = sig
    type symbol

    val insert : string -> symbol

    (*
    symbol should implement EqType.
    This is a destructive substitution, which removes the type t and replaces it with symbol
    *)
    include EqType with type t := symbol
    include ShowType with type t := symbol
  end

  (*
  The empty parameter marks the Make functor as generative.
  This means that applications of a functor to the same module will still generate different types, unlike applicative functors.
  *)
  module Make () : Signature = struct
    type symbol = { id : int; string : string }

    (*
      We need the functor to be generative because we are initializing mutable state.
      This value is an implementation detail and is not present in the interaace
    *)
    let counter = ref 0
    (*
      initialize this to a map value
      *)

    let map = ref []
    let insert _ = failwith ""
    let equal _ = failwith ""
    let show _ = failwith ""
  end
end

module Ast : sig
  type expr =
    | Var of string
    | Lam of string * expr
    | App of expr * expr
    | Let of (string * expr) * expr

  val pretty : expr -> string
end = struct
  type expr =
    | Var of string
    | Lam of string * expr
    | App of expr * expr
    | Let of (string * expr) * expr

  (* implement this with a builder *)
  let pretty expr = failwith ""
end
