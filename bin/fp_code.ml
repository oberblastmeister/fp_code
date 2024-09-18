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

  module Make (Eq : EqType) : Signature with type Key.t := Eq.t = struct
    module Key = Eq

    type 'a map = (Key.t * 'a) list

    let empty = []
    let add _ = failwith "implement me!"
    let remove _ = failwith "implement me!"
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

  val fold :
    ('a element -> 'a element) -> 'a element -> 'a container -> 'a element
end

module Container_funcs = struct
  module type Signature = sig
    type 'a container
    type 'a element

    val fold :
      ('a element -> 'a element) -> 'a element -> 'a container -> 'a element

    (* returns the number of elements in the container*)
    val length : 'a container -> int

    (* returns the number of elements in the container satisfying a predicate *)
    val count : ('a element -> bool) -> 'a container -> int

    (* returns the first element that satisfies the predicate, `None` if there aren't any *)
    val find : ('a -> bool) -> 'a container -> 'a option

    (* returns the maximum element in the container using the provided comparison function, `None` if there aren't any *)
    val max_element : ('a -> 'a -> int) -> 'a container -> 'a option
  end

  module Make (Fold : Foldable) :
    Signature
      with type 'a container := 'a Fold.container
       and type 'b element := 'b Fold.element = struct
    type 'a container = 'a Fold.container
    type 'a element = 'a Fold.element

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

  module Make (Eq : EqType) : Signature with type Key.t := Eq.t = struct
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
    val add : Key.t
    val mem : Key.t -> set -> bool

    include
      Container_funcs.Signature
      (* Notice how both of these ignore the `'a`; our `set` type isn't polymorphic *)
        with type 'a container := set
         and type 'a element := Key.t

    (*
      If this set implementation was something half-decent like a
      binary search tree, we would have a faster implementation of
      `max_element` than the one provided by `Container_funcs` as long as the
      comparison function is the same as the one used for the tree itself.
      We would like to replace the `max_element` with the faster one that
      doesn't take the comparison function as an argument and provide access
      to the slower one under the name `max_element_with`. To do this we would
      define `max_element_with` to be the same as `max_element` and then
      shadow `max_element` with our new implementation. But alas, we didn't
      implement a binary search tree.
    *)
    val max_element_with : (Key.t -> Key.t -> int) -> set -> Key.t option

    (* This function won't actaully exist, so we'll comment out its signature *)
    (* val max_element : t -> Key.t option *)
  end

  module Make (Eq : EqType) = struct
    type set (* implement me! *)

    module Key = Eq

    let empty _ = failwith "implement me!"
    let add _ = failwith "implement me!"
    let mem _ = failwith "implement me!"

    include Container_funcs.Make (struct
      type 'a container = set
      type 'a element = Eq.t

      let fold _ = failwith "implement me!"
    end)

    let max_element_with = max_element

    (* let max_element = ... *)
  end
end

(*
  The interface for a list builder.
  We want to support fast appends.
*)
module type Builder = sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
end

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
