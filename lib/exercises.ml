module type Comparable = sig
	type t
	val compare : t -> t -> int

module Find (C : Comparable) = failwith "implement me :("
