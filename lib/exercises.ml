(*exercise 1: implement a functor which can find an element when given a comparable*)
module type Comparable = sig
	type t
	val compare : t -> t -> int
end 

module Find (C : Comparable) = struct 
	let find _ _ = failwith "implement me :(" 
end
