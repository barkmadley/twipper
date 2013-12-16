module type StringUUID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module String_uuid : StringUUID = struct
  type t = string
  let of_string x = x
  let to_string x = x
end