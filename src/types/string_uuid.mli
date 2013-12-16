
module type StringUUID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module String_uuid : StringUUID
