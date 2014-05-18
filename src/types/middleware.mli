open Core.Std
open Async.Std

(** [Middleware] lazy body + request -> lazy response *)
module type Middleware =
sig
  val uri : string
  val run : body:string Deferred.t -> host:string -> Cohttp_async.Request.t -> Cohttp_async.Server.response Deferred.t
end
