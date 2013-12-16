open Core.Std
open Async.Std
open Cohttp_async

val command:
  ( ( body:string Deferred.t -> host:string -> Request.t -> Server.response Deferred.t ) ->
    port:int ->
    host:string ->
    unit ->
    unit Deferred.t
  ) ->
  int Command.Spec.param ->
  string Command.Spec.param ->
  string * Command.t