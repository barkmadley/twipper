open Core.Std
open Async.Std
open Cohttp_async

val command:
  ( ( body:string Deferred.t -> Request.t -> Server.response Deferred.t ) ->
    port:'a ->
    unit ->
    unit Deferred.t
  ) ->
  'a Command.Spec.param ->
  string * Command.t