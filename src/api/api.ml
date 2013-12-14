open Core.Std
open Async.Std

open Middleware

(** [App]
  *)
module App : Middleware =
struct
  let uri = "/api"
  let run ~body request =
  begin
    let module R = Cohttp_async.Request in
    let module S = Cohttp_async.Server in
    match R.meth request, Uri.path (R.uri request) with
    | `GET, "/" ->
    begin
      S.respond_with_string "root"
    end
    | _ ->
    begin
      S.respond_with_string "not root"
    end
  end
end

(** Given a way to start the server, and a way to parse the port argument:
  *   return the (command name, Command.t) pair used to create sub commands
  *)
let command start_server port_arg =
  (** [Command.async_basic] is used for creating a command.
    * Every command takes a text summary and a command line spec
    *  as well as the commands implementation
    *)
  "api",
  Command.async_basic
    ~summary:"start api server"
    (** Command line specs are built up component by component, using a small
      * combinator library whose operators are contained in [Command.Spec]
      *)
    Command.Spec.(
      (** convert the port argument to a named argument *)
      step (fun m port -> m ~port)
      +> port_arg
    )
    (** The command-line spec determines the argument to this function, which
      * show up in an order that matches the spec.
      *)
    (start_server App.run)
