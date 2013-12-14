open Core.Std
open Async.Std

open Middleware

(** [AppMiddleware] respond to homepage requests, or defer to the api
  *)
module App : Middleware =
struct
  let uri = "/"
  let run ~body request =
  begin
    let module R = Cohttp_async.Request in
    let module S = Cohttp_async.Server in
    match R.meth request, Uri.path (R.uri request) with
    (* Broken code now * )
    | _, uripath when String.is_prefix ~prefix:(Api.App.uri ^ "/") uripath ->
      let subpath = String.chop_prefix_exn ~prefix:Api.App.uri uripath in
      let subrequest =
        R.make
          ~meth:(R.meth request)
          ~version:(R.version request)
          ~encoding:(R.encoding request)
          ~headers:(R.headers request)
          (Uri.with_path (R.uri request) subpath)
      in
      Api.App.run ~body subrequest
    ( *  *)
    | `GET, "/" ->
      (* homepage *)
      S.respond_with_string "homepage"
    | _ ->
    begin
      S.respond_with_string "not homepage"
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
  "app",
  Command.async_basic
    ~summary:"start app server"
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
