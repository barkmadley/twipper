open Core.Std
open Async.Std

(** [Middleware] lazy body + request -> lazy response *)
module type Middleware =
  sig
    val run : body:string list Deferred.t -> Cohttp_async.Request.t -> Cohttp_async.Server.response Deferred.t
  end

(** [EchoMiddleware] pipe the body of the request to the body of the response
  *)
module EchoMiddleware : Middleware =
  struct
    let run ~body (_request: Cohttp_async.Request.t) =
    begin
      body >>= fun lines ->
      Cohttp_async.Server.respond_with_string (String.concat lines)
    end
  end

(** [runmiddleware]
  * convert the string Pipe.Reader.t option to [`Eof|`Ok of string] Deferred.t
  * strip the socket option as I am currently not using it.
  *)
let runmiddleware middleware ~body _socket _request =
  let pipe_content : string list Deferred.t  =
    match body with
    | None      -> return []
    | Some pipe -> Pipe.to_list pipe
  in
  middleware ~body:pipe_content _request

(** [start_server] with a particular middleware on a particular port *)
let start_server middleware ~port () =
  let _server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t =
    Cohttp_async.Server.(
      create
        (Tcp.on_port port)
        (runmiddleware middleware)
    )
  in
  Deferred.never ()

(** A very basic command-line program, using Command, Core's Command line
  * parsing library.
  *)
let start_server_command =
  (** [Command.async_basic] is used for creating a command.
    * Every command takes a text summary and a command line spec
    *  as well as the commands implementation
    *)
  Command.async_basic
    ~summary:"start server"
    (** Command line specs are built up component by component, using a small
      * combinator library whose operators are contained in [Command.Spec]
      *)
    Command.Spec.(
      (** convert the port argument to a named argument *)
      step (fun m port -> m ~port)
      +> flag "-port" (optional_with_default 8080 int) ~doc:"port"
    )
    (** The command-line spec determines the argument to this function, which
      * show up in an order that matches the spec.
      *)
    (start_server EchoMiddleware.run)

let () = Command.run start_server_command
