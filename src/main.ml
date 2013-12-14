open Core.Std
open Async.Std

(** [Middleware] lazy body + request -> lazy response *)
module type Middleware =
sig
  val uri : string
  val run : body:string Deferred.t -> Cohttp_async.Request.t -> Cohttp_async.Server.response Deferred.t
end

(** [ApiMiddleware] pipe the body of the request to the body of the response
  *)
module ApiMiddleware : Middleware =
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

(** [TwipperMiddleware] respond to homepage requests, or defer to the api
  *)
module TwipperMiddleware : Middleware =
struct
  let uri = "/"
  let run ~body request =
  begin
    let module R = Cohttp_async.Request in
    let module S = Cohttp_async.Server in
    match R.meth request, Uri.path (R.uri request) with
    | _, uripath when String.is_prefix ~prefix:(ApiMiddleware.uri ^ "/") uripath ->
      let subpath = String.chop_prefix_exn ~prefix:ApiMiddleware.uri uripath in
      let subrequest =
        R.make
          ~meth:(R.meth request)
          ~version:(R.version request)
          ~encoding:(R.encoding request)
          ~headers:(R.headers request)
          (Uri.with_path (R.uri request) subpath)
      in
      ApiMiddleware.run ~body subrequest
    | `GET, "/" ->
      (* homepage *)
      S.respond_with_string "homepage"
    | _ ->
    begin
      S.respond_with_string "not homepage"
    end
  end
end

(** [runmiddleware]
  * convert the string Pipe.Reader.t option to string list Deferred.t
  * strip the socket option as I am currently not using it.
  *)
let runmiddleware middleware ~body _socket _request =
  let body_option = Option.map ~f:Cohttp_async.body_to_string body in
  let body = Option.value ~default:(return "") body_option in
  middleware ~body _request

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
    (start_server TwipperMiddleware.run)

let () = Command.run start_server_command
