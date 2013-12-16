open Core.Std
open Async.Std

open Middleware


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

let port_arg =
  Command.Spec.(flag "-port" (optional_with_default 8080 int) ~doc:"port")

let group =
  Command.group
    ~summary:"Twipper"
    [ (* App.command start_server port_arg; *)
      Api.command start_server port_arg;
    ]

let () = Command.run group
