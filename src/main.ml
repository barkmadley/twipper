open Core.Std
open Async.Std

open Middleware


(** [runmiddleware]
  * convert the string Pipe.Reader.t option to string list Deferred.t
  * strip the socket option as I am currently not using it.
  *)
let runmiddleware middleware host ~body _socket request =
  (* let body_option = Option.map ~f:Cohttp_async.body_to_string body in *)
  (* let body = Option.value ~default:(return "") body_option in *)
  middleware ~body:(Cohttp_async.Body.to_string body) ~host request

(** [start_server] with a particular middleware on a particular port *)
let start_server middleware ~port ~host () =
  let _server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t =
    Cohttp_async.Server.(
      create
        (Tcp.on_port port)
        (runmiddleware middleware ("http://" ^ host ^ ":" ^ string_of_int port))
    )
  in
  Deferred.never ()

let port_arg =
  Command.Spec.(flag "-port" (optional_with_default 8080 int) ~doc:"port")

let host_arg =
  Command.Spec.(flag "-host" (optional_with_default "localhost" string) ~doc:"host")

let group =
  Command.group
    ~summary:"Twipper"
    [ (* App.command start_server port_arg; *)
      Api.command start_server port_arg host_arg;
    ]

let () = Command.run group
