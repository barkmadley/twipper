open Core.Std
open Async.Std

let port_arg () =
  Command.Spec.(
    flag "-port" (optional_with_default 8080 int)
      ~doc:"port"
  )

(* A very basic command-line program, using Command, Core's Command line
   parsing library.  *)

let command =
  (** [Command.async_basic] is used for creating a command.
    * Every command takes a text summary and a command line spec
    *  as well as the commands implementation
    *)
  Command.async_basic
    ~summary:"twipper"
    (** Command line specs are built up component by component, using a small
      * combinator library whose operators are contained in [Command.Spec]
      *)
    Command.Spec.(
      empty +> port_arg ()
    )
    (** The command-line spec determines the argument to this function, which
      * show up in an order that matches the spec.
      *)
    (fun port () ->
      let _ =
        Cohttp_async.Server.(
          create
          (Tcp.on_port port)
          (fun ~body _ _ -> respond_with_string "hello world")
        )
      in
      Deferred.never ()
    )

let () = Command.run command
