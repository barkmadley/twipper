open Core.Std
open Async.Std

open Middleware

open User
open Tweep


let users =
  {
    uuid = User_UUID.of_string "1234";
    name = "Someone"
  }

let tweeps =
  [
    {
      uuid = Tweep_UUID.of_string "12345";
      text = "blah";
      user_uuid = users.uuid;
    };
    {
      uuid = Tweep_UUID.of_string "123451";
      text = "something else";
      user_uuid = users.uuid;
    };
  ]

let tweep_to_html prefix suffix base_uri tweep =
  Printf.sprintf
"
%s
  <a rel=self href=\"/tweep/%s\">self</a>
  <a itemprop=user href=\"/user/%s\">User</a>
  <span itemprop=text>%s</span>
%s
"
  prefix
  (Tweep_UUID.to_string tweep.uuid)
  (User_UUID.to_string tweep.user_uuid)
  tweep.text
  suffix

(** [App]
  *)
module App : Middleware =
struct
  let uri = "/api"
  let run ~body request =
  begin
    let module R = Cohttp_async.Request in
    let module S = Cohttp_async.Server in
    let () = Printf.printf "%s\n" (Uri.path (R.uri request)) in
    match R.meth request, Uri.path (R.uri request) with
    | `GET, "/" ->
    begin

      S.respond_with_string (
        Printf.sprintf
"
<html>
  <body>
    <ul>
      %s
    </ul>
  </body>
</html>
"
        (String.concat ~sep:"\n" (List.map ~f:(tweep_to_html "<li>" "</li>" "/") tweeps))
      )
    end
    | `GET, uri when String.is_prefix uri ~prefix:"/tweep/" ->
    begin
      match String.chop_prefix ~prefix:"/tweep/" uri with
      | None -> S.respond_with_string "404" (* Should not happen *)
      | Some suffix ->
        let uuid_suffix = Tweep_UUID.of_string suffix in
        match List.findi ~f:(fun i t -> t.uuid = uuid_suffix) tweeps with
        | None -> S.respond_with_string "404"
        | Some (i, tweep) ->
          S.respond_with_string (
            Printf.sprintf
"
<html>
  <body>
      %s
  </body>
</html>
"
            (tweep_to_html "" "" "/" tweep)
          )
    end
    | _, uri ->
    begin
      S.respond_with_string ("not root:" ^ uri)
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
