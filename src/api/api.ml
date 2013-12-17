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

let tweeps = ref
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

let tweep_uri base_uri tweep =
  Printf.sprintf "%s/tweep/%s" base_uri (Tweep_UUID.to_string tweep.uuid)

let user_uri base_uri user_uuid =
  Printf.sprintf "%s/user/%s" base_uri (User_UUID.to_string user_uuid)

let tweep_to_html prefix suffix base_uri tweep =
  Printf.sprintf
"
%s
<div itemtype=\"\">
  <a rel=self href=\"%s\">self</a>
  <a itemprop=user href=\"%s\">User</a>
  <span itemprop=text>%s</span>
</div>
%s
"
  prefix
  (tweep_uri base_uri tweep)
  (user_uri base_uri tweep.user_uuid)
  tweep.text
  suffix

(** [App]
  *)
module App : Middleware =
struct
  let uri = "/api"
  let run ~body ~host request =
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
    <form data-rel=\"tweep\" method=\"POST\" action=\"%s/tweep\">
      <input type=\"hidden\" name=\"user_uuid\" value=\"%s\" />
      <input type=\"text\" name=\"text\" />
    </form>
  </body>
</html>
"
        ( String.concat ~sep:"\n" (
            List.map ~f:(tweep_to_html "<li>" "</li>" host) !tweeps)
        )
        host
        (User_UUID.to_string users.uuid)
      )
    end
    | `POST, "/tweep" ->
    begin
      body >>= fun body ->
      let post_body = Uri.query_of_encoded body in
      let text_o = List.Assoc.find post_body "text" in
      let text_s = Option.value text_o ~default:[] in
      let text = Option.value (List.hd text_s) ~default:"" in
      let new_tweep =
        {
          uuid = Tweep_UUID.of_string "testing";
          text = text;
          user_uuid = users.uuid;
        }
      in
      tweeps := new_tweep :: !tweeps;
      let headers =
        Cohttp.Header.init_with "location" (tweep_uri host new_tweep)
      in
      let body = Pipe.of_list [body] in
      return (S.respond ~headers `Created ~body:(Some body))
    end
    | `GET, uri when String.is_prefix uri ~prefix:"/tweep/" ->
    begin
      match String.chop_prefix ~prefix:"/tweep/" uri with
      | None -> S.respond_with_string "404" (* Should not happen *)
      | Some suffix ->
        let uuid_suffix = Tweep_UUID.of_string suffix in
        match List.findi ~f:(fun i t -> t.uuid = uuid_suffix) !tweeps with
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
            (tweep_to_html "" "" host tweep)
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
let command start_server port_arg host_arg : string * Command.t =
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
      step (fun m port host -> m ~port ~host)
      +> port_arg
      +> host_arg
    )
    (** The command-line spec determines the argument to this function, which
      * show up in an order that matches the spec.
      *)
    (start_server App.run)
