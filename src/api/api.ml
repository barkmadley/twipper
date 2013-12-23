open Core.Std
open Async.Std

open Cow

open Middleware

open User
open Tweep

type ('inline, 'linked) inlined_html_t =
  | Inline of 'inline
  | Linked of 'linked

let html_of_inlined_html_t html_of_inline html_of_linked inlined_html_t =
  match inlined_html_t with
  | Inline inline -> html_of_inline inline
  | Linked linked -> html_of_linked linked

type self_uri_t = Uri.t
type prop_uri_t = (string * Uri.t)

let html_of_self_uri_t uri_t =
  <:html<
    <a rel="self" href="$str:Uri.to_string uri_t$">self</a>
  >>

let html_of_prop_uri_t (prop,uri_t) =
  <:html<
    <a itemprop="$str:prop$" href="$str:Uri.to_string uri_t$">$str:prop$</a>
  >>

type user_html_t =
  {
    self : self_uri_t;
    uuid : User_UUID.t;
    handle : string;
  }

let html_of_user_html_t (user_html_t: user_html_t) =
  <:html<
    <div id="$str:User_UUID.to_string user_html_t.uuid$" itemscope="itemscope" itemtype="http://schema.org/Person">
      $ html_of_self_uri_t user_html_t.self $
      <div itemprop="handle">$str: user_html_t.handle $</div>
    </div>
  >>

let html_of_prop_user_html_t (prop, user_html_t) =
  <:html<
    <div itemprop="$str:prop$" itemscope="itemscope" itemtype="http://schema.org/Person">
      $ html_of_self_uri_t user_html_t.self $
      <div itemprop="handle">$str: user_html_t.handle $</div>
    </div>
  >>

let user_to_user_html_t to_user_uri (user : user) =
  {
    self = Uri.of_string (to_user_uri user.uuid);
    uuid = user.uuid;
    handle = user.name;
  }

type tweep_html_t =
  {
    self : Uri.t;
    uuid : Tweep_UUID.t;
    user : ((string * user_html_t), prop_uri_t) inlined_html_t;
    text : Html.t;
  }

let html_of_tweep_html_t (tweep_html_t: tweep_html_t) =
  <:html<
    <div id="$str:Tweep_UUID.to_string tweep_html_t.uuid$" itemscope="itemscope" itemtype="test">
      $ html_of_self_uri_t tweep_html_t.self $
      $ html_of_inlined_html_t html_of_prop_user_html_t html_of_prop_uri_t tweep_html_t.user $
      <div itemprop="text">$ tweep_html_t.text $</div>
    </div>
  >>

let tweep_to_tweep_html_t to_user_uri to_tweep_uri ?user tweep =
  let user =
    match user with
    | None -> Linked ("user", Uri.of_string (to_user_uri tweep.user_uuid))
    | Some user -> Inline ("user", user_to_user_html_t to_user_uri user)
  in
  {
    self = Uri.of_string (to_tweep_uri tweep.uuid);
    uuid = tweep.uuid;
    text = <:html<$str: tweep.text$>>;
    user = user;
  }

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

let tweep_uuid_to_tweep_uri base_uri tweep_uuid =
  Printf.sprintf "%s/tweep/%s" base_uri (Tweep_UUID.to_string tweep_uuid)

let tweep_uri base_uri (tweep: tweep) =
  tweep_uuid_to_tweep_uri base_uri tweep.uuid

let user_uuid_to_user_uri base_uri user_uuid =
  Printf.sprintf "%s/user/%s" base_uri (User_UUID.to_string user_uuid)

let user_uri base_uri user_uuid =
  user_uuid_to_user_uri base_uri user_uuid

let tweep_form post_uri user_uuid =
  <:html<
    <form data-rel="tweep" method="POST" action="$str:post_uri$">
      <input type="hidden" name="user_uuid" value="$str:User_UUID.to_string user_uuid$" />
      <input type="text" name="text" />
    </form>
  >>

let tweep_list tweep_html_ts =
  let tweep_htmls : Html.t list =
    List.map ~f:html_of_tweep_html_t tweep_html_ts
  in
  let tweep_htmls : Html.t list =
    List.map ~f:(fun html -> <:html< <li>$html$</li> >>) tweep_htmls
  in
  <:html<
    <ul>
      $list:tweep_htmls$
    </ul>
  >>

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
    | `GET, "/inline-repr" ->
    begin
      (** Inline the representation of users by passing
        * the user flag to tweep_to_tweep_html_t
        *)
      let tweep_html_ts : tweep_html_t list =
        List.map ~f:(fun tweep ->
          tweep_to_tweep_html_t (user_uuid_to_user_uri host) (tweep_uuid_to_tweep_uri host) ~user:users tweep
        ) !tweeps
      in
      let post_uri = host ^ "/tweep" in
      let html =
        <:html<
          <html>
            <body>
              $ tweep_list tweep_html_ts $
              $ tweep_form post_uri users.uuid $
            </body>
          </html>
        >>
      in
      S.respond_with_string (Html.to_string html)
    end
    | `GET, "/inline-link" ->
    begin
      (** Inline the links of users by not passing the user flag to
        * tweep_to_tweep_html_t, instead passing a custom uri function
        * that refers to an html node in the local document
        *)
      let user_html_t =
        user_to_user_html_t (user_uuid_to_user_uri host) users
      in
      let user_html = html_of_user_html_t user_html_t in
      let user_inline_link user_uuid =
        "#" ^ (User_UUID.to_string user_uuid)
      in
      let tweep_html_ts : tweep_html_t list =
        List.map ~f:(fun tweep ->
          tweep_to_tweep_html_t user_inline_link (tweep_uuid_to_tweep_uri host) tweep
        ) !tweeps
      in
      let post_uri = host ^ "/tweep" in
      let html =
        <:html<
          <html>
            <body>
              $ user_html $
              $ tweep_list tweep_html_ts $
              $ tweep_form post_uri users.uuid $
            </body>
          </html>
        >>
      in
      S.respond_with_string (Html.to_string html)
    end
    | `GET, "/" ->
    begin
      (** Don't inline the links to users by not passing the user flag to
        * tweep_to_tweep_html_t. The user uri function refers to the canonical
        * uri for that user
        *)
      let tweep_html_ts : tweep_html_t list =
        List.map ~f:(fun tweep ->
          tweep_to_tweep_html_t (user_uuid_to_user_uri host) (tweep_uuid_to_tweep_uri host) tweep
        ) !tweeps
      in
      let post_uri = host ^ "/tweep" in
      let html =
        <:html<
          <html>
            <body>
              $ tweep_list tweep_html_ts $
              $ tweep_form post_uri users.uuid $
            </body>
          </html>
        >>
      in
      S.respond_with_string (Html.to_string html)
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
      return (S.respond ~headers `Created ~body)
    end
    | `GET, uri when String.is_prefix uri ~prefix:"/tweep/" ->
    begin
      match String.chop_prefix ~prefix:"/tweep/" uri with
      | None -> S.respond_with_string "404" (* Should not happen *)
      | Some suffix ->
        let uuid_suffix = Tweep_UUID.of_string suffix in
        match List.findi ~f:(fun i (t:tweep) -> t.uuid = uuid_suffix) !tweeps with
        | None -> S.respond_with_string "404"
        | Some (i, tweep) ->
          let tweep_html_t =
            tweep_to_tweep_html_t (user_uuid_to_user_uri host) (tweep_uuid_to_tweep_uri host) tweep
          in
          let html =
            <:html<
              <html>
                <body>
                  $html_of_tweep_html_t tweep_html_t$
                </body>
              </html>
            >>
          in
          S.respond_with_string (Html.to_string html)
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
