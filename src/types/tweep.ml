open Core.Std

open User

open String_uuid

module Tweep_UUID : StringUUID = String_uuid

type tweep =
  {
    uuid: Tweep_UUID.t;
    text: string;
    user_uuid: User_UUID.t;
  }
