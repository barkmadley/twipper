
open String_uuid

module User_UUID : StringUUID = String_uuid

type user =
  {
    uuid: User_UUID.t;
    name: string;
  }
