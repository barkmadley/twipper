
open String_uuid

module User_UUID : StringUUID

type user =
  {
    uuid: User_UUID.t;
    name: string;
  }
