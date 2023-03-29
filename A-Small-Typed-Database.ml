let proof_of_bug = [|
  {code=0; contact={name="foo"; phone_number=1,2,3,4}};
  {code=0; contact={name="bar"; phone_number=2,2,3,4}};
  {code=1; contact={name="foo"; phone_number=1,2,3,4}};
  {code=2; contact={name="bar"; phone_number=2,2,3,4}};
|] ;;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let name = contact.name in
    let keep contact = contact.name <> name in
    let contacts' = Array.of_list ((List.filter keep (Array.to_list db.contacts)) @ [nobody])
    in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = contacts';
    }
    in
    (true, db', contact);;

let update db contact =
  let (status, _, _) as r = insert db contact in
  if status then r
  else if db.number_of_contacts < Array.length db.contacts then
    let rename c = {c with phone_number = contact.phone_number} in
    let touch c =
      if c.name = contact.name then rename c
      else c
    in
    let db' = {
      number_of_contacts = db.number_of_contacts;
      contacts = Array.map touch db.contacts;
    } in
    (true, db', contact)
  else
    (false, db, contact)

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
