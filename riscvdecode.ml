open Libsail
open Ast
open Ast_defs
open Ast_util

let types = Hashtbl.create 997
let sigs = Hashtbl.create 997
let operands = Hashtbl.create 997
let encodings = Hashtbl.create 997
let assembly = Hashtbl.create 997
let functions = Hashtbl.create 997
let op_functions = Hashtbl.create 997
let inames = Hashtbl.create 997
let idescriptions = Hashtbl.create 997
let iformats = Hashtbl.create 997

let string_of_arg = function
  | E_aux (E_id id, _) -> "\"" ^ string_of_id id ^ "\""
  | exp -> ("exp " ^ string_of_exp exp)

let rec parse_exp e = match e with
  | E_aux (E_app (f, args), _) ->
      print_endline ("E_app \"" ^ string_of_id f ^ "\" [" ^ Util.string_of_list ", " string_of_arg args ^ "]")
  | _ -> print_endline ("parse_exp other" ^ string_of_exp e)

let rec parse_mpat x = match x with
  | MP_aux (MP_lit ( l ), _) -> print_endline ("MP_lit " ^ string_of_lit l)
  | MP_aux (MP_id ( i ), _) -> print_endline ("MP_id " ^ string_of_id i)
  | MP_aux (MP_app ( i, pl ), _) ->
      print_endline ("MP_app " ^ (string_of_id i) ^ " -->");
      List.iter parse_mpat pl;
      print_endline ("<-- MP_app " ^ (string_of_id i))
  | MP_aux (MP_vector_concat ( mpl ), _) ->
      print_endline "MP_vector_concat";
      List.iter parse_mpat mpl
  | MP_aux (MP_string_append ( pl ), _) ->
      print_endline "MP_string_append";
      List.iter parse_mpat pl
  | MP_aux (MP_typ ( mp, at ), _) ->
      print_endline "MP_typ";
      parse_mpat mp
  | _ -> print_endline "mpat other"

let rec string_list_of_mpat x = match x with
  | MP_aux (MP_lit ( l ), _) ->
      print_endline ("MP_lit " ^ string_of_lit l);
      [ string_of_lit l ]
  | MP_aux (MP_id ( i ), _) ->
      print_endline ("MP_id " ^ string_of_id i);
      [ string_of_id i ]
  | MP_aux (MP_app ( i, pl ), _) ->
      print_endline ("MP_app " ^ string_of_id i);
      begin match string_of_id i with
      | "spc" | "sep" -> []
      | _ -> let b = List.concat (List.map string_list_of_mpat pl) in
          begin
            print_endline ("<-- MP_app" ^ string_of_id i);
            [ (string_of_id i) ^ "(" ^ (String.concat "," b) ^ ")" ]
          end
      end
  | MP_aux (MP_vector_concat ( mpl ), _) ->
      print_endline "MP_vector_concat";
      List.concat (List.map string_list_of_mpat mpl)
  | MP_aux (MP_string_append ( pl ), _) ->
      print_endline "MP_string_append";
      List.concat (List.map string_list_of_mpat pl)
  | MP_aux (MP_typ ( mp, at ), _) ->
      print_endline "MP_typ";
      string_list_of_mpat mp
  | _ -> assert false

let parse_MPat_aux p = match p with
  | MPat_aux ( MPat_pat (p), _ ) ->
      print_endline ("MPat_pat " ^ string_of_mpat p);
      parse_mpat p
  | MPat_aux ( MPat_when (p, e), _ ) ->
      print_endline ("MPat_when " ^ (string_of_mpat p) ^ " when " ^ (string_of_exp e));
      parse_mpat p;
      parse_exp e
  | _ -> print_endline "MCL_bidir other"

let string_lists_of_MPat_aux p = match p with
  | MPat_aux ( MPat_pat (p), _ ) ->
      (* print_endline ("MPat_pat " ^ string_of_mpat p); *)
      (string_list_of_mpat p, None)
  | MPat_aux ( MPat_when (p, e), _ ) ->
      (* print_endline ("MPat_when " ^ (string_of_mpat p) ^ " when " ^ (string_of_exp e)); *)
      (string_list_of_mpat p, Some (string_of_exp e))
  | _ -> assert false

let parse_encdec_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      let operandl = List.concat (List.map string_list_of_mpat mpl) in
      begin
        List.iter print_endline operandl;
        Hashtbl.add operands (string_of_id app_id) operandl;
        print_endline "MCL_bidir (right part)";
        match pb with
        | MPat_aux ( MPat_pat (p), _ ) ->
            print_endline ("MPat_pat ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add encodings (string_of_id app_id) (string_list_of_mpat p)
        | MPat_aux ( MPat_when (p, e), _ ) ->
            print_endline ("MPat_when ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add encodings (string_of_id app_id) (string_list_of_mpat p)
        | _ ->
            print_endline ("assert ");
            assert false
      end
  | _ -> assert false

let parse_encdec i mc = match mc with
  | MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline "MCL_bidir (left part)";
      begin match pa with
      | MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat ");
          parse_encdec_mpat p pb
      | MPat_aux ( MPat_when (p, e), _ ) ->
          print_endline ("MPat_when ");
          parse_encdec_mpat p pb
      | _ ->
          print_endline ("assert ");
          assert false
      end
  | _ -> assert false

let add_assembly app_id p = 
  let x = string_list_of_mpat p in
    begin
      (* We only support "simple" assembly at the moment,
         where the quoted literal mnemonic is in the statement. *)
      if String.get (List.hd x) 0 = '"' then begin
        print_endline ("assembly.add " ^ string_of_id app_id ^ " : " ^ List.hd x);
        Hashtbl.add assembly (string_of_id app_id) x
      end
    end

let parse_assembly_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      let operandl = List.concat (List.map string_list_of_mpat mpl) in
      begin
        List.iter print_endline operandl;
        Hashtbl.add operands (string_of_id app_id) operandl;
        print_endline "MCL_bidir (right part)";
        match pb with
        | MPat_aux ( MPat_pat (p), _ ) ->
            print_endline ("MPat_pat assembly");
            add_assembly app_id p
        | MPat_aux ( MPat_when (p, e), _ ) ->
            print_endline ("MPat_when assembly");
            add_assembly app_id p
        | _ ->
            print_endline ("assert ");
            assert false
      end
  | _ -> assert false

let parse_assembly i mc = match mc with
  | MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline "MCL_bidir";
      begin match pa with
      | MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat ");
          parse_assembly_mpat p pb
      | MPat_aux ( MPat_when (p, e), _ ) ->
          print_endline ("MPat_when ");
          parse_assembly_mpat p pb
      | _ ->
          print_endline ("assert ");
          assert false
      end
  | _ -> assert false

let add_iname app_id p =
  let x = string_list_of_mpat p in begin
    print_endline ("add_iname " ^ (string_of_id app_id) ^ ":" ^ (String.concat "; " x));
    Hashtbl.add inames (string_of_id app_id) (String.concat "; " x)
  end

let parse_iname_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      print_endline "MCL_bidir (right part)";
      begin match pb with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat iname");
          add_iname app_id p
      | _ ->
          print_endline ("assert ");
          assert false
      end
  | _ -> assert false

let parse_iname i mc = match mc with
    MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline ("MCL_bidir " ^ string_of_id i);
      begin match pa with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat TBD");
          parse_iname_mpat p pb
      | _ -> assert false
      end
  | _ -> assert false

let add_idescription app_id p =
  let x = string_list_of_mpat p in begin
    print_endline ("add_idescription " ^ (string_of_id app_id) ^ ":" ^ (String.concat " " x));
    List.iter (fun s -> print_endline (String.sub s 1 (String.length s - 2))) x;
    (* Convert a list of quoted strings into a single string. *)
    let description = String.concat " " (List.map (fun s -> (String.sub s 1 (String.length s - 2))) x) in
      Hashtbl.add idescriptions (string_of_id app_id) description
  end

let parse_idescription_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      print_endline "MCL_bidir (right part)";
      begin match pb with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat idescription");
          add_idescription app_id p
      | _ ->
          print_endline ("assert ");
          assert false
      end
  | _ -> assert false

let parse_idescription i mc = match mc with
    MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline ("MCL_bidir " ^ string_of_id i);
      begin match pa with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat TBD");
          parse_idescription_mpat p pb
      | _ -> assert false
      end
  | _ -> assert false

let add_iformat app_id p = 
  let x = string_list_of_mpat p in begin
    print_endline ("add_iformat " ^ (string_of_id app_id) ^ ":" ^ (String.concat " " x));
    List.iter (fun s -> print_endline (String.sub s 1 (String.length s - 2))) x;
    Hashtbl.add iformats (string_of_id app_id) (String.concat " " (List.map (fun s -> (String.sub s 1 (String.length s - 2))) x));
    (* Convert a list of quoted strings into a single string. *)
    let format = String.concat " " (List.map (fun s -> (String.sub s 1 (String.length s - 2))) x) in
      Hashtbl.add iformats (string_of_id app_id) format;
  end

let parse_iformat_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      print_endline "MCL_bidir (right part)";
      begin match pb with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat iformat");
          add_iformat app_id p
      | _ ->
          print_endline ("assert ");
          assert false
      end
  | _ -> assert false

let parse_iformat i mc = match mc with
    MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline ("MCL_bidir " ^ string_of_id i);
      begin match pa with
        MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat TBD");
          parse_iformat_mpat p pb
      | _ -> assert false
      end
  | _ -> assert false

let parse_SD_mapcl i mc =
  print_endline ("SD_mapcl " ^ string_of_id i);
  match string_of_id i with
    "encdec" ->
      print_endline "ENCDEC!";
      parse_encdec i mc
  | "assembly" ->
      print_endline "ASSEMBLY!";
      parse_assembly i mc
  | "iname" -> (* experimental *)
      print_endline "INAME!";
      parse_iname i mc
  | "idescription" -> (* experimental *)
      print_endline "IDESCRIPTION!";
      parse_idescription i mc
  | "iformat" -> (* experimental *)
      print_endline "IFORMAT!";
      parse_iformat i mc
  | _ -> ();
  match mc with
  | MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      parse_MPat_aux pa;
      parse_MPat_aux pb
  | _ -> print_endline "mapcl other"

let parse_execute p e =
  let x = match p with
        P_aux ( P_app (i, pl), _ ) ->
          print_endline ("P_app " ^ string_of_id i);
          string_of_id i
      | _ -> raise (Failure "pat other")
    in begin
      print_endline "<- pat";
      print_endline "exp -> ";
      print_endline (string_of_exp e);
      print_endline "<- exp";
      Hashtbl.add functions x (string_of_exp e)
    end

let parse_SD_funcl fcl =
  print_endline "SD_funcl";
  match fcl with
  | FCL_aux ( FCL_Funcl ( i, Pat_aux ( j, _ ) ), _ ) ->
      print_endline ("FCL_Funcl " ^ string_of_id i);
      if (string_of_id i) = "execute" then begin
        match j with
        | Pat_exp ( p, e ) -> (* parse_exp e *)
            print_endline "Pat_exp";
            print_endline (string_of_pat p);
            print_endline "pat -> ";
            begin match p with
                P_aux ( P_app (x, pl), _ ) ->
                  print_endline ("P_app " ^ string_of_id x);
              | _ -> print_endline "pat other"
            end;
            print_endline "<- pat";
            print_endline "exp -> ";
            print_endline (string_of_exp e);
            print_endline "<- exp";
            parse_execute p e
        | Pat_when ( p, e, w ) ->
            print_endline "Pat_when";
            print_endline (string_of_pat p);
            print_endline (string_of_exp e);
            print_endline (string_of_exp w);
            parse_execute p e
        | _ -> raise (Failure "FCL_Funcl other")
      end
  | _ -> raise (Failure "SD_funcl other")

let parse_SD_unioncl i ucl =
  print_endline ("SD_unioncl " ^ string_of_id i);
  match ucl with
  | Tu_aux ( Tu_ty_id ( c, d ), _ ) ->
      print_string ("Tu_ty_id " ^ string_of_id d ^ "(");
      (* print_endline (string_of_typ c); *)
      begin match c with
      | Typ_aux ( Typ_tup ( x ), _ ) ->
          (* Typ_tuple in later versions of sail *)
          List.iter (fun x0 ->
              let type_name = string_of_typ x0 in
                let type_type = try Hashtbl.find types (string_of_typ x0)
                  with Not_found -> "None"
                in print_string (type_name ^ ":" ^ type_type ^ " ")
          ) x;
          let l = List.map string_of_typ x in
            Hashtbl.add sigs (string_of_id d) l;
      | _ -> print_endline "Tu_ty_id other"
      end;
      print_endline ")"
  | _ -> print_endline "SD_unioncl other"

let parse_DEF_type def =
  print_endline "DEF_type";
  match def with
  | TD_aux (TD_abbrev (d, e, f), _) ->
    print_endline ( "TD_abbrev " ^ string_of_id d ^ ":" ^ string_of_typ_arg f);
    Hashtbl.add types (string_of_id d) (string_of_typ_arg f);
    (* print_endline ( "TD_abbrev " ^ string_of_typquant e ); *)
    (* print_endline ( "TD_abbrev " ^ string_of_typ_arg f ); *)
    (*
    begin match e with
    | TypQ_aux ( x, _ ) ->
        print_endline "TypQ_aux";
        begin match x with
        | TypQ_tq ( y ) ->
            print_endline "TypQ_tq";
            List.iter (fun y0 -> begin match y0 with
              | QI_aux ( z, _ ) ->
                print_endline "QI_aux";
                begin match z with
                | QI_id ( a ) -> print_endline "QI_id";
                | QI_constraint ( a ) -> print_endline "QI_constraint"
                | _ -> print_endline "QI_aux other";
                end
              | _ -> print_endline "TypQ_tq other"
              end
            ) y
        | TypQ_no_forall -> print_endline "TypQ_no_forall"
        | _ -> print_endline "TypQ_aux other";
        end
    | _ -> print_endline "typquant other"
    end
    *)
  | TD_aux ( TD_record (d, e, f, g), _) -> print_endline ( "TD_record " ^ string_of_id d )
  | TD_aux ( TD_variant (d, e, f, g), _) -> print_endline ( "TD_variant " ^ string_of_id d )
  | TD_aux ( TD_enum (d, e, f), _) -> print_endline ( "TD_enum " ^ string_of_id d )
  | TD_aux ( TD_bitfield (d, e, f), _) -> print_endline ( "TD_bitfield " ^ string_of_id d )
  | _ -> print_endline "DEF_type other"

let json_of_operand op = "\"" ^ op ^ "\""

let json_of_key_operand key op t =
  "\n{\n" ^
  "  \"name\": \"" ^ op ^ "\", \"type\": \"" ^ t ^ "\"\n" ^
  "}"

let json_of_operands k =
  let ops = Hashtbl.find_opt operands k
  and types = Hashtbl.find_opt sigs k in
    match (ops, types) with
      (Some opslist, Some typeslist) ->
        let fk = json_of_key_operand k in
          String.concat ", " (List.map2 fk opslist typeslist)
    | (_, _) -> ""

let rec basetype t =
  match Hashtbl.find_opt types t with
    None -> t
  | Some bt -> basetype bt

let string_of_sizeof_field k f =
  if String.starts_with ~prefix:"0b" f then string_of_int (String.length f - 2)
  else if String.contains f '(' then
    let op_func = List.hd (String.split_on_char '(' f) in
      Hashtbl.find op_functions op_func
  else begin
    print_endline ("sizeof " ^ k ^ " " ^ f);
    let opmap = List.combine (Hashtbl.find operands k) (Hashtbl.find sigs k) in
      begin match List.assoc_opt f opmap with
        Some t ->
          let bt = basetype t in
            if bt = "bool" then
              "1"
            else if String.starts_with ~prefix:"bits(" bt then
              List.hd (String.split_on_char ')' (List.hd (List.tl ((String.split_on_char '(' bt)))))
            else begin
              print_endline ("unfamiliar base type " ^ bt);
              "72"
            end
      | None ->
          print_endline ("not found " ^ f);
          "0"
      end
  end

let json_of_field k f =
  "{ \"field\": \"" ^ f ^ "\", \"size\": " ^ string_of_sizeof_field k f ^ " }"

let json_of_fields k =
  match Hashtbl.find_opt encodings k with
    None -> ""
  | Some (fields) -> String.concat ", " (List.map (fun f -> json_of_field k f) fields)

let json_of_function k =
  match Hashtbl.find_opt functions k with
    None -> ""
  | Some (f) -> String.escaped f

let json_of_iname k =
  match Hashtbl.find_opt inames k with
    None -> "\"TBD\""
  | Some (s) -> s

let json_of_idescriptions k =
  match Hashtbl.find_opt idescriptions k with
    None -> "\"TBD\""
  | Some (s) -> "\"" ^ (String.escaped s) ^ "\""

let json_of_iformat k =
  match Hashtbl.find_opt iformats k with
    None -> "\"TBD\""
  | Some (s) -> "\"" ^ s ^ "\""

let json_of_instruction k =
  let m = Hashtbl.find assembly k in
    "{\n" ^
    "  \"mnemonic\": " ^ List.hd m ^ ",\n" ^
    "  \"name\": " ^ (json_of_iname k) ^ ",\n" ^
    "  \"operands\": [ " ^ (json_of_operands k) ^ " ],\n" ^
    "  \"format\": " ^ (json_of_iformat k) ^ ",\n" ^
    "  \"fields\": [ " ^ (json_of_fields k) ^ " ],\n" ^
    "  \"function\": \"" ^ (json_of_function k) ^ "\",\n" ^
    "  \"description\": " ^ (json_of_idescriptions k) ^ "\n" ^
    "}"

let rec parse_typ name t = match t with
    Typ_aux (Typ_bidir (tl, tr), _) ->
      print_endline "Typ_bidir";
      parse_typ name tl; parse_typ name tr
  | Typ_aux (Typ_app (id, args), _) -> print_endline (string_of_id id);
      print_endline (string_of_id id ^ "(" ^ (String.concat ", " (List.map string_of_typ_arg args)) ^ ")");
      begin match string_of_id id with
          "bitvector" ->
            print_endline (string_of_typ_arg (List.hd args));
            Hashtbl.add op_functions name (string_of_typ_arg (List.hd args))
        | _ -> print_endline "Typ_app other"
      end
  | _ -> print_endline "typ other"

let parse_typschm name ts = match ts with
    TypSchm_aux ( TypSchm_ts ( _, x ), _ ) ->
      parse_typ name x (* This compiles as if x is a 'typ' instead of 'atyp' (?) *)
  | _ -> assert false

let riscv_decode_info ast env =
  List.iter (fun def ->
    match def with
      DEF_type ( def ) -> parse_DEF_type def
    | DEF_spec ( vs ) ->
        print_endline "DEF_spec";
        begin match vs with
          VS_aux ( VS_val_spec (ts, i, _, _), _) ->
            parse_typschm (string_of_id i) ts
        end
    | DEF_scattered ( def ) ->
        begin match def with
        | SD_aux ( SD_funcl ( fcl ), _ ) -> parse_SD_funcl fcl
        | SD_aux ( SD_function ( _, _, _ ), _ ) -> print_endline "SD_function"
        | SD_aux ( SD_variant ( _, _ ), _ ) -> print_endline "SD_variant"
        | SD_aux ( SD_unioncl ( i, ucl ), _ ) -> parse_SD_unioncl i ucl
        | SD_aux ( SD_mapping ( _, _ ), _ ) -> print_endline "SD_mapping"
        | SD_aux ( SD_mapcl ( i, mc ), _ ) -> parse_SD_mapcl i mc
        | _ -> print_endline "b"
        end
    | _ -> print_string ""
  ) ast.defs;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) types;
  print_endline "sigs";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) sigs;
  print_endline "operands";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) operands;
  print_endline "encodings";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) encodings;
  print_endline "assembly";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) assembly;
  print_endline "functions";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) functions;
  print_endline "op_functions";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) op_functions;
  print_endline "inames";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) inames;
  print_endline "idescriptions";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) idescriptions;
  print_endline "iformats";
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) iformats;
  print_endline "{";
  print_endline "  \"instructions\": [";
  (* Filter out keys which have no match in 'assembly'. *)
  Hashtbl.iter (fun k v -> if Hashtbl.find_opt assembly k == None then Hashtbl.remove sigs k) sigs;
  print_endline (String.concat ",\n" (List.map json_of_instruction (Hashtbl.fold (fun k v init -> k :: init) sigs [])));
  print_endline "  ]";
  print_endline "}";
  exit 0

let _ =
  Target.register
    ~name:"riscv_decode"
    ~pre_descatter_hook:riscv_decode_info
    (fun _ _ _ _ _ -> ())
