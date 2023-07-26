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

let string_of_arg = function
  | E_aux (E_id id, _) -> "\"" ^ string_of_id id ^ "\""
  (* | exp -> invalid_decode ("call arg " ^ string_of_exp exp) *)
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
      print_endline ("<-- MP_app " ^ (string_of_id i));
  | MP_aux (MP_vector_concat ( mpl ), _) ->
      print_endline "MP_vector_concat";
      List.iter parse_mpat mpl;
  | MP_aux (MP_string_append ( pl ), _) ->
      print_endline "MP_string_append";
      List.iter parse_mpat pl
  | MP_aux (MP_typ ( mp, at ), _) ->
      print_endline "MP_typ";
      parse_mpat mp;
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
      | _ -> let b = List.concat (List.map string_list_of_mpat pl) in begin
           print_endline ("<-- MP_app" ^ string_of_id i);
           b
        end
      end
  | MP_aux (MP_vector_concat ( mpl ), _) ->
      print_endline "MP_vector_concat";
      List.concat (List.map string_list_of_mpat mpl);
  | MP_aux (MP_string_append ( pl ), _) ->
      print_endline "MP_string_append";
      List.concat (List.map string_list_of_mpat pl)
  | MP_aux (MP_typ ( mp, at ), _) ->
      print_endline "MP_typ";
      string_list_of_mpat mp;
  | _ -> assert false

let parse_MPat_aux p = match p with
  | MPat_aux ( MPat_pat (p), _ ) ->
      print_endline ("MPat_pat " ^ string_of_mpat p);
      parse_mpat p;
  | MPat_aux ( MPat_when (p, e), _ ) ->
      print_endline ("MPat_when " ^ (string_of_mpat p) ^ " when " ^ (string_of_exp e));
      parse_mpat p;
      parse_exp e;
  | _ -> print_endline "MCL_bidir other"

let string_lists_of_MPat_aux p = match p with
  | MPat_aux ( MPat_pat (p), _ ) ->
      (* print_endline ("MPat_pat " ^ string_of_mpat p); *)
      (* (string_list_of_mpat p, None) *)
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
        begin match pb with
        | MPat_aux ( MPat_pat (p), _ ) ->
            print_endline ("MPat_pat ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add encodings (string_of_id app_id) (string_list_of_mpat p);
        | MPat_aux ( MPat_when (p, e), _ ) ->
            print_endline ("MPat_when ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add encodings (string_of_id app_id) (string_list_of_mpat p);
        | _ ->
            print_endline ("assert ");
            assert false
        end
      end
  | _ -> assert false

let parse_encdec i mc = match mc with
  | MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      print_endline "MCL_bidir (left part)";
      begin match pa with
      | MPat_aux ( MPat_pat (p), _ ) ->
          print_endline ("MPat_pat ");
          parse_encdec_mpat p pb;
      | MPat_aux ( MPat_when (p, e), _ ) ->
          print_endline ("MPat_when ");
          parse_encdec_mpat p pb;
      | _ ->
          print_endline ("assert ");
          assert false
      end;
  | _ -> assert false

let parse_assembly_mpat mp pb = match mp with
  | MP_aux (MP_app ( app_id, mpl ), _) ->
      print_endline ("MP_app " ^ string_of_id app_id);
      let operandl = List.concat (List.map string_list_of_mpat mpl) in
      begin
        List.iter print_endline operandl;
        Hashtbl.add operands (string_of_id app_id) operandl;
        print_endline "MCL_bidir (right part)";
        begin match pb with
        | MPat_aux ( MPat_pat (p), _ ) ->
            print_endline ("MPat_pat ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add assembly (string_of_id app_id) (string_list_of_mpat p);
        | MPat_aux ( MPat_when (p, e), _ ) ->
            print_endline ("MPat_when ");
            List.iter print_endline (string_list_of_mpat p);
            Hashtbl.add assembly (string_of_id app_id) (string_list_of_mpat p);
        | _ ->
            print_endline ("assert ");
            assert false
        end
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

let parse_SD_mapcl i mc =
  print_endline ("SD_mapcl " ^ string_of_id i);
  if string_of_id i = "encdec" then
  begin
    print_endline "ENCDEC!";
    ignore (parse_encdec i mc);
  end
  else if string_of_id i = "assembly" then
  begin
    print_endline "ASSEMBLY!";
    parse_assembly i mc;
  end;
  begin match mc with
  | MCL_aux ( MCL_bidir ( pa, pb ), _ ) ->
      parse_MPat_aux pa;
      parse_MPat_aux pb
  | _ -> print_endline "mapcl other"
  end

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
  begin match fcl with
  | FCL_aux ( FCL_Funcl ( i, Pat_aux ( j, _ ) ), _ ) ->
      print_endline ("FCL_Funcl " ^ string_of_id i);
      if (string_of_id i) = "execute" then begin
        match j with
        | Pat_exp ( p, e ) -> (* parse_exp e *)
            print_endline "Pat_exp";
            print_endline (string_of_pat p);
            print_endline "pat -> ";
            let x = match p with
                  P_aux ( P_app (x, pl), _ ) ->
                    print_endline ("P_app " ^ string_of_id x);
                | _ -> print_endline "pat other"
              in ();
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
            parse_execute p e;
        | _ -> raise (Failure "FCL_Funcl other")
      end
  | _ -> raise (Failure "SD_funcl other")
  end

let parse_SD_unioncl i ucl =
  print_endline ("SD_unioncl " ^ string_of_id i);
  begin match ucl with
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
            )
            x;
          let l = List.map string_of_typ x in
            Hashtbl.add sigs (string_of_id d) l;
      | _ -> print_endline "Tu_ty_id other"
      end;
      print_endline ")"
  | _ -> print_endline "SD_unioncl other"
  end

let parse_DEF_type def =
  print_endline "DEF_type";
  begin match def with
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
  end

let riscv_decode_info ast env =
  List.iter (fun def ->
    match def with
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
    | DEF_type ( def ) -> parse_DEF_type def
    | _ -> print_string ""
  ) ast.defs;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) types;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) sigs;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) operands;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) encodings;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ Util.string_of_list ", " (fun x -> x) v)) assembly;
  Hashtbl.iter (fun k v -> print_endline (k ^ ":" ^ v)) functions;
  exit 0

let _ =
  Target.register
    ~name:"riscv_decode"
    ~pre_descatter_hook:riscv_decode_info
    (fun _ _ _ _ _ -> ())
