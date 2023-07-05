open Libsail
open Ast
open Ast_defs
open Ast_util

let string_of_arg = function
  | E_aux (E_id id, _) -> "\"" ^ string_of_id id ^ "\""
  (* | exp -> invalid_decode ("call arg " ^ string_of_exp exp) *)
  | exp -> ("exp " ^ string_of_exp exp)

let rec parse_Pat_exp x exp = match exp with
  | E_aux (E_block exps, _) ->
      print_endline "parse_Pat_exp E_block-last";
      parse_Pat_exp x (Util.last exps)
  | E_aux (E_var (_, _, exp), _) ->
      print_endline "parse_Pat_exp E_var";
      parse_Pat_exp x exp
  | E_aux (E_app (f, args), _) ->
      print_endline ("parse_Pat_exp E_app \"" ^ string_of_id f ^ "\" [" ^ Util.string_of_list ", " string_of_arg args ^ "]")
  | _ -> print_endline ("parse_Pat_exp invalid decode " ^ string_of_exp exp)

let parse_MPat_aux p = match p with
  | MPat_aux ( MPat_pat (p), _ ) -> print_endline ("MPat_pat " ^ string_of_mpat p)
  | MPat_aux ( MPat_when (p, e), _ ) -> print_endline ("MPat_when " ^ (string_of_mpat p) ^ " when " ^ (string_of_exp e))
  | _ -> print_endline "MCL_bidir other"

let parse_MCL_bidir pa pb =
  parse_MPat_aux pa;
  parse_MPat_aux pb

let parse_SD_mapcl i mc =
  print_endline ("SD_mapcl " ^ string_of_id i);
  begin match mc with
  | MCL_aux ( MCL_bidir ( a, b ), _ ) -> parse_MCL_bidir a b
  | _ -> print_endline "mapcl other"
  end

let parse_SD_funcl fcl =
  print_endline "SD_funcl";
  begin match fcl with
  | FCL_aux ( e, f ) ->
      print_endline "FCL_aux";
      begin match e with
      | FCL_Funcl ( g, h ) ->
          print_endline "FCL_Funcl";
          begin match h with
          | Pat_aux ( i, j ) ->
              print_endline "Pat_aux";
              begin match i with
              | Pat_when ( j, k, l ) -> print_endline "Pat_when"
              | Pat_exp ( m, n ) -> parse_Pat_exp m n
              | _ -> print_endline "Pat_aux other"
              end
          | _ -> print_endline "FCL_Funcl other"
          end
      | _ -> print_endline "FCL_aux other"
      end
  | _ -> print_endline "SD_funcl other"
  end

let parse_SD_unioncl i ucl =
  print_endline ("SD_unioncl " ^ string_of_id i);
  begin match ucl with
  | Tu_aux ( a, b ) ->
      print_endline "Tu_aux";
      begin match a with
      | Tu_ty_id ( c, d ) ->
          print_string ("Tu_ty_id " ^ string_of_id d ^ "(");
          (* print_endline (string_of_typ c); *)
          begin match c with
          | Typ_aux ( i, _ ) ->
              (* print_endline "Typ_aux "; *)
              begin match i with
              | Typ_id ( _ ) -> print_endline "Typ_id"
              | Typ_var ( _ ) -> print_endline "Typ_var"
              | Typ_fn ( _ ) -> print_endline "Typ_fn"
              | Typ_bidir ( _ ) -> print_endline "Typ_bidir"
              | Typ_tup ( x ) ->
                  (* print_endline "Typ_tuple"; (* Typ_tuple in later versions of sail *) *)
                  List.iter (fun x0 -> print_string (string_of_typ x0 ^ " ")) x;
              | Typ_app ( _ ) -> print_endline "Typ_app"
              | _ -> print_endline "Typ_aux other"
              end
          | _ -> print_endline "Tu_ty_id other"
          end;
          print_endline ")"
      (* | Tu_ty_anon_rec ( c, d ) -> print_endline "Tu_ty_anon_rec"; *)
      | _ -> print_endline "Tu_aux other"
      end
  | _ -> print_endline "SD_unioncl ?"
  end

let parse_DEF_type def =
  print_endline "DEF_type";
  begin match def with
  | TD_aux (b, _) ->
      print_endline "TD_aux";
      begin match b with
      | TD_abbrev (d, e, f) ->
          print_endline ( "TD_abbrev " ^ string_of_id d ^ ":" ^ string_of_typ_arg f);
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
      | TD_record (d, e, f, g) -> print_endline ( "TD_record " ^ string_of_id d )
      | TD_variant (d, e, f, g) -> print_endline ( "TD_variant " ^ string_of_id d )
      | TD_enum (d, e, f) -> print_endline ( "TD_enum " ^ string_of_id d )
      | TD_bitfield (d, e, f) -> print_endline ( "TD_bitfield " ^ string_of_id d )
      | _ -> print_endline "TD_aux other "
      end
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
  exit 0
  
let _ =
  Target.register
    ~name:"riscv_decode"
    ~pre_descatter_hook:riscv_decode_info
    (fun _ _ _ _ _ -> ())
