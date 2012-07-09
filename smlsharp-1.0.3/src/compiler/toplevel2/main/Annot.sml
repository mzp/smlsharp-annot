
structure Annot =
struct
  local open TypedCalc in

  fun output stream str =
      (TextIO.outputSubstr (stream, (Substring.full str));
       TextIO.outputSubstr (stream, (Substring.full "\n")))

  fun sexp (from, to) ty =
      "(" ^
        "\"" ^ (Loc.fileNameOfPos from) ^ "\" " ^
        "(" ^ (Int.toString (Loc.lineOfPos from)) ^ " " ^ (Int.toString (Loc.colOfPos from)) ^ ")" ^
        " " ^
        "(" ^ (Int.toString (Loc.lineOfPos to)) ^ " " ^ (Int.toString (Loc.colOfPos to)) ^ ")" ^
        " " ^ "\"" ^ Types.tyToString ty ^ "\"" ^
      ")"

  fun annot stream loc ty =
      output stream (sexp loc ty)

  fun fromIdstatus idstatus =
      case idstatus of
          RECFUNID (info,_) =>
          #ty info
        | VARID info =>
          #ty info

  fun outputExpr stream (e : tpexp) =
      case e of
        TPAPPM {funTy, loc, funExp, argExpList, ...} =>
        (annot stream loc funTy;
         outputExpr stream funExp;
         List.app (outputExpr stream) argExpList)
      | TPCASEM {ruleBodyTy, loc, expList, ruleList, ...} =>
        (annot stream loc ruleBodyTy;
         List.app (outputExpr stream) expList;
         List.app (fn x => outputExpr stream (#body x)) ruleList)
      | TPCAST (exp, ty, loc) =>
        (annot stream loc ty;
         outputExpr stream exp)
      | TPCONSTANT {loc, ty, ...} =>
        annot stream loc ty
      | TPDATACONSTRUCT {loc, con, argExpOpt, ...} =>
        (annot stream loc (#ty con);
         case argExpOpt of
             SOME x =>
             outputExpr stream x
           | NONE =>
             ())
      | TPERROR =>
        ()
      | TPEXNCONSTRUCT {argExpOpt, exn,loc, ...} =>
        (case exn of
            EXEXN i =>
            annot stream loc (#ty i)
          | EXN i =>
            annot stream loc (#ty i);
         case argExpOpt of
             SOME x =>
             outputExpr stream x
           | NONE =>
             ())
      | TPEXN_CONSTRUCTOR {exnInfo, loc} =>
        annot stream loc (#ty exnInfo)
      | TPEXEXN_CONSTRUCTOR {exExnInfo, loc} =>
        annot stream loc (#ty exExnInfo)
      | TPEXVAR (info, loc) =>
        annot stream loc (#ty info)
      | TPFFIIMPORT {loc, ptrExp:tpexp, stubTy:Types.ty,...} =>
        (annot stream loc stubTy;
         outputExpr stream ptrExp)
      | TPFNM {bodyExp, bodyTy, loc, ...} =>
        (annot stream loc bodyTy;
         outputExpr stream bodyExp)
      | TPGLOBALSYMBOL {loc, ty, ...} =>
        annot stream loc ty
      | TPHANDLE {exnVar, exp, handler, loc} =>
        (annot stream loc (#ty exnVar);
         outputExpr stream exp;
         outputExpr stream handler)
      | TPLET {body, decls, loc, tys} =>
        (List.app (outputExpr stream) body;
         List.app (outputDecl  stream) decls
        )
      | TPMODIFY {elementExp , loc, recordExp, recordTy, ...} =>
        (annot stream loc recordTy;
         outputExpr stream elementExp;
         outputExpr stream recordExp)
      | TPMONOLET { binds, bodyExp, loc} =>
        (List.app (fn (_,exp) =>
                      outputExpr stream exp) binds;
         outputExpr stream bodyExp)
      | TPOPRIMAPPLY {argExp, loc, oprimOp, ...} =>
        (annot stream loc (#ty oprimOp);
         outputExpr stream argExp)
      | TPPOLY {exp, expTyWithoutTAbs, loc, ...} =>
        (annot stream loc expTyWithoutTAbs;
         outputExpr stream exp)
      | TPPOLYFNM {bodyExp, bodyTy, loc, ...} =>
        (annot stream loc bodyTy;
         outputExpr stream bodyExp)
      | TPPRIMAPPLY {argExp,loc, primOp, ...} =>
        (annot stream loc (#ty primOp);
         outputExpr stream argExp)
      | TPRAISE {exp, loc, ty} =>
        (annot stream loc ty;
         outputExpr stream exp)
      | TPRECFUNVAR {loc, var,...} =>
        annot stream loc (#ty var)
      | TPRECORD {loc, recordTy, fields} =>
        (annot stream loc recordTy;
         LabelEnv.app (outputExpr stream) fields)
      | TPSELECT {exp, loc, resultTy,...} =>
        (annot stream loc resultTy;
         outputExpr stream exp)
      | TPSEQ {expList, ...} =>
        List.app (outputExpr stream) expList
      | TPSIZEOF (ty, loc) =>
        annot stream loc ty
      | TPSQLSERVER {loc, resultTy, server, ...} =>
        annot stream loc resultTy
      | TPTAPP {exp, ...} =>
        outputExpr stream exp
      | TPVAR (info, loc) =>
        annot stream loc (#ty info)

  and outputDecl stream decl =
      case decl of
          TPEXD (xs, loc) =>
          List.app (fn x => annot stream (#loc x) (#ty (#exnInfo x))) xs
        | TPEXNTAGD ({exnInfo, varInfo}, loc) =>
          annot stream loc (#ty varInfo)
        | TPEXPORTEXN (exnInfo, loc) =>
          annot stream loc (#ty exnInfo)
        | TPEXPORTVAR (x, loc) =>
          annot stream loc (fromIdstatus x)
        | TPEXTERNEXN ({ty, ...},loc) =>
          annot stream loc ty
        | TPEXTERNVAR ({ty, ...}, loc) =>
          annot stream loc ty
        | TPFUNDECL (xs, loc) =>
          List.app (fn x =>
                       (annot stream loc (#ty (#funVarInfo x));
                        (List.app (fn rule =>
                                      outputExpr stream (#body rule))
                                  (#ruleList x))))
                   xs
        | TPPOLYFUNDECL (_, xs, loc) =>
          List.app (fn x => (
                       annot stream loc (#ty (#funVarInfo x));
                       (List.app (fn rule =>
                                     outputExpr stream (#body rule))
                                 (#ruleList x)))) xs
        | TPVAL (xs, loc) =>
          List.app (fn (info,exp) =>
                       (annot stream loc (#ty info);
                        outputExpr stream exp)) xs
        | TPVALPOLYREC (_, xs, loc) =>
          List.app (fn x =>
                       (annot stream loc (#ty (#var x));
                        outputExpr stream (#exp x))
                   ) xs
        | TPVALREC (xs, loc) =>
          List.app (fn x =>
                       (annot stream loc (#ty (#var x));
                        outputExpr stream (#exp x))
                   ) xs

  fun dump name decls =
      let
          val stream = TextIO.openOut name
      in
          output stream "(";
          List.app (outputDecl stream) decls;
          output stream ")";
          TextIO.closeOut stream
      end
  end
end
