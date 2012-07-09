(**
 * Platform indepedent type-directed compilation.
 *
 * @copyright (c) 2011, Tohoku University.
 * @author UENO Katsuhiro
 * @author Atsushi Ohori
 *
 * This module relys on the type structure of rank-1 polymorphism.
 *)
structure RecordCompilation : sig

  val compile : RecordCalc.rcdecl list -> RecordCalc.rcdecl list

end =
struct
  fun printRcexp rcexp = print (RecordCalcFormatter.rcexpToString nil rcexp)
  fun printRcdecl rcexp = print (RecordCalcFormatter.rcdecToString rcexp)
  structure RC = RecordCalc
  structure T = Types

  fun newVar ty =
      let
        val id = VarID.generate ()
      in
        {path = ["$" ^ VarID.toString id], ty = ty, id = id} : RC.varInfo
      end

  fun mapi f l =
      let
        fun loop f i nil = nil
          | loop f i (h::t) = f (h, i) :: loop f (i + 1) t
      in
        loop f 0 l
      end

  fun mapToLabelEnv f nil = LabelEnv.empty
    | mapToLabelEnv f (h::t) =
      let
        val (label, value) = f h
      in
        LabelEnv.insert (mapToLabelEnv f t, label, value)
      end

  fun Exp (exp, expTy) =
      (fn loc => exp, expTy)

  fun Var (var as {ty,...}) =
      (fn loc => RC.RCVAR (var, loc), ty)

  fun SELECT (label, (exp, expTy)) =
      let
        val resultTy =
            case expTy of
              T.RECORDty fields =>
              (case LabelEnv.find (fields, label) of
                 SOME ty => ty
               | NONE => raise Control.Bug ("SELECT " ^ label))
            | _ => raise Control.Bug "SELECT (not record)"
      in
        (fn loc => RC.RCSELECT {indexExp = RC.RCINDEXOF (label, expTy, loc),
                                label = label,
                                exp = exp loc,
                                expTy = expTy,
                                resultTy = resultTy,
                                loc = loc},
         resultTy)
      end

  fun RECORD fields =
      let
        val recordTy = T.RECORDty (LabelEnv.map (fn (exp, expTy) => expTy) fields)
      in
        (fn loc => RC.RCRECORD
                     {fields = LabelEnv.map (fn (exp, expTy) => exp loc) fields,
                      recordTy = recordTy,
                      loc = loc},
         recordTy)
      end

  fun APPM ((exp, expTy), args) =
      (fn loc => RC.RCAPPM {funExp = exp loc,
                            funTy = expTy,
                            argExpList = map (fn (exp,ty) => exp loc) args,
                            loc = loc},
       case TypesUtils.derefTy expTy of
         T.FUNMty (argTys, retTy) => retTy
       | _ => raise Control.Bug "APPM")

  fun POLYFNM (btvEnv, args, (bodyExp, bodyTy)) =
      (fn loc => RC.RCPOLYFNM {btvEnv = btvEnv,
                               argVarList = args,
                               bodyTy = bodyTy,
                               bodyExp = bodyExp loc,
                               loc = loc},
       T.POLYty {boundtvars = btvEnv, body = T.FUNMty (map #ty args, bodyTy)})

  fun TAPP ((exp, expTy), instTyList) =
      (fn loc => RC.RCTAPP {exp = exp loc,
                            expTy = expTy,
                            instTyList = instTyList,
                            loc = loc},
       TypesUtils.tpappTy (expTy, instTyList))
      handle e => raise e

  fun LET (dec, (exp, expTy)) =
      (fn loc => RC.RCLET {decls=[dec loc], body=[exp loc], tys=[expTy],
                           loc=loc},
       expTy)

  fun VALDEC binds loc =
      RC.RCVAL
        (map (fn (var,(exp,expTy:T.ty)) => (var, exp loc)) binds,
         loc)

  fun VALRECDEC binds loc =
      RC.RCVALREC
        (map (fn (var,(exp,expTy)) =>
                 {var = var, expTy = expTy, exp = exp loc})
             binds,
         loc)

  fun etaExpandPolyCon (conInfo, loc) =
      case TypesUtils.derefTy (#ty conInfo) of
        T.POLYty {boundtvars, body} =>
        let
          val instTyList =
              map T.BOUNDVARty (BoundTypeVarID.Map.listKeys boundtvars)
        in
          case TypesUtils.derefTy body of
            T.FUNMty ([argTy], ranTy) =>
            let
              val newVar = newVar argTy
            in
              RC.RCPOLYFNM
                {btvEnv = boundtvars,
                 argVarList = [newVar],
                 bodyTy = ranTy,
                 bodyExp = RC.RCDATACONSTRUCT
                             {con = conInfo,
                              instTyList = instTyList,
                              argExpOpt = SOME (RC.RCVAR (newVar, loc)),
                              loc = loc},
                 loc = loc}
            end
          | _ =>
            RC.RCPOLY {btvEnv = boundtvars,
                       expTyWithoutTAbs = body,
                       exp = RC.RCDATACONSTRUCT
                               {con = conInfo,
                                instTyList = instTyList,
                                argExpOpt = NONE,
                                loc = loc},
                       loc = loc}
        end
      | _ =>
        RC.RCDATACONSTRUCT {con = conInfo,
                            instTyList = nil,
                            argExpOpt = NONE,
                            loc = loc}

  structure SingletonTyOrd : ORD_KEY =
  struct

    type ord_key = T.singletonTy

    fun order sty =
        case sty of
          T.TAGty _ => 0
        | T.SIZEty _ => 1
        | T.INDEXty _ => 2
        | T.INSTCODEty _ => 3

    fun compare (sty1, sty2) =
        case (sty1, sty2) of
          (T.INSTCODEty op1, T.INSTCODEty op2) =>
          OverloadKind.compareSelector (op1, op2)
        | (T.INDEXty i1, T.INDEXty i2) =>
          RecordKind.compareIndex (i1, i2)
        | (T.SIZEty ty1, T.SIZEty ty2) =>
          UnivKind.compareTagTy (ty1, ty2)
        | (T.TAGty ty1, T.TAGty ty2) =>
          UnivKind.compareSizeTy (ty1, ty2)
        | (T.INSTCODEty _, _) => Int.compare (order sty1, order sty2)
        | (T.INDEXty _, _) => Int.compare (order sty1, order sty2)
        | (T.SIZEty _, _) => Int.compare (order sty1, order sty2)
        | (T.TAGty _, _) => Int.compare (order sty1, order sty2)

  end

  structure SingletonTyMap = BinaryMapFn(SingletonTyOrd)
  structure SingletonTySet = BinarySetFn(SingletonTyOrd)

  fun generateExtraArgsOfKind btvEnv (btv, recordKind) =
      case recordKind of
        T.UNIV => UnivKind.generateSingletonTy btv
      | T.OCONSTkind tys => nil
      | T.OPRIMkind k =>
        UnivKind.generateSingletonTy btv
        @ OverloadKind.generateSingletonTy btvEnv k
      | T.REC r => RecordKind.generateSingletonTy (btv, r)

  fun generateExtraArgs btvEnv =
      let
        val args =
            map (fn (tid, {eqKind, tvarKind}) =>
                    foldl (fn (x,z) => SingletonTySet.add (z,x))
                          SingletonTySet.empty
                          (generateExtraArgsOfKind btvEnv (tid, tvarKind)))
                (BoundTypeVarID.Map.listItemsi btvEnv)
        fun unique (occurred, nil) = nil
          | unique (occurred, h::t) =
            SingletonTySet.listItems
              (SingletonTySet.difference (h, occurred))
            @ unique (SingletonTySet.union (occurred, h), t)
      in
        map T.SINGLETONty (unique (SingletonTySet.empty, args))
      end

  fun generateExtraArgVars btvEnv =
      map newVar (generateExtraArgs btvEnv)

  type context =
      {
        instanceEnv: RC.varInfo SingletonTyMap.map,
        btvEnv: RC.btvEnv
      }

  fun extendBtvEnv ({instanceEnv, btvEnv}:context) newBtvEnv =
      {instanceEnv = instanceEnv,
       btvEnv = BoundTypeVarID.Map.unionWith #2 (btvEnv, newBtvEnv)}
      : context

  fun addExtraBinds ({instanceEnv, btvEnv}:context) vars =
      {
        instanceEnv =
          foldl
            (fn (var as {ty = T.SINGLETONty sty, ...} : RC.varInfo,
                 instanceEnv) =>
                SingletonTyMap.insert (instanceEnv, sty, var)
              | _ => raise Control.Bug "addExtraBinds")
            instanceEnv
            vars,
        btvEnv = btvEnv
      } : context

  fun compileTy ty =
      case ty of
        T.SINGLETONty _ => ty
      | T.ERRORty => ty
      | T.DUMMYty id => ty
      | T.TYVARty tv => ty  (* what used to be tyvar contains no POLYty. *)
      | T.BOUNDVARty tid => ty
      | T.FUNMty (argTys, retTy) =>
        (* argTys may contain polyTy due to functor. *)
        T.FUNMty (map compileTy argTys, compileTy retTy)
      | T.RECORDty fields =>
        T.RECORDty (LabelEnv.map compileTy fields)
      | T.CONSTRUCTty {tyCon, args} =>
        T.CONSTRUCTty {tyCon = tyCon, args = map compileTy args}
      | T.POLYty {boundtvars, body} =>
        case generateExtraArgs boundtvars of
          nil =>
          T.POLYty {boundtvars = boundtvars, body = compileTy body}
        | extraTys =>
          T.POLYty {boundtvars = boundtvars,
                    body = T.FUNMty (extraTys, compileTy body)}

  fun compileVarInfo ({path, ty, id} : RC.varInfo) =
      {path = path, ty = compileTy ty, id = id} : RC.varInfo

  fun compileExVarInfo ({path, ty} : RC.exVarInfo) =
      {path = path, ty = compileTy ty} : RC.exVarInfo


  datatype instance =
      APP of {appExp: RC.rcexp -> RC.rcexp, argTy: T.ty, bodyTy: T.ty,
              singletonTy: T.singletonTy, loc: Loc.loc}
    | EXP of RC.rcexp

  fun toExp instance =
      case instance of
        EXP exp => exp
      | APP {appExp, argTy, bodyTy, singletonTy, loc} =>
        let
          val arg = newVar argTy
        in
          RC.RCCAST (RC.RCFNM {argVarList = [arg],
                               bodyTy = bodyTy,
                               bodyExp = appExp (RC.RCVAR (arg, loc)),
                               loc = loc},
                     T.SINGLETONty singletonTy,
                     loc)
        end

  fun generateConcreteInstance (context as {btvEnv,...}:context) sty loc =
      case sty of
        T.INDEXty index =>
        Option.map EXP (RecordKind.generateInstance index loc)
      | T.TAGty ty =>
        Option.map EXP (UnivKind.generateTagInstance btvEnv ty loc)
      | T.SIZEty ty =>
        Option.map EXP (UnivKind.generateSizeInstance btvEnv ty loc)
      | T.INSTCODEty operator =>
        case OverloadKind.generateInstance operator loc of
          NONE => NONE
        | SOME (OverloadKind.APP app) => SOME (APP app)
        | SOME (OverloadKind.EXP exp) =>
          (* may contain RCTAPP. need more type-directed compilation *)
          SOME (EXP (compileExp context exp))

  and generateInstance (context as {instanceEnv,...}) sty loc =
      case generateConcreteInstance context sty loc of
        SOME inst => inst
      | NONE =>
        case SingletonTyMap.find (instanceEnv, sty) of
          SOME var => EXP (RC.RCVAR (var, loc))
        | NONE => (print (Control.prettyPrint
                            (T.format_ty nil (T.SINGLETONty sty)) ^ "\n");
                   raise Control.Bug "generateInstance")

  and generateInstances context tys loc =
      map (fn ty as T.SINGLETONty sty => generateInstance context sty loc
            | _ => raise Control.Bug "generateExtraInstExps")
          tys

  and compileExp context rcexp =
      case rcexp of
        RC.RCFOREIGNAPPLY {funExp, foreignFunTy, argExpList, loc} =>
        RC.RCFOREIGNAPPLY
          {funExp = compileExp context funExp,
           argExpList = map (compileExp context) argExpList,
           foreignFunTy = foreignFunTy,  (* contains no POLYty *)
           loc = loc}
      | RC.RCEXPORTCALLBACK {funExp, foreignFunTy, loc} =>
        RC.RCEXPORTCALLBACK
          {funExp = compileExp context funExp,
           foreignFunTy = foreignFunTy,
           loc = loc}
      | RC.RCTAGOF (ty, loc) =>
        (* contains no POLYty *)
        toExp (generateInstance context (T.TAGty ty) loc)
      | RC.RCSIZEOF (ty, loc) =>
        (* contains no POLYty *)
        toExp (generateInstance context (T.SIZEty ty) loc)
      | RC.RCINDEXOF (label, recordTy, loc) =>
        (* recordTy may contain POLYty due to rank-1 poly *)
        let
          val recordTy = compileTy recordTy
        in
          toExp (generateInstance context (T.INDEXty (label, recordTy)) loc)
        end
      | RC.RCCONSTANT {const, ty, loc} =>
        RC.RCCONSTANT {const=const, ty=ty, loc=loc}
      | RC.RCGLOBALSYMBOL symbol =>
        (* contains no POLYty *)
        RC.RCGLOBALSYMBOL symbol
      | RC.RCVAR (varInfo, loc) =>
        RC.RCVAR (compileVarInfo varInfo, loc)
      | RC.RCEXVAR (exVarInfo, loc) =>
        RC.RCEXVAR (compileExVarInfo exVarInfo, loc)
      | RC.RCAPPM {funExp, funTy, argExpList, loc} =>
        RC.RCAPPM
          {funExp = compileExp context funExp,
           funTy = compileTy funTy,
           argExpList = map (compileExp context) argExpList,
           loc = loc}
      | RC.RCMONOLET {binds, bodyExp, loc} =>
        RC.RCMONOLET
          {binds = map (fn (v,e) => (v, compileExp context e)) binds,
           bodyExp = compileExp context bodyExp,
           loc = loc}
      | RC.RCLET {decls, body, tys, loc} =>
        RC.RCLET {decls = List.concat (map (compileDecl context) decls),
                  body = map (compileExp context) body,
                  tys = map compileTy tys,
                  loc = loc}
      | RC.RCRECORD {fields, recordTy, loc} =>
        RC.RCRECORD
          {fields = LabelEnv.map (compileExp context) fields,
           recordTy = compileTy recordTy,
           loc = loc}
      | RC.RCSELECT {indexExp, label, exp, expTy, resultTy, loc} =>
        RC.RCSELECT
          {indexExp = compileExp context indexExp,
           label = label,
           exp = compileExp context exp,
           expTy = compileTy expTy,
           resultTy = compileTy resultTy,
           loc = loc}
      | RC.RCMODIFY {indexExp, label, recordExp, recordTy, elementExp,
                     elementTy, loc} =>
        RC.RCMODIFY
          {indexExp = compileExp context indexExp,
           label = label,
           recordExp = compileExp context recordExp,
           recordTy = compileTy recordTy,
           elementExp = compileExp context elementExp,
           elementTy = compileTy elementTy,
           loc = loc}
      | RC.RCRAISE {exp, ty, loc} =>
        (* ty may contain POLYty due to rank-1 poly.
         * Consider the following example:
         *   fun f 0 = fn x => x
         * TypeInference infers the type of f as "int -> ['a.'a -> 'a]" and
         * MatchCompiler generates the default case branch which just raises
         * "Match" exception.  The RCRAISE in the default branch may have
         * polymorphic type ['a. 'a -> 'a] due to the typing rule of RCSWITCH.
         *)
        RC.RCRAISE {exp = compileExp context exp,
                    ty = compileTy ty,
                    loc = loc}
      | RC.RCHANDLE {exp, exnVar, handler, loc} =>
        RC.RCHANDLE
          {exp = compileExp context exp,
           exnVar = exnVar, (* contains no POLYty *)
           handler = compileExp context handler,
           loc = loc}
      | RC.RCCASE {exp, expTy, ruleList, defaultExp, loc} =>
        RC.RCCASE
          {exp = compileExp context exp,
           expTy = expTy, (* contains no POLYty *)
           ruleList = map (fn (c,v,e) => (c,
                                          v, (* contains no POLYty *)
                                          compileExp context e))
                          ruleList,
           defaultExp = compileExp context defaultExp,
           loc = loc}
      | RC.RCEXNCASE {exp, expTy, ruleList, defaultExp, loc} =>
        RC.RCEXNCASE
          {exp = compileExp context exp,
           expTy = expTy, (* contains no POLYty *)
           ruleList = map (fn (c,v,e) => (c,
                                          v, (* contains no POLYty *)
                                          compileExp context e))
                          ruleList,
           defaultExp = compileExp context defaultExp,
           loc = loc}
      | RC.RCSWITCH {switchExp, expTy, branches, defaultExp, loc} =>
        RC.RCSWITCH
          {switchExp = compileExp context switchExp,
           expTy = expTy, (* contains no POLYty *)
           branches = map (fn (c,e) => (c, compileExp context e)) branches,
           defaultExp = compileExp context defaultExp,
           loc = loc}
      | RC.RCFNM {argVarList, bodyTy, bodyExp, loc} =>
        (* argVarList may contain POLYty due to functor *)
        RC.RCFNM
          {argVarList = map compileVarInfo argVarList,
           bodyTy = compileTy bodyTy,
           bodyExp = compileExp context bodyExp,
           loc = loc}
      | RC.RCPOLYFNM {btvEnv, argVarList, bodyTy, bodyExp, loc} =>
        let
          val extraArgs = generateExtraArgVars btvEnv
          val newContext = addExtraBinds context extraArgs
          val newContext = extendBtvEnv newContext btvEnv
          val newArgVarList = argVarList (* contains no POLYty *)
          val newBodyTy = compileTy bodyTy
          val newBodyExp = compileExp newContext bodyExp
        in
          case extraArgs of
            nil =>
            RC.RCPOLYFNM {btvEnv = btvEnv,
                          argVarList = newArgVarList,
                          bodyTy = newBodyTy,
                          bodyExp = newBodyExp,
                          loc = loc}
          | _::_ =>
            RC.RCPOLYFNM {btvEnv = btvEnv,
                          argVarList = extraArgs,
                          bodyTy = T.FUNMty (map #ty newArgVarList, newBodyTy),
                          bodyExp = RC.RCFNM {argVarList = newArgVarList,
                                              bodyTy = newBodyTy,
                                              bodyExp = newBodyExp,
                                              loc = loc},
                          loc = loc}
        end
      | RC.RCPOLY {btvEnv, expTyWithoutTAbs, exp, loc} =>
        let
          val extraArgs = generateExtraArgVars btvEnv
          val newContext = addExtraBinds context extraArgs
          val newContext = extendBtvEnv newContext btvEnv
          val newExpTyWithoutTAbs = compileTy expTyWithoutTAbs
          val newExp = compileExp newContext exp
        in
          case extraArgs of
            nil =>
            RC.RCPOLY {btvEnv = btvEnv,
                       expTyWithoutTAbs = expTyWithoutTAbs,
                       exp = newExp,
                       loc = loc}
          | _::_ =>
            RC.RCPOLYFNM {btvEnv = btvEnv,
                          argVarList = extraArgs,
                          bodyTy = newExpTyWithoutTAbs,
                          bodyExp = newExp,
                          loc = loc}
        end
      | RC.RCEXNCONSTRUCT {exn, instTyList, argExpOpt, loc} =>
        RC.RCEXNCONSTRUCT
          {exn = exn,
           instTyList = instTyList, (* contains no POLYty *)
           argExpOpt = Option.map (compileExp context) argExpOpt,
           loc = loc}
      | RC.RCEXN_CONSTRUCTOR {exnInfo,loc} =>
        (* FIXME check this case *)
        RC.RCEXN_CONSTRUCTOR {exnInfo=exnInfo,loc=loc}
      | RC.RCEXEXN_CONSTRUCTOR {exExnInfo,loc} =>
        (* FIXME check this case *)
        RC.RCEXEXN_CONSTRUCTOR {exExnInfo=exExnInfo,loc=loc}
      | RC.RCPRIMAPPLY {primOp, instTyList, argExp, loc} =>
        RC.RCPRIMAPPLY
          {primOp = primOp,
           instTyList = instTyList, (* contains no POLYty *)
           argExp = compileExp context argExp,
           loc = loc}
      | RC.RCDATACONSTRUCT {con, instTyList=nil, argExpOpt=NONE, loc} =>
        (
          (* In order to keep consistency of global calling convention,
           * every polymorphic constructor is compiled into a polymorphic
           * function.  To prevent this, we need some kind of escape
           * analysis so that we can determine whether a constructor is
           * exported as a external value or not. *)
          case etaExpandPolyCon (con, loc) of
            exp as RC.RCDATACONSTRUCT _ => exp
          | exp => compileExp context exp
        )
      | RC.RCDATACONSTRUCT {con, instTyList, argExpOpt, loc} =>
        RC.RCDATACONSTRUCT
          {con = con,
           instTyList = instTyList,  (* contains no POLYty *)
           argExpOpt = Option.map (compileExp context) argExpOpt,
           loc = loc}
      | RC.RCOPRIMAPPLY {oprimOp={id, ty, path}, instTyList, argExp, loc} =>
        let
          val argExp = compileExp context argExp
          val primTy = compileTy ty
          val primTy = TypesUtils.tpappTy (primTy, instTyList)
                       handle e => raise e
          val extraArgTys =
              case primTy of T.FUNMty (argTys, retTy) => argTys | _ => nil
          val singletonTy =
              case List.find
                     (fn T.SINGLETONty (T.INSTCODEty {oprimId,...}) =>
                         id = oprimId
                       | _ => false) extraArgTys of
                SOME (T.SINGLETONty sty) => sty
              | _ => raise Control.Bug "compileExp: RCTAPP: RCOPRIM"
          val primInst = generateInstance context singletonTy loc
        in
          case primInst of
            APP {appExp, ...} => appExp argExp
          | EXP exp =>
            let
              val funTy = TypesUtils.tpappTy (ty, instTyList)
                          handle e => raise e
            in
              RC.RCAPPM {funExp = RC.RCCAST (exp, funTy, loc),
                         funTy = funTy,
                         argExpList = [argExp],
                         loc = loc}
            end
        end
      | RC.RCTAPP {exp, expTy, instTyList, loc} =>
        let
          val newExp = compileExp context exp
          val newExpTy = compileTy expTy
          val newInstTyList = instTyList (* contains no POLYty *)
          val funTy = TypesUtils.tpappTy (newExpTy, newInstTyList)
                      handle e => raise e
          val extraArgs =
              case funTy of
                T.FUNMty (argTys, retTy) =>
                if List.exists (fn T.SINGLETONty _ => true | _ => false) argTys
                then map toExp (generateInstances context argTys loc)
                else nil
              | _ => nil
        in
          case extraArgs of
            nil =>
            RC.RCTAPP {exp = newExp,
                       expTy = newExpTy,
                       instTyList = newInstTyList,
                       loc = loc}
          | _::_ =>
            RC.RCAPPM {funExp = RC.RCTAPP {exp = newExp,
                                           expTy = newExpTy,
                                           instTyList = newInstTyList,
                                           loc = loc},
                       funTy = funTy,
                       argExpList = extraArgs,
                       loc = loc}
        end
      | RC.RCSEQ {expList, expTyList, loc} =>
        RC.RCSEQ
          {expList = map (compileExp context) expList,
           expTyList = map compileTy expTyList,
           loc = loc}
      | RC.RCCAST (rcexp, ty, loc) =>
        RC.RCCAST (compileExp context rcexp, compileTy ty, loc)
      | RC.RCSQL exp =>
        raise Control.Bug "RCSQL"
      | RC.RCFFI exp =>
        raise Control.Bug "RCFFI"

  and compileDecl context rcdecl =
      case rcdecl of
        RC.RCEXD (exnBinds, loc) =>
        [RC.RCEXD (exnBinds, loc)]  (* contains no POLYty *)
      | RC.RCEXNTAGD (bind, loc) => (* FIXME check this *)
        [RC.RCEXNTAGD (bind, loc)]  (* contains no POLYty *)
      | RC.RCEXPORTVAR (varInfo, loc) =>
        [RC.RCEXPORTVAR (compileVarInfo varInfo, loc)]
      | RC.RCEXPORTEXN (exnInfo, loc) =>
        [RC.RCEXPORTEXN (exnInfo, loc)]  (* contains no POLYty *)
      | RC.RCEXTERNVAR (exVarInfo, loc) =>
        [RC.RCEXTERNVAR (compileExVarInfo exVarInfo, loc)]
      | RC.RCEXTERNEXN (exExnInfo, loc) =>
        [RC.RCEXTERNEXN (exExnInfo, loc)]  (* contains no POLYty *)
      | RC.RCVAL (bindList, loc) =>
        [RC.RCVAL (map (fn (v,e) => (compileVarInfo v, compileExp context e))
                       bindList, loc)]
      | RC.RCVALREC (bindList, loc) =>
        [RC.RCVALREC (map (fn {var, expTy, exp} =>
                              {var = compileVarInfo var,
                               expTy = compileTy expTy,
                               exp = compileExp context exp})
                          bindList,
                      loc)]
      | RC.RCVALPOLYREC (btvEnv, bindList, loc) =>
        let
          val extraArgs = generateExtraArgVars btvEnv
          val newContext = extendBtvEnv context btvEnv
        in
          case extraArgs of
            nil =>
            [RC.RCVALPOLYREC (btvEnv,
                              map (fn {var, expTy, exp} =>
                                      {var = compileVarInfo var,
                                       expTy = compileTy expTy,
                                       exp = compileExp newContext exp})
                                   bindList,
                              loc)]
          | _::_ =>
            let
              (*
               * ['a#K. val rec f_1 = e_1 ... and f_n = e_n]
               *       ||
               *       vv
               * val F = ['a#K. fn A => let val rec f_1 = e_1'
               *                            ... and f_n = e_n'
               *                        in (f_1, ..., f_n) end]
               * val f_1 = ['a#K. fn A => #1 (F {'a} A)]
               * ...
               * val f_n = ['a#K. fn A => #n (F {'a} A)]
               *
               * This case breaks the uniqueness condition of bound type
               * variables for efficiency.
               *
               * This compilation introduces new POLYtys for each variable
               * f_1, ..., f_n. To give fresh ids to those bound type
               * variables, we need to manipulate all occurrance of f_1,
               * ..., f_n in this program in order to replace type
               * information. This does not make sense.
               *)
              val newContext = addExtraBinds newContext extraArgs
              val instTyList =
                  map T.BOUNDVARty (BoundTypeVarID.Map.listKeys btvEnv)

              val recBinds =
                  mapi
                    (fn ({var as {path, ty, id}, expTy, exp}, i) =>
                        {localVar = var,
                         var = {path = path,
                                ty = compileTy (T.POLYty {boundtvars = btvEnv,
                                                          body = ty}),
                                id = id} : T.varInfo,
                         label = Int.toString (i + 1),
                         expTy = compileTy expTy,
                         exp = compileExp newContext exp})
                    bindList

              val localRecExp =
                  POLYFNM
                    (btvEnv, extraArgs,
                     LET (VALRECDEC (map (fn {localVar, exp, expTy, ...} =>
                                             (localVar, Exp (exp, expTy)))
                                         recBinds),
                          RECORD (mapToLabelEnv (fn {localVar, label, ...} =>
                                                (label, Var localVar))
                                            recBinds)))
              val localRecVar = newVar (#2 localRecExp)

              val bodyBinds =
                  map (fn {var, label, ...} =>
                          (var,
                           POLYFNM
                             (btvEnv, extraArgs,
                              SELECT (label,
                                      APPM (TAPP (Var localRecVar, instTyList),
                                            map Var extraArgs)))))
                      recBinds
                      handle e => raise e
            in
              [VALDEC [(localRecVar, localRecExp)] loc,
               VALDEC bodyBinds loc]
            end
        end

  fun compile topBlockList =
      let
        val context = {instanceEnv = SingletonTyMap.empty,
                       btvEnv = BoundTypeVarID.Map.empty} : context
      in
        List.concat (map (compileDecl context) topBlockList)
      end

end
