(**
 * Module compiler flattens structure.
 * @copyright (c) 2006, Tohoku University.
 * @author Liu Bochao
 * @version $Id: utils.sml,v 1.16.6.6 2010/01/29 06:41:35 hiro-en Exp $
 *)
structure VALREC_Utils =
struct
local
  open IDCalc
in
  fun getFreeIdsInExp icexp =
      case icexp of
	ICERROR _ => VarID.Set.empty
      | ICCONSTANT (const, loc) => VarID.Set.empty
      | ICGLOBALSYMBOL (str, globalSymbolKind, loc) => VarID.Set.empty
      | ICVAR (varInfo, loc) => VarID.Set.singleton(#id varInfo)
      | ICEXVAR _ => VarID.Set.empty
      | ICEXVAR_TOBETYPED _ => VarID.Set.empty
      | ICBUILTINVAR _ => VarID.Set.empty
      | ICCON _ => VarID.Set.empty
      | ICEXN _ => VarID.Set.empty
      | ICEXEXN _ => VarID.Set.empty
      | ICEXN_CONSTRUCTOR _ => VarID.Set.empty
      | ICEXEXN_CONSTRUCTOR _ => VarID.Set.empty
      | ICOPRIM _ => VarID.Set.empty
      | ICTYPED (exp,ty,loc) => getFreeIdsInExp exp
      | ICSIGTYPED {path,icexp,ty,loc, revealKey} => getFreeIdsInExp icexp
      | ICAPPM (funExp,argExpList,loc) =>
        VarID.Set.union(getFreeIdsInExp funExp, 
			getFreeIdsInExpList argExpList)
      | ICAPPM_NOUNIFY (funExp,argExpList,loc) =>
        VarID.Set.union(getFreeIdsInExp funExp, 
			getFreeIdsInExpList argExpList)
      | ICLET (localDeclList,mainExpList,loc) =>
        VarID.Set.union(getFreeIdsInDeclList localDeclList,
			VarID.Set.difference
			    (getFreeIdsInExpList mainExpList,
			     getBoundIdsInDeclList localDeclList))
      | ICTYCAST (tycast, exp, loc) => getFreeIdsInExp exp
      | ICRECORD (elementList,loc) =>
        foldl 
            (fn ((label,exp),S) =>
                VarID.Set.union(S,getFreeIdsInExp exp))
            VarID.Set.empty
            elementList
      | ICRAISE (exp,loc) => getFreeIdsInExp exp
      | ICHANDLE (handler,matchList, loc) =>
        foldl 
            (fn ((pat,exp),S) =>
                VarID.Set.union
                    (S,
                     VarID.Set.difference
                         (getFreeIdsInExp exp,
                          getFreeIdsInPat pat)))
            (getFreeIdsInExp handler)
            matchList
      | ICFNM (matchList,loc) =>                      
        foldl 
            (fn ({args,body},S) =>
                VarID.Set.union
                    (S,
                     VarID.Set.difference
                         (getFreeIdsInExp body,
                          getFreeIdsInPatList args)))
            VarID.Set.empty
            matchList
      | ICFNM1 (varInfoTyListList,exp,loc)=>
	VarID.Set.difference
	(getFreeIdsInExp exp,
	 foldl
	     (fn ((varInfo,tyList),S) =>
		 VarID.Set.add(S,#id varInfo))
				VarID.Set.empty
				varInfoTyListList)
      | ICFNM1_POLY (varInfoTyList,exp,loc)=>
	VarID.Set.difference
	(getFreeIdsInExp exp,
	 foldl
	     (fn ((varInfo,tyList),S) =>
		 VarID.Set.add(S,#id varInfo))
				VarID.Set.empty
				varInfoTyList)
      | ICCASEM (selectorList, matchList, kind, loc) =>
        foldl 
            (fn ({args,body},S) =>
                VarID.Set.union
                    (S,
                     VarID.Set.difference
                         (getFreeIdsInExp body,
                          getFreeIdsInPatList args)))
            (getFreeIdsInExpList selectorList)
            matchList
      | ICRECORD_UPDATE (exp,elementList,loc) =>
        foldl 
            (fn ((label,exp),S) =>
                VarID.Set.union(S,getFreeIdsInExp exp))
            (getFreeIdsInExp exp)
            elementList
      | ICRECORD_SELECTOR _ => VarID.Set.empty
      | ICSELECT (label,exp, loc) => getFreeIdsInExp exp
      | ICSEQ (expList,loc) => getFreeIdsInExpList expList
      | ICCAST (exp,loc) => getFreeIdsInExp exp
      | ICFFIIMPORT (exp,ty,loc) => getFreeIdsInExp exp
      | ICFFIEXPORT (exp,ty,loc) => getFreeIdsInExp exp
      | ICFFIAPPLY (cconv,funExp,args,retTy,loc) =>
        foldl (fn (ICFFIARG (exp, ty, loc), z) =>
                  VarID.Set.union (z, getFreeIdsInExp exp)
                | (ICFFIARGSIZEOF (ty, SOME exp, loc), z) =>
                  VarID.Set.union (z, getFreeIdsInExp exp)
                | (ICFFIARGSIZEOF (ty, NONE, loc), z) => z)
              (getFreeIdsInExp funExp)
              args
      | ICSQLSERVER (server, schema, loc) => VarID.Set.empty
      | ICSQLDBI (pat, exp, loc) =>
        VarID.Set.difference(getFreeIdsInExp exp, getFreeIdsInPat pat)

  and getFreeIdsInExpList icexpList =
      foldl 
          (fn (exp,S) => VarID.Set.union(S,getFreeIdsInExp exp))
          VarID.Set.empty
          icexpList

  and getFreeIdsInFundeclList fidRuleListList =
      let
	val boundList =       
            foldl
		(fn ({funVarInfo,rules},S) =>
		    VarID.Set.add(S,#id funVarInfo))
		VarID.Set.empty
		fidRuleListList
	val freeList = 
            foldl
		(fn ({funVarInfo,rules},S) =>
		    VarID.Set.union(getFreeIdsInRule rules,S))
		VarID.Set.empty
          fidRuleListList
      in
	VarID.Set.difference(freeList, boundList)
    end

  and getFreeIdsInRule patListExpList =
      foldl 
          (fn ({args,body},S) =>
              VarID.Set.union
                  (S,
                   VarID.Set.difference
                       (getFreeIdsInExp body,
                        getFreeIdsInPatList args)))
          VarID.Set.empty
          patListExpList

  and getFreeIdsInPatList patList =
      foldl 
          (fn (pat,S) => VarID.Set.union(S,getFreeIdsInPat pat))
          VarID.Set.empty
          patList

  and getFreeIdsInPat icpat = 
      case icpat of 
        ICPATERROR _ => VarID.Set.empty
      | ICPATWILD _ => VarID.Set.empty
      | ICPATVAR (varInfo, loc) => VarID.Set.singleton(#id varInfo)
      | ICPATCON _ => VarID.Set.empty
      | ICPATEXN _ => VarID.Set.empty
      | ICPATEXEXN _ => VarID.Set.empty
      | ICPATCONSTANT _ => VarID.Set.empty
      | ICPATCONSTRUCT {con, arg, loc} => getFreeIdsInPat arg
      | ICPATRECORD {flex, fields, loc} =>
        foldl 
            (fn ((label,pat),S) =>
                VarID.Set.union(S,getFreeIdsInPat pat))
            VarID.Set.empty
            fields
      | ICPATLAYERED {patVar, tyOpt, pat, loc} => 
        VarID.Set.add(getFreeIdsInPat pat,#id patVar)
      | ICPATTYPED (pat, ty, loc) => getFreeIdsInPat pat

  and getFreeIdsInExBind {exncon, tyOpt, loc} = VarID.Set.empty

  and getFreeIdsInDecl icdecl =
      case icdecl of 
        ICVAL (tvarList, bindList, loc) => getFreeIdsInBindList bindList
      | ICDECFUN  {guard, funbinds, loc} => getFreeIdsInFundeclList funbinds
      | ICNONRECFUN  _ => raise Control.Bug "invalid declaration"
      | ICVALREC {guard, recbinds, loc} =>
        VarID.Set.difference(getFreeIdsInRecBinds recbinds,
                             getBoundIdsInRecBinds recbinds)
      | ICABSTYPE {tybinds,body,loc} => getFreeIdsInDeclList body
      | ICEXND (_, loc) => VarID.Set.empty
      | ICEXNTAGD ({exnInfo, varInfo}, loc) =>
        VarID.Set.singleton (#id varInfo)
      | ICEXPORTVAR _ => VarID.Set.empty
      | ICEXPORTTYPECHECKEDVAR _ => VarID.Set.empty
      | ICEXPORTFUNCTOR _ => VarID.Set.empty
      | ICEXPORTEXN _ => VarID.Set.empty
      | ICEXTERNVAR _ => VarID.Set.empty
      | ICEXTERNEXN _ => VarID.Set.empty
      | ICOVERLOADDEF _ => VarID.Set.empty

  and getFreeIdsInDeclList icdeclList =
      #1 (foldl
              (fn (decl,(freeIds,boundIds)) =>
                  (VarID.Set.union(freeIds,
				   VarID.Set.difference
				       (getFreeIdsInDecl decl,boundIds)),
                   VarID.Set.union(boundIds,getBoundIdsInDecl decl)))
              (VarID.Set.empty,VarID.Set.empty)
              icdeclList)

  and getBoundIdsInExBind {exncon, tyOpt, loc} = VarID.Set.empty

  and getBoundIdsInDecl icdecl = 
      case icdecl of
        ICVAL (tvarList, bindList, loc) => getBoundIdsInBindList bindList 
      | ICDECFUN {guard, funbinds, loc} => getBoundIdsInFundeclList funbinds
      | ICNONRECFUN _ => raise Control.Bug "invalid declaration"
      | ICVALREC {guard, recbinds, loc} => getBoundIdsInRecBinds recbinds
      | ICABSTYPE {tybinds, body, loc} => getBoundIdsInDeclList body
      | ICEXND _ => VarID.Set.empty
      | ICEXNTAGD _ => VarID.Set.empty
      | ICEXPORTVAR _ => VarID.Set.empty
      | ICEXPORTTYPECHECKEDVAR _ => VarID.Set.empty
      | ICEXPORTFUNCTOR _ => VarID.Set.empty
      | ICEXPORTEXN _ => VarID.Set.empty
      | ICEXTERNVAR _ => VarID.Set.empty
      | ICEXTERNEXN _ => VarID.Set.empty
      | ICOVERLOADDEF _ => VarID.Set.empty

  and getBoundIdsInDeclList icdeclList =
      foldl 
          (fn (decl,S) => VarID.Set.union(S,getBoundIdsInDecl decl))
          VarID.Set.empty
          icdeclList

  and getFreeIdsInBindList bindList =
      #1 (foldl
              (fn ((pat,exp),(freeIds,boundIds)) =>
                  (VarID.Set.union(freeIds,
				   VarID.Set.difference
				       (getFreeIdsInExp exp,boundIds)),
                   VarID.Set.union(boundIds,getFreeIdsInPat pat)))
              (VarID.Set.empty,VarID.Set.empty)
              bindList)
      
  and getFreeIdsInRecBinds bindList =
      #1 (foldl
              (fn ({varInfo,body},(freeIds,boundIds)) =>
                  (VarID.Set.union(freeIds,
				   VarID.Set.difference
				       (getFreeIdsInExp body,boundIds)),
                   VarID.Set.add(boundIds,#id varInfo)))
	      (VarID.Set.empty,VarID.Set.empty)
	      bindList)
      
  and getBoundIdsInBindList bindList =
      foldl
          (fn ((pat,exp),S) => VarID.Set.union(getFreeIdsInPat pat,S))
          VarID.Set.empty
          bindList
	  
  and getBoundIdsInRecBinds bindList =
      foldl
          (fn ({varInfo,body},S) => 
	      VarID.Set.add(S,#id varInfo))
          VarID.Set.empty
          bindList
	  
  and getBoundIdsInFundeclList fidRuleListList =
      foldl
          (fn ({funVarInfo,rules},S) => 
	      VarID.Set.add(S,#id funVarInfo))
          VarID.Set.empty
          fidRuleListList
	  
end
end

