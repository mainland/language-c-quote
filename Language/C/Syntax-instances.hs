instance Located Id where
  locOf (Id _ l) = locOf l
  locOf (AntiId _ l) = locOf l
instance Located StringLit where
  locOf (StringLit _ _ l) = locOf l
instance Located Storage where
  locOf (Tauto l) = locOf l
  locOf (Tregister l) = locOf l
  locOf (Tstatic l) = locOf l
  locOf (Textern _ l) = locOf l
  locOf (Ttypedef l) = locOf l
  locOf (T__block l) = locOf l
  locOf (TObjC__weak l) = locOf l
  locOf (TObjC__strong l) = locOf l
  locOf (TObjC__unsafe_unretained l) = locOf l
instance Located TypeQual where
  locOf (Tconst l) = locOf l
  locOf (Tvolatile l) = locOf l
  locOf (EscTypeQual _ l) = locOf l
  locOf (AntiTypeQual _ l) = locOf l
  locOf (AntiTypeQuals _ l) = locOf l
  locOf (Tinline l) = locOf l
  locOf (Trestrict l) = locOf l
  locOf (T__restrict l) = locOf l
  locOf (TAttr _) = noLoc
  locOf (TCUDAdevice l) = locOf l
  locOf (TCUDAglobal l) = locOf l
  locOf (TCUDAhost l) = locOf l
  locOf (TCUDAconstant l) = locOf l
  locOf (TCUDAshared l) = locOf l
  locOf (TCUDArestrict l) = locOf l
  locOf (TCUDAnoinline l) = locOf l
  locOf (TCLprivate l) = locOf l
  locOf (TCLlocal l) = locOf l
  locOf (TCLglobal l) = locOf l
  locOf (TCLconstant l) = locOf l
  locOf (TCLreadonly l) = locOf l
  locOf (TCLwriteonly l) = locOf l
  locOf (TCLkernel l) = locOf l
instance Located Sign where
  locOf (Tsigned l) = locOf l
  locOf (Tunsigned l) = locOf l
instance Located TypeSpec where
  locOf (Tvoid l) = locOf l
  locOf (Tchar _ l) = locOf l
  locOf (Tshort _ l) = locOf l
  locOf (Tint _ l) = locOf l
  locOf (Tlong _ l) = locOf l
  locOf (Tlong_long _ l) = locOf l
  locOf (Tfloat l) = locOf l
  locOf (Tdouble l) = locOf l
  locOf (Tlong_double l) = locOf l
  locOf (Tstruct _ _ _ l) = locOf l
  locOf (Tunion _ _ _ l) = locOf l
  locOf (Tenum _ _ _ l) = locOf l
  locOf (Tnamed _ _ l) = locOf l
  locOf (T_Bool l) = locOf l
  locOf (Tfloat_Complex l) = locOf l
  locOf (Tdouble_Complex l) = locOf l
  locOf (Tlong_double_Complex l) = locOf l
  locOf (Tfloat_Imaginary l) = locOf l
  locOf (Tdouble_Imaginary l) = locOf l
  locOf (Tlong_double_Imaginary l) = locOf l
  locOf (TtypeofExp _ l) = locOf l
  locOf (TtypeofType _ l) = locOf l
  locOf (Tva_list l) = locOf l
instance Located DeclSpec where
  locOf (DeclSpec _ _ _ l) = locOf l
  locOf (AntiDeclSpec _ l) = locOf l
  locOf (AntiTypeDeclSpec _ _ _ l) = locOf l
instance Located ArraySize where
  locOf (ArraySize _ _ l) = locOf l
  locOf (VariableArraySize l) = locOf l
  locOf (NoArraySize l) = locOf l
instance Located Decl where
  locOf (DeclRoot l) = locOf l
  locOf (Ptr _ _ l) = locOf l
  locOf (Array _ _ _ l) = locOf l
  locOf (Proto _ _ l) = locOf l
  locOf (OldProto _ _ l) = locOf l
  locOf (AntiTypeDecl _ l) = locOf l
  locOf (BlockPtr _ _ l) = locOf l
instance Located Type where
  locOf (Type _ _ l) = locOf l
  locOf (AntiType _ l) = locOf l
instance Located Designator where
  locOf (IndexDesignator _ l) = locOf l
  locOf (MemberDesignator _ l) = locOf l
instance Located Designation where
  locOf (Designation _ l) = locOf l
instance Located Initializer where
  locOf (ExpInitializer _ l) = locOf l
  locOf (CompoundInitializer _ l) = locOf l
  locOf (AntiInit _ l) = locOf l
  locOf (AntiInits _ l) = locOf l
instance Located Init where
  locOf (Init _ _ _ _ _ l) = locOf l
instance Located Typedef where
  locOf (Typedef _ _ _ l) = locOf l
instance Located InitGroup where
  locOf (InitGroup _ _ _ l) = locOf l
  locOf (TypedefGroup _ _ _ l) = locOf l
  locOf (AntiDecl _ l) = locOf l
  locOf (AntiDecls _ l) = locOf l
instance Located Field where
  locOf (Field _ _ _ l) = locOf l
instance Located FieldGroup where
  locOf (FieldGroup _ _ l) = locOf l
  locOf (AntiSdecl _ l) = locOf l
  locOf (AntiSdecls _ l) = locOf l
instance Located CEnum where
  locOf (CEnum _ _ l) = locOf l
  locOf (AntiEnum _ l) = locOf l
  locOf (AntiEnums _ l) = locOf l
instance Located Attr where
  locOf (Attr _ _ l) = locOf l
  locOf (AntiAttr _ l) = locOf l
  locOf (AntiAttrs _ l) = locOf l
instance Located Param where
  locOf (Param _ _ _ l) = locOf l
  locOf (AntiParam _ l) = locOf l
  locOf (AntiParams _ l) = locOf l
instance Located Params where
  locOf (Params _ _ l) = locOf l
instance Located Func where
  locOf (Func _ _ _ _ _ l) = locOf l
  locOf (OldFunc _ _ _ _ _ _ l) = locOf l
instance Located Definition where
  locOf (FuncDef _ l) = locOf l
  locOf (DecDef _ l) = locOf l
  locOf (EscDef _ l) = locOf l
  locOf (AntiFunc _ l) = locOf l
  locOf (AntiEsc _ l) = locOf l
  locOf (AntiEdecl _ l) = locOf l
  locOf (AntiEdecls _ l) = locOf l
  locOf (ObjCClassDec _ l) = locOf l
  locOf (ObjCClassIface _ _ _ _ _ _ l) = locOf l
  locOf (ObjCCatIface _ _ _ _ _ l) = locOf l
  locOf (ObjCProtDec _ l) = locOf l
  locOf (ObjCProtDef _ _ _ l) = locOf l
  locOf (ObjCClassImpl _ _ _ _ l) = locOf l
  locOf (ObjCCatImpl _ _ _ l) = locOf l
  locOf (ObjCSynDef _ l) = locOf l
  locOf (ObjCDynDef _ l) = locOf l
  locOf (ObjCMethDef _ _ l) = locOf l
  locOf (ObjCCompAlias _ _ l) = locOf l
  locOf (AntiObjCMeth _ l) = locOf l
  locOf (AntiObjCMeths _ l) = locOf l
instance Located Stm where
  locOf (Label _ _ _ l) = locOf l
  locOf (Case _ _ l) = locOf l
  locOf (CaseRange _ _ _ l) = locOf l
  locOf (Default _ l) = locOf l
  locOf (Exp _ l) = locOf l
  locOf (Block _ l) = locOf l
  locOf (If _ _ _ l) = locOf l
  locOf (Switch _ _ l) = locOf l
  locOf (While _ _ l) = locOf l
  locOf (DoWhile _ _ l) = locOf l
  locOf (For _ _ _ _ l) = locOf l
  locOf (Goto _ l) = locOf l
  locOf (Continue l) = locOf l
  locOf (Break l) = locOf l
  locOf (Return _ l) = locOf l
  locOf (Pragma _ l) = locOf l
  locOf (Comment _ _ l) = locOf l
  locOf (EscStm _ l) = locOf l
  locOf (AntiEscStm _ l) = locOf l
  locOf (AntiPragma _ l) = locOf l
  locOf (AntiComment _ _ l) = locOf l
  locOf (AntiStm _ l) = locOf l
  locOf (AntiStms _ l) = locOf l
  locOf (Asm _ _ _ _ _ _ l) = locOf l
  locOf (AsmGoto _ _ _ _ _ _ l) = locOf l
  locOf (ObjCTry _ _ _ l) = locOf l
  locOf (ObjCThrow _ l) = locOf l
  locOf (ObjCSynchronized _ _ l) = locOf l
  locOf (ObjCAutoreleasepool _ l) = locOf l
instance Located BlockItem where
  locOf (BlockDecl _) = noLoc
  locOf (BlockStm _) = noLoc
  locOf (AntiBlockItem _ l) = locOf l
  locOf (AntiBlockItems _ l) = locOf l
instance Located Const where
  locOf (IntConst _ _ _ l) = locOf l
  locOf (LongIntConst _ _ _ l) = locOf l
  locOf (LongLongIntConst _ _ _ l) = locOf l
  locOf (FloatConst _ _ l) = locOf l
  locOf (DoubleConst _ _ l) = locOf l
  locOf (LongDoubleConst _ _ l) = locOf l
  locOf (CharConst _ _ l) = locOf l
  locOf (StringConst _ _ l) = locOf l
  locOf (AntiConst _ l) = locOf l
  locOf (AntiInt _ l) = locOf l
  locOf (AntiUInt _ l) = locOf l
  locOf (AntiLInt _ l) = locOf l
  locOf (AntiULInt _ l) = locOf l
  locOf (AntiLLInt _ l) = locOf l
  locOf (AntiULLInt _ l) = locOf l
  locOf (AntiFloat _ l) = locOf l
  locOf (AntiDouble _ l) = locOf l
  locOf (AntiLongDouble _ l) = locOf l
  locOf (AntiChar _ l) = locOf l
  locOf (AntiString _ l) = locOf l
instance Located Exp where
  locOf (Var _ l) = locOf l
  locOf (Const _ l) = locOf l
  locOf (BinOp _ _ _ l) = locOf l
  locOf (Assign _ _ _ l) = locOf l
  locOf (PreInc _ l) = locOf l
  locOf (PostInc _ l) = locOf l
  locOf (PreDec _ l) = locOf l
  locOf (PostDec _ l) = locOf l
  locOf (UnOp _ _ l) = locOf l
  locOf (SizeofExp _ l) = locOf l
  locOf (SizeofType _ l) = locOf l
  locOf (Cast _ _ l) = locOf l
  locOf (Cond _ _ _ l) = locOf l
  locOf (Member _ _ l) = locOf l
  locOf (PtrMember _ _ l) = locOf l
  locOf (Index _ _ l) = locOf l
  locOf (FnCall _ _ l) = locOf l
  locOf (CudaCall _ _ _ l) = locOf l
  locOf (Seq _ _ l) = locOf l
  locOf (CompoundLit _ _ l) = locOf l
  locOf (StmExpr _ l) = locOf l
  locOf (EscExp _ l) = locOf l
  locOf (AntiEscExp _ l) = locOf l
  locOf (AntiExp _ l) = locOf l
  locOf (AntiArgs _ l) = locOf l
  locOf (BuiltinVaArg _ _ l) = locOf l
  locOf (BlockLit _ _ _ l) = locOf l
  locOf (ObjCMsg _ _ _ l) = locOf l
  locOf (ObjCLitConst _ _ l) = locOf l
  locOf (ObjCLitString _ l) = locOf l
  locOf (ObjCLitBool _ l) = locOf l
  locOf (ObjCLitArray _ l) = locOf l
  locOf (ObjCLitDict _ l) = locOf l
  locOf (ObjCLitBoxed _ l) = locOf l
  locOf (ObjCEncode _ l) = locOf l
  locOf (ObjCProtocol _ l) = locOf l
  locOf (ObjCSelector _ l) = locOf l
  locOf (Lambda _ _ _ l) = locOf l
instance Located LambdaIntroducer where
  locOf (LambdaIntroducer _ l) = locOf l
instance Located LambdaDeclarator where
  locOf (LambdaDeclarator _ _ _ l) = locOf l
instance Located BlockType where
  locOf (BlockVoid l) = locOf l
  locOf (BlockParam _ l) = locOf l
  locOf (BlockType _ l) = locOf l
instance Located ExeConfig where
  locOf (ExeConfig _ _ _ _ l) = locOf l
instance Located ObjCIvarDecl where
  locOf (ObjCIvarVisi _ l) = locOf l
  locOf (ObjCIvarDecl _ l) = locOf l
instance Located ObjCVisibilitySpec where
  locOf (ObjCPrivate l) = locOf l
  locOf (ObjCPublic l) = locOf l
  locOf (ObjCProtected l) = locOf l
  locOf (ObjCPackage l) = locOf l
instance Located ObjCIfaceDecl where
  locOf (ObjCIfaceProp _ _ l) = locOf l
  locOf (ObjCIfaceReq _ l) = locOf l
  locOf (ObjCIfaceMeth _ l) = locOf l
  locOf (ObjCIfaceDecl _ l) = locOf l
  locOf (AntiObjCProp _ l) = locOf l
  locOf (AntiObjCProps _ l) = locOf l
  locOf (AntiObjCIfaceDecl _ l) = locOf l
  locOf (AntiObjCIfaceDecls _ l) = locOf l
instance Located ObjCPropAttr where
  locOf (ObjCGetter _ l) = locOf l
  locOf (ObjCSetter _ l) = locOf l
  locOf (ObjCReadonly l) = locOf l
  locOf (ObjCReadwrite l) = locOf l
  locOf (ObjCAssign l) = locOf l
  locOf (ObjCRetain l) = locOf l
  locOf (ObjCCopy l) = locOf l
  locOf (ObjCNonatomic l) = locOf l
  locOf (ObjCAtomic l) = locOf l
  locOf (ObjCStrong l) = locOf l
  locOf (ObjCWeak l) = locOf l
  locOf (ObjCUnsafeUnretained l) = locOf l
  locOf (AntiObjCAttr _ l) = locOf l
  locOf (AntiObjCAttrs _ l) = locOf l
instance Located ObjCMethodReq where
  locOf (ObjCRequired l) = locOf l
  locOf (ObjCOptional l) = locOf l
instance Located ObjCParam where
  locOf (ObjCParam _ _ _ _ l) = locOf l
  locOf (AntiObjCParam _ l) = locOf l
  locOf (AntiObjCParams _ l) = locOf l
instance Located ObjCMethodProto where
  locOf (ObjCMethodProto _ _ _ _ _ _ l) = locOf l
  locOf (AntiObjCMethodProto _ l) = locOf l
instance Located ObjCCatch where
  locOf (ObjCCatch _ _ l) = locOf l
instance Located ObjCRecv where
  locOf (ObjCRecvSuper l) = locOf l
  locOf (ObjCRecvExp _ l) = locOf l
  locOf (AntiObjCRecv _ l) = locOf l
instance Located ObjCArg where
  locOf (ObjCArg _ _ l) = locOf l
  locOf (AntiObjCArg _ l) = locOf l
  locOf (AntiObjCArgs _ l) = locOf l
instance Located ObjCDictElem where
  locOf (ObjCDictElem _ _ l) = locOf l
  locOf (AntiObjCDictElems _ l) = locOf l
instance Relocatable Id where
  reloc l (Id x0 _) = (Id x0 (fromLoc l))
  reloc l (AntiId x0 _) = (AntiId x0 (fromLoc l))
instance Relocatable StringLit where
  reloc l (StringLit x0 x1 _) = (StringLit x0 x1 (fromLoc l))
instance Relocatable Storage where
  reloc l (Tauto _) = (Tauto (fromLoc l))
  reloc l (Tregister _) = (Tregister (fromLoc l))
  reloc l (Tstatic _) = (Tstatic (fromLoc l))
  reloc l (Textern x0 _) = (Textern x0 (fromLoc l))
  reloc l (Ttypedef _) = (Ttypedef (fromLoc l))
  reloc l (T__block _) = (T__block (fromLoc l))
  reloc l (TObjC__weak _) = (TObjC__weak (fromLoc l))
  reloc l (TObjC__strong _) = (TObjC__strong (fromLoc l))
  reloc l (TObjC__unsafe_unretained _) = (TObjC__unsafe_unretained (fromLoc l))
instance Relocatable TypeQual where
  reloc l (Tconst _) = (Tconst (fromLoc l))
  reloc l (Tvolatile _) = (Tvolatile (fromLoc l))
  reloc l (EscTypeQual x0 _) = (EscTypeQual x0 (fromLoc l))
  reloc l (AntiTypeQual x0 _) = (AntiTypeQual x0 (fromLoc l))
  reloc l (AntiTypeQuals x0 _) = (AntiTypeQuals x0 (fromLoc l))
  reloc l (Tinline _) = (Tinline (fromLoc l))
  reloc l (Trestrict _) = (Trestrict (fromLoc l))
  reloc l (T__restrict _) = (T__restrict (fromLoc l))
  reloc _ (TAttr x0) = (TAttr x0)
  reloc l (TCUDAdevice _) = (TCUDAdevice (fromLoc l))
  reloc l (TCUDAglobal _) = (TCUDAglobal (fromLoc l))
  reloc l (TCUDAhost _) = (TCUDAhost (fromLoc l))
  reloc l (TCUDAconstant _) = (TCUDAconstant (fromLoc l))
  reloc l (TCUDAshared _) = (TCUDAshared (fromLoc l))
  reloc l (TCUDArestrict _) = (TCUDArestrict (fromLoc l))
  reloc l (TCUDAnoinline _) = (TCUDAnoinline (fromLoc l))
  reloc l (TCLprivate _) = (TCLprivate (fromLoc l))
  reloc l (TCLlocal _) = (TCLlocal (fromLoc l))
  reloc l (TCLglobal _) = (TCLglobal (fromLoc l))
  reloc l (TCLconstant _) = (TCLconstant (fromLoc l))
  reloc l (TCLreadonly _) = (TCLreadonly (fromLoc l))
  reloc l (TCLwriteonly _) = (TCLwriteonly (fromLoc l))
  reloc l (TCLkernel _) = (TCLkernel (fromLoc l))
instance Relocatable Sign where
  reloc l (Tsigned _) = (Tsigned (fromLoc l))
  reloc l (Tunsigned _) = (Tunsigned (fromLoc l))
instance Relocatable TypeSpec where
  reloc l (Tvoid _) = (Tvoid (fromLoc l))
  reloc l (Tchar x0 _) = (Tchar x0 (fromLoc l))
  reloc l (Tshort x0 _) = (Tshort x0 (fromLoc l))
  reloc l (Tint x0 _) = (Tint x0 (fromLoc l))
  reloc l (Tlong x0 _) = (Tlong x0 (fromLoc l))
  reloc l (Tlong_long x0 _) = (Tlong_long x0 (fromLoc l))
  reloc l (Tfloat _) = (Tfloat (fromLoc l))
  reloc l (Tdouble _) = (Tdouble (fromLoc l))
  reloc l (Tlong_double _) = (Tlong_double (fromLoc l))
  reloc l (Tstruct x0 x1 x2 _) = (Tstruct x0 x1 x2 (fromLoc l))
  reloc l (Tunion x0 x1 x2 _) = (Tunion x0 x1 x2 (fromLoc l))
  reloc l (Tenum x0 x1 x2 _) = (Tenum x0 x1 x2 (fromLoc l))
  reloc l (Tnamed x0 x1 _) = (Tnamed x0 x1 (fromLoc l))
  reloc l (T_Bool _) = (T_Bool (fromLoc l))
  reloc l (Tfloat_Complex _) = (Tfloat_Complex (fromLoc l))
  reloc l (Tdouble_Complex _) = (Tdouble_Complex (fromLoc l))
  reloc l (Tlong_double_Complex _) = (Tlong_double_Complex (fromLoc l))
  reloc l (Tfloat_Imaginary _) = (Tfloat_Imaginary (fromLoc l))
  reloc l (Tdouble_Imaginary _) = (Tdouble_Imaginary (fromLoc l))
  reloc l (Tlong_double_Imaginary _) = (Tlong_double_Imaginary (fromLoc l))
  reloc l (TtypeofExp x0 _) = (TtypeofExp x0 (fromLoc l))
  reloc l (TtypeofType x0 _) = (TtypeofType x0 (fromLoc l))
  reloc l (Tva_list _) = (Tva_list (fromLoc l))
instance Relocatable DeclSpec where
  reloc l (DeclSpec x0 x1 x2 _) = (DeclSpec x0 x1 x2 (fromLoc l))
  reloc l (AntiDeclSpec x0 _) = (AntiDeclSpec x0 (fromLoc l))
  reloc l (AntiTypeDeclSpec x0 x1 x2 _) =
    (AntiTypeDeclSpec x0 x1 x2 (fromLoc l))
instance Relocatable ArraySize where
  reloc l (ArraySize x0 x1 _) = (ArraySize x0 x1 (fromLoc l))
  reloc l (VariableArraySize _) = (VariableArraySize (fromLoc l))
  reloc l (NoArraySize _) = (NoArraySize (fromLoc l))
instance Relocatable Decl where
  reloc l (DeclRoot _) = (DeclRoot (fromLoc l))
  reloc l (Ptr x0 x1 _) = (Ptr x0 x1 (fromLoc l))
  reloc l (Array x0 x1 x2 _) = (Array x0 x1 x2 (fromLoc l))
  reloc l (Proto x0 x1 _) = (Proto x0 x1 (fromLoc l))
  reloc l (OldProto x0 x1 _) = (OldProto x0 x1 (fromLoc l))
  reloc l (AntiTypeDecl x0 _) = (AntiTypeDecl x0 (fromLoc l))
  reloc l (BlockPtr x0 x1 _) = (BlockPtr x0 x1 (fromLoc l))
instance Relocatable Type where
  reloc l (Type x0 x1 _) = (Type x0 x1 (fromLoc l))
  reloc l (AntiType x0 _) = (AntiType x0 (fromLoc l))
instance Relocatable Designator where
  reloc l (IndexDesignator x0 _) = (IndexDesignator x0 (fromLoc l))
  reloc l (MemberDesignator x0 _) = (MemberDesignator x0 (fromLoc l))
instance Relocatable Designation where
  reloc l (Designation x0 _) = (Designation x0 (fromLoc l))
instance Relocatable Initializer where
  reloc l (ExpInitializer x0 _) = (ExpInitializer x0 (fromLoc l))
  reloc l (CompoundInitializer x0 _) = (CompoundInitializer x0 (fromLoc l))
  reloc l (AntiInit x0 _) = (AntiInit x0 (fromLoc l))
  reloc l (AntiInits x0 _) = (AntiInits x0 (fromLoc l))
instance Relocatable Init where
  reloc l (Init x0 x1 x2 x3 x4 _) = (Init x0 x1 x2 x3 x4 (fromLoc l))
instance Relocatable Typedef where
  reloc l (Typedef x0 x1 x2 _) = (Typedef x0 x1 x2 (fromLoc l))
instance Relocatable InitGroup where
  reloc l (InitGroup x0 x1 x2 _) = (InitGroup x0 x1 x2 (fromLoc l))
  reloc l (TypedefGroup x0 x1 x2 _) = (TypedefGroup x0 x1 x2 (fromLoc l))
  reloc l (AntiDecl x0 _) = (AntiDecl x0 (fromLoc l))
  reloc l (AntiDecls x0 _) = (AntiDecls x0 (fromLoc l))
instance Relocatable Field where
  reloc l (Field x0 x1 x2 _) = (Field x0 x1 x2 (fromLoc l))
instance Relocatable FieldGroup where
  reloc l (FieldGroup x0 x1 _) = (FieldGroup x0 x1 (fromLoc l))
  reloc l (AntiSdecl x0 _) = (AntiSdecl x0 (fromLoc l))
  reloc l (AntiSdecls x0 _) = (AntiSdecls x0 (fromLoc l))
instance Relocatable CEnum where
  reloc l (CEnum x0 x1 _) = (CEnum x0 x1 (fromLoc l))
  reloc l (AntiEnum x0 _) = (AntiEnum x0 (fromLoc l))
  reloc l (AntiEnums x0 _) = (AntiEnums x0 (fromLoc l))
instance Relocatable Attr where
  reloc l (Attr x0 x1 _) = (Attr x0 x1 (fromLoc l))
  reloc l (AntiAttr x0 _) = (AntiAttr x0 (fromLoc l))
  reloc l (AntiAttrs x0 _) = (AntiAttrs x0 (fromLoc l))
instance Relocatable Param where
  reloc l (Param x0 x1 x2 _) = (Param x0 x1 x2 (fromLoc l))
  reloc l (AntiParam x0 _) = (AntiParam x0 (fromLoc l))
  reloc l (AntiParams x0 _) = (AntiParams x0 (fromLoc l))
instance Relocatable Params where
  reloc l (Params x0 x1 _) = (Params x0 x1 (fromLoc l))
instance Relocatable Func where
  reloc l (Func x0 x1 x2 x3 x4 _) = (Func x0 x1 x2 x3 x4 (fromLoc l))
  reloc l (OldFunc x0 x1 x2 x3 x4 x5 _) =
    (OldFunc x0 x1 x2 x3 x4 x5 (fromLoc l))
instance Relocatable Definition where
  reloc l (FuncDef x0 _) = (FuncDef x0 (fromLoc l))
  reloc l (DecDef x0 _) = (DecDef x0 (fromLoc l))
  reloc l (EscDef x0 _) = (EscDef x0 (fromLoc l))
  reloc l (AntiFunc x0 _) = (AntiFunc x0 (fromLoc l))
  reloc l (AntiEsc x0 _) = (AntiEsc x0 (fromLoc l))
  reloc l (AntiEdecl x0 _) = (AntiEdecl x0 (fromLoc l))
  reloc l (AntiEdecls x0 _) = (AntiEdecls x0 (fromLoc l))
  reloc l (ObjCClassDec x0 _) = (ObjCClassDec x0 (fromLoc l))
  reloc l (ObjCClassIface x0 x1 x2 x3 x4 x5 _) =
    (ObjCClassIface x0 x1 x2 x3 x4 x5 (fromLoc l))
  reloc l (ObjCCatIface x0 x1 x2 x3 x4 _) =
    (ObjCCatIface x0 x1 x2 x3 x4 (fromLoc l))
  reloc l (ObjCProtDec x0 _) = (ObjCProtDec x0 (fromLoc l))
  reloc l (ObjCProtDef x0 x1 x2 _) = (ObjCProtDef x0 x1 x2 (fromLoc l))
  reloc l (ObjCClassImpl x0 x1 x2 x3 _) =
    (ObjCClassImpl x0 x1 x2 x3 (fromLoc l))
  reloc l (ObjCCatImpl x0 x1 x2 _) = (ObjCCatImpl x0 x1 x2 (fromLoc l))
  reloc l (ObjCSynDef x0 _) = (ObjCSynDef x0 (fromLoc l))
  reloc l (ObjCDynDef x0 _) = (ObjCDynDef x0 (fromLoc l))
  reloc l (ObjCMethDef x0 x1 _) = (ObjCMethDef x0 x1 (fromLoc l))
  reloc l (ObjCCompAlias x0 x1 _) = (ObjCCompAlias x0 x1 (fromLoc l))
  reloc l (AntiObjCMeth x0 _) = (AntiObjCMeth x0 (fromLoc l))
  reloc l (AntiObjCMeths x0 _) = (AntiObjCMeths x0 (fromLoc l))
instance Relocatable Stm where
  reloc l (Label x0 x1 x2 _) = (Label x0 x1 x2 (fromLoc l))
  reloc l (Case x0 x1 _) = (Case x0 x1 (fromLoc l))
  reloc l (CaseRange x0 x1 x2 _) = (CaseRange x0 x1 x2 (fromLoc l))
  reloc l (Default x0 _) = (Default x0 (fromLoc l))
  reloc l (Exp x0 _) = (Exp x0 (fromLoc l))
  reloc l (Block x0 _) = (Block x0 (fromLoc l))
  reloc l (If x0 x1 x2 _) = (If x0 x1 x2 (fromLoc l))
  reloc l (Switch x0 x1 _) = (Switch x0 x1 (fromLoc l))
  reloc l (While x0 x1 _) = (While x0 x1 (fromLoc l))
  reloc l (DoWhile x0 x1 _) = (DoWhile x0 x1 (fromLoc l))
  reloc l (For x0 x1 x2 x3 _) = (For x0 x1 x2 x3 (fromLoc l))
  reloc l (Goto x0 _) = (Goto x0 (fromLoc l))
  reloc l (Continue _) = (Continue (fromLoc l))
  reloc l (Break _) = (Break (fromLoc l))
  reloc l (Return x0 _) = (Return x0 (fromLoc l))
  reloc l (Pragma x0 _) = (Pragma x0 (fromLoc l))
  reloc l (Comment x0 x1 _) = (Comment x0 x1 (fromLoc l))
  reloc l (EscStm x0 _) = (EscStm x0 (fromLoc l))
  reloc l (AntiEscStm x0 _) = (AntiEscStm x0 (fromLoc l))
  reloc l (AntiPragma x0 _) = (AntiPragma x0 (fromLoc l))
  reloc l (AntiComment x0 x1 _) = (AntiComment x0 x1 (fromLoc l))
  reloc l (AntiStm x0 _) = (AntiStm x0 (fromLoc l))
  reloc l (AntiStms x0 _) = (AntiStms x0 (fromLoc l))
  reloc l (Asm x0 x1 x2 x3 x4 x5 _) = (Asm x0 x1 x2 x3 x4 x5 (fromLoc l))
  reloc l (AsmGoto x0 x1 x2 x3 x4 x5 _) =
    (AsmGoto x0 x1 x2 x3 x4 x5 (fromLoc l))
  reloc l (ObjCTry x0 x1 x2 _) = (ObjCTry x0 x1 x2 (fromLoc l))
  reloc l (ObjCThrow x0 _) = (ObjCThrow x0 (fromLoc l))
  reloc l (ObjCSynchronized x0 x1 _) = (ObjCSynchronized x0 x1 (fromLoc l))
  reloc l (ObjCAutoreleasepool x0 _) = (ObjCAutoreleasepool x0 (fromLoc l))
instance Relocatable BlockItem where
  reloc _ (BlockDecl x0) = (BlockDecl x0)
  reloc _ (BlockStm x0) = (BlockStm x0)
  reloc l (AntiBlockItem x0 _) = (AntiBlockItem x0 (fromLoc l))
  reloc l (AntiBlockItems x0 _) = (AntiBlockItems x0 (fromLoc l))
instance Relocatable Const where
  reloc l (IntConst x0 x1 x2 _) = (IntConst x0 x1 x2 (fromLoc l))
  reloc l (LongIntConst x0 x1 x2 _) = (LongIntConst x0 x1 x2 (fromLoc l))
  reloc l (LongLongIntConst x0 x1 x2 _) =
    (LongLongIntConst x0 x1 x2 (fromLoc l))
  reloc l (FloatConst x0 x1 _) = (FloatConst x0 x1 (fromLoc l))
  reloc l (DoubleConst x0 x1 _) = (DoubleConst x0 x1 (fromLoc l))
  reloc l (LongDoubleConst x0 x1 _) = (LongDoubleConst x0 x1 (fromLoc l))
  reloc l (CharConst x0 x1 _) = (CharConst x0 x1 (fromLoc l))
  reloc l (StringConst x0 x1 _) = (StringConst x0 x1 (fromLoc l))
  reloc l (AntiConst x0 _) = (AntiConst x0 (fromLoc l))
  reloc l (AntiInt x0 _) = (AntiInt x0 (fromLoc l))
  reloc l (AntiUInt x0 _) = (AntiUInt x0 (fromLoc l))
  reloc l (AntiLInt x0 _) = (AntiLInt x0 (fromLoc l))
  reloc l (AntiULInt x0 _) = (AntiULInt x0 (fromLoc l))
  reloc l (AntiLLInt x0 _) = (AntiLLInt x0 (fromLoc l))
  reloc l (AntiULLInt x0 _) = (AntiULLInt x0 (fromLoc l))
  reloc l (AntiFloat x0 _) = (AntiFloat x0 (fromLoc l))
  reloc l (AntiDouble x0 _) = (AntiDouble x0 (fromLoc l))
  reloc l (AntiLongDouble x0 _) = (AntiLongDouble x0 (fromLoc l))
  reloc l (AntiChar x0 _) = (AntiChar x0 (fromLoc l))
  reloc l (AntiString x0 _) = (AntiString x0 (fromLoc l))
instance Relocatable Exp where
  reloc l (Var x0 _) = (Var x0 (fromLoc l))
  reloc l (Const x0 _) = (Const x0 (fromLoc l))
  reloc l (BinOp x0 x1 x2 _) = (BinOp x0 x1 x2 (fromLoc l))
  reloc l (Assign x0 x1 x2 _) = (Assign x0 x1 x2 (fromLoc l))
  reloc l (PreInc x0 _) = (PreInc x0 (fromLoc l))
  reloc l (PostInc x0 _) = (PostInc x0 (fromLoc l))
  reloc l (PreDec x0 _) = (PreDec x0 (fromLoc l))
  reloc l (PostDec x0 _) = (PostDec x0 (fromLoc l))
  reloc l (UnOp x0 x1 _) = (UnOp x0 x1 (fromLoc l))
  reloc l (SizeofExp x0 _) = (SizeofExp x0 (fromLoc l))
  reloc l (SizeofType x0 _) = (SizeofType x0 (fromLoc l))
  reloc l (Cast x0 x1 _) = (Cast x0 x1 (fromLoc l))
  reloc l (Cond x0 x1 x2 _) = (Cond x0 x1 x2 (fromLoc l))
  reloc l (Member x0 x1 _) = (Member x0 x1 (fromLoc l))
  reloc l (PtrMember x0 x1 _) = (PtrMember x0 x1 (fromLoc l))
  reloc l (Index x0 x1 _) = (Index x0 x1 (fromLoc l))
  reloc l (FnCall x0 x1 _) = (FnCall x0 x1 (fromLoc l))
  reloc l (CudaCall x0 x1 x2 _) = (CudaCall x0 x1 x2 (fromLoc l))
  reloc l (Seq x0 x1 _) = (Seq x0 x1 (fromLoc l))
  reloc l (CompoundLit x0 x1 _) = (CompoundLit x0 x1 (fromLoc l))
  reloc l (StmExpr x0 _) = (StmExpr x0 (fromLoc l))
  reloc l (EscExp x0 _) = (EscExp x0 (fromLoc l))
  reloc l (AntiEscExp x0 _) = (AntiEscExp x0 (fromLoc l))
  reloc l (AntiExp x0 _) = (AntiExp x0 (fromLoc l))
  reloc l (AntiArgs x0 _) = (AntiArgs x0 (fromLoc l))
  reloc l (BuiltinVaArg x0 x1 _) = (BuiltinVaArg x0 x1 (fromLoc l))
  reloc l (BlockLit x0 x1 x2 _) = (BlockLit x0 x1 x2 (fromLoc l))
  reloc l (ObjCMsg x0 x1 x2 _) = (ObjCMsg x0 x1 x2 (fromLoc l))
  reloc l (ObjCLitConst x0 x1 _) = (ObjCLitConst x0 x1 (fromLoc l))
  reloc l (ObjCLitString x0 _) = (ObjCLitString x0 (fromLoc l))
  reloc l (ObjCLitBool x0 _) = (ObjCLitBool x0 (fromLoc l))
  reloc l (ObjCLitArray x0 _) = (ObjCLitArray x0 (fromLoc l))
  reloc l (ObjCLitDict x0 _) = (ObjCLitDict x0 (fromLoc l))
  reloc l (ObjCLitBoxed x0 _) = (ObjCLitBoxed x0 (fromLoc l))
  reloc l (ObjCEncode x0 _) = (ObjCEncode x0 (fromLoc l))
  reloc l (ObjCProtocol x0 _) = (ObjCProtocol x0 (fromLoc l))
  reloc l (ObjCSelector x0 _) = (ObjCSelector x0 (fromLoc l))
  reloc l (Lambda x0 x1 x2 _) = (Lambda x0 x1 x2 (fromLoc l))
instance Relocatable LambdaIntroducer where
  reloc l (LambdaIntroducer x0 _) = (LambdaIntroducer x0 (fromLoc l))
instance Relocatable LambdaDeclarator where
  reloc l (LambdaDeclarator x0 x1 x2 _) =
    (LambdaDeclarator x0 x1 x2 (fromLoc l))
instance Relocatable BlockType where
  reloc l (BlockVoid _) = (BlockVoid (fromLoc l))
  reloc l (BlockParam x0 _) = (BlockParam x0 (fromLoc l))
  reloc l (BlockType x0 _) = (BlockType x0 (fromLoc l))
instance Relocatable ExeConfig where
  reloc l (ExeConfig x0 x1 x2 x3 _) = (ExeConfig x0 x1 x2 x3 (fromLoc l))
instance Relocatable ObjCIvarDecl where
  reloc l (ObjCIvarVisi x0 _) = (ObjCIvarVisi x0 (fromLoc l))
  reloc l (ObjCIvarDecl x0 _) = (ObjCIvarDecl x0 (fromLoc l))
instance Relocatable ObjCVisibilitySpec where
  reloc l (ObjCPrivate _) = (ObjCPrivate (fromLoc l))
  reloc l (ObjCPublic _) = (ObjCPublic (fromLoc l))
  reloc l (ObjCProtected _) = (ObjCProtected (fromLoc l))
  reloc l (ObjCPackage _) = (ObjCPackage (fromLoc l))
instance Relocatable ObjCIfaceDecl where
  reloc l (ObjCIfaceProp x0 x1 _) = (ObjCIfaceProp x0 x1 (fromLoc l))
  reloc l (ObjCIfaceReq x0 _) = (ObjCIfaceReq x0 (fromLoc l))
  reloc l (ObjCIfaceMeth x0 _) = (ObjCIfaceMeth x0 (fromLoc l))
  reloc l (ObjCIfaceDecl x0 _) = (ObjCIfaceDecl x0 (fromLoc l))
  reloc l (AntiObjCProp x0 _) = (AntiObjCProp x0 (fromLoc l))
  reloc l (AntiObjCProps x0 _) = (AntiObjCProps x0 (fromLoc l))
  reloc l (AntiObjCIfaceDecl x0 _) = (AntiObjCIfaceDecl x0 (fromLoc l))
  reloc l (AntiObjCIfaceDecls x0 _) = (AntiObjCIfaceDecls x0 (fromLoc l))
instance Relocatable ObjCPropAttr where
  reloc l (ObjCGetter x0 _) = (ObjCGetter x0 (fromLoc l))
  reloc l (ObjCSetter x0 _) = (ObjCSetter x0 (fromLoc l))
  reloc l (ObjCReadonly _) = (ObjCReadonly (fromLoc l))
  reloc l (ObjCReadwrite _) = (ObjCReadwrite (fromLoc l))
  reloc l (ObjCAssign _) = (ObjCAssign (fromLoc l))
  reloc l (ObjCRetain _) = (ObjCRetain (fromLoc l))
  reloc l (ObjCCopy _) = (ObjCCopy (fromLoc l))
  reloc l (ObjCNonatomic _) = (ObjCNonatomic (fromLoc l))
  reloc l (ObjCAtomic _) = (ObjCAtomic (fromLoc l))
  reloc l (ObjCStrong _) = (ObjCStrong (fromLoc l))
  reloc l (ObjCWeak _) = (ObjCWeak (fromLoc l))
  reloc l (ObjCUnsafeUnretained _) = (ObjCUnsafeUnretained (fromLoc l))
  reloc l (AntiObjCAttr x0 _) = (AntiObjCAttr x0 (fromLoc l))
  reloc l (AntiObjCAttrs x0 _) = (AntiObjCAttrs x0 (fromLoc l))
instance Relocatable ObjCMethodReq where
  reloc l (ObjCRequired _) = (ObjCRequired (fromLoc l))
  reloc l (ObjCOptional _) = (ObjCOptional (fromLoc l))
instance Relocatable ObjCParam where
  reloc l (ObjCParam x0 x1 x2 x3 _) = (ObjCParam x0 x1 x2 x3 (fromLoc l))
  reloc l (AntiObjCParam x0 _) = (AntiObjCParam x0 (fromLoc l))
  reloc l (AntiObjCParams x0 _) = (AntiObjCParams x0 (fromLoc l))
instance Relocatable ObjCMethodProto where
  reloc l (ObjCMethodProto x0 x1 x2 x3 x4 x5 _) =
    (ObjCMethodProto x0 x1 x2 x3 x4 x5 (fromLoc l))
  reloc l (AntiObjCMethodProto x0 _) = (AntiObjCMethodProto x0 (fromLoc l))
instance Relocatable ObjCCatch where
  reloc l (ObjCCatch x0 x1 _) = (ObjCCatch x0 x1 (fromLoc l))
instance Relocatable ObjCRecv where
  reloc l (ObjCRecvSuper _) = (ObjCRecvSuper (fromLoc l))
  reloc l (ObjCRecvExp x0 _) = (ObjCRecvExp x0 (fromLoc l))
  reloc l (AntiObjCRecv x0 _) = (AntiObjCRecv x0 (fromLoc l))
instance Relocatable ObjCArg where
  reloc l (ObjCArg x0 x1 _) = (ObjCArg x0 x1 (fromLoc l))
  reloc l (AntiObjCArg x0 _) = (AntiObjCArg x0 (fromLoc l))
  reloc l (AntiObjCArgs x0 _) = (AntiObjCArgs x0 (fromLoc l))
instance Relocatable ObjCDictElem where
  reloc l (ObjCDictElem x0 x1 _) = (ObjCDictElem x0 x1 (fromLoc l))
  reloc l (AntiObjCDictElems x0 _) = (AntiObjCDictElems x0 (fromLoc l))
