module Translate where
import AbsC
import AbsC
import ErrM
type Result = Err String
--0
-- failure :: Show a => a -> Result
-- failure x = Bad $ "Undefined case: " ++ show x
--1
transIdent :: Ident -> Ident
transIdent x = case x of
  Ident str  -> Ident str
--変数

transUnsigned :: Unsigned -> Unsigned 
transUnsigned x = case x of
   Unsigned str  -> Unsigned str
--符号なし整数

-- ansLong :: Long -> Long
-- transLong x = case x of
--   Long str  -> Long str
-- --64bit整数

transUnsignedLong :: UnsignedLong -> UnsignedLong
transUnsignedLong x = case x of
   UnsignedLong str  -> UnsignedLong str
--64bit整数

transHexadecimal :: Hexadecimal -> Hexadecimal
transHexadecimal x = case x of
    Hexadecimal str  -> Hexadecimal str 
-- --16進数？

transHexUnsigned :: HexUnsigned -> HexUnsigned
transHexUnsigned x = case x of
   HexUnsigned str  -> HexUnsigned str
--符号なし16進数

transHexLong :: HexLong -> HexLong
transHexLong x = case x of
   HexLong str  -> HexLong str
--64bit 16進数

transHexUnsLong :: HexUnsLong -> HexUnsLong
transHexUnsLong x = case x of
  HexUnsLong str  -> HexUnsLong str


transOctal :: Octal ->Octal
transOctal x = case x of
  Octal str  -> Octal str

transOctalUnsigned :: OctalUnsigned -> OctalUnsigned
transOctalUnsigned x = case x of
  OctalUnsigned str  -> OctalUnsigned str


transOctalLong :: OctalLong -> OctalLong
transOctalLong x = case x of
  OctalLong str  -> OctalLong str


transOctalUnsLong :: OctalUnsLong -> OctalUnsLong
transOctalUnsLong x = case x of
  OctalUnsLong str  -> OctalUnsLong str


transCDouble :: CDouble -> CDouble
transCDouble x = case x of
  CDouble str  ->  CDouble str


transCFloat :: CFloat -> CFloat
transCFloat x = case x of
  CFloat str  -> CFloat str


transCLongDouble :: CLongDouble -> CLongDouble
transCLongDouble x = case x of
  CLongDouble str  -> CLongDouble str

--16
transProgram :: Program -> Program
transProgram x = case x of
  Progr external_declarations  ->
    Progr (map transExternal_declaration external_declarations)
--17

transExternal_declaration :: External_declaration -> External_declaration
transExternal_declaration x = case x of
   Afunc function_def  -> Afunc (transFunction_def  function_def)
   Global dec  -> Global(transDec dec)

--18
transFunction_def :: Function_def -> Function_def
transFunction_def x = case x of
   OldFunc declaration_specifiers declarator decs compound_stm  -> OldFunc (map transDeclaration_specifier declaration_specifiers) (transDeclarator declarator) (map transDec decs) (transCompound_stm compound_stm)
   NewFunc declaration_specifiers declarator compound_stm  -> NewFunc (map transDeclaration_specifier declaration_specifiers) (transDeclarator declarator) (transCompound_stm compound_stm)
   OldFuncInt declarator decs compound_stm  -> OldFuncInt (transDeclarator declarator) (map transDec decs) (transCompound_stm compound_stm)
   NewFuncInt declarator compound_stm  -> NewFuncInt (transDeclarator declarator) (transCompound_stm compound_stm)

--19
transDec :: Dec -> Dec
transDec x = case x of
  NoDeclarator declaration_specifiers  ->
    NoDeclarator (map transDeclaration_specifier declaration_specifiers)
  Declarators declaration_specifiers init_declarators  ->
    Declarators (map transDeclaration_specifier declaration_specifiers) (map transInit_declarator init_declarators)

--20
transDeclaration_specifier :: Declaration_specifier -> Declaration_specifier
transDeclaration_specifier x = case x of
   Type type_specifier  -> Type (transType_specifier type_specifier)
   Storage storage_class_specifier  ->  Storage (transStorage_class_specifier storage_class_specifier)
   SpecProp type_qualifier  -> SpecProp (transType_qualifier type_qualifier)

--21
transInit_declarator :: Init_declarator -> Init_declarator
transInit_declarator x = case x of
    OnlyDecl declarator  -> OnlyDecl (transDeclarator declarator)
    InitDecl declarator initializer  -> InitDecl (transDeclarator declarator) (transInitializer initializer)

--22
transType_specifier :: Type_specifier -> Type_specifier
transType_specifier x = case x of
   Tvoid  -> Tvoid
   Tchar  -> Tchar
   Tshort  -> Tshort
   Tint  -> Tint
   Tlong  -> Tlong
   Tfloat  -> Tfloat
   Tdouble  -> Tdouble
   Tsigned  -> Tsigned
   Tunsigned  -> Tunsigned
   Tstruct struct_or_union_spec  -> Tstruct (transStruct_or_union_spec struct_or_union_spec)
   Tenum enum_specifier  -> Tenum (transEnum_specifier enum_specifier)
   Tname  -> Tname

--23
transStorage_class_specifier :: Storage_class_specifier -> Storage_class_specifier
transStorage_class_specifier x = case x of
  MyType  -> MyType
  GlobalPrograms  -> GlobalPrograms
  LocalProgram  -> LocalProgram
  LocalBlock  -> LocalBlock
  LocalReg  -> LocalReg

--24
transType_qualifier :: Type_qualifier -> Type_qualifier
transType_qualifier x = case x of
  Const  -> Const
  NoOptim  -> NoOptim

--25
transStruct_or_union_spec :: Struct_or_union_spec -> Struct_or_union_spec
transStruct_or_union_spec x = case x of
  Tag struct_or_union id struct_decs  -> Tag (transStruct_or_union struct_or_union) id (map transStruct_dec struct_decs)
  Unique struct_or_union struct_decs  -> Unique (transStruct_or_union struct_or_union) (map transStruct_dec struct_decs)
  TagType struct_or_union id  -> TagType (transStruct_or_union struct_or_union) id

--26
transStruct_or_union :: Struct_or_union -> Struct_or_union
transStruct_or_union x = case x of
  Struct  -> Struct
  Union  -> Union

--27
transStruct_dec :: Struct_dec -> Struct_dec
transStruct_dec x = case x of
  Structen spec_quals struct_declarators  -> Structen (map transSpec_qual spec_quals) (map transStruct_declarator struct_declarators)

--28
transSpec_qual :: Spec_qual ->  Spec_qual
transSpec_qual x = case x of
  TypeSpec type_specifier  -> TypeSpec (transType_specifier type_specifier)
  QualSpec type_qualifier  -> QualSpec (transType_qualifier type_qualifier)

--29
transStruct_declarator :: Struct_declarator -> Struct_declarator
transStruct_declarator x = case x of
  Decl declarator  ->  Decl (transDeclarator declarator)
  Field constant_expression  -> Field (transConstant_expression constant_expression)
  DecField declarator constant_expression  ->  DecField (transDeclarator declarator) (transConstant_expression constant_expression)

--30
transEnum_specifier :: Enum_specifier -> Enum_specifier
transEnum_specifier x = case x of
  EnumDec enumerators  -> EnumDec (map transEnumerator enumerators)
  EnumName id enumerators  -> EnumName id (map transEnumerator enumerators)
  EnumVar id  ->  EnumVar id

--31
transEnumerator :: Enumerator -> Enumerator
transEnumerator x = case x of
  Plain id  -> Plain id
  EnumInit id constant_expression  -> EnumInit id (transConstant_expression constant_expression)

--32
transDeclarator :: Declarator -> Declarator
transDeclarator x = case x of
  BeginPointer pointer direct_declarator  -> BeginPointer (transPointer pointer) (transDirect_declarator direct_declarator)
  NoPointer direct_declarator  -> NoPointer (transDirect_declarator direct_declarator)

--33
transDirect_declarator :: Direct_declarator -> Direct_declarator
transDirect_declarator x = case x of
  Name id  -> Name id
  ParenDecl declarator  -> ParenDecl (transDeclarator declarator)
  InnitArray direct_declarator constant_expression  -> InnitArray (transDirect_declarator direct_declarator) (transConstant_expression constant_expression)
  Incomplete direct_declarator  -> Incomplete (transDirect_declarator direct_declarator)
  NewFuncDec direct_declarator parameter_type  -> NewFuncDec (transDirect_declarator direct_declarator) (transParameter_type parameter_type)
  OldFuncDef direct_declarator ids  -> OldFuncDef (transDirect_declarator direct_declarator) ids
  OldFuncDec direct_declarator  -> OldFuncDec (transDirect_declarator direct_declarator)
--34
transPointer :: Pointer -> Pointer
transPointer x = case x of
  Point  -> Point
  PointQual type_qualifiers  -> PointQual (map transType_qualifier type_qualifiers)
  PointPoint pointer  -> PointPoint (transPointer pointer)
  PointQualPoint type_qualifiers pointer  -> PointQualPoint (map transType_qualifier type_qualifiers) (transPointer pointer)

--35
transParameter_type :: Parameter_type -> Parameter_type
transParameter_type x = case x of
  AllSpec parameter_declarations  -> AllSpec (transParameter_declarations parameter_declarations)
  More parameter_declarations  -> More (transParameter_declarations parameter_declarations)

--36
transParameter_declarations :: Parameter_declarations -> Parameter_declarations
transParameter_declarations x = case x of
  ParamDec parameter_declaration  -> ParamDec (transParameter_declaration parameter_declaration)
  MoreParamDec parameter_declarations parameter_declaration  -> MoreParamDec (transParameter_declarations parameter_declarations) (transParameter_declaration parameter_declaration)

--37
transParameter_declaration :: Parameter_declaration -> Parameter_declaration
transParameter_declaration x = case x of
  OnlyType declaration_specifiers  -> OnlyType (map transDeclaration_specifier declaration_specifiers)
  TypeAndParam declaration_specifiers declarator  -> TypeAndParam (map transDeclaration_specifier declaration_specifiers) (transDeclarator declarator)
  Abstract declaration_specifiers abstract_declarator  -> Abstract (map transDeclaration_specifier declaration_specifiers) (transAbstract_declarator abstract_declarator)

--38
transInitializer :: Initializer -> Initializer
transInitializer x = case x of
  InitExpr exp  -> InitExpr (transExp exp)
  InitListOne initializers  -> InitListOne (transInitializers initializers)
  InitListTwo initializers  ->  InitListTwo (transInitializers initializers)

--39
transInitializers :: Initializers -> Initializers
transInitializers x = case x of
  AnInit initializer  -> AnInit (transInitializer initializer)
  MoreInit initializers initializer  -> MoreInit (transInitializers initializers) (transInitializer initializer)
--40
transType_name :: Type_name -> Type_name
transType_name x = case x of
  PlainType spec_quals  ->  PlainType (map transSpec_qual spec_quals)
  ExtendedType spec_quals abstract_declarator  -> ExtendedType (map transSpec_qual spec_quals) (transAbstract_declarator abstract_declarator)

--41
transAbstract_declarator :: Abstract_declarator -> Abstract_declarator
transAbstract_declarator x = case x of
  PointerStart pointer  -> PointerStart (transPointer pointer)
  Advanced dir_abs_dec  ->  Advanced (transDir_abs_dec dir_abs_dec)
  PointAdvanced pointer dir_abs_dec  -> PointAdvanced (transPointer pointer) (transDir_abs_dec dir_abs_dec)

--42
transDir_abs_dec :: Dir_abs_dec -> Dir_abs_dec
transDir_abs_dec x = case x of
  WithinParentes abstract_declarator  -> WithinParentes (transAbstract_declarator abstract_declarator)
  Array  ->  Array
  InitiatedArray constant_expression  ->  InitiatedArray (transConstant_expression constant_expression)
  UnInitiated dir_abs_dec  -> UnInitiated (transDir_abs_dec dir_abs_dec)
  Initiated dir_abs_dec constant_expression  -> Initiated (transDir_abs_dec dir_abs_dec) (transConstant_expression constant_expression)
  OldFunction  ->  OldFunction
  NewFunction parameter_type  ->  NewFunction (transParameter_type parameter_type)
  OldFuncExpr dir_abs_dec  -> OldFuncExpr (transDir_abs_dec dir_abs_dec)
  NewFuncExpr dir_abs_dec parameter_type  -> NewFuncExpr (transDir_abs_dec dir_abs_dec) (transParameter_type parameter_type)

--43
transStm :: Stm -> Stm
transStm x = case x of
  LabelS labeled_stm  -> LabelS (transLabeled_stm labeled_stm)
  CompS compound_stm  ->  CompS (transCompound_stm compound_stm)
  ExprS expression_stm  -> ExprS (transExpression_stm expression_stm)
  SelS selection_stm  ->  SelS selection_stm
  IterS iter_stm  -> IterS iter_stm
  JumpS jump_stm  -> JumpS jump_stm

--44
transLabeled_stm :: Labeled_stm -> Labeled_stm
transLabeled_stm x = case x of
  SlabelOne id stm  -> SlabelOne id(transStm stm)
  SlabelTwo constant_expression stm  -> SlabelTwo (transConstant_expression constant_expression)(transStm stm)
  SlabelThree stm  -> SlabelThree(transStm stm)
--45
transCompound_stm :: Compound_stm -> Compound_stm
transCompound_stm x = case x of
  ScompOne  -> ScompOne
  ScompTwo stms  -> ScompTwo stms
  ScompThree decs  -> ScompThree (map transDec decs)
  ScompFour decs stms  -> ScompFour (map transDec decs) stms

--46
transExpression_stm :: Expression_stm -> Expression_stm
transExpression_stm x = case x of
  SexprOne  -> SexprOne
  SexprTwo exp  -> SexprTwo (transExp exp)

--47
transSelection_stm :: Selection_stm -> Selection_stm
transSelection_stm x = case x of
  SselOne exp stm  -> SselOne (transExp exp) stm
  SselTwo exp stm1 stm2  -> SselTwo (transExp exp) stm1 stm2
  SselThree exp stm  -> SselThree (transExp exp) stm

--48
transIter_stm :: Iter_stm -> Iter_stm
transIter_stm x = case x of
  SiterOne exp stm  -> SiterOne (transExp exp) stm
  SiterTwo stm exp  -> SiterTwo stm (transExp exp)
  SiterThree expression_stm1 expression_stm2 stm3  -> SiterThree (transExpression_stm expression_stm1) (transExpression_stm expression_stm2) stm3
  SiterFour expression_stm1 expression_stm2 exp3 stm4  -> SiterFour (transExpression_stm expression_stm1) (transExpression_stm expression_stm2) (transExp exp3) stm4

--49
transJump_stm :: Jump_stm -> Jump_stm
transJump_stm x = case x of
  SjumpOne id  -> SjumpOne id
  SjumpTwo  -> SjumpTwo
  SjumpThree  -> SjumpThree
  SjumpFour  -> SjumpFour
  SjumpFive exp  -> SjumpFive (transExp exp)

--50
transExp :: Exp -> Exp
transExp x = case x of
  Ecomma exp1 exp2  -> Ecomma (transExp exp1) (transExp exp2)
  Eassign exp1 assignment_op2 exp3  -> Eassign (transExp exp1) (transAssignment_op assignment_op2) (transExp exp3)
  Econdition exp1 exp2 exp3  -> Econdition (transExp exp1) (transExp exp2) (transExp exp3)
  Elor exp1 exp2  -> Elor (transExp exp1) (transExp exp2)
  Eland exp1 exp2  -> Eland (transExp exp1) (transExp exp2)
  Ebitor exp1 exp2  -> Ebitor (transExp exp1) (transExp exp2)
  Ebitexor exp1 exp2  -> Ebitexor (transExp exp1) (transExp exp2)
  Ebitand exp1 exp2  -> Ebitand (transExp exp1) (transExp exp2)
  Eeq exp1 exp2  -> Eeq (transExp exp1) (transExp exp2)
  Eneq exp1 exp2  ->  Eneq (transExp exp1) (transExp exp2)
  Elthen exp1 exp2  -> Elthen (transExp exp1) (transExp exp2)
  Egrthen exp1 exp2  -> Egrthen (transExp exp1) (transExp exp2)
  Ele exp1 exp2  -> Ele (transExp exp1) (transExp exp2)
  Ege exp1 exp2  -> Ege (transExp exp1) (transExp exp2)
  Eleft exp1 exp2  -> Eleft (transExp exp1) (transExp exp2)
  Eright exp1 exp2  -> Eright (transExp exp1) (transExp exp2)
  Eplus exp1 exp2  -> Eplus (transExp exp1) (transExp exp2)
  Eminus exp1 exp2  -> Eminus (transExp exp1) (transExp exp2)
  Etimes exp1 exp2  -> Etimes (transExp exp1) (transExp exp2)
  Ediv exp1 exp2  -> Ediv (transExp exp1) (transExp exp2)
  Emod exp1 exp2  -> Emod (transExp exp1) (transExp exp2)
  Etypeconv type_name exp  -> Etypeconv (transType_name type_name) (transExp exp)
  Epreinc exp  -> Epreinc (transExp exp)
  Epredec exp  -> Epredec (transExp exp)
  Epreop unary_operator exp  -> Epreop (transUnary_operator unary_operator) (transExp exp)
  Ebytesexpr exp  -> Ebytesexpr (transExp exp)
  Ebytestype type_name  -> Ebytestype (transType_name type_name) 
  Earray exp1 exp2  -> Earray (transExp exp1) (transExp exp2)
  Efunk exp  -> Efunk (transExp exp)
  Efunkpar exp exps  ->  Efunkpar (transExp exp) (map transExp exps)
  Eselect exp id  -> Eselect (transExp exp) id
  Epoint exp id  -> Epoint (transExp exp) id
  Epostinc exp  -> Epostinc (transExp exp)
  Epostdec exp  -> Epostdec (transExp exp)
  Evar id  -> Evar id
  Econst constant  -> Econst (transConstant constant)
  Estring str  ->  Estring str
-- エラーをはくとき =undefinedにする
--51
transConstant :: Constant -> Constant
transConstant x = case x of
  Efloat d  -> Efloat d
  Echar c  -> Echar c
  Eunsigned unsigned  -> Eunsigned (transUnsigned unsigned)
  Elong long  -> Elong long
  Eunsignlong unsignedlong  -> Eunsignlong unsignedlong
  Ehexadec hexadecimal  ->  Ehexadec hexadecimal
  Ehexaunsign hexunsigned  -> Ehexaunsign hexunsigned
  Ehexalong hexlong  ->  Ehexalong hexlong
  Ehexaunslong hexunslong  -> Ehexaunslong hexunslong
  Eoctal octal  -> Eoctal octal
  Eoctalunsign octalunsigned  ->  Eoctalunsign octalunsigned 
  Eoctallong octallong  ->  Eoctallong octallong
  Eoctalunslong octalunslong  ->  Eoctalunslong octalunslong
  Ecdouble cdouble  -> Ecdouble cdouble
  Ecfloat cfloat  -> Ecfloat cfloat
  Eclongdouble clongdouble  -> Eclongdouble clongdouble
  Eint n  ->  Eint n
  Elonger n  -> Elonger n
  Edouble d  -> Edouble d

--52
transConstant_expression :: Constant_expression -> Constant_expression
transConstant_expression x = case x of
  Especial exp  -> Especial (transExp exp)
--53
transUnary_operator :: Unary_operator -> Unary_operator 
transUnary_operator x = case x of
   Address  -> Address
   Indirection  -> Indirection  
   Plus  -> Plus 
   Negative  -> Negative
   Complement  -> Complement 
   Logicalneg  -> Logicalneg 

--54
transAssignment_op :: Assignment_op -> Assignment_op
transAssignment_op x = case x of
   Assign  -> Assign
   AssignMul  -> AssignMul
   AssignDiv  -> AssignDiv
   AssignMod  ->  AssignMod
   AssignAdd  -> AssignAdd
   AssignSub  -> AssignSub
   AssignLeft  -> AssignLeft
   AssignRight  ->  AssignRight
   AssignAnd  ->  AssignAnd
   AssignXor  -> AssignXor
   AssignOr  -> AssignOr
