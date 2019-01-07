module Translate where

import AbsC
import AbsC
import ErrM
type Result = Err String

-- failure :: Show a => a -> Result
-- failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Ident
transIdent x = case x of
  Ident str  -> Ident str
--変数

transUnsigned :: Unsigned -> Unsigned 
transUnsigned x = case x of
   Unsigned str  -> Unsigned str
--符号なし整数

--ansLong :: Long -> Long
--transLong x = case x of
  -- Long str  -> Long str
--64bit整数

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

transProgram :: Program -> Program
transProgram x = case x of
  Progr external_declarations  ->
    Progr (map transExternal_declaration external_declarations)


transExternal_declaration :: External_declaration -> External_declaration
transExternal_declaration x = case x of
   Afunc function_def  -> Afunc function_def
   Global dec  -> Global dec


transFunction_def :: Function_def -> Function_def
transFunction_def x = case x of
   OldFunc declaration_specifiers declarator decs compound_stm  -> OldFunc (Declaration_specifier declaration_specifiers) declarator decs compound_stm
   NewFunc declaration_specifiers declarator compound_stm  -> NewFunc (Declaration_specifier declaration_specifiers) declarator compound_stm
   OldFuncInt declarator decs compound_stm  -> OldFuncInt declarator decs compound_stm
   NewFuncInt declarator compound_stm  -> NewFuncInt declarator compound_stm


transDec :: Dec -> Dec
transDec x = case x of
  NoDeclarator declaration_specifiers  ->
    NoDeclarator (Declaration_specifier declaration_specifiers)
  Declarators declaration_specifiers init_declarators  ->
    Declarators (Declaration_specifier declaration_specifiers) (transInit_declarator init_declarators)


-- transDeclaration_specifier :: Declaration_specifier -> Declaration_specifier
--  transDeclaration_specifier x = case x of
--    Type type_specifier  -> Type (transType_specifier type_specifier)
--    Storage storage_class_specifier  ->  Storage (transStorage_class_specifier storage_class_specifier)
--    SpecProp type_qualifier  -> SpecProp (transType_qualifier type_qualifier)


-- transInit_declarator :: Init_declarator -> Init_declarator
-- transInit_declarator x = case x of
--    OnlyDecl declarator  -> OnlyDecl declarator
--    InitDecl declarator initializer  -> InitDecl declarator initializer

-- transType_specifier :: Type_specifier -> Type_specifier
-- transType_specifier x = case x of
--    Tvoid  -> Tvoid
--    Tchar  -> Tchar
--    Tshort  -> Tshort
--    Tint  -> Tint
--    Tlong  -> Tlong
--    Tfloat  -> Tfloat
--    Tdouble  -> Tdouble
--    Tsigned  -> Tsigned
--    Tunsigned  -> Tunsigned
--    Tstruct struct_or_union_spec  -> Tstruct struct_or_union_spec
--    Tenum enum_specifier  -> Tenum enum_specifier
--    Tname  -> Tname


-- transStorage_class_specifier :: Storage_class_specifier -> Storage_class_specifier
-- transStorage_class_specifier x = case x of
--   MyType  -> MyType
--   GlobalPrograms  -> GlobalPrograms
--   LocalProgram  -> LocalProgram
--   LocalBlock  -> LocalBlock
--   LocalReg  -> LocalReg


-- transType_qualifier :: Type_qualifier -> Type_qualifier
-- transType_qualifier x = case x of
--   Const  -> Const
--   NoOptim  -> NoOptim


-- transStruct_or_union_spec :: Struct_or_union_spec -> Struct_or_union_spec
-- transStruct_or_union_spec x = case x of
--   Tag struct_or_union id struct_decs  -> Tag struct_or_union id struct_decs
--   Unique struct_or_union struct_decs  -> Unique struct_or_union struct_decs
--   TagType struct_or_union id  -> TagType struct_or_union id


-- transStruct_or_union :: Struct_or_union -> Struct_or_union
-- transStruct_or_union x = case x of
--   Struct  -> Struct
--   Union  -> Union

-- transStruct_dec :: Struct_dec -> Struct_dec
-- transStruct_dec x = case x of
--   Structen spec_quals struct_declarators  -> Structen spec_quals struct_declarators


-- transSpec_qual :: Spec_qual ->  Spec_qual
-- transSpec_qual x = case x of
--   TypeSpec type_specifier  -> TypeSpec type_specifier
--   QualSpec type_qualifier  -> QualSpec type_qualifier


-- transStruct_declarator :: Struct_declarator -> Struct_declarator
-- transStruct_declarator x = case x of
--   Decl declarator  ->  Decl declarator
--   Field constant_expression  -> Field constant_expression
--   DecField declarator constant_expression  ->  DecField declarator constant_expression


-- transEnum_specifier :: Enum_specifier -> Enum_specifier
-- transEnum_specifier x = case x of
--   EnumDec enumerators  -> EnumDec enumerators
--   EnumName id enumerators  -> EnumName id enumerators
--   EnumVar id  ->  EnumVar id


-- transEnumerator :: Enumerator -> Enumerator
-- transEnumerator x = case x of
--   Plain id  -> Plain id
--   EnumInit id constant_expression  -> EnumInit id constant_expression


-- transDeclarator :: Declarator -> Declarator
-- transDeclarator x = case x of
--   BeginPointer pointer direct_declarator  -> BeginPointer pointer direct_declarator
--   NoPointer direct_declarator  -> NoPointer direct_declarator


-- transDirect_declarator :: Direct_declarator -> Direct_declarator
-- transDirect_declarator x = case x of
--   Name id  -> Name id
--   ParenDecl declarator  -> ParenDecl declarator
--   InnitArray direct_declarator constant_expression  -> InnitArray direct_declarator constant_expression
--   Incomplete direct_declarator  -> Incomplete direct_declarator
--   NewFuncDec direct_declarator parameter_type  -> NewFuncDec direct_declarator parameter_type
--   OldFuncDef direct_declarator ids  -> OldFuncDef direct_declarator ids
--   OldFuncDec direct_declarator  -> OldFuncDec direct_declarator

-- transPointer :: Pointer -> Pointer
-- transPointer x = case x of
--   Point  -> Point
--   PointQual type_qualifiers  -> PointQual type_qualifiers
--   PointPoint pointer  -> PointPoint pointer
--   PointQualPoint type_qualifiers pointer  -> PointQualPoint type_qualifiers pointer


-- transParameter_type :: Parameter_type -> Parameter_type
-- transParameter_type x = case x of
--   AllSpec parameter_declarations  -> AllSpec parameter_declarations
--   More parameter_declarations  -> More parameter_declarations


-- transParameter_declarations :: Parameter_declarations -> Parameter_declarations
-- transParameter_declarations x = case x of
--   ParamDec parameter_declaration  -> ParamDec parameter_declaration
--   MoreParamDec parameter_declarations parameter_declaration  -> MoreParamDec parameter_declarations parameter_declaration


-- transParameter_declaration :: Parameter_declaration -> Parameter_declaration
-- transParameter_declaration x = case x of
--   OnlyType declaration_specifiers  -> OnlyType declaration_specifiers
--   TypeAndParam declaration_specifiers declarator  -> TypeAndParam declaration_specifiers declarator
--   Abstract declaration_specifiers abstract_declarator  -> Abstract declaration_specifiers abstract_declarator


-- transInitializer :: Initializer -> Initializer
-- transInitializer x = case x of
---   InitExpr exp  -> InitExpr exp
--   InitListOne initializers  -> InitListOne initializers
--   InitListTwo initializers  ->  InitListTwo initializers


-- transInitializers :: Initializers -> Initializers
-- transInitializers x = case x of
--   AnInit initializer  -> AnInit initializer
--   MoreInit initializers initializer  -> MoreInit initializers initializer

-- transType_name :: Type_name -> Type_name
-- transType_name x = case x of
--   PlainType spec_quals  ->  PlainType spec_quals
--   ExtendedType spec_quals abstract_declarator  -> ExtendedType spec_quals abstract_declarator


-- transAbstract_declarator :: Abstract_declarator -> Abstract_declarator
-- transAbstract_declarator x = case x of
--   PointerStart pointer  -> PointerStart pointer
--   Advanced dir_abs_dec  ->  Advanced dir_abs_dec
--   PointAdvanced pointer dir_abs_dec  -> PointAdvanced pointer dir_abs_dec


-- transDir_abs_dec :: Dir_abs_dec -> Dir_abs_dec
-- transDir_abs_dec x = case x of
--   WithinParentes abstract_declarator  -> WithinParentes abstract_declarator
--   Array  ->  Array
--   InitiatedArray constant_expression  ->  InitiatedArray constant_expression
--   UnInitiated dir_abs_dec  -> UnInitiated dir_abs_dec
--   Initiated dir_abs_dec constant_expression  -> Initiated dir_abs_dec constant_expression
--   OldFunction  ->  OldFunction
--   NewFunction parameter_type  ->  NewFunction parameter_type
--   OldFuncExpr dir_abs_dec  -> OldFuncExpr dir_abs_dec
--   NewFuncExpr dir_abs_dec parameter_type  -> NewFuncExpr dir_abs_dec parameter_type


-- transStm :: Stm -> Stm
-- transStm x = case x of
--   LabelS labeled_stm  -> LabelS labeled_stm
--   CompS compound_stm  ->  CompS compound_stm
--   ExprS expression_stm  -> ExprS expression_stm
--   SelS selection_stm  ->  SelS selection_stm
--   IterS iter_stm  -> IterS iter_stm
--   JumpS jump_stm  -> JumpS jump_stm


-- transLabeled_stm :: Labeled_stm -> Labeled_stm
-- transLabeled_stm x = case x of
--   SlabelOne id stm  -> SlabelOne id(transStm stm)
--   SlabelTwo constant_expression stm  -> SlabelTwo constant_expression(transStm stm)
--   SlabelThree stm  -> SlabelThree(transStm stm)

-- transCompound_stm :: Compound_stm -> Compound_stm
-- transCompound_stm x = case x of
--   ScompOne  -> ScompOne
--   ScompTwo stms  -> ScompTwo stms
--   ScompThree decs  -> ScompThree decs
--   ScompFour decs stms  -> ScompFour decs stms


-- transExpression_stm :: Expression_stm -> Expression_stm
-- transExpression_stm x = case x of
--   SexprOne  -> SexprOne
--   SexprTwo exp  -> SexprTwo exp


-- transSelection_stm :: Selection_stm -> Selection_stm
-- transSelection_stm x = case x of
--   SselOne exp stm  -> SselOne exp stm
--   SselTwo exp stm1 stm2  -> SselTwo exp stm1 stm2
--   SselThree exp stm  -> SselThree exp stm


-- transIter_stm :: Iter_stm -> Iter_stm
-- transIter_stm x = case x of
--   SiterOne exp stm  -> SiterOne exp stm
--   SiterTwo stm exp  -> SiterTwo stm exp
--   SiterThree expression_stm1 expression_stm2 stm3  -> SiterThree expression_stm1 expression_stm2 stm3
--   SiterFour expression_stm1 expression_stm2 exp3 stm4  -> SiterFour expression_stm1 expression_stm2 exp3 stm4


-- transJump_stm :: Jump_stm -> Jump_stm
-- transJump_stm x = case x of
--   SjumpOne id  -> SjumpOne id
--   SjumpTwo  -> SjumpTwo
--   SjumpThree  -> SjumpThree
--   SjumpFour  -> SjumpFour
--   SjumpFive exp  -> SjumpFive (transExp exp)


-- transExp :: Exp -> Exp
-- transExp x = case x of
--  Ecomma exp1 exp2  -> Ecomma (transExp exp1) (transExp exp2)
--    Eassign exp1 assignment_op2 exp3  -> Eassign (transExp exp1) (transAssignment_op assignment_op2) (transExp exp3)
--    Econdition exp1 exp2 exp3  -> Econdition (transExp exp1) (transExp exp2) (transExp exp3)
--    Elor exp1 exp2  -> Elor (transExp exp1) (transExp exp2)
--    Eland exp1 exp2  -> Eland (transExp exp1) (transExp exp2)
--    Ebitor exp1 exp2  -> Ebitor (transExp exp1) (transExp exp2)
--    Ebitexor exp1 exp2  -> Ebitexor (transExp exp1) (transExp exp2)
--    Ebitand exp1 exp2  -> Ebitand (transExp exp1) (transExp exp2)
--    Eeq exp1 exp2  -> Eeq (transExp exp1) (transExp exp2)
--    Eneq exp1 exp2  ->  Eneq (transExp exp1) (transExp exp2)
--    Elthen exp1 exp2  -> Elthen (transExp exp1) (transExp exp2)
--    Egrthen exp1 exp2  -> Egrthen (transExp exp1) (transExp exp2)
--    Ele exp1 exp2  -> Ele (transExp exp1) (transExp exp2)
--    Ege exp1 exp2  -> Ege (transExp exp1) (transExp exp2)
--    Eleft exp1 exp2  -> Eleft (transExp exp1) (transExp exp2)
--    Eright exp1 exp2  -> Eright (transExp exp1) (transExp exp2)
--    Eplus exp1 exp2  -> Eplus (transExp exp1) (transExp exp2)
--    Eminus exp1 exp2  -> Eminus (transExp exp1) (transExp exp2)
--    Etimes exp1 exp2  -> Etimes (transExp exp1) (transExp exp2)
--    Ediv exp1 exp2  -> Ediv (transExp exp1) (transExp exp2)
--    Emod exp1 exp2  -> Emod (transExp exp1) (transExp exp2)
--    Etypeconv type_name exp  -> Etypeconv (transType_name type_name) (transExp exp)
--    Epreinc exp  -> Epreinc (transExp exp)
--    Epredec exp  -> Epredec (transExp exp)
--    Epreop unary_operator exp  -> Epreop (transUnary_operator unary_operator) (transExp exp)
--    Ebytesexpr exp  -> Ebytesexpr (transExp exp)
--    Ebytestype type_name  -> Ebytestype (transType_name type_name) 
--    Earray exp1 exp2  -> Earray (transExp exp1) (transExp exp2)
--    Efunk exp  -> Efunk (transExp exp)
--    Efunkpar exp exps  ->  Efunkpar (transExp exp) (map transExp exps)
--    Eselect exp id  -> Eselect (transExp exp) id
--    Epoint exp id  -> Epoint (transExp exp) id
--    Epostinc exp  -> Epostinc (transExp exp)
--    Epostdec exp  -> Epostdec (transExp exp)
--    Evar id  -> Evar id
--    Econst constant  -> Econst (transConstant constant)
--    Estring str  ->  Estring str

--  transConstant :: Constant -> Constant
--  transConstant x = undefined -- case x of
--   Efloat d  -> Efloat 
--   Echar c  -> Echar c
--   Eunsigned unsigned  -> Eunsigned unsigned
--   Elong long  -> Elong long
--   Eunsignlong unsignedlong  -> Eunsignlong unsignedlong
--   Ehexadec hexadecimal  ->  Ehexadec hexadecimal
--   Ehexaunsign hexunsigned  -> Ehexaunsign hexunsigned
--   Ehexalong hexlong  ->  Ehexalong hexlong
--   Ehexaunslong hexunslong  -> Ehexaunslong hexunslong
--   Eoctal octal  -> Eoctal octal
--   Eoctalunsign octalunsigned  ->  Eoctalunsign octalunsigned 
--   Eoctallong octallong  ->  Eoctallong octallong
--   Eoctalunslong octalunslong  ->  Eoctalunslong octalunslong
--   Ecdouble cdouble  -> Ecdouble cdouble
--   Ecfloat cfloat  -> Ecfloat cfloat
--   Eclongdouble clongdouble  -> Eclongdouble clongdouble
--   Eint n  ->  Eint n
--   Elonger n  -> Elonger n
--   Edouble d  -> Edouble d


-- transConstant_expression :: Constant_expression -> Constant_expression
-- transConstant_expression x = case x of
--   Especial exp  -> Especial (transExp exp)

-- transUnary_operator :: Unary_operator -> Unary_operator 
-- transUnary_operator x = case x of
--    Address  -> Address
--    Indirection  -> Indirection  
--    Plus  -> Plus 
--    Negative  -> Negative
--    Complement  -> Complement 
--    Logicalneg  -> Logicalneg 


--  transAssignment_op :: Assignment_op -> Assignment_op
--  transAssignment_op x = case x of
--    Assign  -> Assign
--    AssignMul  -> AssignMul
--    AssignDiv  -> AssignDiv
--    AssignMod  ->  AssignMod
--    AssignAdd  -> AssignAdd
--    AssignSub  -> AssignSub
--    AssignLeft  -> AssignLeft
--    AssignRight  ->  AssignRight
--    AssignAnd  ->  AssignAnd
--    AssignXor  -> AssignXor
--    AssignOr  -> AssignOr
