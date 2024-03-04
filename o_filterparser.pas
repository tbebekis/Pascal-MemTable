unit o_FilterParser;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
   SysUtils
  ,Classes
  ,Controls
  ,Types
  ,Variants

  ,DB
  ,TypInfo
  ,Masks
  ,StrUtils
  ,DateUtils


  ,FmtBcd
  ;

type
(*----------------------------------------------------------------------------*)
  TExtractFieldValueEvent = function (const FieldName: string; RecBuf: PChar): Variant of object;
(*----------------------------------------------------------------------------*)
  TFilterParser = class(TObject)
  protected
    FRecBuf    : PChar;
    FData      : PChar;
    FEvent     : TExtractFieldValueEvent;
    FNodeOfs   : Integer;

    function  Parse(pNode: Pointer): Variant;

    function  ParseUNARY    (pNode: Pointer): Variant;
    function  ParseBINARY   (pNode: Pointer): Variant;
    function  ParseCOMPARE  (pNode: Pointer): Variant;
    function  ParseFIELD    (pNode: Pointer): Variant;
    function  ParseCONST    (pNode: Pointer): Variant;
    function  ParseFUNC     (pNode: Pointer): Variant;
    function  ParseLISTELEM (pNode: Pointer): Variant;

    class function  IsNull(const Value: Variant): Boolean;
    class function  EnumValueToStr(Info: PTypeInfo; Value, PrefixCut: Integer): string;
  public
    function FilterRecord(RecBuf, FilterData: PChar; Event: TExtractFieldValueEvent): Boolean;
  end;

implementation





type
  TCANOperator = (
    coNOTDEFINED,                      {                                   }
    coISBLANK,                         { coUnary;  is operand blank.      }
    coNOTBLANK,                        { coUnary;  is operand not blank.  }
    coEQ,                              { coBinary, coCompare; equal.     }
    coNE,                              { coBinary; NOT equal.             }
    coGT,                              { coBinary; greater than.          }
    coLT,                              { coBinary; less than.             }
    coGE,                              { coBinary; greater or equal.      }
    coLE,                              { coBinary; less or equal.         }
    coNOT,                             { coUnary; NOT                     }
    coAND,                             { coBinary; AND                    }
    coOR,                              { coBinary; OR                     }
    coTUPLE2,                          { coUnary; Entire record is operand. }
    coFIELD2,                          { coUnary; operand is field        }
    coCONST2,                          { coUnary; operand is constant     }
    coMINUS,                           { coUnary;  minus. }
    coADD,                             { coBinary; addition. }
    coSUB,                             { coBinary; subtraction. }
    coMUL,                             { coBinary; multiplication. }
    coDIV,                             { coBinary; division. }
    coMOD,                             { coBinary; modulo division. }
    coREM,                             { coBinary; remainder of division. }
    coSUM,                             { coBinary, accumulate sum of. }
    coCOUNT,                           { coBinary, accumulate count of. }
    coMIN,                             { coBinary, find minimum of. }
    coMAX,                             { coBinary, find maximum of. }
    coAVG,                             { coBinary, find average of. }
    coCONT,                            { coBinary; provides a link between two }
    coUDF2,                            { coBinary; invokes a User defined fn }
    coCONTINUE2,                       { coUnary; Stops evaluating records }
    coLIKE,                            { coCompare, extended binary compare        }
    coIN,                              { coBinary field in list of values }
    coLIST2,                           { List of constant values of same type }
    coUPPER,                           { coUnary: upper case }
    coLOWER,                           { coUnary: lower case }
    coFUNC2,                           { coFunc: Function }
    coLISTELEM2,                       { coListElem: List Element }
    coASSIGN                           { coBinary: Field assignment }
  );

  NODEClass = (                         { Node Class }
    nodeNULL,                           { Null node                   }
    nodeUNARY,                          { Node is a unary             }
    nodeBINARY,                         { Node is a binary            }
    nodeCOMPARE,                        { Node is a compare           }
    nodeFIELD,                          { Node is a field             }
    nodeCONST,                          { Node is a constant          }
    nodeTUPLE,                          { Node is a record }
    nodeCONTINUE,                       { Node is a continue node     }
    nodeUDF,                            { Node is a UDF node }
    nodeLIST,                           { Node is a LIST node }
    nodeFUNC,                           { Node is a Function node }
    nodeLISTELEM                        { Node is a List Element node }
  );





{ taken from Delphi's 6 BDE.int }
(*----------------------------------------------------------------------------
  Definitions taken from Delphi's 6 BDE.int

  The nodeClass and canOp field of each record changed to Integer type
  in order to have the proper memory alignment for the record
 ----------------------------------------------------------------------------*)
type
  pCANHdr = ^CANHdr;
  CANHdr = packed record                { Header part common to all     (*) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record              { Unary Node                    (*) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iOperand1       : Word;             { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record             { Binary Node                   (*) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record              { Field }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iFieldNum       : Word;
    iNameOffset     : Word;             { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record              { Constant }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iType           : Word;             { Constant type. }
    iSize           : Word;             { Constant size. (in bytes) }
    iOffset         : Word;             { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record              { Tuple (record) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iSize           : Word;             { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;          { A Continue node is used to stop evaluating when a certain condition is false for the first time.}
  CANContinue = packed record           { Break Node                    (*) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iContOperand    : Word;             { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record            { Extended compare Node (text fields) (*) }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;          { canLIKE, canEQ }
    bCaseInsensitive : WordBool;        { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen     : Word;             { Partial fieldlength (0 is full length) }
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record               { Function }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iNameOffset     : Word;             { Name offset in Literal pool }
    iElemOffset     : Word;             { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record           { List Element }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iOffset         : Word;             { Arg offset in Node pool }
    iNextOffset     : Word;             { Offset in Node pool of next ListElem or 0 if end of list }
  end;

  pCANUdf = ^CANUdf;
  CANUdf = packed record                { A user defined function }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iOffSzFuncName  : Word;             { Offset in literal pool to Function Name string(0 terminated) }
    iOperands       : Word;             { Byte offset of Operands (concatenated using canCONT) }
    iDrvDialect     : Word;             { Driver Dialect ID for UDF string supplied }
    iOffSzUDF       : Word;             { Offset in literal pool to UDF string (0 terminated) }
  end;

  pCANList = ^CANList;
  CANList = packed record           { List of Constants }
    nodeClass       : Integer; // NODEClass;
    canOp           : Integer; // TCANOperator;
    iType           : Word;            { Constant type. }
    iTotalSize      : Word;            { Total list size; }
    iElemSize       : Word;            { Size of each elem for fix-width types }
    iElems          : Word;            { Number of elements in list }
    iOffset         : Word;            { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr      : CANHdr);
      1: (canUnary    : CANUnary);
      2: (canBinary   : CANBinary);
      3: (canField    : CANField);
      4: (canConst    : CANConst);
      5: (canTuple    : CANTuple);
      6: (canContinue : CANContinue);
      7: (canCompare  : CANCompare);
      8: (canList     : CANList);
      9: (canFunc     : CANFunc);
     10: (canListElem : CANListElem);
  end;



type
  ppCANExpr = ^pCANExpr;
  pCANExpr  = ^CANExpr;
  CANExpr   = packed record             { Expression Tree }
    iVer            : Word;             { Version tag of expression. }
    iTotalSize      : Word;             { Size of this structure }
    iNodes          : Word;             { Number of nodes }
    iNodeStart      : Word;             { Starting offet of Nodes in this }
    iLiteralStart   : Word;             { Starting offset of Literals in this }
  end;




class function  TFilterParser.IsNull(const Value: Variant): Boolean;
begin
  Result := VarIsNull(Value) or VarIsClear(Value) or VarIsEmpty(Value)
end;
class function TFilterParser.EnumValueToStr(Info: PTypeInfo; Value, PrefixCut: Integer): string;
begin
  Result := GetEnumName(Info, Value);
  if  (PrefixCut <> 0) then
    Result := Copy(Result, PrefixCut + 1, Length(Result) - PrefixCut);
end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseUNARY(pNode: Pointer): Variant;
var
  Node1       : Pointer;
  Value1      : Variant;
  Operator_    : TCANOperator;
begin

  Node1     := FData + FNodeOfs + pCANUnary(pNode)^.iOperand1;
  Value1    := Parse(Node1);
  Operator_  := TCANOperator(pCANUnary(pNode)^.canOp);

  case Operator_ of
    coISBLANK  : Result := IsNull(Value1);
    coNOTBLANK : Result := not IsNull(Value1);
    coNOT      : if IsNull(Value1) then Result := Null else Result := not Value1;
    coMINUS    : if IsNull(Value1) then Result := Null else Result := -Value1;
    coUPPER    : if IsNull(Value1) then Result := Null else Result := UpperCase(Value1);
    coLOWER    : if IsNull(Value1) then Result := Null else Result := LowerCase(Value1);
  end;

end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseBINARY(pNode: Pointer): Variant;
var
  Node1       : Pointer;
  Node2       : Pointer;

  Value1      : Variant;
  Value2      : Variant;

  Operator_    : TCANOperator;

  i           : Integer;

  S1          : string;
  S2          : string;
begin

  Node1 := FData + FNodeOfs + PCANBinary(pNode)^.iOperand1;
  Node2 := FData + FNodeOfs + PCANBinary(pNode)^.iOperand2;

  Value1 := Parse(Node1);
  Value2 := Parse(Node2);

  Operator_ := TCANOperator(PCANBinary(pNode)^.canOp);

  if IsNull(Value1) or IsNull(Value2) then
  begin

    if (Operator_ in [coADD, coSUB, coMUL, coDIV, coMOD, coREM]) then
      Result := Null
    else
      Result := False;

  end else begin
    case Operator_ of
      coEQ      : Result := (Value1 =   Value2);
      coNE      : Result := (Value1 <>  Value2);
      coGT      : Result := (Value1 >   Value2);
      coGE      : Result := (Value1 >=  Value2);
      coLT      : Result := (Value1 <   Value2);
      coLE      : Result := (Value1 <=  Value2);
      coOR      : Result := (Value1 or  Value2);
      coAND     : Result := (Value1 and Value2);

      coADD     : Result := (Value1 + Value2);
      coSUB     : Result := (Value1 - Value2);
      coMUL     : Result := (Value1 * Value2);
      coDIV     : Result := (Value1 / Value2);
      coMOD     ,
      coREM     : Result := (Value1 mod Value2);

      coIN      : begin
                    if VarIsArray(Value2) then
                    begin
                      Result := False;
                      for i := 0 to VarArrayHighBound(Value2, 1) do
                      begin
                        if VarIsEmpty(Value2[i]) then break;
                        Result := (Value1 = Value2[i]);
                        if Result then
                          Break;
                      end;
                    end
                    else
                      Result := (Value1 = Value2);
                  end;

      coLike    : begin
                    S1      := VarToStr(Value1);
                    S2      := VarToStr(Value2);
                    S2      := AnsiReplaceStr(S2, '%', '*');
                    Result  := MatchesMask(S1, S2);
                  end;
      else        DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);
    end;
  end;

end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseCOMPARE(pNode: Pointer): Variant;
var
  Node1       : Pointer;
  Node2       : Pointer;

  Value1      : Variant;
  Value2      : Variant;

  Operator_    : TCANOperator;

  S1          : string;
  S2          : string;
begin

  Node1 := FData + FNodeOfs + pCANCompare(pNode)^.iOperand1;
  Node2 := FData + FNodeOfs + pCANCompare(pNode)^.iOperand2;

  Value1  := Parse(Node1);
  Value2  := Parse(Node2);

  if IsNull(Value1) or IsNull(Value2) then
    Result := False
  else begin
    S1 := Value1;
    S2 := Value2;

    if pCANCompare(pNode)^.bCaseInsensitive then
    begin
      S1 := AnsiUpperCase(S1);
      S2 := AnsiUpperCase(S2);
    end;

    if pCANCompare(pNode)^.iPartialLen > 0 then  { Partial fieldlength (0 is full length) }
    begin
      S1 := Copy(S1, 1, pCANCompare(pNode)^.iPartialLen);
      S2 := Copy(S2, 1, pCANCompare(pNode)^.iPartialLen);
    end;

    Operator_ := TCANOperator(pCANCompare(pNode)^.canOp);

    case Operator_ of
      coEQ    : Result := (S1 = S2);
      coNE    : Result := (S1 <> S2);
      coLIKE  : begin
                  S1  := VarToStr(Parse(Node1));
                  S2  := VarToStr(Parse(Node2));
                  S2  := AnsiReplaceStr(S2, '%', '*');
                  Result := MatchesMask(S1, S2);
                end;
      else      DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);
    end;
  end;
end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseFIELD(pNode: Pointer): Variant;
var
  Operator_    : TCANOperator;

  Offset      : Integer;
  Data1       : PChar;
  FieldName   : string;
begin
  Operator_ := TCANOperator(pCANField(pNode)^.canOp);

  if Operator_ <> coFIELD2 then
    DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);

  Offset       := pCANExpr(FData)^.iLiteralStart + pCANField(pNode)^.iNameOffset;
  Data1        := FData + Offset;
  FieldName    := string(Data1);

  Result       := FEvent(FieldName, FRecBuf);
end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseCONST(pNode: Pointer): Variant;
var
  Operator_    : TCANOperator;

  Offset      : Integer;
  Data1       : PChar;
  FieldType   : TFieldType;

  TS          : TTimeStamp;
  BCD         : TBCD;
  CY          : Currency;
begin
  Operator_ := TCANOperator(pCANConst(pNode)^.canOp);

  if Operator_ <> coCONST2 then
    DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);

  Offset       := pCANExpr(FData)^.iLiteralStart + pCANConst(pNode)^.iOffset;
  Data1        := FData + Offset;

  FieldType    := TFieldType(pCANConst(pNode)^.iType);

  case FieldType of
    ftSmallInt      ,
    ftWord          : Result := PWord(Data1)^;

    ftInteger       ,
    ftAutoInc       : Result := PInteger(Data1)^;

    ftFloat         ,
    ftCurrency      : Result := PDouble(Data1)^;

    ftGUID          ,
    ftWideString    : Result := PWideString(Data1)^;

    ftString        ,
    ftFixedChar     : Result := string(Data1);

    ftDate          : begin
                        TS.Date := PInteger(Data1)^;
                        TS.Time := 0;
                        Result := TimeStampToDateTime(TS);
                      end;

    ftTime          : begin
                        TS.Date := 0;
                        TS.Time := PInteger(Data1)^;
                        Result  := TimeStampToDateTime(TS);
                      end;

    ftDateTime      : begin
                        TS      := MSecsToTimeStamp(PDouble(Data1)^);
                        Result  := TimeStampToDateTime(TS);
                      end;

    ftBoolean        : Result := PWordBool(Data1)^;

    // TODO: ftTimeStamp      : Result := VarSQLTimeStampCreate(PSQLTimeStamp(Data1)^);

    ftBCD            ,
    ftFmtBCD         : begin
                         BCD := PBCD(Data1)^;
                         BCDToCurr(BCD, CY);
                         Result := CY;
                       end;

    else DatabaseErrorFmt('Unknown field type in filter expression: %s', [FieldTypeNames[FieldType]]);

  end;
end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseFUNC(pNode: Pointer): Variant;
  {------------------------------------------------------------}
  function DateDecodeFunc(DT: TDateTime; Tag: Integer): Word;
  var
    Year              : Word;
    Month             : Word;
    Day               : Word;
  begin
    DecodeDate(DT, Year, Month, Day);
    case Tag of
      0   : Result := Year;
      1   : Result := Month;
      else  Result := Day;
    end;
  end;
  {------------------------------------------------------------}
  function TimeDecodeFunc(DT: TDateTime; Tag: Integer): Word;
  var
    Hour              : Word;
    Minute            : Word;
    Second            : Word;
    MSec              : Word;
  begin
    DecodeTime(DT, Hour, Minute, Second, MSec);
    case Tag of
      0   : Result := Hour;
      1   : Result := Minute;
      else  Result := Second;
    end;
  end;
  {------------------------------------------------------------}
  function Substring(Params: Variant): string;
  var
    S     : string;
    Index : Integer;
    Count : Integer;
  begin
    Result := '';
    try
      if VarIsArray(Params) and (VarArrayHighBound(Params, 1) >= 1) then
      begin
        S      := VarToStr(Params[0]);
        Index  := Params[1];
        if VarArrayHighBound(Params, 1) >= 2 then
          Count := Params[2]
        else
          Count := MaxInt;
        Result := Copy(S, Index, Count);
      end;
    except
    end;

  end;
  {------------------------------------------------------------}
var
  Operator_    : TCANOperator;

  Offset      : Integer;
  Data1       : Pointer;
  FuncName    : string;
  Value1      : Variant;
begin
  Operator_ := TCANOperator(pCANFunc(pNode)^.canOp);

  if Operator_ <> coFUNC2 then
    DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);

  Offset       := pCANExpr(FData)^.iLiteralStart + pCANFunc(pNode)^.iNameOffset;
  FuncName     := String(FData + Offset);

  Data1        := FData + FNodeOfs + pCANFunc(pNode)^.iElemOffset;
  Value1       := Parse(Data1);

  if IsNull(Value1) then
    Result := Null
  else begin
         if AnsiSameText(FuncName, 'Upper'     ) then Result := AnsiUpperCase(VarToStr(Value1))
    else if AnsiSameText(FuncName, 'Lower'     ) then Result := AnsiLowerCase(VarToStr(Value1))
    else if AnsiSameText(FuncName, 'Substring' ) then Result := Substring(Value1)
    else if AnsiSameText(FuncName, 'Trim'      ) then Result := Trim(VarToStr(Value1))
    else if AnsiSameText(FuncName, 'TrimLeft'  ) then Result := TrimLeft(VarToStr(Value1))
    else if AnsiSameText(FuncName, 'TrimRight' ) then Result := TrimRight(VarToStr(Value1))
    else if AnsiSameText(FuncName, 'Year'      ) then Result := DateDecodeFunc(VarToDateTime(Value1), 0)
    else if AnsiSameText(FuncName, 'Month'     ) then Result := DateDecodeFunc(VarToDateTime(Value1), 1)
    else if AnsiSameText(FuncName, 'Day'       ) then Result := DateDecodeFunc(VarToDateTime(Value1), 2)
    else if AnsiSameText(FuncName, 'Hour'      ) then Result := TimeDecodeFunc(VarToDateTime(Value1), 0)
    else if AnsiSameText(FuncName, 'Minute'    ) then Result := TimeDecodeFunc(VarToDateTime(Value1), 1)
    else if AnsiSameText(FuncName, 'Second'    ) then Result := TimeDecodeFunc(VarToDateTime(Value1), 2)
    else if AnsiSameText(FuncName, 'GetDate'   ) then Result := Now
    else if AnsiSameText(FuncName, 'Date'      ) then Result := DateOf(VarToDateTime(Value1))
    else if AnsiSameText(FuncName, 'Time'      ) then Result := TimeOf(VarToDateTime(Value1))
    else Result := Null
  end;

end;
(*----------------------------------------------------------------------------*)
function TFilterParser.ParseLISTELEM(pNode: Pointer): Variant;
var
  Operator_    : TCANOperator;
  Node        : Pointer;
  List        : TList;
  i           : Integer;
begin
  Operator_ := TCANOperator(pCANListElem(pNode)^.canOp);

  if Operator_ <> coLISTELEM2 then
    DatabaseErrorFmt('Filter operator not supported: %s', [EnumValueToStr(TypeInfo(TCANOperator), Ord(Operator_), 2)]);


  List := TList.Create;
  try
    Node   := pNode;

    while True do
    begin
      List.Add(Node);

      if pCANListElem(Node)^.iNextOffset > 0 then
        Node := FData + FNodeOfs + pCANListElem(Node)^.iNextOffset
      else
        Break;

    end;

    Result := VarArrayCreate([0, List.Count - 1], VarVariant);

    for i := 0 to List.Count - 1 do
    begin
      Node := FData + FNodeOfs + pCANListElem(List[i])^.iOffset;
      Result[i] := Parse(Node);
    end;

  finally
    List.Free;
  end;

end;
(*----------------------------------------------------------------------------*)
function TFilterParser.Parse(pNode: Pointer): Variant;
begin
  case NODEClass(pCANHdr(pNode)^.nodeClass) of
    nodeNULL       : DatabaseError('Can not parse a filter Node : NULL');
    nodeUNARY      : Result := ParseUNARY(pNode);
    nodeBINARY     : Result := ParseBINARY(pNode);
    nodeCOMPARE    : Result := ParseCOMPARE(pNode);
    nodeFIELD      : Result := ParseFIELD(pNode);
    nodeCONST      : Result := ParseCONST(pNode);
    nodeTUPLE      : DatabaseError('Can not parse a filter Node : TUPLE');
    nodeCONTINUE   : DatabaseError('Can not parse a filter Node : CONTINUE');
    nodeUDF        : DatabaseError('Can not parse a filter Node : UDF');
    nodeLIST       : DatabaseError('Can not parse a filter Node : LIST');
    nodeFUNC       : Result := ParseFUNC(pNode);
    nodeLISTELEM   : Result := ParseLISTELEM(pNode);
  end;
end;
(*----------------------------------------------------------------------------*)
function TFilterParser.FilterRecord(RecBuf, FilterData: PChar; Event: TExtractFieldValueEvent): Boolean;
begin
  Result := False;
  if Assigned(RecBuf) and Assigned(RecBuf) and Assigned(Event) then
  begin
    FRecBuf   := RecBuf      ;
    FData     := FilterData  ;
    FEvent    := Event       ;

    FNodeOfs  := pCANExpr(FData)^.iNodeStart;

    Result    := WordBool(Parse(FData + FNodeOfs));
  end;
end;


end.

