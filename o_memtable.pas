unit o_MemTable;

{$mode objfpc}{$H+}

//{$DEFINE CURSOR_DEBUG}
{$DEFINE STRING_BLOBS}      { see comments below for Blob buffer layout }
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
   Classes
  ,SysUtils
  ,DateUtils
  //,MaskUtils
  ,SyncObjs
  ,Variants
  //,Generics.Collections
  //,FGL
  //,Contnrs
  ,TypInfo

  ,DB
  //,DBConst
  ,FmtBCD


  ;


type
  TMemTableSortMode  = (smNone, smAsc, smDesc);

(*============================================================================
                           Record buffer layout
 -----------------------------------------------------------------------------


 +-----------------------+-----------------------+-----------------------+-----------------------+
 |                       |                       |                       |                       |
 |      Data Fields      |   Calculated Fields   |      Blob fields      |       TRecInfo        |
 |                       |                       |                       |                       |
 +-----------------------+-----------------------+-----------------------+-----------------------+
 0                       CalcOfs                 BlobOfs                 BookOfs


 ----------------------------------------------------------------------------
 Data fields         : fkData and fkInternalCalc fixed length fields
 Calculated fields   : fkLookup and fkCalculated fields
 Blob fields         : pointers to the actual blobs
 ----------------------------------------------------------------------------
 Field buffer layout : byte  0         is the Null Flag Byte (0 = null, else not null)
                       bytes 1..n      field data
 ----------------------------------------------------------------------------
 Blob buffer layout  : byte  0         is the Null Flag Byte (0 = null, else not null)
                       bytes 1..n      blob data

                       see TBlob below, which is used in accessing blobs.

 NOTE (for blobs)    : the compiler directive STRING_BLOBS controls the
                       type of the TBlob.Data, that is the data type of the storage buffer
                       where the actual blob data is stored.
                       This could be either a string or a dynamic array of byte.
                       It seems that string is a little bit faster than
                       the dynamic byte array.

                       Except of strings and dynamic byte arrays, I tried
                       the classic approach, that is, plain pointer buffers
                       and AllocMem, FreeMem, Move etc. It turned out that
                       this last appoach is the slower. So choose either string
                       or dynamic array of byte.
 ----------------------------------------------------------------------------
 TRecInfo layout      : see below
 ============================================================================*)


type
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Bookmark       : Integer;         // the BookMark actually
    BookmarkFlag   : TBookmarkFlag;   // = (bfCurrent, bfBOF, bfEOF, bfInserted)
    Status         : TUpdateStatus;   // = (usUnmodified, usModified, usInserted, usDeleted)
  end;


type
   PBlob = ^TBlob;
   TBlob = packed record
     Flag   : Byte;
     Data   : string;
   end;

   TBlobs = array[0..High(Word)] of TBlob;
   PBlobs = ^TBlobs;

 type
    { used in comparing fields in order to break the comparison loops }
    TBreakMode    = ( bmG
                     ,bmGE
                     ,bmNE
                     ,bmL
                     ,bmLE
                     );

 type
   { the cursor may be in one or more modes }
   TCursorMode = ( cmStatus
                   ,cmLink
                   ,cmRange
                   ,cmIndex
                   ,cmFilter
                  );

   TCursorModes = set of TCursorMode;

 type
   TKeyBufferIndex = (biMaster
                     ,biRangeStart
                     ,biRangeEnd
                     ,biLocate
                     );

type
  TMemTable = class;

 type
   { TFieldInfo }
   TFieldInfo = class(TObject)
   protected
     FDataset           : TMemTable;
     FField             : TField;
     function  GetIsAutoInc: Boolean;
   public
     constructor Create(Field: TField);

     property Field        : TField read FField;
     property IsAutoInc    : Boolean read GetIsAutoInc;
     //property Value        : Variant read GetValue write SetValue;
   end;


type

  { TMemTable }
  TMemTable = class(TDataSet)
  private
    procedure SetCurRecIndex(Value: Integer);
  private
    FIndexFieldNames: string;
    FTableName                 : string;
    FLock                      : SyncObjs.TCriticalSection;
    FLockCount                 : Integer;

    FAllRows                   : TList;   { all rows }
    FRows                      : TList;   { current rows }

    FFields                    : TList;   { all TFieldInfo fields - owned }
    FBlobFields                : TList;   { TFieldInfo list }
    FIndexFields               : TList;   { TFieldInfo list }

    FDetailFields              : TList;   { TFieldInfo list }
    FRangeFields               : TList;   { TFieldInfo list }
    FModifiedFields            : TList;   { TField list }

    { size indicator }
    FRecBufSize                : Integer;                 { record buffer total size - all fields + SizeOf(TRecInfo) }

    { offsets }
    FCalcOfs                   : Integer;
    FBlobOfs                   : Integer;
    FBookOfs                   : Integer;

    { field attribute arrays }
    FFieldTypes                : array of TFieldType;     // FTypes
    FSizes                     : array of Integer;        { data size of field }
    FOffsets                   : array of Integer;        { offset of a field data. The first byte is the Null Flag  byte }

    FCurRecIndex               : Integer;                 { the current record index in the FRows list }
    FLastBookmark              : Integer;                 { an auto inc number unique for each record, stored in TRecInfo.Bookmark }

    FModes                     : TCursorModes;
    FStatusOptions             : TUpdateStatusSet;
    FKeyBuffers                : array[TKeyBufferIndex] of PChar;

    //FFilterParser              : TFilterParser;
    //FFilterData                : PChar;
    FEmptyBlob                 : TBlob;

    FMasterLink                : TMasterDataLink;
    FDetailFieldNames          : string;

    FIsInRange                 : Boolean;
    FRangeExclusive            : Boolean;
    FInitialized               : Boolean;

    function  GetMasterDataSource: TDataSource;
    function  GetMasterFieldNames: string;
    function  GetStatusFilter: TUpdateStatusSet;
    procedure SetDetailFieldNames(Value: string);
    procedure SetIndexFieldNames(Value: string);
    procedure SetMasterDataSource(Value: TDataSource);
    procedure SetMasterFieldNames(Value: string);
    procedure SetStatusFilter(Value: TUpdateStatusSet);

    property CurRecIndex: Integer read FCurRecIndex write SetCurRecIndex;

    procedure Lock; virtual;
    procedure UnLock; virtual;

    { initialization }
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure DeleteRows; virtual;

    { get/set field data }
    function  GetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; FieldIndex: Integer): Boolean; virtual;
    procedure SetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; FieldIndex: Integer); virtual;

    { blobs }
    procedure InitializeBlobs(RecBuf: TRecordBuffer); virtual;
    procedure FinalizeBlobs(RecBuf: TRecordBuffer); virtual;

    procedure FreeBlobs(RecBuf: TRecordBuffer); virtual;
    procedure FreeBlob(RecBuf: TRecordBuffer; FieldIndex: Integer); virtual;
    procedure CopyBlobs(SourceRecBuf, DestRecBuf: TRecordBuffer); virtual;
    procedure CopyBlob(SourceRecBuf, DestRecBuf: TRecordBuffer; FieldIndex: Integer); virtual;

    { record }
    procedure CopyRecord(SourceRecBuf, DestRecBuf: TRecordBuffer); virtual;
    function  CanDisplayRecord(RecBuf: TRecordBuffer): Boolean; virtual;
    function  GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;

    { bookmark }
    function  GoToBookmarkInternal(BM: Integer): Boolean; virtual;
    function  IndexOfBookmark(BM: Integer): Integer; virtual;
    function  GetBookmarkInternal(RecBuf: TRecordBuffer): Integer; virtual;

    { TFieldInfo related }
    procedure AddFields();  virtual;

    procedure GetFieldNames(List: TStrings); virtual;
    function  IndexOfField(Field: TFieldInfo): Integer; virtual;
    function  IndexOfFieldName(const FieldName: string): Integer; virtual;
    function  FindInfoField(const FieldName: string): TFieldInfo; virtual;
    function  FieldInfoByName(const FieldName: string): TFieldInfo; virtual;

    procedure GetFieldInfoList(List: TList; const FieldNames: string); virtual; overload;
    function  GetFieldInfoList(const FieldNames: string): TList; virtual;overload;

    function  GetFieldDataSize(FieldInfo: TFieldInfo): Integer; virtual;
    function  GetFieldSize(FieldInfo: TFieldInfo): Integer; virtual;

    { blobs }
    function  GetBlobSize(RecBuf: PChar; FieldIndex: Integer): LongWord;  virtual;
    function  GetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer): LongWord; virtual;
    procedure SetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer; BlobSize: LongWord); virtual;

    { comparing-sorting }
    function  CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; Options: TLocateOptions): Integer; virtual;
    function  CompareRecords(const Buf1, Buf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer; virtual;
    procedure QuickSort(L, R: Integer; const RowList: TList; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode); virtual;

    { records - miscs }
    function  GetUpdateStatus(RecBuf: TRecordBuffer): TUpdateStatus; virtual;
    function  GetRecordByIndex(RecBuf: TRecordBuffer; RecordIndex: Integer): Boolean; virtual;
    function  LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; var RecIndex: Integer): Boolean; overload; virtual;
    function  LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;  overload; virtual;

    { field get/set value }
    function  IsFieldNull(const FieldIndex: Integer; RecBuf: TRecordBuffer): Boolean; virtual;
    function  GetFieldValue(const FieldIndex: Integer; RecBuf: TRecordBuffer): Variant; virtual;
    procedure SetFieldValue(const FieldIndex: Integer; RecBuf: TRecordBuffer; const Value: Variant); virtual;
    function  GetValue(const FieldIndex: Integer; FieldData: Pointer): Variant; virtual;
    procedure SetValue(const FieldIndex: Integer; FieldData: Pointer; Value: Variant); virtual;

    { master-detail }
    procedure OnMasterLinkChanged(Sender: TObject);
    procedure OnMasterLinkDisabled(Sender: TObject);
    procedure LoadDetailFieldList();

    { operations }
    procedure SetLink(Value: Boolean);

    { miscs }
    procedure LoadIndexFieldList();
    function  IsFieldBufferNull(Buf: PChar): Boolean;
    procedure VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant); virtual;
  protected
    {== TDataset overrides ==}

    { record buffer }
    function  AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
    function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;

    { bookmark }
    function  GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(pBM: Pointer); override;
    procedure InternalSetToRecord(RecBuf: TRecordBuffer); override;

    { navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;

    { editing }
    procedure InternalInitRecord(RecBuf: TRecordBuffer); override;
    procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;

    { open/close }
    function  IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

    { miscs }
    procedure InternalInitFieldDefs; override;
    procedure ClearCalcFields(RecBuf: TRecordBuffer); override;
    procedure InternalHandleException; override;

    procedure DoOnNewRecord; override;
    procedure DoAfterScroll; override;

    { optional }
    function  GetRecordCount: Integer; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {== TDataset overrides ==}
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function  BookmarkValid(BM: TBookmark): Boolean; override;

    function  Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function  Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function  UpdateStatus: TUpdateStatus; override;

    { additional }
    procedure CreateDataset;
    procedure EmptyDataSet;

    procedure SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
    procedure CancelRange;

    procedure CancelUpdates();

    procedure Sort; virtual;
    procedure Rebuild; virtual;

    { supported field types }
    class function IsSupportedFieldType(FieldType: TFieldType): Boolean; virtual;

    class function IsStringFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsWideStringFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsIntegerFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsFloatFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsBCDFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsDateTimeFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsVariantFieldType(FieldType: TFieldType): Boolean; virtual;
    class function IsBlobFieldType(FieldType: TFieldType): Boolean; virtual;

    { helpers }
    class function  DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
    class function  NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
    class function  CompareDateTimes(const A, B: TDateTime): Integer;
    class function  BufferToWide(Buffer: Pointer): WideString;
    class procedure WideToBuffer(WS: WideString; Buffer: Pointer);
    class procedure GetFloatFormatAndDigits(CurrencyFormat: Boolean; var FloatFormat: TFloatFormat; var Digits: Integer);
    class procedure ClearObjectList(List: TList);
    class function  Min(const A, B: Integer): Integer;
    class function  Max(const A, B: Integer): Integer;

    { properties }
    property MasterSource      : TDataSource read GetMasterDataSource write SetMasterDataSource;
    property MasterFieldNames  : string read GetMasterFieldNames write SetMasterFieldNames;
    property DetailFieldNames  : string read FDetailFieldNames  write SetDetailFieldNames;
    property IndexFieldNames   : string read FIndexFieldNames write SetIndexFieldNames;
    property StatusFilter      : TUpdateStatusSet read GetStatusFilter write SetStatusFilter;

    property IsInRange         : Boolean read FIsInRange;
    property ModifiedFields    : TList read FModifiedFields;    // TField list - valid for the current record only
  published
    property Active;
    property TableName: string read FTableName write FTableName;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;

  end;



implementation



const
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SNotEditing = 'Dataset not in edit or insert mode';
  SCircularDataLink = 'Circular datalinks are not allowed';









{ TFieldInfo }
constructor TFieldInfo.Create(Field: TField);
begin
  inherited Create();
  Self.FField := Field;
end;
function TFieldInfo.GetIsAutoInc: Boolean;
begin
  Result := Field.DataType = ftAutoInc;
end;









type
  { TBlobStream }

  TBlobStream = class(TMemoryStream)
  protected
    FFieldIndex  : Integer;
    FField       : TBlobField;
    FMode        : TBlobStreamMode;
    FModified    : Boolean;
    FRecBuf      : TRecordBuffer;
    FDataset     : TMemTable;
  public
    constructor Create(Dataset: TMemTable; Field: TBlobField; RecBuf: TRecordBuffer; Mode: TBlobStreamMode);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
  end;




{ TBlobStream }

constructor TBlobStream.Create(Dataset: TMemTable; Field: TBlobField; RecBuf: TRecordBuffer; Mode: TBlobStreamMode);
var
  BlobSize : LongWord;
begin
  inherited Create();

  FDataset     := Dataset;
  FField       := Field;
  FRecBuf      := RecBuf;
  FMode        := Mode;
  FFieldIndex  := Dataset.IndexOfFieldName(FField.FieldName);


  if Assigned(FRecBuf) then
  begin

    if FMode <> bmRead then
    begin
      if FField.ReadOnly then
        DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FField.DataSet);
      if not (FField.DataSet.State in [dsEdit, dsInsert, dsNewValue]) then
        DatabaseError(SNotEditing, FField.DataSet);
    end;

    if FMode = bmWrite then    { truncate the blob data }
    begin
      Clear;
      FModified := True;
    end else  begin             { read the blob data }
      BlobSize := FDataset.GetBlobSize(FRecBuf, FFieldIndex);
      if BlobSize > 0 then
      begin
        Self.Position := 0;
        SetSize(BlobSize);
        FDataset.GetBlobData(FRecBuf, Memory, FFieldIndex);
      end;
    end;

  end;


end;
destructor TBlobStream.Destroy;
begin

  if FModified then
  try
    Position := 0;
    FDataset.SetBlobData(FRecBuf, Memory, FFieldIndex, Self.Size);
    FField.Modified := True;
    FDataset.DataEvent(deFieldChange, Longint(FField));
  except
    if Assigned(Classes.ApplicationHandleException) then
      Classes.ApplicationHandleException(Self);
  end;

  inherited Destroy;
end;

function TBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;
















{ TMemTable }

constructor TMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLock              := SyncObjs.TCriticalSection.Create();

  FAllRows           := TList.Create;
  FRows              := TList.Create;

  FFields            := TList.Create();
  FBlobFields        := TList.Create();
  FIndexFields       := TList.Create();
  FDetailFields      := TList.Create();
  FRangeFields       := TList.Create();
  FModifiedFields    := TList.Create();

  FMasterLink                 := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange  := @OnMasterLinkChanged;
  FMasterLink.OnMasterDisable := @OnMasterLinkDisabled;

  FStatusOptions    := [usModified, usInserted, usUnmodified];

  FCurRecIndex      := -1;
end;

destructor TMemTable.Destroy;
begin
  inherited Destroy;


  FreeAndNil(FModifiedFields);
  FreeAndNil(FRangeFields);
  FreeAndNil(FDetailFields);
  FreeAndNil(FIndexFields);
  FreeAndNil(FBlobFields);
  FreeAndNil(FFields);

  FreeAndNil(FRows);
  FreeAndNil(FAllRows);

  FLock.Free;
end;

function TMemTable.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
  InternalInitRecord(Result);

  if FBlobFields.Count > 0 then
    InitializeBlobs(Result);
end;

procedure TMemTable.FreeRecordBuffer(var RecBuf: TRecordBuffer);
begin
  if FBlobFields.Count > 0 then
  begin
    FreeBlobs(RecBuf);
    FinalizeBlobs(RecBuf);
  end;

  FreeMem(RecBuf);

  RecBuf := nil;
end;
function TMemTable.GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  SourceRecBuf  : TRecordBuffer;
  RecCount      : Integer;
begin
  Lock();
  try
    RecCount := FRows.Count;

    SourceRecBuf := nil;

    if RecCount < 1 then
    begin
       Result := grEOF
    end else begin

       Result := grOk;

       case GetMode of
         gmPrior   : begin
                       if CurRecIndex <= 0 then
                         Result := grBOF
                       else
                         CurRecIndex := CurRecIndex - 1;
                     end;

         gmCurrent : begin
                       if (CurRecIndex < 0) or (CurRecIndex >= RecCount) then
                         Result := grError;
                     end;

         gmNext    : begin
                       if CurRecIndex >= RecCount - 1 then
                         Result := grEOF
                       else
                         CurRecIndex := CurRecIndex + 1;
                     end;

       end;

       if (Result = grOK) and Assigned(RecBuf) then
       begin
         SourceRecBuf := FRows[CurRecIndex];
         CopyRecord(SourceRecBuf, RecBuf);

         PRecInfo(RecBuf + FBookOfs)^.Bookmark        := PRecInfo(SourceRecBuf + FBookOfs)^.Bookmark;
         PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag    := bfCurrent;
         PRecInfo(RecBuf + FBookOfs)^.Status          := PRecInfo(SourceRecBuf + FBookOfs)^.Status;

       end else if (Result = grError) and DoCheck then
         DatabaseError('No records');
     end;

  finally
    UnLock();
  end;

end;

function TMemTable.GetRecordSize: Word;
begin
  Result := FRecBufSize;
end;
function TMemTable.GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
end;
procedure TMemTable.SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag := Value;
end;
procedure TMemTable.GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  BM: Integer;
begin
  BM := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
  PInteger(Data)^ := BM;
end;
procedure TMemTable.SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  BM: Integer;
begin
  BM := PInteger(Data)^;
  PRecInfo(RecBuf + FBookOfs)^.Bookmark := BM;
end;
procedure TMemTable.InternalGotoBookmark(pBM: Pointer);
var
  BM: Integer;
begin
  BM := PInteger(pBM)^;
  GoToBookmarkInternal(BM);
end;
procedure TMemTable.InternalSetToRecord(RecBuf: TRecordBuffer);
var
  BM: Integer;
begin
  BM := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
  GoToBookmarkInternal(BM);
end;
function TMemTable.GoToBookmarkInternal(BM: Integer): Boolean;
var
  Index  : Integer;
begin
  Result := False;
  Lock();
  try
    Index := IndexOfBookmark(BM);

    if Index <> -1 then
    begin
      Result := True;
      CurRecIndex := Index;
    end;

  finally
    UnLock();
  end;
end;
function TMemTable.GetBookmarkInternal(RecBuf: TRecordBuffer): Integer;
begin
  Result := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
end;
function TMemTable.IndexOfBookmark(BM: Integer): Integer;
var
  i     : Integer;
begin
  Lock();
  try
    Result := -1;
    if BM > 0 then
    begin
      for i := 0 to FRows.Count - 1 do
        if (BM = PRecInfo(PChar(FRows[i]) + FBookOfs)^.Bookmark) then
        begin
          Result := i;
          Break; //==>
        end;
    end;
  finally
    UnLock();
  end;

end;

procedure TMemTable.InternalFirst;
begin
  CurRecIndex := -1;
end;
procedure TMemTable.InternalLast;
begin
  CurRecIndex := FRows.Count;
end;
procedure TMemTable.SetCurRecIndex(Value: Integer);
begin
  if FCurRecIndex <> Value then
     FCurRecIndex := Value;
end;



procedure TMemTable.InternalInitRecord(RecBuf: TRecordBuffer);
begin
  FillChar(RecBuf^, FRecBufSize, 0);
end;
procedure TMemTable.InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean);

  function GetNextAutoIncValue(FieldIndex: Integer): Integer;
  var
    i             : Integer;
    pRec          : TRecordBuffer;
    pField        : TRecordBuffer;
    V             : Integer;
  begin
    Result := 0;

    for i := 0 to FAllRows.Count -1 do
    begin
      pRec := TRecordBuffer(FAllRows[i]);
      pField  := pRec + FOffsets[FieldIndex];
      if not IsFieldBufferNull(pField) then
      begin
        Inc(pField);
        V := PInteger(pField)^;
        Result := Max(Result, V);
      end;
    end;

    Result := Result + 1;
  end;

  procedure AssignAutoIncValues();
  var
    i             : Integer;
    FieldInfo     : TFieldInfo;
    Field         : TField;
    P             : TRecordBuffer;
  begin
    for i := 0 to FFields.Count - 1 do
    begin
      FieldInfo := TFieldInfo(FFields[i]);
      if FieldInfo.IsAutoInc then
      begin
        Field := FieldInfo.Field;

        P             := RecBuf + FOffsets[Field.Index];
        Byte(P[0])    := Byte(1);
        Inc(P);

        PInteger(P)^ := GetNextAutoIncValue(Field.Index);
      end;
    end;
  end;

var
  DestRecBuf    : TRecordBuffer;
  RecIndex      : Integer;

begin
  Lock();
  try
    if CurRecIndex < 0 then
       CurRecIndex := 0;

    AssignAutoIncValues();

    Inc(FLastBookmark);

    DestRecBuf     := AllocRecordBuffer();
    CopyRecord(TRecordBuffer(RecBuf), DestRecBuf);

    PRecInfo(RecBuf + FBookOfs)^.Bookmark      := FLastBookmark;
    PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag  := bfCurrent;
    PRecInfo(RecBuf + FBookOfs)^.Status        := usInserted;

    PRecInfo(DestRecBuf + FBookOfs)^.Bookmark      := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
    PRecInfo(DestRecBuf + FBookOfs)^.BookmarkFlag  := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
    PRecInfo(DestRecBuf + FBookOfs)^.Status        := PRecInfo(RecBuf + FBookOfs)^.Status;

    FAllRows.Add(DestRecBuf);

    if CanDisplayRecord(DestRecBuf) then
    begin
      RecIndex := CurRecIndex;
      if RecIndex < 0 then
        RecIndex := 0;

      if IsAppend or (FRows.Count <= 0) then
      begin
        FRows.Add(DestRecBuf);
        InternalLast();
      end else begin
        FRows.Insert(RecIndex, DestRecBuf);
      end;

      if cmIndex in FModes then
        Sort();
    end;

  finally
    UnLock();
  end;

end;
procedure TMemTable.InternalPost;
var
  RecBuf          : TRecordBuffer;
  DestRecBuf      : TRecordBuffer;
begin
  inherited InternalPost(); // checks required fields

  Lock();
  try
    RecBuf := ActiveBuffer();

    if State = dsEdit then
    begin
      DestRecBuf := FRows[CurRecIndex];
      CopyRecord(RecBuf, DestRecBuf);

      PRecInfo(DestRecBuf + FBookOfs)^.Bookmark      := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
      PRecInfo(DestRecBuf + FBookOfs)^.BookmarkFlag  := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
      if PRecInfo(DestRecBuf + FBookOfs)^.Status = usUnmodified then
        PRecInfo(DestRecBuf + FBookOfs)^.Status := usModified;

      if not CanDisplayRecord(DestRecBuf) then
      begin
        FRows.Remove(DestRecBuf);
        CurRecIndex := Min(FRows.Count - 1, CurRecIndex)
      end else begin
        Sort();
      end;
    end else begin
        InternalAddRecord(RecBuf, False);
    end;

    FModifiedFields.Clear;

  finally
    UnLock();
  end;

end;

procedure TMemTable.InternalEdit;
begin
  inherited InternalEdit;
  FModifiedFields.Clear;
end;

procedure TMemTable.InternalCancel;
begin
  inherited InternalCancel;
  FModifiedFields.Clear;
  FreeBlobs(ActiveBuffer);
end;

procedure TMemTable.InternalDelete;
var
  RecBuf    : TRecordBuffer;
begin
  Lock();
  try
    RecBuf := FRows[CurRecIndex];

    FAllRows.Remove(RecBuf);
    FRows.Delete(CurRecIndex);

    FreeRecordBuffer(RecBuf);

    CurRecIndex := Min(FRows.Count - 1, CurRecIndex);

  finally
    UnLock();
  end;

end;
function TMemTable.IsCursorOpen: Boolean;
begin
  Result := FInitialized;
end;
procedure TMemTable.InternalOpen;
var
  i : Integer;
begin
  for i := 0 to FieldCount - 1 do
    if not IsSupportedFieldType(Fields[i].DataType) then
      DatabaseErrorFmt('Unsupported field type: %s', [Fields[i].FieldName]);

  InternalInitFieldDefs();

  if DefaultFields then
    CreateFields;

  BindFields(True);

  AddFields();

  LoadDetailFieldList;
  LoadIndexFieldList;

  Initialize();

end;
procedure TMemTable.InternalClose;
begin
  Finalize();

  if DefaultFields then
    DestroyFields;
end;
procedure TMemTable.Initialize;
var
  i             : Integer;
  FieldInfo     : TFieldInfo;
  Field         : TField;
  BufferIndex   : TKeyBufferIndex;
  DataFields    : TList;
  CalcFields    : TList;

  Ofs           : Integer;

  DataSize      : Integer;                 { plain data fields total size, i.e. fkData, fkInternalCalc - NO Calculated or Blob fields }
  CalcsSize     : Integer;                 { calc fields total size, i.e. fkCalculated, fkLookup }
  BlobsSize     : Integer;                 { blob fields total size }

  IsBlob        : Boolean;
begin

  Lock();
  try
    try
      if FInitialized then
        Finalize();

      BookmarkSize := SizeOf(Integer);

      DataFields := TList.Create();
      CalcFields := TList.Create();

      CurRecIndex               := -1;

      { separate the fields }
      for i := 0 to FFields.Count - 1 do
      begin
        FieldInfo := TFieldInfo(FFields[i]);
        Field     := FieldInfo.Field;
        IsBlob    := IsBlobFieldType(Field.DataType);

        if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
          DataFields.Add(FieldInfo)
        else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up fields }
          CalcFields.Add(FieldInfo)
        else if IsBlob then                                                                { blob fields }
          FBlobFields.Add(FieldInfo);
      end;

      { recreate the FieldInfo list according to the record buffer layout }
      FFields.Clear;                // just clear the list, do NOT free the TFieldInfo instances
      FFields.AddList(DataFields);
      FFields.AddList(CalcFields);
      FFields.AddList(FBlobFields);

      SetLength(FFieldTypes   , FFields.Count);
      SetLength(FSizes        , FFields.Count);
      SetLength(FOffsets      , FFields.Count);

      { process the FieldInfo list, feed the property arrays, calculate FieldInfo offsets, and FieldInfo section sizes }
      FRecBufSize := 0;
      Ofs         := 0;
      DataSize    := 0;
      CalcsSize   := 0;
      BlobsSize   := 0;

      for i := 0 to FFields.Count - 1 do
      begin
        FieldInfo := TFieldInfo(FFields[i]);
        Field     := FieldInfo.Field;
        IsBlob    := IsBlobFieldType(Field.DataType);

        FFieldTypes[i]  := Field.DataType;
        FSizes[i]       := GetFieldSize(FieldInfo);

        FOffsets[Field.Index]  := Ofs;
        Ofs                    := Ofs + FSizes[i] + 1;

        if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
          DataSize := DataSize + FSizes[i] + 1
        else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up fields }
          CalcsSize := CalcsSize + FSizes[i] + 1
        else if IsBlob then                                                                { blob fields }
          BlobsSize := BlobsSize + FSizes[i] + 1;
      end;


      { NOTE: this check will raise an exception in cases where there are ftWideString look up fields.
              That is, if there are ftWideStrings look ups, (CalcsSize <> FCalcFieldsSize)  will always be True.
              And since CalcFieldsSize is not used anywhere, I think it's safe to deactivate the check }
      (*
      if (CalcsSize <> FCalcFieldsSize) and (FCalcFieldsSize <> -1) then
        raise Exception.Create('CalcSize <> CalcFieldsSize');
      *)


      { section offsets and total record buffer size }
      FCalcOfs                   := DataSize;
      FBlobOfs                   := FCalcOfs + CalcsSize ;
      FBookOfs                   := FBlobOfs + BlobsSize;
      FRecBufSize                := FBookOfs + SizeOf(TRecInfo);

      { initialize key buffers }
      for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
        FKeyBuffers[BufferIndex] := AllocRecordBuffer();

      FInitialized               := True;
    finally
      DataFields.Free;
      CalcFields.Free;
    end;
  finally
    UnLock();
  end;

end;
procedure TMemTable.Finalize;
var
  BufferIndex : TKeyBufferIndex;
begin
  Lock();
  try
    if (FInitialized) then
    begin
      { free key buffers }
      for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
        FreeMem(FKeyBuffers[BufferIndex], FRecBufSize);

      DeleteRows();

      FBlobFields.Clear;
      ClearObjectList(FFields);

      FRecBufSize                := 0;

      FCalcOfs                   := 0;
      FBlobOfs                   := 0;
      FBookOfs                   := 0;

      FFieldTypes                := nil;
      FSizes                     := nil;
      FOffsets                   := nil;

      FInitialized := False;
    end;
  finally
    UnLock();
  end;

end;

procedure TMemTable.DeleteRows;
var
  i      : Integer;
  RecBuf : PChar;
begin
  Lock();
  try
    FRows.Clear;

    for i := 0 to FAllRows.Count - 1 do
    begin
      RecBuf := FAllRows[i];
      FreeRecordBuffer(RecBuf);
    end;

    FAllRows.Clear;

    CurRecIndex := -1;
  finally
    UnLock();
  end;

end;
procedure TMemTable.CreateDataset;
begin
  CheckInactive;

  if Fields.Count > 0 then
  begin
    FieldDefs.Clear;
    InitFieldDefsFromFields;
  end else begin
    CreateFields;
  end;

  Open();
end;
procedure TMemTable.EmptyDataSet;
begin
  if Active then
  begin
    CheckBrowseMode;
    DeleteRows();
    First;
  end;
end;

procedure TMemTable.SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
begin
  CheckBrowseMode;

  Lock();
  try
    FRangeFields.Clear;
    GetFieldInfoList(FRangeFields, RangeFieldNames);

    if FRangeFields.Count = 0 then
      FModes := FModes - [cmRange]
    else begin
      if FInitialized then
      begin
        FModes := FModes + [cmRange];

        FRangeExclusive := Exclusive;

        VariantValuesToRecordBuffer(FRangeFields, FKeyBuffers[biRangeStart], StartValues);
        VariantValuesToRecordBuffer(FRangeFields, FKeyBuffers[biRangeEnd], EndValues);
      end;
    end;

    Rebuild();
  finally
    UnLock();
  end;

  First;
  FIsInRange := True;

end;

procedure TMemTable.CancelRange;
begin
  SetRange('', Null, Null, False);
  if Active then
    First;
  FIsInRange := False;
end;

procedure TMemTable.InternalInitFieldDefs;
begin
  { nothing }
end;
procedure TMemTable.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
end;

procedure TMemTable.DoOnNewRecord;
var
  i : Integer;
begin
  FModifiedFields.Clear;

  { Default field values. NOTE: SQL expression not supported, just simple values }
  for i := 0 to Fields.Count - 1 do
    if (Fields[i].DataType <> ftLargeInt) and (Fields[i].DefaultExpression <> '') then { VCL does not fully supports LareInt Variants }
      TField(Fields[i]).Value := TField(Fields[i]).DefaultExpression;

  inherited DoOnNewRecord;
end;

procedure TMemTable.DoAfterScroll;
begin
  FModifiedFields.Clear;
  inherited DoAfterScroll;
end;

function TMemTable.GetRecordCount: Integer;
begin
  Result := FRows.Count;
end;
function TMemTable.GetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; FieldIndex: Integer): Boolean;
var
  P          : TRecordBuffer;
begin
  Lock();
  try
    P          := RecBuf + FOffsets[FieldIndex];
    Result     := Boolean(P[0]);
    if Result and Assigned(Buffer) then
    begin
      FillChar(Buffer^, FSizes[FieldIndex], 0);
      Inc(P);
      Move(P^, Buffer^, FSizes[FieldIndex]);
    end;
  finally
    UnLock();
  end;
end;
procedure TMemTable.SetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; FieldIndex: Integer);
var
  P             : TRecordBuffer;
  V             : Byte;
begin
  Lock();
  try
    P             := RecBuf + FOffsets[FieldIndex];
    V             := PByte(Buffer)^;
    Byte(P[0])    := V;
    Inc(P);

    if V <> 0 then
    begin
      Move(Buffer^, P^, FSizes[FieldIndex])
    end else begin
      FillChar(P^, FSizes[FieldIndex], 0);
    end;
  finally
    UnLock();
  end;

end;
function TMemTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf      : PChar;
begin
  Result := False;
  RecBuf := nil;
  if GetActiveRecBuf(RecBuf) then
  begin
    if Field.FieldKind in [fkData, fkInternalCalc] then
    begin
      if (not ((State = dsBrowse) and IsEmpty)) then
         Result := GetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
    end else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Result := GetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
      (*
      Inc(RecBuf, FCursor.CalcOfs + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
      *)
    end;
  end;

end;
procedure TMemTable.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf      : PChar;
begin
  RecBuf := nil;

  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing);

  GetActiveRecBuf(RecBuf);

  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
    Field.Validate(Buffer);

    if FModifiedFields.IndexOf(Field) = -1 then
      FModifiedFields.Add(Field);

    SetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName));

  end else if (State <> dsInternalCalc) then
  begin
    SetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
    (*
    Inc(RecBuf, FCursor.CalcOfs + Field.Offset);
    Boolean(RecBuf[0]) := LongBool(Buffer);
    if Boolean(RecBuf[0]) then
      Move(Buffer^, RecBuf[1], Field.DataSize);
    *)
  end;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));


end;



procedure TMemTable.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

procedure TMemTable.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;
procedure TMemTable.InitializeBlobs(RecBuf: TRecordBuffer);
var
  Blob : PBlob;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      if FBlobFields.Count > 0 then
      begin
        Index := TFieldInfo(FBlobFields[0]).Field.Index;  // we need just the 1st pointer
        Blob := PBlob(RecBuf + FOffsets[Index]);
        System.Initialize(Blob^, FBlobFields.Count);
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FinalizeBlobs(RecBuf: TRecordBuffer);
var
  Blob : PBlob;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      if FBlobFields.Count > 0 then
      begin
        Index := TFieldInfo(FBlobFields[0]).Field.Index;  // we need just the 1st pointer
        Blob := PBlob(RecBuf + FOffsets[Index]);
        System.Finalize(Blob^, FBlobFields.Count);
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FreeBlobs(RecBuf: TRecordBuffer);
var
  i : Integer;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      for i := 0 to FBlobFields.Count - 1 do
      begin
        Index := TFieldInfo(FBlobFields[i]).Field.Index;
        PBlob(RecBuf + FOffsets[Index])^ := FEmptyBlob;
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FreeBlob(RecBuf: TRecordBuffer; FieldIndex: Integer);
begin
  Lock();
  try
    PBlob(RecBuf + FOffsets[FieldIndex])^ := FEmptyBlob;
  finally
    UnLock();
  end;
end;
procedure TMemTable.CopyBlobs(SourceRecBuf, DestRecBuf: TRecordBuffer);
var
  i : Integer;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      for i := 0 to FBlobFields.Count - 1 do
      begin
        Index := TFieldInfo(FBlobFields[i]).Field.Index;
        PBlob(DestRecBuf + FOffsets[Index])^ := PBlob(SourceRecBuf + FOffsets[Index])^
      end;

    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.CopyBlob(SourceRecBuf, DestRecBuf: TRecordBuffer; FieldIndex: Integer);
begin
  Lock();
  try
    PBlob(DestRecBuf + FOffsets[FieldIndex])^  := PBlob(SourceRecBuf + FOffsets[FieldIndex])^ ;
  finally
    UnLock();
  end;
end;

procedure TMemTable.CopyRecord(SourceRecBuf, DestRecBuf: TRecordBuffer);
begin
  System.Move(SourceRecBuf^, DestRecBuf^, FBlobOfs);     { copy buffer EXCEPT blobs and RecInfo }

  if FBlobFields.Count > 0 then
    CopyBlobs(SourceRecBuf, DestRecBuf);                 { copy blobs, if any }
end;

function TMemTable.CanDisplayRecord(RecBuf: TRecordBuffer): Boolean;
var
  RRS, RRE : Integer; // Range Results
  TempStatusOptions : TUpdateStatusSet;
begin

  Result := (FModes * [cmStatus, cmLink, cmRange, cmFilter] = []);

  if (not Result) and FInitialized then
  begin

    { status }
    if (cmStatus in FModes) then
      TempStatusOptions := FStatusOptions
    else
      TempStatusOptions := [usModified, usInserted, usUnmodified];

    if not (PRecInfo(RecBuf + FBookOfs)^.Status in TempStatusOptions) then
      Exit; //==>


    { master-detail }
    if (cmLink in FModes) then
      if CompareRecords(FKeyBuffers[biMaster], RecBuf, FDetailFields, [loCaseInsensitive], smNone, bmNE) <> 0 then
        Exit; //==>


    { range }
    if (cmRange in FModes) then
    begin
      RRS := CompareRecords(RecBuf, FKeyBuffers[biRangeStart], FRangeFields, [loCaseInsensitive], smNone, bmNE);
      RRE := CompareRecords(RecBuf, FKeyBuffers[biRangeEnd  ], FRangeFields, [loCaseInsensitive], smNone, bmNE);

      case FRangeExclusive of
        False : if not ((RRS >= 0) and (RRE <= 0)) then
                  Exit; //==>
        True  : if not ((RRS > 0) and (RRE < 0)) then
                  Exit; //==>
      end;
    end;


    { TODO: filter
    if (cmFilter in FModes) then
    begin

      if Assigned(FOnRecordFilter) then
        if not FOnRecordFilter(RecBuf) then
          Exit; //==>

      if Assigned(FFilterData) then
        if not FFilterParser.FilterRecord(RecBuf, FFilterData, FilterParser_OnExtractFieldValue) then
          Exit; //==>
    end;
    }

    Result := True;
  end;
end;
function TMemTable.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  case State of
    dsBlockRead    ,
    dsBrowse       : if IsEmpty or (BookmarkSize = 0) then
                       RecBuf := nil
                     else
                       RecBuf := ActiveBuffer();


    dsEdit         ,
    dsInsert       : RecBuf := ActiveBuffer();

    dsCalcFields   ,
    dsInternalCalc : RecBuf := CalcBuffer;
    //TODO: dsFilter       : RecBuf := FFilterBuffer;

    dsNewValue     ,
    dsOldValue     ,
    dsCurValue     : DatabaseError('GetActiveRecBuf');
    else             RecBuf := nil;

  end;

  Result := RecBuf <> nil;

end;
class function TMemTable.IsSupportedFieldType(FieldType: TFieldType): Boolean;
begin
   Result := FieldType in
   [ftString
   ,ftFixedChar
   ,ftGuid

   ,ftWideString
   ,ftFixedWideChar

   ,ftSmallint
   ,ftAutoInc
   ,ftInteger
   ,ftWord
   ,ftLargeint

   ,ftBoolean

   ,ftFloat
   ,ftCurrency

   ,ftBCD
   ,ftFMTBcd

   ,ftDate
   ,ftTime
   ,ftDateTime
   ,ftTimeStamp

   ,ftMemo
   ,ftWideMemo
   ,ftFmtMemo

   ,ftBlob
   ,ftGraphic

   ,ftOraBlob
   ,ftOraClob

   ,ftVariant
   ];
end;
class function TMemTable.IsStringFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftString
  ,ftGuid
  ,ftFixedChar
  ];
end;

class function TMemTable.IsWideStringFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftWideString
  ,ftFixedWideChar
  ];
end;

class function TMemTable.IsIntegerFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftAutoInc
  ,ftSmallint
  ,ftInteger
  ,ftWord
  ,ftLargeint
  ];
end;
class function TMemTable.IsFloatFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftFloat
  ,ftCurrency
  ];
end;

class function TMemTable.IsBCDFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftBCD
  ,ftFMTBcd
  ];
end;

class function TMemTable.IsDateTimeFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftDate
  ,ftTime
  ,ftDateTime
  ,ftTimeStamp
  ];
end;
class function TMemTable.IsVariantFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftVariant
  ];
end;
class function TMemTable.IsBlobFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftMemo
  ,ftWideMemo
  ,ftFmtMemo

  ,ftBlob
  ,ftGraphic

  ,ftOraBlob
  ,ftOraClob
  ];
end;
procedure TMemTable.AddFields();
var
  i : Integer;
  FieldInfo : TFieldInfo;
begin
  Active := False;

  for i := 0 to Fields.Count - 1 do
  begin
    FieldInfo := TFieldInfo.Create(Fields[i]);
    FFields.Add(FieldInfo);
    //AddField(Fields[i].FieldName, Fields[i].DataType, Fields[i].FieldKind, Fields[i].Size, Fields[i].FieldNo, Fields[i]).DisplayLabel := Fields[i].DisplayLabel;
  end;

  if Assigned(Fields.DataSet) then
  begin
    // TODO: FCalcFieldsSize := _TDataset(Fields.DataSet).CalcFieldsSize;
  end;

end;
procedure TMemTable.GetFieldNames(List: TStrings);
var
  i : Integer;
begin
  for i := 0 to FFields.Count - 1 do
    List.Add(TFieldInfo(FFields[i]).Field.FieldName);
end;
function TMemTable.IndexOfField(Field: TFieldInfo): Integer;
begin
  Result := FFields.IndexOf(Field)
end;

function TMemTable.IndexOfFieldName(const FieldName: string): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FFields.Count - 1 do
    if AnsiSameText(FieldName, TFieldInfo(FFields[i]).Field.FieldName) then
    begin
      Result := i;
      Exit; //==>
    end;
end;
function TMemTable.FindInfoField(const FieldName: string): TFieldInfo;
var
  Index : Integer;
begin
  Index := IndexOfFieldName(FieldName);
  if Index = -1 then
    Result := nil
  else
    Result := TFieldInfo(FFields[Index]);
end;

function TMemTable.FieldInfoByName(const FieldName: string): TFieldInfo;
begin
  Result := FindInfoField(FieldName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Field not found: %s', [FieldName]);
end;
procedure TMemTable.GetFieldInfoList(List: TList; const FieldNames: string);
var
  Pos   : Integer;
  Field : TFieldInfo;
begin
  if Assigned(List) and (FieldNames <> '') then
  begin
    Pos := 1;
    while Pos <= Length(FieldNames) do
    begin
      Field := FieldInfoByName(ExtractFieldName(FieldNames, Pos));
      List.Add(Field);
    end;
  end;

end;
function TMemTable.GetFieldInfoList(const FieldNames: string): TList;
begin
  Result := TList.Create();
  GetFieldInfoList(Result, FieldNames);
end;
function TMemTable.GetFieldDataSize(FieldInfo: TFieldInfo): Integer;
var
  Field: TField;
begin
  Result := 0;
  Field  := FieldInfo.Field;

  case Field.DataType of
    ftString        : Result := (Field.Size + 1) * SizeOf(AnsiChar);
    ftWideString    : Result := ((Field.Size + 1) * SizeOf(WideChar)) + SizeOf(LongWord);
    ftGuid          : Result := (Field.Size + 1) * SizeOf(AnsiChar);

    ftFixedChar     : Result := (Field.Size + 1) * SizeOf(AnsiChar);
    ftFixedWideChar : Result := (Field.Size + 1) * SizeOf(WideChar);

    ftAutoInc       : Result := SizeOf(Integer);
    ftSmallint      : Result := SizeOf(SmallInt);
    ftInteger       : Result := SizeOf(Integer);
    ftWord          : Result := SizeOf(Word);
    ftLargeint      : Result := SizeOf(LargeInt);

    ftBoolean       : Result := SizeOf(WordBool);

    ftFloat         : Result := SizeOf(Double);
    ftCurrency      : Result := SizeOf(Double);
    ftBCD           : Result := SizeOf(TBcd);
    ftFMTBcd        : Result := SizeOf(TBcd);

    ftDate          : Result := SizeOf(Integer);
    ftTime          : Result := SizeOf(Integer);
    ftDateTime      : Result := SizeOf(TDateTime);
    ftTimeStamp     : Result := SizeOf(TTimeStamp);

    ftMemo          : Result := 0;
    ftWideMemo      : Result := 0;
    ftFmtMemo       : Result := 0;

    ftBlob          : Result := 0;
    ftGraphic       : Result := 0;

    ftOraBlob       : Result := 0;
    ftOraClob       : Result := 0;

    ftVariant       : Result := SizeOf(PVariant);
  else
    raise Exception.Create('FieldType not supported');
  end;
end;
function TMemTable.GetFieldSize(FieldInfo: TFieldInfo): Integer;
var
  Field: TField;
begin
  Field  := FieldInfo.Field;

  if IsBlobFieldType(Field.DataType) then
    Result := SizeOf(TBlob)
  else if (Field.DataType = ftWideString) then
    Result := ((Field.Size + 1) * SizeOf(WideChar)) + SizeOf(LongWord)
  else
    Result := GetFieldDataSize(FieldInfo);
end;
function TMemTable.IsFieldBufferNull(Buf: PChar): Boolean;
var
  V : Byte;
begin

  if Assigned(Buf) then
  begin
    V := PByte(Buf)^;
    Result := V = 0;
  end else
    Result := False;

{
if Assigned(Buf) then
  Result := Byte(Buf[0]) <> 0
else
  Result := False;
}
end;
function TMemTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  RecBuf: TRecordBuffer;
begin
  RecBuf := nil;
  GetActiveRecBuf(RecBuf);
  Result := TBlobStream.Create(Self, TBlobField(Field), RecBuf, Mode);
end;

function TMemTable.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  CheckActive();

  Result := 0;

  if Active then
  begin
    if (Bookmark1 = nil) and (Bookmark2 = nil) then
      Result := 0
    else if (Bookmark1 <> nil) and (Bookmark2 = nil) then
      Result := 1
    else if (Bookmark1 = nil) and (Bookmark2 <> nil) then
      Result := -1
    else if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1;
  end;

end;

function TMemTable.BookmarkValid(BM: TBookmark): Boolean;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive();

  Result := False;

  if Active then
  begin
    CursorPosChanged;
    RecBuf := AllocRecordBuffer();
    Result := (IndexOfBookmark(PInteger(BM)^) <> -1)  and  (GetRecord(RecBuf, gmCurrent, False) = grOK);
  end;

end;

function TMemTable.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  CheckActive;

  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemTable.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  CheckActive;

  Result := Null;

  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TMemTable.UpdateStatus: TUpdateStatus;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive;

  if State = dsInternalCalc then
    Result := usUnModified
  else
  begin
    if State = dsCalcFields then
      RecBuf := CalcBuffer
    else
      RecBuf := ActiveBuffer;

    Result := PRecInfo(RecBuf + FBookOfs)^.Status;
  end;
end;
procedure TMemTable.CancelUpdates();
var
  TempList : TList;
  i        : Integer;
  RecBuf   : PChar;
begin
  if Active then
  begin
    Lock;
    try
      TempList := TList.Create;
      try
        TempList.Assign(FAllRows);
        FAllRows.Clear;

        for i := 0 to TempList.Count - 1 do
        begin
          RecBuf := TempList[i];

          if PRecInfo(RecBuf + FBookOfs)^.Status <> usDeleted then
          begin
            PRecInfo(RecBuf + FBookOfs)^.Status := usUnmodified;
            FAllRows.Add(RecBuf);
          end else
            FreeRecordBuffer(RecBuf);
        end;

      finally
        TempList.Free;
      end;

    finally
      UnLock;
    end;

    First;

  end;
end;


function TMemTable.GetBlobSize(RecBuf: PChar; FieldIndex: Integer): LongWord;
begin
  Result := GetBlobData(RecBuf, nil, FieldIndex);
end;
function TMemTable.GetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer): LongWord;
begin
  Lock();
  try
    Result := 0;
    if Boolean(PBlob(RecBuf + FOffsets[FieldIndex])^.Flag) then
    begin
      Result := Length(PBlob(RecBuf + FOffsets[FieldIndex])^.Data);
      if Assigned(Buffer) then
        Move(PChar(PBlob(RecBuf + FOffsets[FieldIndex])^.Data)^, PChar(Buffer)^, Result);
    end;
  finally
    UnLock();
  end;
end;
procedure TMemTable.SetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer; BlobSize: LongWord);
begin
  Lock();
  try
    if (BlobSize > 0) and Assigned(Buffer) then
    begin
      Boolean(PBlob(RecBuf + FOffsets[FieldIndex])^.Flag) := True;
{$IFDEF STRING_BLOBS}
      SetString(PBlob(RecBuf + FOffsets[FieldIndex])^.Data, Buffer, BlobSize);
{$ELSE}
      SetLength(PBlob(RecBuf + FOffsets[FieldIndex])^.Data, BlobSize);
      Move(Buffer^,  PChar(PBlob(RecBuf + FOffsets[FieldIndex])^.Data)^, BlobSize);
{$ENDIF}
    end else
      PBlob(RecBuf + FOffsets[FieldIndex])^ := FEmptyBlob;
  finally
    UnLock();
  end;
end;
procedure TMemTable.Sort;
var
  CurBuf : PChar;
begin
  if FInitialized and (cmIndex in FModes) and (FRows.Count > 0) and (FIndexFields.Count > 0) then
  begin
    if (CurRecIndex < 0) or (CurRecIndex >= FRows.Count) then
      CurBuf := nil
    else
      CurBuf := FRows[CurRecIndex];

    QuickSort(0, FRows.Count - 1, FRows, FIndexFields, [], smAsc);

    CurRecIndex := FRows.IndexOf(CurBuf);
  end;

end;
procedure TMemTable.Rebuild;
var
  i      : Integer;
  CurBuf : PChar;
  RecBuf : PChar;
  Index  : Integer;
begin

  if FInitialized then
  begin
    if (CurRecIndex < 0) or (CurRecIndex >= FRows.Count) then
      CurBuf := nil
    else
      CurBuf := FRows[CurRecIndex] ;

    FRows.Clear;

    for i := 0 to FAllRows.Count - 1 do
    begin
      RecBuf := FAllRows[i];
      if CanDisplayRecord(RecBuf) then
        FRows.Add(RecBuf);
    end;

    FModes := FModes - [cmStatus];
    Sort();

    Index := FRows.IndexOf(CurBuf);
    if Index <> -1 then
      CurRecIndex := Index
    else
      First();
  end;

end;

(*----------------------------------------------------------------------------
  Compare functions should return
  -1  if Item1 < Item2,
   0  if Item1 = Item2,
   1  if Item1 > Item2
 ----------------------------------------------------------------------------*)
function TMemTable.CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; Options: TLocateOptions): Integer;
var
  L, L2: Integer;

  CY, CY2 : Currency;

  DT, DT2   : TDateTime;
begin
  Result := 0;

  case FieldType of
    ftGuid         ,
    ftFixedChar    ,
    ftString       : begin
                       L  := StrLen(PChar(Data1));
                       L2 := StrLen(PChar(Data2));

                       if (loCaseInsensitive in Options) then
                       begin
                         if (loPartialKey in Options) then
                            Result := strlicomp(PChar(Data1), PChar(Data2), Min(L, L2))
                         else
                            Result := stricomp(PChar(Data1), PChar(Data2));
                       end else begin
                          if (loPartialKey in Options) then
                            Result := strlcomp(PChar(Data1), PChar(Data2), Min(L, L2))
                         else
                            Result := strcomp(PChar(Data1), PChar(Data2));
                       end;
                     end;

    ftWideString   : begin
                        L  := StrLen(PWideChar(Data1));
                        L2 := StrLen(PWideChar(Data2));

                        if (loCaseInsensitive in Options) then
                        begin
                          if (loPartialKey in Options) then
                             Result := strlicomp(PWideChar(Data1), PWideChar(Data2), Min(L, L2))
                          else
                             Result := stricomp(PWideChar(Data1), PWideChar(Data2));
                        end else begin
                           if (loPartialKey in Options) then
                             Result := strlcomp(PWideChar(Data1), PWideChar(Data2), Min(L, L2))
                          else
                             Result := strcomp(PWideChar(Data1), PWideChar(Data2));
                        end;
                     end;

    ftSmallint     : if SmallInt(Data1^) < SmallInt(Data2^) then
                       Result := -1
                     else if SmallInt(Data1^) > SmallInt(Data2^)
                       then Result := 1;

    ftInteger     ,
    ftAutoInc     :  if Longint(Data1^) < Longint(Data2^) then
                       Result := -1
                     else if Longint(Data1^) > Longint(Data2^) then
                       Result := 1;

    ftWord        : if Word(Data1^) < Word(Data2^) then
                      Result := -1
                    else if Word(Data1^) > Word(Data2^) then
                      Result := 1;

    ftBoolean     : if not WordBool(Data1^) and WordBool(Data2^) then
                      Result := -1
                    else if WordBool(Data1^) and not WordBool(Data2^) then
                      Result := 1;

    ftFloat       ,
    ftCurrency    : if Double(Data1^) < Double(Data2^) then
                      Result := -1
                    else if Double(Data1^) > Double(Data2^) then
                      Result := 1;

    ftBCD         ,
    ftFmtBCD      : begin
                     CY := 0;
                     CY2 := 0;

                     if BCDToCurr(PBcd(Data1)^, CY) and BCDToCurr(PBcd(Data2)^, CY2) then
                      if CY < CY2 then
                        Result := -1
                      else if CY > CY2 then
                        Result := 1;

                    end;
    ftDate        ,
    ftTime        ,
    ftDateTime    : begin
                      DT     := NativeToDateTime(FieldType, TDateTimeRec(Data1^));
                      DT2    := NativeToDateTime(FieldType, TDateTimeRec(Data2^));

                      { reduce the accuracy to one second }
                      Result := CompareDateTimes(DT, DT2);
                    end;

    ftTimeStamp   : begin
                      DT   := TimeStampToDateTime(TTimeStamp(Data1^));  //VarSQLTimeStampCreate(TSQLTimeStamp(Data1^));
                      DT2  := TimeStampToDateTime(TTimeStamp(Data2^));  //VarSQLTimeStampCreate(TSQLTimeStamp(Data2^));

                      Result := CompareDateTime(DT, DT2);

                    end;

    ftLargeint    : if Int64(Data1^) < Int64(Data2^) then
                      Result := -1
                    else if Int64(Data1^) > Int64(Data2^) then
                      Result := 1;

    ftVariant     : Result := 0;

  end;
end;
(*----------------------------------------------------------------------------
  Compare functions should return
  -1  if Item1 < Item2,
   0  if Item1 = Item2,
   1  if Item1 > Item2

   IndexList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.CompareRecords(const Buf1, Buf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer;
var
  Data1   : PChar;
  Data2   : PChar;
  i       : Integer;
  Field   : TField;
  Index   : Integer;
begin

  Result := 0;
  if IndexFieldList <> nil then
  begin
    for i := 0 to IndexFieldList.Count - 1 do
    begin
      Field   := TFieldInfo(IndexFieldList[i]).Field;
      Index   := Field.Index;
      Data1   := Buf1 + FOffsets[Index];

      if Data1 <> nil then
      begin
        Data2    := Buf2 + FOffsets[Index];
        if Data2 <> nil then
        begin

          if Boolean(Data1[0]) and Boolean(Data2[0]) then
          begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, Field.DataType, Options);
          end else if Boolean(Data1[0]) then
            Result := 1
          else if Boolean(Data2[0]) then
            Result := -1;

          if (SortMode = smDesc) then
            Result := -Result;

          case Result of
            -1 : if (BreakMode in [bmNE, bmL, bmLE]) then Break;
             1 : if (BreakMode in [bmNE, bmG, bmGE]) then Break;
          end;


        end;
      end;
    end;
  end;


  if (Result = 0) and (SortMode <> smNone) {and Assigned(RowList)} then
  begin
    if PRecInfo(Buf1 + FBookOfs)^.Bookmark  >  PRecInfo(Buf2 + FBookOfs)^.Bookmark then
      Result := 1
    else if PRecInfo(Buf1 + FBookOfs)^.Bookmark  <  PRecInfo(Buf2 + FBookOfs)^.Bookmark then
      Result := -1;

    if (SortMode = smDesc) then
       Result := -Result;
  end;
end;
(*----------------------------------------------------------------------------*)
procedure TMemTable.QuickSort(L, R: Integer; const RowList: TList; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode);
var
  I, J: Integer;
  P   : PChar;
begin

  repeat
    I := L;
    J := R;
    P := RowList[(L + R) shr 1];
    repeat
      while CompareRecords(RowList[I], P, IndexFieldList, Options, SortMode, bmNE) < 0 do Inc(I);
      while CompareRecords(RowList[J], P, IndexFieldList, Options, SortMode, bmNE) > 0 do Dec(J);
      if I <= J then begin
        RowList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, RowList, IndexFieldList, Options, SortMode);
    L := I;
  until I >= R;

end;

function TMemTable.GetUpdateStatus(RecBuf: TRecordBuffer): TUpdateStatus;
begin
   Result :=  PRecInfo(RecBuf + FBookOfs)^.Status;
end;

function TMemTable.GetRecordByIndex(RecBuf: TRecordBuffer; RecordIndex: Integer): Boolean;
var
  SourceRecBuf : PChar;
begin
  Lock;
  try
    if FInitialized and (RecordIndex >= 0) and (RecordIndex <= FRows.Count - 1) then
      SourceRecBuf := FRows[RecordIndex]
    else
      SourceRecBuf := nil;

    Result := Assigned(SourceRecBuf);

    if Result then
      System.Move(SourceRecBuf^, RecBuf^, FRecBufSize);
  finally
    UnLock;
  end;

end;
(*----------------------------------------------------------------------------
  IndexFieldList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean;  var RecIndex: Integer): Boolean;
var
  i                 : Integer;
  IndexList         : TList;
begin
  Lock();
  try
    Result := False;
    RecIndex := -1;

    IndexList := GetFieldInfoList(IndexFieldNames);
    try
      VariantValuesToRecordBuffer(IndexList, FKeyBuffers[biLocate], KeyValues);

      for i := 0 to FRows.Count - 1 do
        if CompareRecords(FKeyBuffers[biLocate], FRows[i], IndexList, Options, smNone, bmNE) = 0 then
        begin
          Result   := True;
          RecIndex := i;
          if SyncCursor then
            FCurRecIndex := RecIndex;
          Break;  // ==>
        end;

    finally
      IndexList.Free;
    end;

  finally
    UnLock();
  end;

end;

function TMemTable.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;
var
  RecIndex : Integer;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  CursorPosChanged;

  RecIndex := -1;
  Result := LocateRecord(KeyFields, KeyValues, Options, SyncCursor, RecIndex);
  if Result then
    Result := GetRecordByIndex(TempBuffer, RecIndex);

end;

procedure TMemTable.ClearCalcFields(RecBuf: TRecordBuffer);
var
  i : Integer;
  Field     : TField;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    Field := TFieldInfo(FFields[i]).Field;
    if not (Field.FieldKind in [fkData, fkInternalCalc]) then
    begin
      SetFieldDataInternal(RecBuf, nil, Field.Index);
    end;
  end;

end;
procedure TMemTable.VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant);
var
  i      : Integer;
  Value  : Variant;
  Index  : Integer;
  Field  : TField;
begin
  { populate the locate record buffer }
  InternalInitRecord(RecBuf);

  for i := 0 to FieldList.Count - 1 do
  begin
    Field := TFieldInfo(FieldList[i]).Field;

    if (FieldList.Count = 1) and not VarIsArray(Values) then
      Value := Values
    else
      Value := Values[i];

    if not VarIsNull(Value) then
    begin
      Index   := Field.Index;
      SetFieldValue(Index, RecBuf, Value);
    end;

  end;

end;


function TMemTable.IsFieldNull(const FieldIndex: Integer; RecBuf: TRecordBuffer): Boolean;
var
  FieldData : Pointer;
begin
  FieldData  := RecBuf + FOffsets[FieldIndex];
  Result := not Boolean(PChar(FieldData)[0]);
end;

function TMemTable.GetFieldValue(const FieldIndex: Integer; RecBuf: TRecordBuffer): Variant;
var
  FieldData : Pointer;
begin
  FieldData  := RecBuf + FOffsets[FieldIndex];

  if Boolean(PChar(FieldData)[0]) then
    Result := GetValue(FieldIndex, PChar(FieldData) + 1)
  else
    Result := Null;
end;
procedure TMemTable.SetFieldValue(const FieldIndex: Integer; RecBuf: TRecordBuffer; const Value: Variant);
var
  FieldData            : Pointer;
begin
  if VarIsNull(Value) then
  begin
    SetFieldDataInternal(RecBuf, nil, FieldIndex);
  end else begin
    FieldData     := AllocMem(FSizes[FieldIndex]);
    try
      SetValue(FieldIndex, FieldData, Value);
      SetFieldDataInternal(RecBuf, FieldData, FieldIndex);
    finally
      FreeMem(FieldData);
    end;
  end;

end;
(*----------------------------------------------------------------------------
 FieldData is the Field's data buffer, null flag byte NOT included.
 This method does not perform any null flag check.
 The caller must be sure the field is not null.
 ----------------------------------------------------------------------------*)
function TMemTable.GetValue(const FieldIndex: Integer; FieldData: Pointer ): Variant;
var
  WS                : WideString;
  CY                : Currency;

begin
  Result := Null;

  case FFieldTypes[FieldIndex] of
    ftString         ,
    ftFixedChar      ,
    ftGuid           : Result := String(PChar(FieldData));

    ftWideString     ,
    ftFixedWideChar  : begin
                         SetString(WS, PWideChar(PChar(FieldData) + SizeOf(LongWord)), LongWord(FieldData^) div SizeOf(WideChar));
                         Result := WS;
                       end;

    ftSmallint       : Result := SmallInt(FieldData^);

    ftAutoInc        ,
    ftInteger        : Result := Longint(FieldData^);

    ftLargeint       : Result := Int64(FieldData^);
    ftWord           : Result := Word(FieldData^);

    ftBoolean        : Result := WordBool(FieldData^);

    ftFloat          ,
    ftCurrency       : Result := Double(FieldData^);

    ftBCD            ,
    ftFMTBcd         : begin
                         CY := 0;
                         if BCDToCurr(PBcd(FieldData)^, CY) then
                           Result := CY;
                       end;

    ftDate           ,
    ftTime           ,
    ftDateTime       : Result  := NativeToDateTime(FFieldTypes[FieldIndex], TDateTimeRec(FieldData^));

    ftTimeStamp      : Result  := TimeStampToMSecs(TTimeStamp(FieldData^)); // VarSQLTimeStampCreate(TSQLTimeStamp(FieldData^));

    ftMemo           : ;
    ftWideMemo       : ;
    ftFmtMemo        : ;

    ftBlob           : ;
    ftGraphic        : ;

    ftOraBlob        : ;
    ftOraClob        : ;

    ftVariant        : Result := 0;
  end;

end;
(*----------------------------------------------------------------------------
 FieldData is the Field's data buffer, null flag byte NOT included.
 This method does set any null flag byte.
 The caller is responsible for setting that flag, IF FieldData comes from a record buffer.
 ----------------------------------------------------------------------------*)
procedure TMemTable.SetValue(const FieldIndex: Integer; FieldData: Pointer; Value: Variant);
var
  Data : TVarData;

  BDC    : TBcd;
  P      : Pointer;

  DTR     : TDateTimeRec;
  DT      : TDateTime;
  //TS      : TSQLTimeStamp;
begin
  Data := TVarData(Value);

  case FFieldTypes[FieldIndex] of
    ftString                 ,
    ftFixedChar              ,
    ftGuid                   : if not VarIsNull(Value) then
                                 StrCopy(PChar(FieldData), PChar(String(Value)));

    ftFixedWideChar          ,
    ftWideString             : if not VarIsNull(Value) then
                                 WideToBuffer(Value, FieldData);

    ftSmallint               : if Data.VType = varByte then
                                 SmallInt(FieldData^) := Byte(Value)
                               else
                                 SmallInt(FieldData^) := SmallInt(Value) ;

    ftAutoInc                ,
    ftInteger                : Integer(FieldData^) := Integer(Value) ;

    ftLargeint               : Int64(FieldData^) := Int64(Value) ;

    ftWord                   : if Data.VType = varByte then
                                 Word(FieldData^) := Byte(Value)
                               else
                                 Word(FieldData^) := Word(Value) ;

    ftBoolean                : WordBool(FieldData^) := WordBool(Value);

    ftFloat                  ,
    ftCurrency               : if Data.VType = varDouble then
                                 Double(FieldData^) := Double(Value)
                               else
                                 Double(FieldData^) := Value;

    ftFMTBcd                 ,
    ftBCD                    : begin
                                 BDC.Precision := 0;
                                 BDC.SignSpecialPlaces := 0;
                                 CurrToBCD(Value, BDC, 32, Fields[FieldIndex].Size);
                                 P := @BDC;
                                 Move(P^, FieldData^, FSizes[FieldIndex]);
                               end;

    ftDate                   ,
    ftTime                   ,
    ftDateTime               : begin
                                 DTR := DateTimeToNative(FFieldTypes[FieldIndex], VarToDateTime(Value));
                                 P   := @DTR;
                                 Move(P^, FieldData^, FSizes[FieldIndex]);
                               end;

    ftTimeStamp              : begin
                                 {
                                   not directly VarToSQLTimeStamp(Value)
                                   because if Value is a string
                                   the VarToSQLTimeStamp creates a never destroyed TSQLTimeStampData
                                   and that leads to memory leak in Delphi 7
                                 }
                                 DT  := VarToDateTime(Value); //VarToSQLTimeStamp(VarToDateTime(Value));
                                 P   := @DT;
                                 Move(P^, FieldData^, FSizes[FieldIndex]);
                                 Value := Null;
                               end;

    ftVariant                : Variant(FieldData^) := Value;
  else
    raise Exception.CreateFmt('Unsupported field type (%s) in field %s', [FieldTypeNames[FFieldTypes[FieldIndex]], Fields[FieldIndex].DisplayLabel]);
  end;

end;
function TMemTable.GetMasterDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;
procedure TMemTable.SetMasterDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    if Value.IsLinkedTo(Self) then
      DatabaseError(SCircularDataLink);

  FMasterLink.DataSource := Value;
end;
function TMemTable.GetMasterFieldNames: string;
begin
  Result := FMasterLink.FieldNames;
end;
procedure TMemTable.SetMasterFieldNames(Value: string);
begin
  if FMasterLink.FieldNames <> Value then
    FMasterLink.FieldNames := Value;
end;
procedure TMemTable.SetDetailFieldNames(Value: string);
begin
  if FDetailFieldNames <> Value then
  begin
    FDetailFieldNames := Value;
    LoadDetailFieldList();
  end;
end;

procedure TMemTable.SetIndexFieldNames(Value: string);
begin
  if FIndexFieldNames <> Value then
  begin
    FIndexFieldNames := Value;

    LoadIndexFieldList();
  end;
end;

function TMemTable.GetStatusFilter: TUpdateStatusSet;
begin
  Result := FStatusOptions
end;
procedure TMemTable.SetStatusFilter(Value: TUpdateStatusSet);
begin
  if Value <> StatusFilter then
  begin
    if Value = [] then
      Value := [usModified, usInserted, usUnmodified] ;

    if FStatusOptions <> Value then
    begin
      FStatusOptions := Value;
      FModes := FModes + [cmStatus];
      Rebuild();
      if (FStatusOptions = [usModified, usInserted, usUnmodified]) then
        FModes := FModes - [cmStatus];
    end;

    if Active then
    begin
      Last;
      First;
    end;
  end;
end;

procedure TMemTable.OnMasterLinkChanged(Sender: TObject);

  function VarEquals(const V1, V2: Variant): Boolean;
  begin
    Result := False;
    try
      Result := VarSameValue(V1, V2);
    except
    end;
  end;

  function FieldValuesChanged(FieldList, FieldInfoList: TList): Boolean;
  var
    i : Integer;
  begin
    Result := False;
    if (FieldList.Count > 0) and (FieldList.Count = FieldInfoList.Count) then
      for i := 0 to FieldList.Count - 1 do

        if not VarEquals(TField(FieldList[i]).Value, TFieldInfo(FieldInfoList[i]).Field.Value) then
        begin
          Result := True;
          Exit; //==>
        end;
  end;

begin
  CheckBrowseMode;

  if Assigned(FMasterLink.DataSet)
    and (FMasterLink.Fields.Count > 0)
    and (not (FMasterLink.DataSet.State in [dsEdit, dsInsert]))
    and (FMasterLink.Fields.Count > 0)
    and (FMasterLink.Fields.Count = FDetailFields.Count) then
  begin
    if FieldValuesChanged(FMasterLink.Fields, FDetailFields) then
    begin
      SetLink(True);
      First;
    end;
  end;

end;
procedure TMemTable.OnMasterLinkDisabled(Sender: TObject);
begin
  SetLink(False);
  if Active then
    First();
end;
procedure TMemTable.LoadDetailFieldList();
begin
  if Active or (FieldCount > 0) then
  begin
    FDetailFields.Clear;
    GetFieldInfoList(FDetailFields, FDetailFieldNames);
  end;
end;
procedure TMemTable.LoadIndexFieldList();
begin
  if Active or (FieldCount > 0) then
  begin

    Lock();
    try
      FIndexFields.Clear;
      GetFieldInfoList(FIndexFields, FIndexFieldNames);

      if (FIndexFields.Count > 0) then
      begin
        FModes := FModes + [cmIndex];
        Sort();
      end else begin
        FModes := FModes - [cmIndex];
      end;
    finally
      UnLock();
    end;

    if Active then
    begin
      UpdateCursorPos();
      Resync([]);
    end;
  end;
end;

procedure TMemTable.SetLink(Value: Boolean);
var
  i            : Integer;
  FieldIndex   : Integer;
  Buf          : Pointer;
  MasterFields : TList;    // MasterFields is a TList containing TField objects of the master TDataLink
begin

  Lock();
  try

    if Value then          // =========== Link
    begin
      MasterFields := TList.Create();
      try
        MasterFields.Assign(FMasterLink.Fields, laOr);

        { populate the master record buffer }
        FillChar(FKeyBuffers[biMaster]^, FRecBufSize, 0);

        for i := 0 to FDetailFields.Count - 1 do
          if not TField(MasterFields[i]).IsNull then
          begin
            FieldIndex   := TFieldInfo(FDetailFields[i]).Field.Index;
            Buf          := AllocMem(FSizes[FieldIndex]);
            try
              TField(MasterFields[i]).GetData(Buf);
              SetFieldDataInternal(FKeyBuffers[biMaster], Buf, FieldIndex);
            finally
              FreeMem(Buf);
            end;
          end;
      finally
        MasterFields.Free;
      end;
      FModes := FModes + [cmLink];
    end else begin      // =========== Un-Link
      FModes := FModes - [cmLink];
    end;

    Rebuild();
  finally
    UnLock();
  end;

end;























(*----------------------------------------------------------------------------*)
class function TMemTable.DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Result.Date := TimeStamp.Date;
    ftTime: Result.Time := TimeStamp.Time;
  else
    Result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;
(*----------------------------------------------------------------------------*)
class function TMemTable.NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
var
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Data.Date;
      end;
    ftTime:
      begin
        TimeStamp.Time := Data.Time;
        TimeStamp.Date := DateDelta;
      end;
  else
    try
      TimeStamp := DateTimeToTimeStamp(Data.DateTime);
    except
      TimeStamp.Time := 0;
      TimeStamp.Date := 0;
    end;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;
(*----------------------------------------------------------------------------*)
class function TMemTable.CompareDateTimes(const A, B: TDateTime): Integer;
begin
  Result := CompareDateTime(A, B);
end;
(*----------------------------------------------------------------------------*)
class function TMemTable.BufferToWide(Buffer: Pointer): WideString;
var
  Len  : LongWord;
begin
  Result := '';
  Len           := PLongWord(Buffer)^;
  if Len <> 0 then
  begin
    SetLength(Result, Len div SizeOf(WideChar));
    Move(Pointer(PChar(Buffer) + SizeOf(LongWord))^, Pointer(Result)^, Len);
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure TMemTable.WideToBuffer(WS: WideString; Buffer: Pointer);
var
  Len : LongWord;
  Source : PChar;
begin
{$WARNINGS OFF}
  if Length(WS) > 0 then
  begin
    Source        := PChar(WS) - SizeOf(LongWord);
    Len           := PLongWord(Source)^;
    Move(Source^, Buffer^, Len + SizeOf(LongWord));
  end;
{$WARNINGS ON}
end;
(*----------------------------------------------------------------------------*)
class procedure TMemTable.GetFloatFormatAndDigits(CurrencyFormat: Boolean; var FloatFormat: TFloatFormat; var Digits: Integer);
begin
  if CurrencyFormat then
  begin
    FloatFormat := ffCurrency;
    Digits      := DefaultFormatSettings.CurrencyFormat;
  end else begin
    FloatFormat := ffGeneral;
    Digits      := 0;
  end;
end;
class procedure TMemTable.ClearObjectList(List: TList);
begin
  while (List.Count > 0) do
  begin
    try
      TObject(List[List.Count - 1]).Free;
    except
    end;
    List.Delete(List.Count - 1);
  end;
end;
(*--------------------------------------------------------------------------------*)
class function  TMemTable.Min(const A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;
(*--------------------------------------------------------------------------------*)
class function  TMemTable.Max(const A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;




end.
