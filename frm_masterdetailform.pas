unit frm_MasterDetailForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids

  ,O_App
  ,o_MemTable
  ;

type

  { TMasterDetailForm }

  TMasterDetailForm = class(TForm)
    gridDetail: TDBGrid;
    gridMaster: TDBGrid;
    Splitter1: TSplitter;
  private
    tblMaster : TMemTable;
    tblDetail : TMemTable;
    dsMaster  : TDatasource;
    dsDetail  : TDatasource;
    procedure InitializeTest();
    procedure CreateMasterTable();
    procedure CreateDetailTable();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  public

  end;


implementation

{$R *.lfm}

{ TMasterDetailForm }

procedure TMasterDetailForm.CreateMasterTable();
begin
  // master
  tblMaster := TMemTable.Create(Self);
  tblMaster.FieldDefs.Add('Id', ftAutoInc);
  tblMaster.FieldDefs.Add('Name', ftString, 100);
  //tblMaster.FieldDefs.Add('Data', ftString, 100);
  tblMaster.CreateDataset;

  dsMaster := TDataSource.Create(Self);
  dsMaster.DataSet := tblMaster;
  gridMaster.DataSource := dsMaster;

  tblMaster.Active := True;

  App.AdjustGridColumns(gridMaster);

  tblMaster.FieldByName('Id').ReadOnly := True;
end;

procedure TMasterDetailForm.CreateDetailTable();
begin
  // detail
  tblDetail := TMemTable.Create(Self);
  tblDetail.FieldDefs.Add('MasterId', ftInteger);
  tblDetail.FieldDefs.Add('Name', ftString, 100);

  tblDetail.CreateDataset;

  dsDetail := TDataSource.Create(Self);
  dsDetail.DataSet := tblDetail;
  gridDetail.DataSource := dsDetail;

  // master - detail
  tblDetail.MasterFieldNames := 'Id';
  tblDetail.DetailFieldNames := 'MasterId';
  tblDetail.MasterSource := dsMaster;

  tblDetail.Active := True;

  App.AdjustGridColumns(gridDetail);
end;
procedure TMasterDetailForm.InitializeTest();
const
  MasterRowCount = 3;
var
  i, j : Integer;

begin
  CreateMasterTable();
  CreateDetailTable();

  for i := 0 to MasterRowCount - 1 do
  begin
    tblMaster.Append();
    tblMaster.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    tblMaster.Post();
  end;

  for i := 0 to MasterRowCount - 1 do
    for j := 0 to 2 do
    begin
      tblDetail.Append();
      tblDetail.FieldByName('MasterId').AsInteger := i + 1;
      tblDetail.FieldByName('Name').AsString := 'Name_'  + IntToStr(i + 1) + '_' + IntToStr(j + 1);
      tblDetail.Post();
    end;
end;

procedure TMasterDetailForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMasterDetailForm.DoShow;
begin
  inherited DoShow;

  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

