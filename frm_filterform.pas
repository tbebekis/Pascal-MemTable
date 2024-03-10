unit frm_FilterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , StdCtrls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids

  ,TypInfo

  ,O_App
  ,o_MemTable
  ;

type

  { TFilterForm }

  TFilterForm = class(TForm)
    btnApplyFilter: TButton;
    btnCancelFilter: TButton;
    edtFilter: TEdit;
    Grid: TDBGrid;
    Label1: TLabel;
    Panel1: TPanel;
  private
    DS: TDatasource;
    Table: TMemTable;
    procedure InitializeTest();
    procedure AnyClick(Sender: TObject);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}


procedure TFilterForm.InitializeTest();
const
    Names  : array[0..6] of string = ('Teo', 'George', 'John', 'Mike', 'Nik', 'Peter', 'Bill');
    //Days : array[0..6] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    Countries : array[0..6] of string = ('Greece', 'Spain', 'Italy', 'France', 'Germany', 'Denmark', 'Poland');
var
  i: Integer;
begin
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Name', ftString, 100);
  Table.FieldDefs.Add('Country', ftString, 100);
  Table.FieldDefs.Add('Amount', ftFloat);
  Table.FieldDefs.Add('Date', ftDate);

  Table.CreateDataset;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

  Table.Active := True;

  App.AdjustGridColumns(Grid);

  Randomize();
  for i := 0 to Length(Names) - 1 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := Names[Random(Length(Names) - 1)];          // range 0..6
    Table.FieldByName('Country').AsString := Countries[Random(Length(Countries) - 1)];
    Table.FieldByName('Amount').AsFloat := (Random(99999) + 1) * 1.5;
    Table.FieldByName('Date').AsDateTime := SysUtils.Date() + Random(7);
    Table.Post();
  end;

  btnApplyFilter.OnClick := @AnyClick;
  btnCancelFilter.OnClick := @AnyClick;

  edtFilter.Text := 'Name = ' + QuotedStr('Teo');
  btnCancelFilter.Enabled := False;
end;

procedure TFilterForm.AnyClick(Sender: TObject);
begin
  if (btnApplyFilter = Sender) then
  begin
    if Length(edtFilter.Text) > 0 then
    begin
      Table.Filtered := False;
      Table.Filter := edtFilter.Text;
      Table.Filtered := True;

      btnApplyFilter.Enabled := False;
      btnCancelFilter.Enabled := True;
    end;
  end else if (btnCancelFilter = Sender) then
  begin
    Table.Filtered := False;

    btnApplyFilter.Enabled := True;
    btnCancelFilter.Enabled := False;
  end;

end;

procedure TFilterForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TFilterForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

