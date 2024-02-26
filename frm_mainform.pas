unit frm_MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , StdCtrls

  ,o_App
  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnSimple: TButton;
    btnMasterDetail: TButton;
  private
    procedure AnyClick(Sender: TObject);
    procedure ShowModalForm(FormClass: TFormClass);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  frm_simpleform
  ,frm_masterdetailform
  ;

{ TMainForm }

procedure TMainForm.AnyClick(Sender: TObject);
begin
   if (btnSimple = Sender) then
       ShowModalForm(TSimpleForm)
   else if (btnMasterDetail = Sender) then
       ShowModalForm(TMasterDetailForm)
   ;
end;

procedure TMainForm.ShowModalForm(FormClass: TFormClass);
var
  F: TForm;
begin
  F := FormClass.Create(nil);
  try
    F.ShowModal();
  finally
    F.Free;
  end;
end;

procedure TMainForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;

  KeyPreview := True;

  btnSimple.OnClick := @AnyClick;
  btnMasterDetail.OnClick := @AnyClick;

  ShowModalForm(TSimpleForm);
end;

end.

