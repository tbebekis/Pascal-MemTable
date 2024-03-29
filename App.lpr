program App;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frm_MainForm, o_MemTable, frm_simpleform, o_App, frm_SortForm, 
frm_StatusFilterForm, frm_RangeForm, frm_LocateLookupForm, frm_BlobForm, 
frm_FilterForm, o_FilterParser
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  Application.Run;
end.

