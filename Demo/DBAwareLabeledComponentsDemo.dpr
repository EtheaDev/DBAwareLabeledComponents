program DBAwareLabeledComponentsDemo;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Vcl.LabeledShellUtils in '..\Source\Vcl.LabeledShellUtils.pas',
  Vcl.LabeledMask in '..\Source\Vcl.LabeledMask.pas',
  Vcl.LabeledExtCtrls in '..\Source\Vcl.LabeledExtCtrls.pas',
  Vcl.LabeledDBListView in '..\Source\Vcl.LabeledDBListView.pas',
  Vcl.LabeledDbImage in '..\Source\Vcl.LabeledDbImage.pas',
  Vcl.LabeledDBCtrls in '..\Source\Vcl.LabeledDBCtrls.pas',
  Vcl.LabeledCurrencyEdit in '..\Source\Vcl.LabeledCurrencyEdit.pas',
  Vcl.LabeledCtrls in '..\Source\Vcl.LabeledCtrls.pas',
  Vcl.LabeledComCtrls in '..\Source\Vcl.LabeledComCtrls.pas',
  Vcl.LabeledColorGrd in '..\Source\Vcl.LabeledColorGrd.pas',
  Vcl.LabeledCheckLst in '..\Source\Vcl.LabeledCheckLst.pas',
  Vcl.DbAwareLabeledUtils in '..\Source\Vcl.DbAwareLabeledUtils.pas',
  Vcl.DbAwareLabeledConsts in '..\Source\Vcl.DbAwareLabeledConsts.pas',
  Vcl.BoundLabel in '..\Source\Vcl.BoundLabel.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Normal and DataAware Labeled Components Editors Demo - Copyright (c) 2021-2025 Ethea S.r.l.';
  //Uses System Style for border / shadow of Forms
  TStyleManager.FormBorderStyle := TStyleManager.TFormBorderStyle.fbsSystemStyle;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
