{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/DBAwareLabeledComponents                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Main;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Data.DB
  , Vcl.Grids
  , Vcl.DBGrids
  , Datasnap.DBClient
  , Vcl.Mask
  , Vcl.DBCtrls
  , Vcl.ExtCtrls
  , Vcl.LabeledDBCtrls
  , Vcl.ComCtrls
  , Vcl.LabeledCtrls
  , Vcl.LabeledCurrencyEdit
  , Vcl.LabeledComCtrls
  , Vcl.LabeledMask
  , Vcl.LabeledExtCtrls
  , Vcl.ColorGrd
  , Vcl.LabeledColorGrd
  , Vcl.CheckLst
  , Vcl.LabeledCheckLst
  , Vcl.LabeledDBListView
  , Vcl.Samples.Spin
//  , Vcl.LabeledButtonEdit
  , Vcl.BoundLabel;

type
  TMainForm = class(TForm)
    ClientDataSet: TClientDataSet;
    ClientDataSetIntegerField: TIntegerField;
    ClientDataSetFloatField: TFloatField;
    ClientDataSetCurrencyField: TCurrencyField;
    ClientDataSetBCDField: TBCDField;
    ClientDataSetExtendedField: TExtendedField;
    ClientDataSetFmtBCDField: TFMTBCDField;
    DataSource: TDataSource;
    PageControl: TPageControl;
    NumberBoxTabSheet: TTabSheet;
    StdControlsTabSheet: TTabSheet;
    ClientDataSetStringField: TStringField;
    ClientDataSetDateField: TDateField;
    ClientDataSetTimeField: TTimeField;
    ClientDataSetDateTimeField: TDateTimeField;
    ClientDataSetSQLTimeStampField: TSQLTimeStampField;
    ClientDataSetBooleanField: TBooleanField;
    ClientDataSetBlobField: TBlobField;
    ClientDataSetMemoField: TMemoField;
    DbGridTabSheet: TTabSheet;
    LabeledDbGrid1: TLabeledDbGrid;
    BottomPanel: TPanel;
    DBNavigator: TDBNavigator;
    OpenButton: TButton;
    LabeledEditEx: TLabeledEditEx;
    LabeledCurrencyEdit: TLabeledCurrencyEdit;
    LabeledSpinEdit: TLabeledSpinEdit;
    LabeledComboBox: TLabeledComboBox;
    LabeledMaskEdit: TLabeledMaskEdit;
    LabeledRadioGroup: TLabeledRadioGroup;
    LabeledRichEdit: TLabeledRichEdit;
    LabeledCheckListBox: TLabeledCheckListBox;
    LabeledListBox: TLabeledListBox;
    LabeledMemo: TLabeledMemo;
    DBTabSheet: TTabSheet;
    LabeledDBComboBox: TLabeledDBComboBox;
    LabeledDBMemo: TLabeledDBMemo;
    LabeledDBRichEdit: TLabeledDBRichEdit;
    LabeledDBListBox: TLabeledDBListBox;
    LabeledDBEditEx: TLabeledDBEdit;
    LabeledDBLookupComboBox: TLabeledDBLookupComboBox;
    LabeledDBLabel: TLabeledDBLabel;
    LabeledDBLookupListBox: TLabeledDBLookupListBox;
    ListClientDataSet: TClientDataSet;
    ListClientDataSetKey: TStringField;
    ListClientDataSetValue: TStringField;
    ListDataSource: TDataSource;
    ClientDataSetRichTextField: TMemoField;
    LabeledDBImage: TLabeledDBImage;
    PositionLabeledComboBox: TLabeledComboBox;
    ColorsTabSheet: TTabSheet;
    LabeledImageEthea: TLabeledImage;
    LabeledImageLogo: TLabeledImage;
    LabeledColorGrid: TLabeledColorGrid;
    LabeledColorBox: TLabeledColorBox;
    VisibleCheckBox: TCheckBox;
    procedure ClientDataSetAfterEdit(DataSet: TDataSet);
    procedure OpenButtonClick(Sender: TObject);
    procedure ClientDataSetIntegerFieldChange(Sender: TField);
    procedure ClientDataSetBooleanFieldGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PositionLabeledComboBoxSelect(Sender: TObject);
    procedure VisibleCheckBoxClick(Sender: TObject);
  private
    FMemoText: string;
    procedure CreateControls;
    procedure CreateAndFillDataSets;
    {$IFDEF D10_4+}
    procedure CreateNumberBoxForField(const AField: TField;
      const ATop, ALeft: Integer);
    {$ENDIF}
    procedure CreateDBCurrencyEdit(const AField: TField;
      const ATop, ALeft: Integer);
    procedure FillEditors;
    procedure SetControlsLabelPosition(ARootControl: TWinControl;
      APosition: TControlLabelPosition; AVisible: Boolean);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.Math
  , Winapi.ShellAPI
  {$IFDEF D10_4+}
  , Vcl.NumberBox
  , Vcl.DBNumberBox
  , Vcl.LabeledNumberBox
  {$ENDIF}
  , System.TypInfo;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  ListClientDataSet.Close;
  ClientDataSet.Close;
  CreateAndFillDataSets;
  ListClientDataSet.Open;
  ClientDataSet.Open;
end;

procedure TMainForm.SetControlsLabelPosition(ARootControl: TWinControl;
  APosition: TControlLabelPosition; AVisible: Boolean);
var
  I: Integer;
  LControl: TControl;
begin
  for I := 0 to ARootControl.ControlCount -1 do
  begin
    LControl := ARootControl.Controls[I];
    SetEditControlLabelPosition(LControl, APosition, AVisible);
    if LControl is TWinControl then
      SetControlsLabelPosition(TWinControl(LControl), APosition, AVisible);
  end;
end;

procedure TMainForm.VisibleCheckBoxClick(Sender: TObject);
begin
  PositionLabeledComboBoxSelect(PositionLabeledComboBox);
end;

procedure TMainForm.PositionLabeledComboBoxSelect(Sender: TObject);
var
  LLabelPosition: TControlLabelPosition;
begin
  LLabelPosition := TControlLabelPosition(PositionLabeledComboBox.ItemIndex);
  SetControlsLabelPosition(PageControl, LLabelPosition,
    VisibleCheckBox.Checked);
end;

procedure TMainForm.ClientDataSetAfterEdit(DataSet: TDataSet);
begin
  ;
end;

procedure TMainForm.ClientDataSetBooleanFieldGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  if Sender.AsBoolean then
    Text := '1'
  else
    Text := '0';
end;

procedure TMainForm.ClientDataSetIntegerFieldChange(Sender: TField);
begin
  ;
end;

procedure TMainForm.CreateDBCurrencyEdit(const AField: TField; const ATop, ALeft: Integer);
var
  LDBCurrencyEdit: TLabeledDBCurrencyEdit;
begin
  LDBCurrencyEdit := TLabeledDBCurrencyEdit.Create(Self);
  LDBCurrencyEdit.BoundCaption := AField.DisplayLabel;
  LDBCurrencyEdit.SetBounds(ALeft,ATop,131,21);
  LDBCurrencyEdit.Hint := 'Hint';
  LDBCurrencyEdit.Alignment := taRightJustify;
  LDBCurrencyEdit.TextHint := 'Text Hint';
  LDBCurrencyEdit.DataSource := DataSource;
  LDBCurrencyEdit.DataField := AField.FieldName;
  LDBCurrencyEdit.parent := NumberBoxTabSheet;
end;

procedure TMainForm.Loaded;
begin
  CreateControls;
  inherited;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;

  FMemoText := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.'+sLineBreak+
    'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';

  CreateAndFillDataSets;
  FillEditors;
end;

procedure TMainForm.FillEditors;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(
    '{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1040{\fonttbl{\f0\fnil\fcharset0 Calibri;}}'+sLineBreak+
     '{\*\generator Riched20 10.0.19041}\viewkind4\uc1'+sLineBreak+
     '\pard\sa200\sl276\slmult1\b\f0\fs22\lang16 LabeledRichEdit\line\b0 Example of RichEdit with \ul BoundLabel\ulnone  and \ul BoundCaption\ulnone\par'+sLineBreak+
     '}');
  try
    LabeledRichEdit.Lines.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;

  LabeledMemo.Lines.Text := FMemoText;
end;

{$IFDEF D10_4+}
procedure TMainForm.CreateNumberBoxForField(const AField: TField;
  const ATop, ALeft: Integer);
var
  LDBNumberBox: TLabeledDBNumberBox;
begin
  LDBNumberBox := TLabeledDBNumberBox.Create(Self);
  LDBNumberBox.BoundCaption := AField.DisplayLabel;
  LDBNumberBox.SetBounds(ALeft,ATop,131,21);
  LDBNumberBox.BoundLabel.SetPosition(lpLeftMiddle);
  LDBNumberBox.Hint := 'Hint';
  LDBNumberBox.Alignment := taRightJustify;
//  LDBNumberBox.CurrencyFormat := 2;
//  LDBNumberBox.MaxValue := 999999999;
//  LDBNumberBox.MinValue := -999999999;
  LDBNumberBox.TextHint := GetEnumName(TypeInfo(TFieldType), Ord(AField.DataType));
  LDBNumberBox.SpinButtonOptions.Placement := nbspInline;
  LDBNumberBox.NegativeValueColor := clRed;
  LDBNumberBox.DataSource := DataSource;
  LDBNumberBox.DataField := AField.FieldName;
  LDBNumberBox.parent := NumberBoxTabSheet;
end;
{$ENDIF}

procedure TMainForm.CreateControls;
begin
  CreateDBCurrencyEdit(ClientDataSetIntegerField, 20,110);
  CreateDBCurrencyEdit(ClientDataSetFloatField, 60,110);
  CreateDBCurrencyEdit(ClientDataSetCurrencyField, 100,110);
  CreateDBCurrencyEdit(ClientDataSetBCDField, 140,110);
  CreateDBCurrencyEdit(ClientDataSetExtendedField, 180,110);
  CreateDBCurrencyEdit(ClientDataSetFmtBCDField, 220,110);

  {$IFDEF D10_4+}
  CreateNumberBoxForField(ClientDataSetIntegerField, 20,360);
  CreateNumberBoxForField(ClientDataSetFloatField, 60,360);
  CreateNumberBoxForField(ClientDataSetCurrencyField, 100,360);
  CreateNumberBoxForField(ClientDataSetBCDField, 140,360);
  CreateNumberBoxForField(ClientDataSetExtendedField, 180,360);
  CreateNumberBoxForField(ClientDataSetFmtBCDField, 220,360);
  {$ENDIF}
end;

procedure TMainForm.CreateAndFillDataSets;
var
  LRoundValue1, LRoundValue2, LRoundValue3, LRoundValue4, LRoundValue5, LRoundValue6: Double;
  LRoundStringValue: Integer;
  LRoundString: string;
  I: Integer;
  LCurrentDateTime: TDateTime;
  LRoundBoolean: Double;
  LStream: TStringStream;
  LMemoryStreamEthea, LMemoryStreamEmb: TMemoryStream;
begin
  ListClientDataSet.CreateDataSet;
  ListClientDataSet.AppendRecord(['First','First Value']);
  ListClientDataSet.AppendRecord(['Second','Second Value']);
  ListClientDataSet.AppendRecord(['Last','Last Value']);

  ClientDataSet.CreateDataSet;
  ClientDataSetFloatField.DisplayFormat := '#,###.000';
  ClientDataSetCurrencyField.DisplayFormat := '€ #,###.00';
  ClientDataSetBCDField.DisplayFormat := '€ #,###.00';

  //Fill ClientDataSet
  LStream := nil;
  LMemoryStreamEthea := nil;
  LMemoryStreamEmb := nil;
  ClientDataSet.DisableControls;
  try
    LMemoryStreamEthea := TMemoryStream.Create;
    LMemoryStreamEmb := TMemoryStream.Create;
    LabeledImageEthea.Picture.Bitmap.SaveToStream(LMemoryStreamEthea);
    LabeledImageLogo.Picture.Bitmap.SaveToStream(LMemoryStreamEmb);
    LStream := TStringStream.Create(
      '{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1040{\fonttbl{\f0\fnil\fcharset0 Calibri;}}'+sLineBreak+
       '{\*\generator Riched20 10.0.19041}\viewkind4\uc1'+sLineBreak+
       '\pard\sa200\sl276\slmult1\b\f0\fs22\lang16 LabeledDBRichEdit\line\b0 Example of DBRichEdit with \ul BoundLabel\ulnone  and \ul BoundCaption\ulnone\par'+sLineBreak+
       '}');
    for I := 0 to 100 do
    begin
      Randomize;
      LRoundValue1 := Random(999999999) - 555555555;
      LRoundValue2 := Random(999999999) - 555555555;
      LRoundValue3 := Random(999999999) - 555555555;
      LRoundValue4 := Random(999999999) - 555555555;
      LRoundValue5 := Random(999999999) - 555555555;
      LRoundValue6 := Random(999999999) - 555555555;
      LRoundBoolean := Random(1)-0.5;
      LRoundStringValue := Trunc(Random(3));
      if LRoundStringValue = 0 then
        LRoundString := 'First'
      else if LRoundStringValue = 1 then
        LRoundString := 'Second'
      else
        LRoundString := 'Last';

      LCurrentDateTime := Now;

      ClientDataSet.AppendRecord(
        [LRoundValue1,     //Integer
        LRoundValue2/1000, //Float
        LRoundValue3/100,  //Currency
        LRoundValue4/100,  //BCD
        LRoundValue5,      //Extended
        LRoundValue6/100, //FmtBCDField
        LRoundString, //String
        Trunc(LCurrentDateTime),//Date
        LCurrentDateTime-Trunc(LCurrentDateTime),//Time
        LCurrentDateTime,//DateTi+me
        LCurrentDateTime,//SQLTimeStamp
        (LRoundBoolean < 0.5),//Boolean
        NULL,//Blob
        LRoundString+sLineBreak+FMemoText //Memo
        ]);
      ClientDataSet.Edit;
      LStream.Position := 0;
      ClientDataSetRichTextField.LoadFromStream(LStream);
      if Odd(I) then
      begin
        LMemoryStreamEthea.Position := 0;
        ClientDataSetBlobField.LoadFromStream(LMemoryStreamEthea);
      end
      else
      begin
        LMemoryStreamEmb.Position := 0;
        ClientDataSetBlobField.LoadFromStream(LMemoryStreamEmb);
      end;
      ClientDataSet.Post;
    end;
  finally
    LMemoryStreamEthea.Free;
    LMemoryStreamEmb.Free;
    LStream.Free;
    ClientDataSet.EnableControls;
    ClientDataSet.First;
  end;
  DataSource.DataSet := ClientDataSet;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
