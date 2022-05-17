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
  , Vcl.BoundLabel
  ;

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
    DbGrid: TLabeledDbGrid;
    BottomPanel: TPanel;
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
    DbGridOptionsPanel: TPanel;
    FontLabel: TLabel;
    RowLinesLabel: TLabel;
    RowMarginLabel: TLabel;
    cbHcr: TCheckBox;
    cbSort: TCheckBox;
    DbGridOptionsGroupBox: TGroupBox;
    cbCurrColor: TColorBox;
    cbEvenColor: TColorBox;
    cbActivateCustomColor: TCheckBox;
    cbIncremental: TCheckBox;
    cbAltColors: TCheckBox;
    cbCustomDraw: TCheckBox;
    FontTrackBar: TTrackBar;
    cbEditing: TCheckBox;
    cbAutoEdit: TCheckBox;
    lbOptions: TCheckListBox;
    cbDrawCheckBoxImages: TCheckBox;
    rgCtl3D: TRadioGroup;
    LineTrackBar: TTrackBar;
    RowMarginTrackBar: TTrackBar;
    filterDataEdit: TLabeledEdit;
    DBNavigator: TDBNavigator;
    btSelectStyle: TButton;
    procedure ClientDataSetAfterEdit(DataSet: TDataSet);
    procedure OpenButtonClick(Sender: TObject);
    procedure ClientDataSetIntegerFieldChange(Sender: TField);
    procedure ClientDataSetBooleanFieldGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure cbHcrClick(Sender: TObject);
    procedure cbSortClick(Sender: TObject);
    procedure cbCurrColorChange(Sender: TObject);
    procedure cbActivateCustomColorClick(Sender: TObject);
    procedure cbIncrementalClick(Sender: TObject);
    procedure cbAltColorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure cbCustomDrawClick(Sender: TObject);
    procedure FontTrackBarChange(Sender: TObject);
    procedure cbEditingClick(Sender: TObject);
    procedure cbAutoEditClick(Sender: TObject);
    procedure lbOptionsClickCheck(Sender: TObject);
    procedure btSelectStyleClick(Sender: TObject);
    procedure cbDrawCheckBoxImagesClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure rgCtl3DClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LineTrackBarChange(Sender: TObject);
    procedure RowMarginTrackBarChange(Sender: TObject);
    procedure filterDataEditExit(Sender: TObject);
    procedure PositionLabeledComboBoxSelect(Sender: TObject);
    procedure VisibleCheckBoxClick(Sender: TObject);
  private
    FMemoText: string;
    procedure BkCellColorAssign(Column : TColumn; DrawingCurrentRecord : boolean; var CellColor : TColor);
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
  , System.TypInfo
  , Vcl.SelectOptionsForm
  , Vcl.Themes;

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
var
  DbGridOptions, OptName: string;
  I: TDBGridOption;
begin
  Caption := Application.Title;

  cbHcr.Checked := DbGrid.HighlightCurrRow;
  cbAltColors.Checked := DbGrid.AlternateRowColor;
  cbSort.Checked := DbGrid.ShowSortOrder;
  cbIncremental.Checked := DbGrid.IncrementalSearch;
  cbCustomDraw.Checked := Assigned(DbGrid.OnDrawColumnCell);
  cbDrawCheckBoxImages.Checked := DbGrid.DrawCheckBoxImages;

  DbGridOptions := GetPropValue(DbGrid, 'Options');
  for i := Low(TDBGridOption) to High(TDBGridOption) do
  begin
    OptName := GetEnumName(TypeInfo(TDBGridOption), Ord(i));
    lbOptions.AddItem(OptName,nil);
    lbOptions.Checked[lbOptions.Items.Count-1] := Pos(OptName, DbGridOptions) > 0;
  end;

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

procedure TMainForm.BkCellColorAssign(Column: TColumn;
  DrawingCurrentRecord: boolean; var CellColor: TColor);
begin
  if DrawingCurrentRecord and DbGrid.HighlightCurrRow then
    CellColor := cbCurrColor.Selected
  else
  begin
    if not Odd(ClientDataSet.RecNo) then
      CellColor := cbEvenColor.Selected
    else
      CellColor := DbGrid.Color;
  end;
end;

procedure TMainForm.cbActivateCustomColorClick(Sender: TObject);
begin
  if cbActivateCustomColor.Checked then
    DbGrid.OnBkCellColorAssign := BkCellColorAssign
  else
    DbGrid.OnBkCellColorAssign := nil;
  DbGrid.Invalidate;
end;

procedure TMainForm.cbAltColorsClick(Sender: TObject);
begin
  DbGrid.AlternateRowColor := cbAltColors.Checked;
end;

procedure TMainForm.cbCurrColorChange(Sender: TObject);
begin
  DbGrid.Invalidate;
end;

procedure TMainForm.cbHcrClick(Sender: TObject);
begin
  DbGrid.HighlightCurrRow := cbHcr.Checked;
end;

procedure TMainForm.cbIncrementalClick(Sender: TObject);
begin
  DbGrid.IncrementalSearch := cbIncremental.Checked;
end;

procedure TMainForm.cbSortClick(Sender: TObject);
begin
  DbGrid.ShowSortOrder := cbSort.Checked;
end;

procedure TMainForm.filterDataEditExit(Sender: TObject);
begin
  if filterDataEdit.Text <> '' then
  begin
    ClientDataSet.Filter := filterDataEdit.Text;
    ClientDataSet.Filtered := True;
  end
  else
    ClientDataSet.Filtered := False;
end;

procedure TMainForm.btSelectStyleClick(Sender: TObject);
var
  NewStyleName: string;
begin
  NewStyleName := CBSelectStyleName(Self.Font);
  if StyleServices.Enabled then
  begin
    TStyleManager.SetStyle(NewStyleName);
  end;
end;

procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  //Quando la main-form cambia di dimensione
  //Il defaultfont dell'oggetto Application non si adatta
  //perciò tutte le form create successivamente non acquisiscono il defaultfont corretto
  Application.DefaultFont.Assign(Font);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FontTrackBar.Position := -DbGrid.Font.Height;
  FontTrackBarChange(FontTrackBar);
  LineTrackBar.Position := DbGrid.LinesPerRow;
  LineTrackBarChange(LineTrackBar);
  RowMarginTrackBar.Position := DbGrid.RowMargin;
  RowMarginTrackBarChange(RowMarginTrackBar);
end;

procedure TMainForm.gridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if (State = []) and not (DbGrid.HighlightCurrRow and DbGrid.DrawingCurrentRecord) then
  begin
    DbGrid.Canvas.Brush.Color := ClientDataSetIntegerField.Value div 255;
  end;
  DbGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TMainForm.cbCustomDrawClick(Sender: TObject);
begin
  if cbCustomDraw.Checked then
    DbGrid.OnDrawColumnCell := gridDrawColumnCell
  else
    DbGrid.OnDrawColumnCell := nil;
  DbGrid.Invalidate;
end;

procedure TMainForm.cbDrawCheckBoxImagesClick(Sender: TObject);
begin
  DbGrid.DrawCheckBoxImages := cbDrawCheckBoxImages.Checked;
end;

procedure TMainForm.FontTrackBarChange(Sender: TObject);
begin
  DbGrid.Font.Height := -FontTrackBar.position;
  DbGrid.TitleFont.Height := -FontTrackBar.position;
  FontLabel.Caption := Format('Font Height: %d',[-DbGrid.Font.Height]);
end;

procedure TMainForm.cbEditingClick(Sender: TObject);
begin
  if cbEditing.Checked then
  begin
    DbGrid.Options := DbGrid.Options + [dgEditing,dgAlwaysShowEditor];
  end
  else
  begin
    DbGrid.Options := DbGrid.Options - [dgEditing,dgAlwaysShowEditor];
  end;
end;

procedure TMainForm.cbAutoEditClick(Sender: TObject);
begin
  DataSource.AutoEdit := cbAutoEdit.Checked;
end;

procedure TMainForm.lbOptionsClickCheck(Sender: TObject);
var
  DbGridOptions : string;
  i : integer;
begin
  DbGridOptions := '';
  for i := 0 to lbOptions.Items.Count -1 do
  begin
    if lbOptions.Checked[i] then
    begin
      if DbGridOptions <> '' then
        DbGridOptions := DbGridOptions + ',';
      DbGridOptions := DbGridOptions + lbOptions.Items[i];
    end;
  end;
  SetPropValue(DbGrid, 'Options', DbGridOptions);
end;

procedure TMainForm.LineTrackBarChange(Sender: TObject);
begin
  DbGrid.LinesPerRow := LineTrackBar.Position;
  RowLinesLabel.Caption := Format('Lines per Row: %d',[DbGrid.LinesPerRow]);
end;

procedure TMainForm.rgCtl3DClick(Sender: TObject);
begin
  DbGrid.Ctl3D := rgCtl3D.ItemIndex = 0;
end;

procedure TMainForm.RowMarginTrackBarChange(Sender: TObject);
begin
  DbGrid.RowMargin := RowMarginTrackBar.Position;
  RowMarginLabel.Caption := Format('Row Margin: %d',[DbGrid.RowMargin]);
end;

initialization
  RegisterGridOddRowsColor(cl3DLight);
  RegisterGridRowMargin(4);

  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
