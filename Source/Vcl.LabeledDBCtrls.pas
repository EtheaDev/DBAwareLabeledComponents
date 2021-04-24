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
unit Vcl.LabeledDBCtrls;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Types
  , System.SysUtils
  , System.Classes
  , Data.DB
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , Vcl.Forms
  , Vcl.Styles
  , Vcl.Controls
  , Vcl.DBCtrls
  , Vcl.DBGrids
  , Vcl.Grids
  , Vcl.Graphics
  , Vcl.DBCGrids
  , Vcl.Buttons
  , Vcl.BoundLabel
  ;

const
  DB_LABEL_OFFSET = 2;
  GRID_SCROLL_BAR_WIDTH = 21;
  INCREMENTAL_DELAY_DEFAULT = 700;

Type
  {TLabeledDBEdit}
  TLabeledDBEdit = class(TDBEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    function GetIsEmpty: Boolean;
  protected
    procedure SetBoundCaption(const Value: TCaption); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBLabel}
  TLabeledDBLabel = class(TDBText)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    FAutoHeight: boolean;
    procedure AdjustHeight;
    procedure SetAutoHeight(Value: boolean);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure Loaded; override;
    function GetClientRect: TRect; override;
    procedure SetBoundCaption(const Value: TCaption); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    property AutoHeight: boolean read FAutoHeight write SetAutoHeight default True;
  end;

  {TCBComboBoxStrings}
  TCBComboBoxStrings = class(TCustomComboBoxStrings)
  public
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    function IndexOf(const S: string): Integer; override;
  end;

  {TLabeledDBComboBox}
  TLabeledDBComboBox = class(TDBComboBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    FValues: TStrings;
    // This field is copied from the base class since it's private there.
    FDataLink: TFieldDataLink;
    procedure SetBoundCaption(const Value: TCaption);
    procedure SetValues(const Value: TStrings);
    // Returns the string in Values corresponding to the given string in Items.
    function GetValueFromItem(const AItem: string): string;
    // Returns the string in Items corresponding to the given string in Values.
    function GetItemFromValue(const AValue: string): string;
    // These four functions are copied from the base class since they're
    // private there.
    function GetComboText: string;
    procedure SetComboText(const Value: string);
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    function GetItemPos(List: TStrings; const AString: string): integer;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    function GetItemsClass: TCustomComboBoxStringsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    // Values written to the field instead of the corresponding displayed
    // text values (which are in Items).
    property Values: TStrings read FValues write SetValues;
  end;

  {TLabeledDBListBox}
  TLabeledDBListBox = class(TDBListBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBMemo}
  TLabeledDBMemo = class(TDBMemo)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoExit; override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property CharCase;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBImage}
  TLabeledDBImage = class(TDBImage)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBLookupListBox}
  TLabeledDBLookupListBox = class(TDBLookupListBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBLookupComboBox}
  TLabeledDBLookupComboBox = class(TDBLookupComboBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledDBRichEdit}
  TLabeledDBRichEdit = class(TDBRichEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpy: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpy;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  { TLabeledDBCtrlGrid }
  TLabeledDBCtrlGrid = class( TDBCtrlGrid )
  end;

  {TLabeledDbGrid}
  TLabeledDbGrid = class;
  TCBSortOrder = (soNone, soDescending, soAscending);
  TCBSortedFieldEvent = procedure(Sender: TLabeledDbGrid; Field: TField; var Sorted : TCBSortOrder) of object;
  TCBBkCellColorAssign = procedure (Column: TColumn; DrawingCurrentRecord : boolean; var CellColor : TColor) of object;
  TCBCanEditColumn = procedure (Column: TColumn; var CanEdit: Boolean) of object;
  TCBCheckBoxedColumnEvent = procedure (Column: TColumn; var IsCheckBoxedColumn: Boolean) of object;
  TColumnNotifyEvent = procedure (Column: TColumn) of object;

  TLabeledDbGrid = class(TDBGrid)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    FOnColWidthsChanged: TNotifyEvent;
    FShowSortOrder: Boolean;
    FOnSortedField: TCBSortedFieldEvent;
    FDrawingCurrentRecord : boolean;
    FHighlightCurrRow: boolean;
    FIncrementalSearch: boolean;
    StrRicercaIncrementale: string;
    FLayoutName: string;
    FOnBkCellColorAssign: TCBBkCellColorAssign;
    FAlternateRowColor: boolean;
    FSearchTimer: TTimer;
    FCheckBoxedFields: string;
    FDrawCheckBoxImages: Boolean;
    FUnsortableFields: string;
    FCanEditColumn: TCBCanEditColumn;
    FCursorIsDefault: Boolean;
    FOnIsCheckBoxedColumn: TCBCheckBoxedColumnEvent;
    FAfterCreateColumnByLayout: TColumnNotifyEvent;
    procedure OnSearchTimer(Sender : TObject);
    procedure SetBoundCaption(const Value: TCaption);
    procedure SetShowSortOrder(const Value: Boolean);
    procedure StandardSort(Field: TField; var SortOrder : TCBSortOrder);
    procedure SetHighlightCurrRow(const Value: boolean);
    procedure SetAlternateRowColor(const Value: boolean);
    procedure SetIncrementalSearch(const Value: boolean);
    procedure ChangeStrSearch(const str: string);
    procedure SetOnBkCellColorAssign(const Value: TCBBkCellColorAssign);
    function IsCurrentRowOdd : boolean;
    function isCheckBoxedColumn(Column : TColumn) : boolean;
    procedure ToggleBooleanField;
    function GetCheckBounds(Rect : TRect; Alignment : TAlignment) : TRect;
    function isMouseOverCheck(X, Y: Integer): boolean;
    function isMouseOverTitleColumn(X, Y: Integer): boolean;
    function CanEditCell(X, Y: integer): boolean;
    function CanSortColumn(X, Y: integer): boolean;
    procedure SetCheckBoxedFields(const Value: string);
    function isCheckBoxedField(Field: TField): boolean;
    function isUnsortableField(Field: TField): boolean;
    procedure doIncrementalLocate;
    procedure SetIncrementalSearchDelay(const Value: integer);
    function GetIncrementalSearchDelay: integer;
    procedure SetDrawCheckBoxImages(const Value: Boolean);
    function CanApplyCustomColors: boolean;
    function GetIsEmpty: Boolean;
    function GetTitleFont: TFont;
    procedure SetTitleFont(const Value: TFont);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure ColWidthsChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CheckIncrementalSearch(var Key: Word; Shift: TShiftState);
    procedure doIncrementalSearch(Key : Char);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure TitleClick(Column: TColumn); override;
    procedure ColExit; override;
    function CanEditModify: Boolean; override;
    procedure DblClick; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetBorderStyle: TBorderStyle;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    procedure StandardTitleClick(Column : TColumn);
    procedure UpdateColumnVisibility;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function FindColumnByField(Field : TField; out Column : TColumn) : boolean;
    function FindColumnByFieldName(const FieldName : string; out Column : TColumn) : boolean;
    procedure ShowColumnByField(Field : TField; Visible : boolean);
    function GetVisibleColumnsWidth : integer;
    procedure DrawCheckImage(Rect: TRect; Column: TColumn);
    procedure CellClick(Column: TColumn); override;
    function CanEditShow: Boolean; override;
    function ChangeColumnFieldName(const OldFieldName, NewFieldName : string) : boolean;
    property DrawingCurrentRecord : boolean read FDrawingCurrentRecord;
    property LeftCol;
    property ScrollBars;
    // Dato il nome di un campo, restituisce un riferimento alla prima colonna
    // della griglia che fa riferimento a quel campo, oppure nil.
    function ColumnByFieldName(const AFieldName: string): TColumn;
    // Dato il nome di un campo, restituisce il numero della prima colonna della
    // griglia che fa riferimento a quel campo.
    function ColumnIndexByFieldName(const AFieldName: string): Integer;
    // Date le coordinate del mouse restituisce il campo sottostante
    function GetMouseOverField(X, Y: Integer): TField;
    property DefaultRowHeight;
  published
    property TitleFont: TFont read GetTitleFont write SetTitleFont stored False;
    property IsEmpty: Boolean read GetIsEmpty;
    property Options default [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete];
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    property HighlightCurrRow : boolean read FHighlightCurrRow write SetHighlightCurrRow default True;
    property AlternateRowColor : boolean read FAlternateRowColor write SetAlternateRowColor default True;
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    property ShowSortOrder: Boolean read FShowSortOrder write SetShowSortOrder default False;
    property OnSortedField: TCBSortedFieldEvent read FOnSortedField write FOnSortedField;
    property OnIsCheckBoxedColumn: TCBCheckBoxedColumnEvent read FOnIsCheckBoxedColumn write FOnIsCheckBoxedColumn;
    property IncrementalSearch : boolean read FIncrementalSearch write SetIncrementalSearch default False;
    property IncrementalSearchDelay : integer read GetIncrementalSearchDelay write SetIncrementalSearchDelay default INCREMENTAL_DELAY_DEFAULT;
    property LayoutName : string read FLayoutName write FLayoutName;
    property OnBkCellColorAssign : TCBBkCellColorAssign read FOnBkCellColorAssign write SetOnBkCellColorAssign;
    property CheckBoxedFields: string read FCheckBoxedFields write SetCheckBoxedFields;
    property DrawCheckBoxImages: Boolean read FDrawCheckBoxImages write SetDrawCheckBoxImages default True;
    property UnsortableFields: string read FUnsortableFields write FUnsortableFields;
    property CanEditColumn: TCBCanEditColumn read FCanEditColumn write FCanEditColumn;
    property AfterCreateColumnByLayout: TColumnNotifyEvent read FAfterCreateColumnByLayout write FAfterCreateColumnByLayout;
  end;

  TNavInsMode = (imInsert, imAppend);

  //Registrazione colore per le righe pari
  procedure RegisterGridOddRowsColor(Color : TColor);

  procedure UpdateColumnsView(Columns: TDBGridColumns;
    maxSize: integer; const ColumnFieldsToHide: string = '');
  
implementation

uses
  //RTL
  DBClient, Math, TypInfo, Variants, Consts, Themes,
  //VCL
  DBActns, UxTheme, UITypes,
  //CB
  Vcl.DbAwareLabeledUtils, Vcl.LabeledCtrls;

var
  DbGridPrintSupport : TStringList;
  StandardOddRowsColor : TColor;

procedure UpdateColumnsView(Columns: TDBGridColumns;
  maxSize: integer; const ColumnFieldsToHide: string = '');
var
  i: integer;
  Field: TField;
begin
  if not Assigned(Columns) then
    Exit;
  for i := 0 to Columns.Count - 1 do
  begin
    if (Columns[i].Width > maxSize) or (Columns[i].Width=64) then
      Columns[i].Width := maxSize;
    Field := Columns[i].Field;
    if Assigned(Field) then
    begin
      if Pos(Field.FieldName+';', ColumnFieldsToHide+';') > 0 then
        Columns[i].Visible := False
      else
        Columns[i].Visible := True;
      if Field is TBooleanField then
      begin
        Columns[i].Title.Alignment := taCenter;
        Columns[i].Alignment := taCenter;
      end;
    end;
  end;
end;

procedure RegisterGridOddRowsColor(Color : TColor);
begin
  StandardOddRowsColor := Color;
end;

procedure DrawSortedShape(Canvas : TCanvas; ARect: TRect; Ascending: Boolean); {min width rect -> dxGridSortedShapeMinWidth}
var
  Width: Integer;
  Height: Integer;
  Points2: array [0..1] of TPoint;
  Points3: array [0..2] of TPoint;
  OldColor : TColor;
  LRect: TRect;
begin
  Width  := Round((ARect.Bottom - ARect.Top) / 2);
  Height := Width;
  LRect := Rect(ARect.Left,ARect.Top+1,ARect.Right,ARect.Bottom-1);
  //sbianco la parte della cella che conterrà il triangolino
  Canvas.FillRect(LRect);
  ARect.Left := ((ARect.Left + ARect.Right) div 2) - (Width div 2);
  ARect.Right := ARect.Left + Width;
  ARect.Top := ((ARect.Top + ARect.Bottom) div 2) - (Height div 2);
  ARect.Bottom := ARect.Top + Height;
  OldColor := Canvas.Pen.Color;
  Try
    if Ascending then
    begin
      // shadow
      Canvas.Pen.Color := GetStyledColor(clBtnShadow);
      Points2[0] := Point(ARect.Left + Width div 2 - 1, ARect.Top);
      Points2[1] := Point(ARect.Left, ARect.Bottom);
      Canvas.Polyline(Points2);
      Points2[0] := Point(ARect.Left + Width div 2 - 2, ARect.Top + 1);
      Points2[1] := Point(ARect.Left, ARect.Bottom - 1);
      Canvas.Polyline(Points2);
      // highlight
      Canvas.Pen.Color := GetStyledColor(clBtnHighlight);
      Points3[0] := Point(ARect.Left + 1, ARect.Bottom - 1);
      Points3[1] := Point(ARect.Left + Width - 1, ARect.Bottom - 1);
      Points3[2] := Point(ARect.Left + Width div 2, ARect.Top - 1);
      Canvas.Polyline(Points3);
      Points2[0] := Point(ARect.Left + Width div 2, ARect.Top + 1);
      Points2[1] := Point(ARect.Left + Width - 1, ARect.Bottom - 1);
      Canvas.Polyline(Points2);
    end
    else
    begin
      // shadow
      Canvas.Pen.Color := GetStyledColor(clBtnShadow);
      Points3[0] := Point(ARect.Left + Width - 1, ARect.Top);
      Points3[1] := Point(ARect.Left, ARect.Top);
      Points3[2] := Point(ARect.Left + Width div 2 - 1, ARect.Bottom);
      Canvas.Polyline(Points3);
      Points2[0] := Point(ARect.Left + 1, ARect.Top + 1);
      Points2[1] := Point(ARect.Left + Width div 2 - 1, ARect.Bottom - 1);
      Canvas.Polyline(Points2);
      // highlight
      Canvas.Pen.Color := GetStyledColor(clBtnHighlight);
      Points2[0] := Point(ARect.Left + Width - 1, ARect.Top + 1);
      Points2[1] := Point(ARect.Left + Width div 2, ARect.Bottom);
      Canvas.Polyline(Points2);
      Points2[0] := Point(ARect.Left + Width - 2, ARect.Top + 1);
      Points2[1] := Point(ARect.Left + Width div 2, ARect.Bottom - 1);
      Canvas.Polyline(Points2);
    end;
  Finally
    Canvas.Pen.Color := OldColor;
  End;
end;

{ TLabeledDBEdit }
constructor TLabeledDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBEdit.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBEdit.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDBEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledDBEdit.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

procedure TLabeledDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //Previene la pressione di canc che modifica il testo anche se non si potrebbe fare editing
  if (Key = VK_DELETE) and (DataSource <> nil) and (DataSource.State = dsBrowse) and not (DataSource.AutoEdit) then
    Key := 0
  else
    inherited;
end;

{ TLabeledDBComboBox }

procedure TLabeledDBComboBox.CMExit(var Message: TCMExit);
begin
  if Assigned(FDataLink) and Assigned(Field) and
    (not FDataLink.Edit) and (Text <> Field.AsString) then
    SetComboText(Field.AsString)
  else
    inherited;
end;

constructor TLabeledDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // This is done to get at the private data link instance of the base class.
  FDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValues := TStringList.Create;
  FBoundLabel := TControlBoundLabel.Create(Self);
end;

procedure TLabeledDBComboBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBComboBox.DataChange(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and not (Style = csSimple) and DroppedDown then
    Exit;
  if FDataLink.Field <> nil then
    SetComboText(GetItemFromValue(FDataLink.Field.Text))
  else
    if csDesigning in ComponentState then
      SetComboText(Name)
    else
      SetComboText('');
end;

procedure TLabeledDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetValueFromItem(GetComboText);
end;

destructor TLabeledDBComboBox.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TLabeledDBComboBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBComboBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBComboBox.SetValues(const Value: TStrings);
begin
  FValues.Assign(Value);
end;

function TLabeledDBComboBox.GetItemPos(List: TStrings; const AString: string): integer;
var
  i: Integer;
begin
  //Prima tenta una ricerca case-sensitive (potrebbero esserci dei doppioni)
  Result := -1;
  for i := 0 to List.Count - 1 do
  begin
    if List[i]=AString then
    begin
      Result := i;
      break;
    end;
  end;
  if Result = -1 then
    Result := SendMessage(Self.Handle, CB_FINDSTRINGEXACT, -1, LongInt(PChar(AString)));
end;

function TLabeledDBComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TCBComboBoxStrings;
end;

function TLabeledDBComboBox.GetValueFromItem(const AItem: string): string;
var
  LIndex: Integer;
begin
  LIndex := GetItemPos(Items, AItem);
  if (LIndex < 0) or (LIndex >= Values.Count) then
    Result := AItem
  else
    Result := Values[LIndex];
end;

function TLabeledDBComboBox.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

function TLabeledDBComboBox.GetItemFromValue(const AValue: string): string;
var
  LIndex: Integer;
begin
  LIndex := GetItemPos(Values, AValue);
  if (LIndex < 0) or (LIndex >= Items.Count) then
    Result := AValue
  else
    Result := Items[LIndex];
end;

procedure TLabeledDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else I := GetItemPos(Items, Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

function TLabeledDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

procedure TLabeledDBComboBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDBListBox }
constructor TLabeledDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBListBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBListBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBListBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledDBListBox.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

procedure TLabeledDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBListBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDBMemo }
constructor TLabeledDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBMemo.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBMemo.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBMemo.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledDBMemo.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

procedure TLabeledDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBMemo.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDBImage }
constructor TLabeledDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
  Self.Stretch := True;
end;

procedure TLabeledDBImage.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBImage.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBImage.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDBLookupListBox }
constructor TLabeledDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBLookupListBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBLookupListBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBLookupListBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledDBLookupListBox.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

procedure TLabeledDBLookupListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBLookupListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBLookupListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBLookupListBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDBLookupComboBox }
constructor TLabeledDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBLookupComboBox.VisibleChanging;
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBLookupComboBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledDBLookupComboBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledDBLookupComboBox.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Field) or (Field.AsString = '');
end;

procedure TLabeledDBLookupComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBLookupComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBLookupComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBLookupComboBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledDbGrid }
constructor TLabeledDbGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete];
  FCursorIsDefault := Cursor = crDefault;
  FBoundLabel := TControlBoundLabel.Create(self);
  FHighlightCurrRow := True;
  FAlternateRowColor := True;
  FDrawCheckBoxImages := True;
  FSearchTimer := TTimer.Create(nil);
  FSearchTimer.Interval := INCREMENTAL_DELAY_DEFAULT;
  FSearchTimer.Enabled := False;
  FSearchTimer.OnTimer := OnSearchTimer;
end;

procedure TLabeledDbGrid.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDbGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

function TLabeledDbGrid.isCheckBoxedColumn(Column: TColumn): boolean;
begin
  Result := isCheckBoxedField(Column.Field);
  if Assigned(FOnIsCheckBoxedColumn) then
    FOnIsCheckBoxedColumn(Column, Result);
end;

function TLabeledDbGrid.isCheckBoxedField(Field : TField): boolean;
const
  SEP = ';';
begin
  Result := (Field <> nil) and
    ((Field.DataType = ftBoolean) or
    ((FCheckBoxedFields<>'') and (Pos(SEP+UpperCase(Field.FieldName)+SEP,UpperCase(SEP+FCheckBoxedFields+SEP)) > 0)));
end;

function TLabeledDbGrid.isUnsortableField(Field: TField): boolean;
const
  SEP = ';';
begin
  Result := (Field <> nil) and
    ((Field.DataType in [ftBlob, ftMemo, ftFmtMemo, ftWideMemo]) or
    ((FUnsortableFields<>'') and (Pos(SEP+UpperCase(Field.FieldName)+SEP,UpperCase(SEP+FUnsortableFields+SEP)) > 0)));
end;

procedure TLabeledDbGrid.CellClick(Column: TColumn);
begin
  inherited;
end;

function TLabeledDbGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if isCheckBoxedColumn(Columns[SelectedIndex]) then
  begin
    HideEditor;
    GetEditText(Col,Row);
  end;
end;

function TLabeledDbGrid.CanSortColumn(X, Y: integer): boolean;
var
  Cell: TGridCoord;
begin
  Cell := MouseCoord(X, Y);
  with Columns[RawToDataColumn(Cell.X)] do
    Result := ShowSortOrder and Assigned(Field) and not isUnsortableField(Field)
end;

procedure TLabeledDbGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDbGrid.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDbGrid.SetAlternateRowColor(const Value: boolean);
begin
  FAlternateRowColor := Value;
  Invalidate;
end;

procedure TLabeledDbGrid.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDbGrid.ColWidthsChanged;
begin
  inherited;
  if (inherited LayoutLock = 0) and Assigned(FOnColWidthsChanged) then
    FOnColWidthsChanged(self);
end;

procedure TLabeledDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  //gestione CTRL-SPAZIO nelle griglie con multiselect
  if (Key = VK_SPACE)
  and (ssCtrl in Shift) and (dgMultiSelect in Options) and (SelectedRows <> nil) then
    SelectedRows.CurrentRowSelected := not SelectedRows.CurrentRowSelected;
end;

procedure TLabeledDbGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  Offset : integer;
  LRect: TRect;
  SortOrder : TCBSortOrder;
  Column : TColumn;
  dxGridSortedShapeMinWidth: Integer;
begin
  dxGridSortedShapeMinWidth := ARect.Bottom - ARect.Top; //16
  //se è il record corrente aggiorno il flag in modo tale che nell'evento
  //ondrawcolumncell il programmatore può testate questo flag!
  Dec(ACol, IndicatorOffset);
  if not ((gdFixed in AState) and (ACol < 0)) and
    (DataLink <> nil) and
    (DataLink.DataSource <> nil) and
    (DataLink.DataSource.DataSet <> nil) and
    (ARow-1 = Datalink.ActiveRecord) then
    FDrawingCurrentRecord := True
  else
    FDrawingCurrentRecord := False;

  Inc(ACol, IndicatorOffset);

  inherited; //disegno il resto

  if dgIndicator in Options then
    Offset := 1 else
    Offset := 0;

  //stampa il triangolino se è una colonna di sort
  if (FShowSortOrder) and
    (ACol-Offset >= 0) and
    (not (csLoading in ComponentState)) and
    (DataSource <> nil) and
    (DataSource.DataSet <> nil) and
    (gdFixed in AState) then
  begin
    Column := Columns[ACol-Offset];
    //verifica che la colonna faccia parte del sort attuale
    SortOrder := soNone;
    if (Column.Field <> nil) then
    begin
      if Assigned(FOnSortedField) then
       FOnSortedField(Self,Column.Field,SortOrder) else
       StandardSort(Column.Field,SortOrder);
    end;

    if SortOrder <> soNone then
    begin
      with ARect do
      begin
        if (Right - Left) >= dxGridSortedShapeMinWidth then
        begin
          //c'è spazio per disegnare il triangolino
          if Column.Title.Alignment = taRightJustify then
            LRect := Rect(Left, Top, Left + dxGridSortedShapeMinWidth, Bottom) else
            LRect := Rect(Right - dxGridSortedShapeMinWidth, Top, Right, Bottom);
          Dec(Right, dxGridSortedShapeMinWidth);
          //disegno il triangolino
          DrawSortedShape(Canvas, LRect, (SortOrder = soAscending));
        end;
      end;
    end;
  end;
end;

procedure TLabeledDbGrid.SetShowSortOrder(const Value: Boolean);
begin
  if Value <> FShowSortOrder then
  begin
    FShowSortOrder := Value;
    Invalidate;
  end;
end;

procedure TLabeledDbGrid.SetTitleFont(const Value: TFont);
begin
  inherited TitleFont.Assign(Value);
end;

procedure TLabeledDbGrid.StandardSort(Field: TField; var SortOrder: TCBSortOrder);
var
  DataSet : TDataSet;
  IndexFields : string;
begin
  SortOrder := soNone;
  DataSet := Field.DataSet;
  if isPublishedProp(Dataset,'OrderByClause') then
    IndexFields := GetStrProp(Dataset,'OrderByClause')
  else if isPublishedProp(Dataset,'IndexFieldNames') then
    IndexFields := GetStrProp(Dataset,'IndexFieldNames')
  else
    IndexFields := '';

  if IndexFields <> '' then
  begin
    //Se il campo è contenuto nell'indice corrente il sort è ascendente
    if SameText(Field.FieldName,IndexFields) then
    begin
      SortOrder := soAscending;
    end
    else if Field.FieldName+' DESC' = IndexFields then
    begin
      SortOrder := soDescending;
    end;
  end;
end;

procedure TLabeledDbGrid.TitleClick(Column: TColumn);
begin
  inherited;
{$IFDEF D14+}
  //N.B. fino a D2007 se OnTitleClick era assegnato veniva attivato in "inherited"
  //da D2010 viene lanciato solo se l'opzione dgTitleClick è a true: devo forzarlo
  if Assigned(OnTitleClick) and not (dgTitleClick in Options) then
    OnTitleClick( Column );
{$ENDIF}
  if not Assigned(OnTitleClick) then
    StandardTitleClick( Column );
end;

procedure TLabeledDbGrid.StandardTitleClick(Column: TColumn);
var
  Field : TField;
  Value : Variant;
  NewOrderBy, ActualOrderBy : string;
begin
  //Se non visualizzo l'ordine sul titolo non faccio nulla
  if not FShowSortOrder then exit;
  Screen.Cursor := crHourGlass;
  Try
    Field := Column.Field;
    if (Field = nil) or (Field.DataSet = nil) or isUnsortableField(Field) then exit;

    if isPublishedProp(Field.DataSet,'OrderByClause') then
    begin
      ActualOrderBy := GetStrProp(Field.DataSet, 'OrderByClause');
      //gestisco il descending se l'order-by è lo stesso
      if SameText(ActualOrderBy, Field.FieldName) then
        NewOrderBy := Field.FieldName+' DESC'
      else
        NewOrderBy := Field.FieldName;
      SetStringPropCheck(Field.DataSet, 'OrderByClause', NewOrderBy);
    end
    else if isPublishedProp(Field.DataSet,'IndexFieldNames') then
    begin
      //prima di cambiare indice recupero il valore del campo chiave
      Value := Field.DataSet.FieldByName(Field.FieldName).Value;
      SetStringPropCheck(Field.DataSet, 'IndexFieldNames', Field.FieldName);
      //mi riposiziono sul record precedente
      Field.DataSet.Locate(Field.FieldName, Value, []);
    end
    else   Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TLabeledDbGrid.SetDrawCheckBoxImages(const Value: Boolean);
begin
  if FDrawCheckBoxImages <> Value then
  begin
    FDrawCheckBoxImages := Value;
    Invalidate;
  end;
end;

destructor TLabeledDbGrid.Destroy;
begin
  FSearchTimer.Free;
  FSearchTimer := nil;
  inherited;
end;

procedure TLabeledDbGrid.SetOnBkCellColorAssign(const Value: TCBBkCellColorAssign);
begin
  FOnBkCellColorAssign := Value;
end;

procedure TLabeledDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    if CanEditCell(X,Y) and isMouseOverCheck(X,Y) then
      ToggleBooleanField;
  end;
end;

procedure TLabeledDbGrid.UpdateColumnVisibility;
var
  i : integer;
  Field : TField;
begin
  inherited;
  if AcquireLayoutLock then
  Try
    //Aggiorno la visibilità delle colonne in base al valore visible dei campi
    for i := 0 to Columns.Count -1 do
    begin
      Field := Columns.Items[i].Field;
      if Field = nil then continue;
      Columns[i].Visible := Field.Visible;
    end;
  Finally
    EndLayout;
  End;
end;

function TLabeledDbGrid.CanApplyCustomColors: boolean;
begin
  Result := not StyleServices.Enabled or (TStyleManager.ActiveStyle.Name = 'Windows');
end;

procedure TLabeledDbGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  CellColor : TColor;

  function GetCellColor : TColor;
  begin
    if IsCurrentRowOdd then
    begin
      if CanApplyCustomColors then
        Result := StandardOddRowsColor  //Colore righe pari e dispari alternato
      else
        Result := GetStyledColor(clBtnFace);
    end
    else
      Result := Column.Color; //Colore righe pari e dispari uguale
  end;

begin
  if FDrawingCurrentRecord then
  begin
    with Canvas do
    begin
      // colore sfondo cella EVIDENZIATO solo se non è la cella corrente
      // e non è impostata l'opzione RowSelect o MultiSelect
      if not ((gdSelected in State) and ((gdFocused in State) or (dgAlwaysShowSelection in Options))) and
        not (dgRowSelect in Options) and not (dgMultiSelect in Options) then
      begin
        if FHighlightCurrRow then
        begin
          if CanApplyCustomColors then
            CellColor := clInfoBk
          else
            CellColor := GetCellColor;
        end
        else
          CellColor := GetCellColor;
        if Assigned(OnBkCellColorAssign) then
          OnBkCellColorAssign(Column, FDrawingCurrentRecord, CellColor);
        Brush.Color := CellColor;
      end
      else
        CellColor := GetCellColor;
    end;
  end
  else
  begin
    with Canvas do
    begin
      // colore sfondo cella NORMALE solo se non è la cella corrente
      if not ((gdSelected in State) and (gdFocused in State)) and
        not (dgMultiSelect in Options) then
      begin
        CellColor := GetCellColor; //Colore righe pari e dispari uguale
        if Assigned(OnBkCellColorAssign) then
          OnBkCellColorAssign(Column, FDrawingCurrentRecord, CellColor);
        Brush.Color := CellColor;
      end
      else
        CellColor := GetCellColor;
    end;
  end;
  DefaultDrawColumnCell(Rect, DataCol, Column, State);

  // Se il tipo di dato è Boolean, mostra in alternativa alle diciture
  // false e true, l'immagine check e uncheck
  if (not (csLoading in ComponentState)) and isCheckBoxedColumn(Column) and FDrawCheckBoxImages then
  begin
    if Assigned(OnDrawColumnCell) then
      OnDrawColumnCell(Self, Rect, DataCol, Column, State);
    DrawCheckImage(Rect, Column);
  end
  else
  begin
    inherited; //disegno il resto
  end;
end;

function TLabeledDbGrid.GetBorderStyle: TBorderStyle;
begin
  Result := inherited BorderStyle;
end;

function TLabeledDbGrid.GetCheckBounds(Rect : TRect; Alignment : TAlignment) : TRect;
var
  Check_Size : integer;
begin
  Check_Size := Rect.Bottom-Rect.Top-1;
  case Alignment of
    taLeftJustify : Result.Left := Rect.Left;
    taRightJustify : Result.Left := Rect.Right - Check_Size;
    taCenter : Result.Left := Rect.Left + ((Rect.Right-Rect.Left+1) div 2) - 8;
  end;
  Result.Right := Result.Left+Check_Size;
  Result.Top := Rect.Top + ((Rect.Bottom-Rect.Top+1) div 2) - (Check_Size div 2);
  Result.Bottom := Result.Top + Check_Size;
end;

procedure TLabeledDbGrid.DrawCheckImage(Rect : TRect; Column : TColumn);
const
  CtrlState: array[Boolean] of integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);

var
  LState: TCheckBoxState;
  LDetails: TThemedElementDetails;
  LOutRect : TRect;

  function GetDrawState(AState: TCheckBoxState): TThemedButton;
  begin
    Result := tbButtonDontCare;

    if not Enabled then
      case AState of
        cbUnChecked: Result := tbCheckBoxUncheckedDisabled;
        cbChecked: Result := tbCheckBoxCheckedDisabled;
        cbGrayed: Result := tbCheckBoxMixedDisabled;
      end
(*
    else if Pressed and MouseInControl then
      case AState of
        cbUnChecked: Result := tbCheckBoxUncheckedPressed;
        cbChecked: Result := tbCheckBoxCheckedPressed;
        cbGrayed: Result := tbCheckBoxMixedPressed;
      end
    else if MouseInControl then
      case AState of
        cbUnChecked: Result := tbCheckBoxUncheckedHot;
        cbChecked: Result := tbCheckBoxCheckedHot;
        cbGrayed: Result := tbCheckBoxMixedHot;
      end
*)
    else
      case AState of
        cbUnChecked: Result := tbCheckBoxUncheckedNormal;
        cbChecked: Result := tbCheckBoxCheckedNormal;
        cbGrayed: Result := tbCheckBoxMixedNormal;
      end;
  end;

begin
  Canvas.FillRect(Rect);
  LOutRect := GetCheckBounds(Rect, Column.Alignment);
  if Column.Field.IsNull then
    LState := cbGrayed
  else if Column.Field.AsBoolean then
    LState := cbChecked
  else
    LState := cbUnchecked;
  LDetails := StyleServices.GetElementDetails(GetDrawState(LState));
  StyleServices.DrawElement(Canvas.Handle, LDetails, LOutRect, LOutRect
    {$IF CompilerVersion > 32}, FCurrentPPI{$IFEND});
  Canvas.Brush.Style := bsClear;
end;

function TLabeledDbGrid.FindColumnByField(Field : TField; out Column : TColumn) : boolean;
var
  i : integer;
begin
  Result := False;
  for i := 0 to Columns.Count -1 do
  begin
    if Columns[i].Field = Field then
    begin
      Result := True;
      Column := Columns[i];
    end;
  end;
end;

function TLabeledDbGrid.FindColumnByFieldName(const FieldName : string; out Column : TColumn) : boolean;
var
  i : integer;
begin
  Result := False;
  for i := 0 to Columns.Count -1 do
  begin
    if SameText(Columns[i].FieldName,FieldName) then
    begin
      Result := True;
      Column := Columns[i];
    end;
  end;
end;

procedure TLabeledDbGrid.ShowColumnByField(Field : TField; Visible : boolean);
var
  Column : TColumn;
begin
  if FindColumnByField(Field, Column) then
    Column.Visible := Visible;
end;

function TLabeledDbGrid.GetVisibleColumnsWidth : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Columns.Count -1 do
  begin
    if Columns[i].Visible then
      Result := Result + Columns[i].Width;
  end;
end;

function TLabeledDbGrid.IsCurrentRowOdd: boolean;
begin
  Result := FAlternateRowColor and Assigned(DataSource) and
    Assigned(DataSource.DataSet) and (DataSource.DataSet.Active) and
    Odd(DataSource.DataSet.RecNo);
end;

procedure TLabeledDbGrid.SetHighlightCurrRow(const Value: boolean);
begin
  FHighlightCurrRow := Value;
  Invalidate;
end;

procedure TLabeledDbGrid.SetIncrementalSearch(const Value: boolean);
begin
  FIncrementalSearch := Value;
end;

procedure TLabeledDbGrid.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if not ((csLoading in ComponentState) and ParentFont) then
    inherited
  else
    FCurrentPPI := MulDiv(96, M, D);
end;

procedure TLabeledDbGrid.ChangeStrSearch(const str : string);
var
  S : String;
  I : Integer;
begin
  if (SelectedIndex < 0) or (Columns.Count < SelectedIndex) then
    Exit;

  StrRicercaIncrementale := str;
  S := Trim(Columns[SelectedIndex].Title.Caption);
  I := Pos(']-',S);
  if I > 0 then S := CopyToEnd(s,I+2);
  if StrRicercaIncrementale <> '' then
    S := '[' + StrRicercaIncrementale + ']-'+ S;
  Columns[SelectedIndex].Title.Caption := S;
end;

procedure TLabeledDbGrid.CheckIncrementalSearch(var Key: Word; Shift: TShiftState);
var
  Tasto : char;
  KeyPressed : Word;
begin
  KeyPressed := Key;
  //Workaround per il tastierino numerico
  if (KeyPressed >= 96) and (KeyPressed <= 105) then
    KeyPressed := KeyPressed - 96 + Ord('0');

  Tasto := Char(KeyPressed);

  if not Assigned(DataSource) or
    not Assigned(DataSource.DataSet) or not Assigned(SelectedField)
    or not (SelectedField.IsValidChar(Tasto)) then
  begin
    ChangeStrSearch('');
    exit;
  end;

  if (Key = VK_ESCAPE) then
  begin
    ChangeStrSearch('');
  end;
end;

procedure TLabeledDbGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if IncrementalSearch then
    CheckIncrementalSearch(Key, Shift);
  inherited;
end;

procedure TLabeledDbGrid.ToggleBooleanField;
var
  Field : TField;
begin
  Field := SelectedField;
  if inherited CanEditModify then
    Field.AsBoolean := not Field.AsBoolean;
end;

procedure TLabeledDbGrid.KeyPress(var Key: Char);
begin
  //Toggle boolean field pressing space
  if (Key = ' ') and isCheckBoxedColumn(Columns[SelectedIndex]) then
    ToggleBooleanField;
  if IncrementalSearch and
    not ((dgEditing in Options) and CanEditModify) and
    not DataLink.Editing and (Key <> #13) then
    doIncrementalSearch(Key)
  else
    inherited;
end;

procedure TLabeledDbGrid.doIncrementalSearch(Key : Char);
const
  ValidKeys = ['0'..'9','A'..'Z','a'..'z',' ','.',',','-','/',#8,'_',#128..#255];
begin
  if IsCheckBoxedColumn(Columns[SelectedIndex]) then
    Exit;

  {$IFDEF DXE+}
  if CharInSet(Key, ValidKeys) then
  {$ELSE}
  if (Key in ValidKeys) then
  {$ENDIF}
  begin
    if (Key = #8) then  //Backspace
      ChangeStrSearch(copy(StrRicercaIncrementale,1,length(StrRicercaIncrementale)-1))
    else
      ChangeStrSearch(StrRicercaIncrementale + Key);
  end
  else
    ChangeStrSearch('');

  //Ricerca solo se il timer è scaduto
  FSearchTimer.Enabled := False;
  FSearchTimer.Enabled := True;
end;

procedure TLabeledDbGrid.doIncrementalLocate;
var
  IntValue : Integer;
  DateValue : TDateTime;
begin
  FSearchTimer.Enabled := False;
  if StrRicercaIncrementale <> '' then
  begin
    Screen.Cursor := crHourGlass;
    Try
      if SelectedField.InheritsFrom(TNumericField) then
      begin
        if TryStrToInt(StrRicercaIncrementale, IntValue) then
          Datasource.DataSet.Locate(SelectedField.FieldName,
            IntValue,[]);
      end
      else if SelectedField.InheritsFrom(TDateTimeField) then
      begin
        if TryStrToDateTime(StrRicercaIncrementale, DateValue) then
          Datasource.DataSet.Locate(SelectedField.FieldName,
            DateValue,[]);
      end
      else if SelectedField.InheritsFrom(TSQLTimeStampField) then
      begin
        if TryStrToDateTime(StrRicercaIncrementale, DateValue) then
          Datasource.DataSet.Locate(SelectedField.FieldName,
            DateValue,[]);
      end
      else
        Datasource.DataSet.Locate(SelectedField.FieldName,
          StrRicercaIncrementale,[loCaseInsensitive, loPartialKey]);
    Finally
      Screen.Cursor := crDefault;
    End;
  end;
end;

procedure TLabeledDbGrid.ColExit;
begin
  ChangeStrSearch('');
  inherited;
end;


function TLabeledDbGrid.ColumnByFieldName(const AFieldName: string): TColumn;
var
  I: Integer;
begin
  Result := nil;
  I := ColumnIndexByFieldName(AFieldName);
  if I >= 0 then
    Result := Columns[I];
end;

function TLabeledDbGrid.ColumnIndexByFieldName(const AFieldName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Pred(Columns.Count) do
  begin
    if CompareText(Columns[I].FieldName, AFieldName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TLabeledDbGrid.CanEditModify: Boolean;
begin
  if isCheckBoxedColumn(Columns[SelectedIndex]) then
    Result := False
  else
    Result := inherited CanEditModify;
end;

procedure TLabeledDbGrid.DblClick;
begin
  inherited;
  if (dgEditing in Options) and
     isCheckBoxedColumn(Columns[SelectedIndex]) then
    ToggleBooleanField;
end;

function TLabeledDbGrid.isMouseOverCheck(X, Y: Integer) : boolean;
var
  Rect : TRect;
  OutRect : TRect;
  Cell: TGridCoord;
  ColIndex : integer;
  RowIndex : integer;
begin
  Result := False;
  //Verifico se entro nello spazio della cella selezionata
  Cell := MouseCoord(X, Y);
  ColIndex := Cell.X-Indicatoroffset;
  RowIndex := Cell.Y-Ord(dgtitles in Options);
  if (ColIndex >= 0) and (RowIndex >= 0) then
  begin
    if isCheckBoxedColumn(Columns[ColIndex]) then
    begin
      //Recupero le dimensioni del checkbox
      Rect := CellRect(Cell.X, Cell.Y);
      OutRect := GetCheckBounds(Rect,Columns[ColIndex].Alignment);
      //Verifico se con il mouse ci sono finito sopra
      if (X > OutRect.Left) and (Y > OutRect.Top) and
        (X < OutRect.Right) and (Y < OutRect.Bottom) then
      begin
        Result := True;
      end;
    end;
  end;
end;

function TLabeledDbGrid.isMouseOverTitleColumn(X, Y: Integer): boolean;
var
  Cell: TGridCoord;
  FTitleOffset: integer;
begin
  Cell := MouseCoord(X,Y);
  if dgTitles in Options then
    FTitleOffset := 1
  else
    FTitleOffset := 0;
  Result := (Cell.X >= IndicatorOffset) and (Cell.Y >= 0) and (Cell.Y < FTitleOffset);
end;

function TLabeledDbGrid.CanEditCell(X, Y: integer) : boolean;
var
  Cell: TGridCoord;
  Column: TColumn;
  ColNum: Integer;
begin
  Result := False;
  Cell := MouseCoord(X, Y);
  if (Cell.X <> -1) and (Cell.Y <> -1) then
  begin
    ColNum := RawToDataColumn(Cell.X);
    if ColNum < 0 then
      Exit;
    Column := Columns[ColNum];
    if (dgediting in Options) and not ReadOnly and Datalink.Active and not Datalink.Readonly then
    begin
      if (DataLink.Editing and (Cell.Y = Row)) or
        (DataLink.DataSource.AutoEdit) then
      begin
        with Column do
        if (not ReadOnly) and Assigned(Field) and Field.CanModify
          and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
        begin
          Result := True;
        end;
      end;
    end;
    if Assigned(FCanEditColumn) then
      FCanEditColumn(Column, Result);
  end;
end;

procedure TLabeledDbGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  //Cambio il cursore da freccia a dito
  if (Shift=[]) then
  begin
    if (isMouseOverCheck(X,Y) and CanEditCell(X,Y)) or //Passaggio su campo boolean editabile
      (isMouseOverTitleColumn(X,Y) and CanSortColumn(X,Y)) then //Passaggio su titolo di colonna ordinabile
    begin
      if Cursor = crDefault then
          FCursorIsDefault := True;
      Cursor := crHandPoint;
    end
    else if FCursorIsDefault then
      Cursor := crDefault;
  end;
end;

procedure TLabeledDbGrid.SetCheckBoxedFields(const Value: string);
begin
  if FCheckBoxedFields <> Value then
  begin
    FCheckBoxedFields := Value;
  end;
end;

procedure TLabeledDbGrid.OnSearchTimer(Sender: TObject);
begin
  doIncrementalLocate;
end;

procedure TLabeledDbGrid.SetIncrementalSearchDelay(const Value: integer);
begin
  FSearchTimer.Interval := Value;
end;

function TLabeledDbGrid.GetIncrementalSearchDelay: integer;
begin
  Result := FSearchTimer.Interval;
end;

function TLabeledDbGrid.GetIsEmpty: Boolean;
begin
  Result := RowCount = 0;
end;

function TLabeledDbGrid.GetMouseOverField(X, Y: Integer): TField;
var
  Cell: TGridCoord;
  Column: TColumn;
  ColNum: Integer;
begin
  Result := nil;
  if not isMouseOverTitleColumn(X,Y) then
  begin
    Cell := MouseCoord(X, Y);
    if (Cell.X <> -1) and (Cell.Y <> -1) then
    begin
      ColNum := RawToDataColumn(Cell.X);
      if ColNum < 0 then
        Exit;
      Column := Columns[ColNum];
      if Datalink.Active then
        Result :=  Column.Field;
    end;
  end;
end;

function TLabeledDbGrid.GetTitleFont: TFont;
begin
  Result := inherited TitleFont;
end;

function TLabeledDbGrid.ChangeColumnFieldName(const OldFieldName,
  NewFieldName: string): boolean;
var
  Column : TColumn;
begin
  Result := FindColumnByFieldName(OldfieldName, Column);
  if Result then
    Column.FieldName := NewFieldName;
end;

{ TLabeledDBLabel }

constructor TLabeledDBLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
  FAutoHeight := True;
  Color := GetStyledColor(clWindow);
end;

procedure TLabeledDBLabel.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDBLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBLabel.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDBLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBLabel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBLabel.DoDrawText(var Rect: TRect; Flags: Integer);
var
  XOffSet : integer;
begin
  if ((BiDiMode = bdRightToLeft) and (Alignment = taRightJustify)) or
     ((BiDiMode = bdLeftToRight) and (Alignment = taLeftJustify)) then
    XOffSet := DB_LABEL_OFFSET
  else if ((BiDiMode = bdLeftToRight) and (Alignment  = taRightJustify)) or
       ((BiDiMode = bdRightToLeft) and (Alignment  = taLeftJustify)) then
    XOffSet := -DB_LABEL_OFFSET
  else
    XOffSet := 0;

  OffsetRect(Rect, XOffSet, DB_LABEL_OFFSET);
  Try
    inherited;
  Finally
    OffsetRect(Rect, -XOffSet, -DB_LABEL_OFFSET);
  End;
end;

function TLabeledDBLabel.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  Result.Top := Result.Top+1;
  Result.Left := Result.Left+1;
  Result.Bottom := Result.Bottom-1;
  Result.Right := Result.Right-1;
(*
  //Sposta di un pixel l'area client
  Result.Left := Result.Left + 2;
  Result.Top := Result.Top + 2;
*)
end;

procedure TLabeledDBLabel.Loaded;
begin
  inherited;
  //MARTELLATA: Forza sempre il colore a clWindow
  inherited Color := GetStyledColor(clWindow);
end;

procedure TLabeledDBLabel.WMPaint(var Message: TWMPaint);
var
  R: TRect;
begin
  inherited;
  with Canvas do
  begin
    R := ClientRect;
    //Disegna il bordino del controllo
    Brush.Color := GetStyledColor(cl3DDkShadow);
    InflateRect(R, +1, +1);
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := Color;
  end;
end;

procedure TLabeledDBLabel.CMFontChanged(var Message: TMessage);
begin
  AdjustHeight;
end;

procedure TLabeledDBLabel.SetAutoHeight(Value: boolean);
begin
  if Value <> FAutoHeight then
  begin
    FAutoHeight := Value;
    if FAutoHeight then
      AdjustHeight;
  end;
end;

procedure TLabeledDBLabel.AdjustHeight;
var
  X : integer;
  W : integer;
  Rect : TRect;
begin
  if Parent = nil then exit;
  if not (csReading in ComponentState) and FAutoHeight then
  begin
    X := Left;
    W := Width;
    Rect := CalcTextBounds(X, ClientRect, Text, Font);
    SetBounds(X, Top, W, Rect.Bottom + (3*DB_LABEL_OFFSET));
  end;
end;

{ TCBComboBoxStrings }

function TCBComboBoxStrings.Add(const S: string): Integer;
begin
  Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

function TCBComboBoxStrings.IndexOf(const S: string): Integer;
begin
  Result := TLabeledDBComboBox(ComboBox).GetItemPos(Self,S);
end;

procedure TCBComboBoxStrings.Insert(Index: Integer; const S: string);
begin
  if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

{ TLabeledDBRichEdit }

constructor TLabeledDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDBRichEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

procedure TLabeledDBRichEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, BoundLabel);
end;

function TLabeledDBRichEdit.GetIsEmpy: Boolean;
begin
  Result := Trim(Lines.Text) = '';
end;

procedure TLabeledDBRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDBRichEdit.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDBRichEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDBRichEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDBRichEdit.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

type
 TWinControlH= class(TWinControl);

initialization
  DbGridPrintSupport := TStringList.Create;
  StandardOddRowsColor := GetStyledColor(clWindow);

finalization
  DbGridPrintSupport.Free;

end.
