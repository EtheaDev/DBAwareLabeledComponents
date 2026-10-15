{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
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
unit Vcl.DBNumberBox;

interface

uses
  Winapi.Messages,
  System.Classes,
  Data.DB,
  Vcl.NumberBox,
  Vcl.DBCtrls,
  Vcl.Controls;

type
  TDbNumberBox = class;

  {TDbNumberBox}

  /// <summary>
  ///  Data-aware version of the VCL TNumberBox (available from Delphi 10.4.2)
  ///  for advanced numeric input. Binds an underlying numeric dataset field
  ///  through DataSource and DataField, automatically synchronizing the editor
  ///  mode (integer, float, currency), the number of decimals, the alignment
  ///  and the displayed value with the linked TField.
  /// </summary>
  TDbNumberBox = class(TCustomNumberBox)
  private
    FFocused: Boolean;
    FDataLink: TFieldDataLink;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetField: TField;
    function GetDataField: string;
    procedure SetDataField(const Value: string);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure SetFocused(Value: Boolean);
  protected
    property DataLink: TFieldDataLink read FDataLink;
    procedure RecordChanged(Sender : TFieldDataLink); virtual;
    procedure DataSetChanged(Sender : TFieldDataLink); virtual;
    procedure LinkEditingChanged(Sender : TFieldDataLink); virtual;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetDisplayText: string; override;
  public
    /// <summary>
    ///  Creates the control and the internal TFieldDataLink, wiring its
    ///  data-change, editing-change, update-data and active-change events.
    /// </summary>
    /// <param name="AOwner">Component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Destroys the control and frees the internal TFieldDataLink.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///  Returns True when the linked field currently holds a NULL value.
    /// </summary>
    /// <returns>True if the bound field is null; otherwise False.</returns>
    function isEmpty : boolean;
    /// <summary>
    ///  Provides access to the textual content of the editor.
    /// </summary>
    property Text;
  published
    /// <summary>
    ///  Enables entering arithmetic expressions that are evaluated into the value.
    /// </summary>
    property AcceptExpressions;
    /// <summary>
    ///  Controls how the control aligns within its parent.
    /// </summary>
    property Align;
    /// <summary>
    ///  Horizontal alignment of the text within the editor.
    /// </summary>
    property Alignment;
    /// <summary>
    ///  Specifies how the control is anchored to its parent when resized.
    /// </summary>
    property Anchors;
    /// <summary>
    ///  Determines whether the text is automatically selected when the control gets focus.
    /// </summary>
    property AutoSelect;
    /// <summary>
    ///  Determines whether the control resizes its height to fit the font.
    /// </summary>
    property AutoSize;
    /// <summary>
    ///  Bi-directional mode used for right-to-left layout support.
    /// </summary>
    property BiDiMode;
    /// <summary>
    ///  Style of the border drawn around the editor.
    /// </summary>
    property BorderStyle;
    /// <summary>
    ///  Controls automatic conversion of the typed characters to upper or lower case.
    /// </summary>
    property CharCase;
    /// <summary>
    ///  Background color of the control.
    /// </summary>
    property Color;
    /// <summary>
    ///  Size constraints (minimum/maximum width and height) for the control.
    /// </summary>
    property Constraints;
    /// <summary>
    ///  Currency symbol used when displaying values in currency mode.
    /// </summary>
    property CurrencyString;
    /// <summary>
    ///  Formatting rule that positions the currency symbol relative to the value.
    /// </summary>
    property CurrencyFormat;
    /// <summary>
    ///  Number of decimal digits shown for float and currency values.
    /// </summary>
    property Decimal;
    /// <summary>
    ///  Cursor displayed while dragging from this control.
    /// </summary>
    property DragCursor;
    /// <summary>
    ///  Whether the drag operation is a drag or a dock operation.
    /// </summary>
    property DragKind;
    /// <summary>
    ///  Determines how the control initiates drag operations.
    /// </summary>
    property DragMode;
    /// <summary>
    ///  Enables double buffering to reduce flicker when painting.
    /// </summary>
    property DoubleBuffered;
    /// <summary>
    ///  Determines whether the control responds to user input.
    /// </summary>
    property Enabled;
    /// <summary>
    ///  Font used to render the text of the editor.
    /// </summary>
    property Font;
    /// <summary>
    ///  Determines whether the selection remains visible when the control loses focus.
    /// </summary>
    property HideSelection;
    /// <summary>
    ///  Input Method Editor mode for international text input.
    /// </summary>
    property ImeMode;
    /// <summary>
    ///  Name of the Input Method Editor used for international text input.
    /// </summary>
    property ImeName;
    /// <summary>
    ///  Increment applied for large steps (for example with PageUp/PageDown).
    /// </summary>
    property LargeStep;
    /// <summary>
    ///  Numeric mode of the editor (integer, float or currency).
    /// </summary>
    property Mode;
    /// <summary>
    ///  Minimum value the editor will accept.
    /// </summary>
    property MinValue;
    /// <summary>
    ///  Maximum value the editor will accept.
    /// </summary>
    property MaxValue;
    /// <summary>
    ///  Maximum number of characters the user can type.
    /// </summary>
    property MaxLength;
    /// <summary>
    ///  Enables OEM-to-ANSI character conversion of the typed text.
    /// </summary>
    property OEMConvert;
    /// <summary>
    ///  Determines whether BiDiMode is inherited from the parent.
    /// </summary>
    property ParentBiDiMode;
    /// <summary>
    ///  Determines whether the Color is inherited from the parent.
    /// </summary>
    property ParentColor;
    /// <summary>
    ///  Determines whether the 3D appearance is inherited from the parent.
    /// </summary>
    property ParentCtl3D;
    /// <summary>
    ///  Determines whether DoubleBuffered is inherited from the parent.
    /// </summary>
    property ParentDoubleBuffered;
    /// <summary>
    ///  Determines whether the Font is inherited from the parent.
    /// </summary>
    property ParentFont;
    /// <summary>
    ///  Determines whether ShowHint is inherited from the parent.
    /// </summary>
    property ParentShowHint;
    /// <summary>
    ///  Context (popup) menu associated with the control.
    /// </summary>
    property PopupMenu;
    /// <summary>
    ///  Increment applied for small steps (for example with the spin buttons).
    /// </summary>
    property SmallStep;
    /// <summary>
    ///  Determines whether the hint is shown when the mouse hovers the control.
    /// </summary>
    property ShowHint;
    /// <summary>
    ///  Position of the control in the tab order of its parent.
    /// </summary>
    property TabOrder;
    /// <summary>
    ///  Determines whether the control can be reached with the Tab key.
    /// </summary>
    property TabStop;
    /// <summary>
    ///  Placeholder text shown when the editor is empty.
    /// </summary>
    property TextHint;
    /// <summary>
    ///  Touch gesturing and interaction settings for the control.
    /// </summary>
    property Touch;
    /// <summary>
    ///  Current numeric value held by the editor.
    /// </summary>
    property Value;
    /// <summary>
    ///  Determines whether the control is visible.
    /// </summary>
    property Visible;
    /// <summary>
    ///  Configuration of the spin up/down buttons.
    /// </summary>
    property SpinButtonOptions;
    /// <summary>
    ///  Specifies which VCL style elements (border, color, font) are applied.
    /// </summary>
    property StyleElements;
    /// <summary>
    ///  Name of the VCL style applied to the control.
    /// </summary>
    property StyleName;
    /// <summary>
    ///  Determines whether a NaN value is allowed to represent an empty/unset state.
    /// </summary>
    property UseNaNValue;
    /// <summary>
    ///  Determines whether the mouse wheel changes the value.
    /// </summary>
    property UseMouseWheel;
    /// <summary>
    ///  Determines whether the Up/Down keys change the value.
    /// </summary>
    property UseUpDownKeys;
    /// <summary>
    ///  Determines whether the value wraps around when it exceeds the min/max bounds.
    /// </summary>
    property Wrap;
    /// <summary>
    ///  Color used to render negative values.
    /// </summary>
    property NegativeValueColor;
    /// <summary>
    ///  Occurs when the text in the editor changes.
    /// </summary>
    property OnChange;
    /// <summary>
    ///  Occurs when the numeric value changes.
    /// </summary>
    property OnChangeValue;
    /// <summary>
    ///  Occurs when the control is clicked.
    /// </summary>
    property OnClick;
    /// <summary>
    ///  Occurs when the user requests the context menu.
    /// </summary>
    property OnContextPopup;
    /// <summary>
    ///  Occurs when the control is double-clicked.
    /// </summary>
    property OnDblClick;
    /// <summary>
    ///  Occurs when an object is dropped on the control during drag-and-drop.
    /// </summary>
    property OnDragDrop;
    /// <summary>
    ///  Occurs while an object is dragged over the control.
    /// </summary>
    property OnDragOver;
    /// <summary>
    ///  Occurs when an entered expression has to be evaluated.
    /// </summary>
    property OnEvaluateExpression;
    /// <summary>
    ///  Occurs when docking of the control ends.
    /// </summary>
    property OnEndDock;
    /// <summary>
    ///  Occurs when a drag operation ends.
    /// </summary>
    property OnEndDrag;
    /// <summary>
    ///  Occurs when the control receives focus.
    /// </summary>
    property OnEnter;
    /// <summary>
    ///  Occurs when the control loses focus.
    /// </summary>
    property OnExit;
    /// <summary>
    ///  Occurs when a touch gesture is performed on the control.
    /// </summary>
    property OnGesture;
    /// <summary>
    ///  Occurs to validate each character typed by the user.
    /// </summary>
    property OnValidateChar;
    /// <summary>
    ///  Occurs when a key is pressed down while the control has focus.
    /// </summary>
    property OnKeyDown;
    /// <summary>
    ///  Occurs when a character key is pressed while the control has focus.
    /// </summary>
    property OnKeyPress;
    /// <summary>
    ///  Occurs when a key is released while the control has focus.
    /// </summary>
    property OnKeyUp;
    /// <summary>
    ///  Occurs when the control is activated by a mouse click.
    /// </summary>
    property OnMouseActivate;
    /// <summary>
    ///  Occurs when a mouse button is pressed over the control.
    /// </summary>
    property OnMouseDown;
    /// <summary>
    ///  Occurs when the mouse pointer enters the control.
    /// </summary>
    property OnMouseEnter;
    /// <summary>
    ///  Occurs when the mouse pointer leaves the control.
    /// </summary>
    property OnMouseLeave;
    /// <summary>
    ///  Occurs when the mouse pointer moves over the control.
    /// </summary>
    property OnMouseMove;
    /// <summary>
    ///  Occurs when a mouse button is released over the control.
    /// </summary>
    property OnMouseUp;
    /// <summary>
    ///  Occurs when docking of the control starts.
    /// </summary>
    property OnStartDock;
    /// <summary>
    ///  Occurs when a drag operation starts.
    /// </summary>
    property OnStartDrag;

    //datalink properties
    /// <summary>
    ///  Read-only reference to the dataset field bound through DataField.
    /// </summary>
    property Field: TField read GetField;
    /// <summary>
    ///  Data source that links the control to a dataset.
    /// </summary>
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    /// <summary>
    ///  Name of the dataset field whose value is edited by the control.
    /// </summary>
    property DataField: string read GetDataField write SetDataField;
    /// <summary>
    ///  When True the bound field cannot be edited through the control.
    /// </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Variants;

{ TDbNumberBox }
constructor TDbNumberBox.Create(AOwner: TComponent);
begin
  FDataLink := TFieldDataLink.Create;
  inherited;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TDbNumberBox.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TDbNumberBox.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

function TDbNumberBox.IsEmpty: boolean;
begin
  Result := Field.IsNull;
end;

procedure TDbNumberBox.DataSetChanged(Sender: TFieldDataLink);
begin
  ; //Dataset level changes
end;

procedure TDbNumberBox.LinkEditingChanged(Sender: TFieldDataLink);
begin
  ; //Dataset level changes
end;

procedure TDbNumberBox.RecordChanged(Sender: TFieldDataLink);
begin
  ; //Dataset level changes
end;

procedure TDbNumberBox.ActiveChange(Sender: TObject);
begin
  ; //Dataset level changes
end;

function TDbNumberBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDbNumberBox.GetDisplayText: string;
begin
  //don't call inherited
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    Result := Text;
end;

procedure TDbNumberBox.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> Value then
    begin
      FDataLink.DataSource := Value;
    end;
  end;
end;

procedure TDbNumberBox.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) then Invalidate;
    FDataLink.Reset;
  end;
end;

function TDbNumberBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDbNumberBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDbNumberBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDbNumberBox.UpdateData(Sender: TObject);
begin
  if Modified then
  begin
    case FDataLink.Field.DataType of
      ftSingle, ftBytes, ftVarBytes, ftSmallint, ftInteger,
      ftWord, ftLargeint, ftLongWord, ftShortint, ftByte:
        FDataLink.Field.AsInteger := self.ValueInt;
      ftFloat:
        FDataLink.Field.AsFloat := self.ValueFloat;
      ftExtended:
        FDataLink.Field.AsExtended := self.Value;
      ftCurrency, ftBCD, ftFMTBcd:
        FDataLink.Field.AsCurrency := self.ValueCurrency;
    else
      FDataLink.Field.value := NULL;
    end;
  end;
end;

procedure TDbNumberBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if FDataLink.Edit then
    Modified := True;
end;

procedure TDbNumberBox.DataChange(Sender: TObject);

  procedure AssignDecimal(ANumericField: TNumericField);
  var
    LPos: Integer;
    LDisplayFormat: string;
    LChar: string;
    LCount: Byte;
  begin
    LDisplayFormat := ANumericField.DisplayFormat;
    LCount := 0;
    LPos := pos('.', LDisplayFormat);
    if LPos > 0 then
    begin
      LDisplayFormat := Copy(LDisplayFormat, LPos+1, MaxInt);
      for LChar in LDisplayFormat do
        if LChar = '0' then
          Inc(LCount)
        else
          break;
    end;
    if LCount > 0 then
      Decimal := LCount;
  end;

begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      self.ValueInt := 0;  {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    if FFocused and FDataLink.CanModify then
    begin
      case FDataLink.Field.DataType of
        ftSingle, ftBytes, ftVarBytes, ftSmallint, ftInteger, ftWord,
        ftLargeint, ftLongWord, ftShortint, ftByte:
          self.ValueInt := FDataLink.Field.AsInteger;
        ftFloat:
          self.ValueFloat := FDataLink.Field.AsFloat;
        ftExtended:
          self.Value := FDataLink.Field.AsExtended;
        ftCurrency, ftBCD, ftFMTBcd:
          self.ValueCurrency := FDataLink.Field.AsCurrency;
      else
        self.Value := 0;
      end;
    end
    else
    begin
      case FDataLink.Field.DataType of
        ftSingle, ftBytes, ftVarBytes, ftSmallint, ftInteger, ftWord, ftLargeint, ftLongWord, ftShortint, ftByte:
        begin
          Mode := nbmInteger;
          Decimal := 0;
        end;
        ftFloat:
        begin
          Mode := nbmFloat;
          AssignDecimal(TNumericField(FDataLink.Field));
        end;
        ftExtended:
        begin
          Mode := nbmFloat;
          AssignDecimal(TNumericField(FDataLink.Field));
        end;
        ftCurrency, ftBCD, ftFMTBcd:
        begin
          Mode := nbmCurrency;
          AssignDecimal(TNumericField(FDataLink.Field));
        end;
      end;
      Text := FDataLink.Field.DisplayText;
      if FDataLink.Editing then
        Modified := True;
    end;
  end else
  begin
    Alignment := taLeftJustify;
    if csDesigning in ComponentState then
      Text := Name else
      Text := '';
  end;
end;

procedure TDbNumberBox.SetReadOnly(const Value: boolean);
begin
  FDataLink.ReadOnly := Value;
  inherited ReadOnly := Value;
end;

function TDbNumberBox.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDbNumberBox.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDbNumberBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  LKeyControlUsed: Boolean;
begin
  LKeyControlUsed := UseUpDownKeys and not ReadOnly and FDataLink.edit and
    ((Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_PRIOR) or (Key = VK_NEXT));

  if FDataLink.edit then
    inherited;

  if (Key = VK_ESCAPE) then
    FDataLink.Reset
  else if LKeyControlUsed then
    Modified := True;
end;

procedure TDbNumberBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= #32) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..High(Char):
      if not FDataLink.Edit then
        Key := #0;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
    ^A:
      begin
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDbNumberBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDbNumberBox.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDbNumberBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocused(False);
    raise;
  end;
  inherited;
end;

initialization

finalization

end.
