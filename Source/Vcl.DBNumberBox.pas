{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021-2022 (Ethea S.r.l.)                                 }
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
  Winapi.Messages
  , System.Classes
  , Vcl.NumberBox
  , Vcl.DbCtrls
  , Vcl.Controls
  , Data.DB
  ;

type
  TDbNumberBox = class;

  {TDbNumberBox}

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
    procedure DoChangeValue; override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isEmpty : boolean;
    property Text;
  published
    property AcceptExpressions;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property CurrencyString;
    property CurrencyFormat;
    property Decimal;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property LargeStep;
    property Mode;
    property MinValue;
    property MaxValue;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SmallStep;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Touch;
    property Value;
    property Visible;
    property SpinButtonOptions;
    property StyleElements;
    property StyleName;
    property UseNaNValue;
    property UseMouseWheel;
    property UseUpDownKeys;
    property Wrap;
    property NegativeValueColor;
    property OnChange;
    property OnChangeValue;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEvaluateExpression;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnValidateChar;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    //datalink properties
    property Field: TField read GetField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses
  System.SysUtils
  , Winapi.Windows
  , System.Variants
  ;

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

procedure TDbNumberBox.DoChangeValue;
begin
  inherited;
//  FDataLink.Edit;
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
  ;
end;

procedure TDbNumberBox.LinkEditingChanged(Sender: TFieldDataLink);
begin
  ;
end;

procedure TDbNumberBox.RecordChanged(Sender: TFieldDataLink);
begin
  ;
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

procedure TDbNumberBox.ActiveChange(Sender: TObject);
begin
  ;
end;

initialization

finalization

end.
