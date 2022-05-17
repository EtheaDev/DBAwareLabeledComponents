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
unit Vcl.BoundLabel;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Types
  , System.SysUtils
  , System.Classes
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Graphics
  ;

Type
  //Label position around control
  TControlLabelPosition = (
    lpTopLeft,
    lpTopCenter,
    lpTopRight,
    lpBottomLeft,
    lpBottomCenter,
    lpBottomRight,
    lpLeftTop,
    lpLeftMiddle,
    lpLeftBottom,
    lpRightTop,
    lpRightMiddle,
    lpRightBottom
    );

const
  DEFAULT_LABEL_SPACING = 1;
  DEFAULT_BUTTON_WIDTH = 20;
  CHECKBOX_WIDTH = 16;
  DEFAULT_LABEL_POSITION = lpTopLeft;

type
  TControlBoundLabel = class(TCustomLabel)
  private
    FOwner : TControl;
    FLabelSpacing: Integer;
    FLabelPosition: TControlLabelPosition;
    procedure SetLabelPosition(const Value: TControlLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure Loaded; override;
  public
    TopOffSet: integer; //Used internally for positioning
    constructor Create(AOwner: TComponent); override;
    procedure SetPosition(APosition: TControlLabelPosition;
      ASpacing: Integer = 1; AVisible: Boolean = True);
    procedure Click; override;
    property FocusControl;
    property ParentFont;
    property Font;
  published
    property Color;
    property Transparent default True;
    property ParentColor;
    property WordWrap;
    property Alignment;
    property AutoSize;
    property Width default 0;
    property LabelPosition: TControlLabelPosition read FLabelPosition write SetLabelPosition default DEFAULT_LABEL_POSITION;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default DEFAULT_LABEL_SPACING;
  end;

function IsVCLStyled: boolean;
function GetStyledColor(Color: TColor): TColor;
procedure EnterEditCtrl( Ctrl: TControl; BoundLabel: TControlBoundLabel);
procedure ExitEditCtrl( Ctrl: TControl; BoundLabel: TControlBoundLabel);
procedure RecalculateBounds(var ALeft, ATop, AWidth, AHeight: Integer;
  BoundLabel : TControlBoundLabel; Owner : TControl );
procedure DrawBoundLabel(BoundLabel : TControlBoundLabel; Owner : TControl );
procedure ChangeBoundCaption(Value : TCaption; var BoundLabel : TControlBoundLabel ;
  Owner : TControl);
procedure SetParentOfLabel(BoundLabel : TControlBoundLabel;
  Parent: TWinControl; Owner : TControl );
function CalcTextBounds( var X: integer; ClientRect : TRect; const Text : string;
  Font : TFont) : TRect;

//Calcola l'altezza standard di un componente di edit in base al suo font e al tipo di 3D
procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);

function GetEditControlFont(AEditControl: TControl): TFont;
procedure SetEditControlLabelPosition(AEditControl: TControl;
  ALabelPosition: TControlLabelPosition; AVisible: Boolean = True);

implementation

uses
  Vcl.Mask
  , Vcl.Themes
  , Vcl.LabeledComCtrls
  , Vcl.LabeledCtrls
  , Vcl.LabeledExtCtrls
  , Vcl.LabeledDBCtrls
  , Vcl.LabeledMask
  , Vcl.LabeledDbImage
  , Vcl.LabeledCheckLst
  , Vcl.LabeledColorGrd
  {$IFDEF D10_4+}
  , Vcl.LabeledNumberBox
  {$ENDIF}
  , Vcl.LabeledCurrencyEdit
  ;


procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
  Width := Metrics.tmAveCharWidth + 2;
end;

function IsVCLStyled: boolean;
begin
  Result := StyleServices.Enabled and (TStyleManager.ActiveStyle.Name <> 'Windows');
end;

function GetStyledColor(Color: TColor): TColor;
begin
  if IsVCLStyled then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := Color;
end;

procedure EnterEditCtrl(Ctrl: TControl; BoundLabel: TControlBoundLabel);
begin
  DrawBoundLabel(BoundLabel, Ctrl);
end;

procedure ExitEditCtrl( Ctrl: TControl; BoundLabel: TControlBoundLabel);
begin
  DrawBoundLabel(BoundLabel, Ctrl);
end;

procedure RecalculateBounds(var ALeft, ATop, AWidth, AHeight: Integer;
  BoundLabel : TControlBoundLabel; Owner : TControl );
var
  CaptionHeight, CaptionWidth : integer;
begin

  //recalculate coordinates:
  //if owner is aligned to alClient and assigned/visible BoundLabel
  //calculate space in client-area to place the BouldLabel
  if (Owner.Align = alClient) and (BoundLabel <> nil)
    and (BoundLabel.Caption <> '') and (BoundLabel.Visible) then
  begin
    CaptionHeight := (BoundLabel.Height + BoundLabel.LabelSpacing);
    CaptionWidth  := (BoundLabel.Width + BoundLabel.LabelSpacing);
    Case BoundLabel.LabelPosition of
      lpTopLeft, lpTopCenter, lpTopRight :
      begin
        ATop := ATop + CaptionHeight;
        AHeight := AHeight - CaptionHeight;
      end;
      lpBottomLeft, lpBottomCenter, lpBottomRight :
      begin
        AHeight := AHeight - CaptionHeight;
      end;
      lpLeftTop, lpLeftMiddle, lpLeftBottom :
      begin
        ALeft := ALeft + CaptionWidth;
        AWidth := AWidth + CaptionWidth;
      end;
      lpRightTop, lpRightMiddle, lpRightBottom :
      begin
        AWidth := AWidth - CaptionWidth;
      end;
    end;
  end;
end;

procedure SetParentOfLabel(BoundLabel : TControlBoundLabel;
  Parent: TWinControl; Owner : TControl );
begin
  if (BoundLabel = nil) or (Owner = nil) then exit;
  BoundLabel.Parent := Parent;
  BoundLabel.Visible := Owner.Visible;
  BoundLabel.Font.Assign(GetEditControlFont(Owner));
end;

procedure DrawBoundLabel(BoundLabel : TControlBoundLabel; Owner : TControl );
var
  ALeft, ATop, AWidth, AHeight : Integer;
  OLeft, OTop, OWidth, OHeight : Integer;
begin
  if (BoundLabel = nil) or (Owner = nil) or (Owner.Parent = nil) then
    exit;

  //Label outside editor
  BoundLabel.Parent := Owner.Parent;
  BoundLabel.Transparent := True;

  ATop    := BoundLabel.Top;
  ALeft   := BoundLabel.Left;
  AWidth  := BoundLabel.Width;
  AHeight := BoundLabel.Height;
  OTop    := Owner.Top;
  OLeft   := Owner.Left;
  OWidth  := Owner.Width;
  OHeight := Owner.Height;

  //Setting left
  case BoundLabel.LabelPosition of
    lpTopLeft, lpBottomLeft :
      ALeft := OLeft;
    lpTopCenter, lpBottomCenter :
      ALeft := OLeft + (OWidth div 2) - (AWidth div 2);
    lpTopRight, lpBottomRight :
      ALeft := OLeft + OWidth - AWidth;
    lpLeftTop, lpLeftMiddle, lpLeftBottom :
      ALeft := OLeft - BoundLabel.LabelSpacing - AWidth;
    lpRightTop, lpRightMiddle, lpRightBottom :
      ALeft := OLeft + OWidth + BoundLabel.LabelSpacing;
  end;

  //Setting Top
  case BoundLabel.LabelPosition of
    lpTopLeft, lpTopCenter, lpTopRight :
      ATop := OTop - BoundLabel.LabelSpacing - BoundLabel.Height;
    lpBottomLeft, lpBottomCenter, lpBottomRight :
      ATop := OTop + OHeight + BoundLabel.LabelSpacing;
    lpLeftTop, lpRightTop :
      ATop := OTop;
    lpLeftMiddle, lpRightMiddle :
      ATop := OTop + (OHeight div 2) - (AHeight div 2);
    lpLeftBottom, lpRightBottom :
      ATop := OTop + OHeight - AHeight;
  end;

  //Setting justify if label AutoSize is True!
  if BoundLabel.AutoSize then
  begin
    case BoundLabel.LabelPosition of
      lpTopLeft, lpBottomLeft, lpRightTop, lpRightMiddle, lpRightBottom:
        BoundLabel.Alignment := taLeftJustify;
      lpTopCenter, lpBottomCenter:
        BoundLabel.Alignment := taCenter;
      lpTopRight, lpBottomRight, lpLeftTop, lpLeftMiddle, lpLeftBottom:
        BoundLabel.Alignment := taRightJustify;
    end;
  end;

  //if TopOffset is present
  if BoundLabel.LabelPosition in [lpTopLeft,lpTopCenter,lpTopRight] then
    ATop := ATop+BoundLabel.TopOffSet;

  //Positioning component
  BoundLabel.SetBounds(ALeft, ATop, AWidth, AHeight)

end;

procedure ChangeBoundCaption(Value : TCaption; var BoundLabel : TControlBoundLabel ;
  Owner : TControl);
begin
  if (csDestroying in Owner.ComponentState) then exit;
  //Setting Label Caption
  if Value <> BoundLabel.Caption then
  begin
    BoundLabel.Caption := Value;
    DrawBoundLabel(BoundLabel, Owner );
  end;
end;

{ TControlBoundLabel }

procedure TControlBoundLabel.Click;
begin
  inherited;
  if (FOwner is TWinControl) and  TWinControl(FOwner).CanFocus then
    TWinControl(FOwner).SetFocus;
end;

procedure TControlBoundLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;

constructor TControlBoundLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := '';  //no name so, no "auto" caption
  SetSubComponent(True);
  if Assigned(AOwner) then
    FOwner := AOwner as TControl;
  FLabelSpacing := DEFAULT_LABEL_SPACING;
  FLabelPosition := lpTopLeft;
  ControlStyle := ControlStyle - [csOpaque]; //Default transparent True
  Transparent := True;
end;

procedure TControlBoundLabel.SetLabelPosition(const Value: TControlLabelPosition);
begin
  FLabelPosition := Value;
  DrawBoundLabel(self,FOwner);
end;

procedure TControlBoundLabel.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  DrawBoundLabel(self,FOwner);
end;

procedure TControlBoundLabel.SetPosition(APosition: TControlLabelPosition;
  ASpacing: Integer = 1; AVisible: Boolean = True);
begin
  FLabelPosition := APosition;
  FLabelSpacing := ASpacing;
  if Visible <> AVisible then
    Visible := AVisible
  else
    DrawBoundLabel(self, FOwner);
end;

function CalcTextBounds( var X: integer; ClientRect : TRect; const Text : string;
  Font : TFont) : TRect;
var
  DC: HDC;
  Canvas : TCanvas;
  Str : string;
  Flags: Integer;
begin
  Str := 'W'+Text;
  //VCL
  Canvas := TCanvas.Create;
  Try
    Result := ClientRect;
    //recupero il device context
    DC := GetDC(0);
    Canvas.Handle := DC;
    Flags := (DT_EXPANDTABS or DT_CALCRECT or DT_NOPREFIX);
    //Stampo il testo con l'opzione DT_CALCRECT per calcolare automaticamente lo spazio
    Canvas.Font := Font;
    DrawText(Canvas.Handle, PChar(Str), Length(Str), Result, Flags);
    //rilascio il device context
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
  Finally
    Canvas.Free;
  End;
end;


procedure TControlBoundLabel.loaded;
begin
  inherited;
  if csDesigning in ComponentState then //per la VCL non deve farlo a run-time
    DrawBoundLabel(Self, FOwner);
end;

{ TCBStyleAttributes }

function GetEditControlFont(AEditControl: TControl): TFont;
begin
  if AEditControl is TLabeledEditEx then
    Result := TLabeledEditEx(AEditControl).Font
  else if AEditControl is TLabeledDBEdit then
    Result := TLabeledDBEdit(AEditControl).Font
  else if AEditControl is TLabeledMaskEdit then
    Result := TLabeledMaskEdit(AEditControl).Font
  else if AEditControl is TLabeledCheckListBox then
    Result := TLabeledCheckListBox(AEditControl).Font
  else if AEditControl is TLabeledDBListBox then
    Result := TLabeledDBListBox(AEditControl).Font
  else if AEditControl is TLabeledDBRichEdit then
    Result := TLabeledDBRichEdit(AEditControl).Font
  else if AEditControl is TLabeledRichEdit then
    Result := TLabeledRichEdit(AEditControl).Font
  else if AEditControl is TLabeledSpinEdit then
    Result := TLabeledSpinEdit(AEditControl).Font
  else if AEditControl is TLabeledColorGrid then
    Result := TLabeledColorGrid(AEditControl).Font
  else if AEditControl is TLabeledComboBox then
    Result := TLabeledComboBox(AEditControl).Font
  else if AEditControl is TLabeledDBComboBox then
    Result := TLabeledDBComboBox(AEditControl).Font
  else if AEditControl is TLabeledColorBox then
    Result := TLabeledColorBox(AEditControl).Font
  else if AEditControl is TLabeledListBox then
    Result := TLabeledListBox(AEditControl).Font
  else if AEditControl is TLabeledMemo then
    Result := TLabeledMemo(AEditControl).Font
  else if AEditControl is TLabeledDBMemo then
    Result := TLabeledDBMemo(AEditControl).Font
  else if AEditControl is TLabeledDBLookupListBox then
    Result := TLabeledDBLookupListBox(AEditControl).Font
  else if AEditControl is TLabeledDBLookupComboBox then
    Result := TLabeledDBLookupComboBox(AEditControl).Font
  else if AEditControl is TLabeledRadioGroup then
    Result := TLabeledRadioGroup(AEditControl).Font
  else if AEditControl is TLabeledDbGrid then
    Result := TLabeledDbGrid(AEditControl).Font
  else if AEditControl is TLabeledImage then
    Result := TLabeledImage(AEditControl).Font
  else if AEditControl is TLabeledDBImageLink then
    Result := TLabeledDBImageLink(AEditControl).Font
  else if AEditControl is TLabeledCheckListBox then
    Result := TLabeledCheckListBox(AEditControl).Font
  else if AEditControl is TLabeledColorGrid then
    Result := TLabeledColorGrid(AEditControl).Font
  else if AEditControl is TLabeledEditEx then
    Result := TLabeledEditEx(AEditControl).Font
  else if AEditControl is TLabeledDBEdit then
    Result := TLabeledDBEdit(AEditControl).Font
  else if AEditControl is TLabeledDBLabel then
    Result := TLabeledDBLabel(AEditControl).Font
  else if AEditControl is TLabeledDBImage then
    Result := TLabeledDBImage(AEditControl).Font
  {$IFDEF D10_4+}
  else if AEditControl is TLabeledDBNumberBox then
    Result := TLabeledDBNumberBox(AEditControl).Font
  {$ENDIF}
  else if AEditControl is TLabeledCurrencyEdit then
    Result := TLabeledCurrencyEdit(AEditControl).Font
  else if AEditControl is TLabeledDBCurrencyEdit then
    Result := TLabeledDBCurrencyEdit(AEditControl).Font
  else
    raise Exception.CreateFmt('Error GetEditControlFont: Control Class %s not accepted',
      [AEditControl.ClassName]);
end;

procedure SetEditControlLabelPosition(AEditControl: TControl;
  ALabelPosition: TControlLabelPosition; AVisible: Boolean = True);
var
  LSpacing: Integer;
begin
  case ALabelPosition of
    lpLeftTop, lpLeftMiddle, lpLeftBottom,
    lpRightTop, lpRightMiddle, lpRightBottom:
      LSpacing := 3;
  else
    LSpacing := 1;
  end;

  if AEditControl is TLabeledEditEx then
    TLabeledEditEx(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBEdit then
    TLabeledDBEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledMaskEdit then
    TLabeledMaskEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledCheckListBox then
    TLabeledCheckListBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBListBox then
    TLabeledDBListBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBRichEdit then
    TLabeledDBRichEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledRichEdit then
    TLabeledRichEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledSpinEdit then
    TLabeledSpinEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledColorGrid then
    TLabeledColorGrid(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledComboBox then
    TLabeledComboBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBComboBox then
    TLabeledDBComboBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledColorBox then
    TLabeledColorBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledListBox then
    TLabeledListBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledMemo then
    TLabeledMemo(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBMemo then
    TLabeledDBMemo(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBLookupListBox then
    TLabeledDBLookupListBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBLookupComboBox then
    TLabeledDBLookupComboBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledRadioGroup then
    TLabeledRadioGroup(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDbGrid then
    TLabeledDbGrid(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledImage then
    TLabeledImage(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBImageLink then
    TLabeledDBImageLink(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledCheckListBox then
    TLabeledCheckListBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledColorGrid then
    TLabeledColorGrid(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledEditEx then
    TLabeledEditEx(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBEdit then
    TLabeledDBEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBLabel then
    TLabeledDBLabel(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBImage then
    TLabeledDBImage(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  {$IFDEF D10_4+}
  else if AEditControl is TLabeledDBNumberBox then
    TLabeledDBNumberBox(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  {$ENDIF}
  else if AEditControl is TLabeledCurrencyEdit then
    TLabeledCurrencyEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible)
  else if AEditControl is TLabeledDBCurrencyEdit then
    TLabeledDBCurrencyEdit(AEditControl).BoundLabel.SetPosition(ALabelPosition, LSpacing, AVisible);
end;

initialization

finalization

end.

