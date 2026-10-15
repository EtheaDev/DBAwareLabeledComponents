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
unit Vcl.BoundLabel;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics;

Type
  /// <summary>
  ///  Position of the label relative to its bound edit control. Twelve values
  ///  cover the three anchors (start/center/end) on each of the four sides
  ///  (top, bottom, left, right) of the control.
  /// </summary>
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
  /// <summary>
  ///  Label control automatically bound to an edit control. It is created as a
  ///  sub-component of the owner control, positions itself around the owner
  ///  according to LabelPosition and LabelSpacing, and forwards a click to the
  ///  owner to give it focus.
  /// </summary>
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
    /// <summary>
    ///  Vertical offset (in pixels) added to the label Top when it is placed
    ///  above the control (top positions). Used internally for fine positioning.
    /// </summary>
    TopOffSet: integer; //Used internally for positioning
    /// <summary>
    ///  Creates the bound label. The label has no name (to suppress the
    ///  auto-generated caption), is flagged as a sub-component, stores the owner
    ///  control and defaults to a transparent label placed at the top-left.
    /// </summary>
    /// <param name="AOwner">Owner control the label is bound to and positioned around.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets position, spacing and visibility of the label in a single call and
    ///  repositions it around the owner control.
    /// </summary>
    /// <param name="APosition">New label position relative to the owner control.</param>
    /// <param name="ASpacing">Gap in pixels between the label and the owner control.</param>
    /// <param name="AVisible">Whether the label should be visible.</param>
    procedure SetPosition(APosition: TControlLabelPosition;
      ASpacing: Integer = 1; AVisible: Boolean = True);
    /// <summary>
    ///  Handles a click on the label: if the owner is a focusable windowed
    ///  control, focus is moved to it.
    /// </summary>
    procedure Click; override;
    /// <summary>
    ///  The control that receives focus when the label is clicked or its
    ///  accelerator key is pressed.
    /// </summary>
    property FocusControl;
    /// <summary>
    ///  Determines whether the label uses the parent's font.
    /// </summary>
    property ParentFont;
    /// <summary>
    ///  Font used to render the label caption.
    /// </summary>
    property Font;
  published
    /// <summary>
    ///  Background color of the label (effective only when not transparent).
    /// </summary>
    property Color;
    /// <summary>
    ///  When True the label background is transparent. Defaults to True.
    /// </summary>
    property Transparent default True;
    /// <summary>
    ///  Determines whether the label uses the parent's color.
    /// </summary>
    property ParentColor;
    /// <summary>
    ///  When True the caption wraps onto multiple lines.
    /// </summary>
    property WordWrap;
    /// <summary>
    ///  Horizontal alignment of the caption. It is set automatically from
    ///  LabelPosition when AutoSize is True.
    /// </summary>
    property Alignment;
    /// <summary>
    ///  When True the label resizes to fit its caption.
    /// </summary>
    property AutoSize;
    /// <summary>
    ///  Width of the label. Defaults to 0.
    /// </summary>
    property Width default 0;
    /// <summary>
    ///  Position of the label relative to the owner control. Changing it
    ///  repositions the label. Defaults to lpTopLeft.
    /// </summary>
    property LabelPosition: TControlLabelPosition read FLabelPosition write SetLabelPosition default DEFAULT_LABEL_POSITION;
    /// <summary>
    ///  Gap in pixels between the label and the owner control. Changing it
    ///  repositions the label. Defaults to 1.
    /// </summary>
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default DEFAULT_LABEL_SPACING;
  end;

/// <summary>
///  Returns True when a VCL style other than the native 'Windows' style is
///  currently active.
/// </summary>
/// <returns>True if a custom VCL style is enabled and active.</returns>
function IsVCLStyled: boolean;
/// <summary>
///  Maps a color through the active VCL style. When a style is active the
///  matching styled system color is returned; otherwise the input color is
///  returned unchanged.
/// </summary>
/// <param name="Color">Color (often a system color) to be resolved.</param>
/// <returns>The styled color when a VCL style is active, else the original color.</returns>
function GetStyledColor(Color: TColor): TColor;
/// <summary>
///  Called when the bound edit control gains focus; redraws/repositions the
///  associated label.
/// </summary>
/// <param name="Ctrl">The edit control that received focus.</param>
/// <param name="BoundLabel">The label bound to the control.</param>
procedure EnterEditCtrl( Ctrl: TControl; BoundLabel: TControlBoundLabel);
/// <summary>
///  Called when the bound edit control loses focus; redraws/repositions the
///  associated label.
/// </summary>
/// <param name="Ctrl">The edit control that lost focus.</param>
/// <param name="BoundLabel">The label bound to the control.</param>
procedure ExitEditCtrl( Ctrl: TControl; BoundLabel: TControlBoundLabel);
/// <summary>
///  Adjusts the owner control bounds to reserve space for the label when the
///  owner is aligned to alClient and the label is visible with a caption. The
///  bounds are shifted/shrunk according to the label position (top, bottom,
///  left or right).
/// </summary>
/// <param name="ALeft">Left coordinate, adjusted in place.</param>
/// <param name="ATop">Top coordinate, adjusted in place.</param>
/// <param name="AWidth">Width, adjusted in place.</param>
/// <param name="AHeight">Height, adjusted in place.</param>
/// <param name="BoundLabel">The label whose size determines the reserved space.</param>
/// <param name="Owner">The owner control being repositioned.</param>
procedure RecalculateBounds(var ALeft, ATop, AWidth, AHeight: Integer;
  BoundLabel : TControlBoundLabel; Owner : TControl );
/// <summary>
///  Positions the label around the owner control based on its LabelPosition and
///  LabelSpacing. It reparents the label to the owner's parent, computes Left
///  and Top for the chosen side/anchor, sets the caption alignment when AutoSize
///  is True, applies TopOffSet for top positions and finally sets the bounds.
/// </summary>
/// <param name="BoundLabel">The label to position.</param>
/// <param name="Owner">The owner control the label is placed around.</param>
procedure DrawBoundLabel(BoundLabel : TControlBoundLabel; Owner : TControl );
/// <summary>
///  Assigns a new caption to the label and repositions it. Does nothing while
///  the owner is being destroyed or when the caption is unchanged.
/// </summary>
/// <param name="Value">The new caption text.</param>
/// <param name="BoundLabel">The label whose caption is updated.</param>
/// <param name="Owner">The owner control the label is bound to.</param>
procedure ChangeBoundCaption(Value : TCaption; var BoundLabel : TControlBoundLabel ;
  Owner : TControl);
/// <summary>
///  Sets the parent of the label, syncs its visibility with the owner and
///  copies the owner edit control font. Does nothing if the label or owner is nil.
/// </summary>
/// <param name="BoundLabel">The label to reparent.</param>
/// <param name="Parent">The new parent windowed control.</param>
/// <param name="Owner">The owner control whose visibility and font are applied.</param>
procedure SetParentOfLabel(BoundLabel : TControlBoundLabel;
  Parent: TWinControl; Owner : TControl );
/// <summary>
///  Computes the bounding rectangle required to draw the given text with the
///  specified font, using DrawText with DT_CALCRECT. A leading 'W' is prepended
///  to the text to add padding.
/// </summary>
/// <param name="X">Horizontal coordinate parameter (passed by reference).</param>
/// <param name="ClientRect">Starting rectangle used as the calculation bounds.</param>
/// <param name="Text">The text to measure.</param>
/// <param name="Font">The font used to measure the text.</param>
/// <returns>The rectangle needed to render the text.</returns>
function CalcTextBounds( var X: integer; ClientRect : TRect; const Text : string;
  Font : TFont) : TRect;

/// <summary>
///  Computes the standard width and height of an edit control based on its font
///  and 3D border style, following the same metrics logic used by the VCL.
/// </summary>
/// <param name="Font">Font used to measure the text metrics.</param>
/// <param name="Ctl3D">Whether the control uses a 3D border (affects the border height).</param>
/// <param name="Width">Receives the computed standard width.</param>
/// <param name="Height">Receives the computed standard height.</param>
//Computes the standard height of an edit control based on its font and 3D style
procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);

/// <summary>
///  Returns the Font of a supported labeled edit control. Raises an exception
///  if the control class is not one of the recognized labeled component types.
/// </summary>
/// <param name="AEditControl">The labeled edit control to inspect.</param>
/// <returns>The font of the recognized labeled control.</returns>
function GetEditControlFont(AEditControl: TControl): TFont;
/// <summary>
///  Sets the label position (and visibility) of a supported labeled edit
///  control. Side positions (left/right) use a spacing of 3 pixels, all others
///  use 1 pixel.
/// </summary>
/// <param name="AEditControl">The labeled edit control whose label is repositioned.</param>
/// <param name="ALabelPosition">The desired label position.</param>
/// <param name="AVisible">Whether the label should be visible.</param>
procedure SetEditControlLabelPosition(AEditControl: TControl;
  ALabelPosition: TControlLabelPosition; AVisible: Boolean = True);

implementation

uses
  Vcl.Mask,
  Vcl.Themes,
  Vcl.LabeledComCtrls,
  Vcl.LabeledCtrls,
  Vcl.LabeledExtCtrls,
  Vcl.LabeledDBCtrls,
  Vcl.LabeledMask,
  Vcl.LabeledDbImage,
  Vcl.LabeledCheckLst,
  Vcl.LabeledColorGrd,
  {$IFDEF D10_4+}
  Vcl.LabeledNumberBox,
  {$ENDIF}
  Vcl.LabeledCurrencyEdit;


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
    //get the device context
    DC := GetDC(0);
    Canvas.Handle := DC;
    Flags := (DT_EXPANDTABS or DT_CALCRECT or DT_NOPREFIX);
    //draw the text with the DT_CALCRECT option to compute the required space automatically
    Canvas.Font := Font;
    DrawText(Canvas.Handle, PChar(Str), Length(Str), Result, Flags);
    //release the device context
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
  Finally
    Canvas.Free;
  End;
end;


procedure TControlBoundLabel.loaded;
begin
  inherited;
  if csDesigning in ComponentState then //in the VCL this must not be done at run-time
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
  else if AEditControl is TLabeledNumberBox then
    Result := TLabeledNumberBox(AEditControl).Font
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

