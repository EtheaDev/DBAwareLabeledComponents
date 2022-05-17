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
unit Vcl.LabeledButtonEdit;

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
  , Vcl.LabeledDBCtrls
  , Vcl.LabeledMask
  , Vcl.DbAwareLabeledConsts
  ;

Type
  /// <summary>
  /// Internal class used to implement custom VCL Style support for the TCBXButtonEdit and TCBXDBButtonEdit.
  /// </summary>
  TLabeledButtonEditStyleHook = class(TEditStyleHook)
  strict private
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
  strict protected
    procedure PaintNC(Canvas: TCanvas); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TButtonEditStyle = (besEllipsis, besDate, besDateHHMM, besDateHHMMSS, besTime, besTimeHHMMSS, besFind, besCalc, besFolder, besFile, besColor);

  { TLabeledButtonEdit }
  TLabeledButtonEdit = class(TLabeledMaskEdit)
  strict private
    class constructor Create;
    class destructor Destroy;
  private
    FButtonEditStyle: TButtonEditStyle;
    FTextIndicatorImages: TImageList;
    FCalendarIndicatorImages: TImageList;
    FButtonWidth: Integer;
    FButtonRect: TRect;
    FMouseOverButton: Boolean;
    FButtonDown: Boolean;
    FOnButtonEditClick: TNotifyEvent;
    FShortCut: TShortCut;
    FCollapsed: boolean;
    FBeforeDialogExecute: TNotifyEvent;

    procedure UpdateEditMask;
    Procedure ClearEditMask;
    function CollapsedWidth : integer;
    procedure SetCollapsed(const Value: boolean);

    // Property Access Methods
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonEditStyle(Value: TButtonEditStyle);

    // Message Handling Methods
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCHitTest(var Msg: TMessage); message WM_NCHITTEST;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Msg: TMessage); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Msg: TMessage); message WM_RBUTTONDOWN;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;

    function GetDateTimeValue: TDateTime;
    procedure SetDateTimeValue(const Value: TDateTime);
    function GetIsEmpty: Boolean;
  protected
    /// <summary>Protected field providing access to the canvas upon which the search indicator button is drawn.</summary>
    FCanvas: TCanvas;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;

    /// <summary>RepaintButton sends a WM_NCPAINT message to the control to instruct the nonclient area of the
    /// control to be repainted. The search indicator is positioned in the nonclient area of the control.</summary>
    procedure RepaintButton;
    /// <summary>DrawButton is used to display the search indicator button.</summary>
    procedure DrawButton(const Canvas: TCanvas); virtual;
    /// <summary>MouseCancel is used to release mouse capturing when the control loses keyboard focus.</summary>
    procedure MouseCancel;

    // Event Dispatch Methods
    /// <summary>Event dispatch method used to generate the OnButtonEditClick event.</summary>
    procedure ButtonEditClick; dynamic;
    procedure KeyPress(var Key: Char); override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    /// <summary>Property providing access to the internal FCanvas field. Used for drawing search indicator.</summary>
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isDateTimeInput : boolean;
    function isColorInput : boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty stored False;
    /// <summary>Specifies the width of the search indicator button.</summary>
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default DEFAULT_BUTTON_WIDTH;
    /// <summary>Specifies the type of indicator icon to display.
    /// besEllipsis - Magnifier icon to indicate text based searching
    /// sbiAudio - Microphone icon to indicate audio based searching</summary>
    property ButtonEditStyle: TButtonEditStyle read FButtonEditStyle write SetButtonEditStyle default besEllipsis;
    /// <summary>This event occurs when the search indicator button is clicked. When ButtonEditStyle is set to besEllipsis,
    /// the OnButtonEditClick event also occurs when the Enter key is pressed.</summary>
    property OnButtonEditClick: TNotifyEvent read FOnButtonEditClick write FOnButtonEditClick;
    property ShortCut: TShortCut read FShortCut write FShortCut default VK_F3;
    property Collapsed: boolean read FCollapsed write SetCollapsed default False;
    property BeforeDialogExecute : TNotifyEvent read FBeforeDialogExecute write FBeforeDialogExecute;
    property DateTimeValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
  end;

  { TLabeledDBButtonEdit }
  TLabeledDBButtonEdit = class(TLabeledDBEdit)
  strict private
    class constructor Create;
    class destructor Destroy;
  private
    FButtonEditStyle: TButtonEditStyle;
    FTextIndicatorImages: TImageList;
    FCalendarIndicatorImages: TImageList;
    FButtonWidth: Integer;
    FButtonRect: TRect;
    FMouseOverButton: Boolean;
    FButtonDown: Boolean;
    FOnButtonEditClick: TNotifyEvent;
    FShortCut: TShortCut;
    FCollapsed: boolean;
    FBeforeDialogExecute: TNotifyEvent;

    //Data field access
    function GetDataField: string;
    procedure SetDataField(const Value: string);

    function CollapsedWidth : integer;
    procedure SetCollapsed(const Value: boolean);

    // Property Access Methods
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonEditStyle(Value: TButtonEditStyle);

    // Message Handling Methods
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCHitTest(var Msg: TMessage); message WM_NCHITTEST;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Msg: TMessage); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var Msg: TMessage); message WM_RBUTTONDOWN;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;

    function GetDateTimeValue : TDateTime;
    procedure SetDateTimeValue(const Value: TDateTime);
    function GetIsEmpty: Boolean;
  protected
    /// <summary>Protected field providing access to the canvas upon which the search indicator button is drawn.</summary>
    FCanvas: TCanvas;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;

    /// <summary>RepaintButton sends a WM_NCPAINT message to the control to instruct the nonclient area of the
    /// control to be repainted. The search indicator is positioned in the nonclient area of the control.</summary>
    procedure RepaintButton;
    /// <summary>DrawButton is used to display the search indicator button.</summary>
    procedure DrawButton(const Canvas: TCanvas); virtual;
    /// <summary>MouseCancel is used to release mouse capturing when the control loses keyboard focus.</summary>
    procedure MouseCancel;

    // Event Dispatch Methods
    /// <summary>Event dispatch method used to generate the OnButtonEditClick event.</summary>
    procedure ButtonEditClick; Dynamic;

    procedure KeyPress(var Key: Char); override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    /// <summary>Property providing access to the internal FCanvas field. Used for drawing search indicator.</summary>
    property Canvas: TCanvas read FCanvas;
  public
    function CanChangeData : boolean;
    procedure SetFieldValue(const Value: Variant);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isDateTimeInput : boolean;
    function isColorInput : boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty stored False;
    /// <summary>Specifies the width of the search indicator button.</summary>
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default DEFAULT_BUTTON_WIDTH;
    /// <summary>Specifies the type of indicator icon to display.
    /// besEllipsis - Magnifier icon to indicate text based searching
    /// sbiAudio - Microphone icon to indicate audio based searching</summary>
    property ButtonEditStyle: TButtonEditStyle read FButtonEditStyle write SetButtonEditStyle default besEllipsis;
    /// <summary>This event occurs when the search indicator button is clicked. When ButtonEditStyle is set to besEllipsis,
    /// the OnButtonEditClick event also occurs when the Enter key is pressed.</summary>
    property OnButtonEditClick: TNotifyEvent read FOnButtonEditClick write FOnButtonEditClick;
    property ShortCut: TShortCut read FShortCut write FShortCut default VK_F3;
    property DataField: string read GetDataField write SetDataField;
    property Collapsed: boolean read FCollapsed write SetCollapsed default False;
    property BeforeDialogExecute : TNotifyEvent read FBeforeDialogExecute write FBeforeDialogExecute;
    property DateTimeValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
  end;

{$R VCLButtonEdit.res}   //utilizza il file VCLButtonEdit.res con le immagini per i bottoni

implementation

uses
  PngImage
  , Vcl.FileCtrl
  , Vcl.Themes
  , Vcl.LabeledShellUtils
  , Vcl.DbAwareLabeledUtils
  , Vcl.LabeledGraphicUtils
  , Vcl.ImgList
  ;

procedure SetEditRect(Handle : THandle; ButtonWidth : integer );
begin
  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, ButtonWidth));
end;

procedure LoadImagesIntoControl(const AWinControl: TWinControl;
  const M, D: Integer);
var
  Png: TPngImage;
  Bmp: TBitmap;
  LScale: Double;
  LTextIndicatorImages: TImageList;
  LCalendarIndicatorImages: TImageList;
begin
  LScale := M / D;
  if AWinControl is TLabeledButtonEdit then
  begin
    LTextIndicatorImages := TLabeledButtonEdit(AWinControl).FTextIndicatorImages;
    LCalendarIndicatorImages := TLabeledButtonEdit(AWinControl).FCalendarIndicatorImages;
  end
  else
  begin
    LTextIndicatorImages := TLabeledDBButtonEdit(AWinControl).FTextIndicatorImages;
    LCalendarIndicatorImages := TLabeledDBButtonEdit(AWinControl).FCalendarIndicatorImages;
  end;
  Png := TPngImage.Create;
  try
    Bmp := TBitmap.Create;
    try
      Png.LoadFromResourceName(HInstance, 'CBXBUTTONEDIT_CBSEARCH');
      LTextIndicatorImages.Width := Round((Png.Width div 4) * LScale);
      LTextIndicatorImages.Height := Round(Png.Height * LScale);
      Bmp.Assign(Png);
      ResizeBitmap(Bmp, Round(Bmp.Width*LScale), Round(Bmp.Height*LScale));
      LTextIndicatorImages.Add(Bmp, nil);

      Png.LoadFromResourceName(HInstance, 'CBXBUTTONEDIT_CBCALENDAR');
      LCalendarIndicatorImages.Width := Round((Png.Width div 4) * LScale);
      LCalendarIndicatorImages.Height := Round(Png.Height * LScale);
      Bmp.Assign(Png);
      ResizeBitmap(Bmp, Round(Bmp.Width*LScale), Round(Bmp.Height*LScale));
      LCalendarIndicatorImages.Add(Bmp, nil);

      //Esempio caricamento bitmap con colore fuchsia trasparente
(*
      Bmp.Transparent := True;
      Bmp.TransparentColor := clFuchsia;
      Bmp.LoadFromResourceName(HInstance, 'CBXBUTTONEDIT_CBAUDIO');
      LCalendarIndicatorImages.Width := Bmp.Width;
      LCalendarIndicatorImages.Height := Bmp.Height;
      LCalendarIndicatorImages.Add(Bmp, nil);
*)
    finally
      Bmp.Free;
    end;
  finally
    Png.Free;
  end;
end;

procedure SelectFileByDialog(Sender : TObject; BeforeDialogExecute : TNotifyEvent;
  const Title, DialogFilter: string);
var
  FileName : string;
  RealizedPath, Path : string;
begin
  if Sender is TLabeledButtonEdit then
    FileName := TLabeledButtonEdit(Sender).Text
  else if Sender is TLabeledDBButtonEdit then
    FileName := TLabeledDBButtonEdit(Sender).Text
  else
    Exit;

  //Trasformo il FileName modificando le cartelle di sistema (es.{app}\pippo.txt)
  Path := ExtractFilePath(FileName); //estraggo {app}
  RealizedPath := RealizeAliasPath(Application.Handle, Path); //trasformo {app} nella cartella reale
  FileName := RealizedPath+ExtractFileName(FileName); //modifico il nome del file con la cartella reale
  if not OpenDialogExecute(FileName, Title, DialogFilter, BeforeDialogExecute) then
    Exit;

  //Provo a convertire la path del file selezionato in path di sistema
  Path := ExtractFilePath(FileName);
  RealizeSystemPath(Application.Handle, Path);
  FileName := Path+ExtractFileName(FileName);

  if Sender is TLabeledButtonEdit then
    TLabeledButtonEdit(Sender).Text := FileName
  else if Sender is TLabeledDBButtonEdit then
    TLabeledDBButtonEdit(Sender).SetFieldValue(FileName);
end;

procedure SelectColorByDialog(Sender : TObject; BeforeDialogExecute : TNotifyEvent);
var
  Color : TColor;
begin
  if Sender is TLabeledButtonEdit then
    Color := HexToColor(TLabeledButtonEdit(Sender).Text)
  else if Sender is TLabeledDBButtonEdit then
    Color := HexToColor(TLabeledDBButtonEdit(Sender).Text)
  else
    Exit;

  if not ColorDialogExecute(Color, '', GetCustomColors, BeforeDialogExecute) then
    Exit;

  if Sender is TLabeledButtonEdit then
  begin
    TLabeledButtonEdit(Sender).Text := ColorToHex(Color);
    //TLabeledButtonEdit(Sender).Invalidate;
  end
  else if Sender is TLabeledDBButtonEdit then
  begin
    TLabeledDBButtonEdit(Sender).SetFieldValue(Color);
    //TLabeledButtonEdit(Sender).Invalidate;
  end;
end;

procedure SelectDirByDialog(Sender : TObject);
var
  Path : string;
begin
  if Sender is TLabeledButtonEdit then
    Path := TLabeledButtonEdit(Sender).Text
  else if Sender is TLabeledDBButtonEdit then
    Path := TLabeledDBButtonEdit(Sender).Text
  else
    Exit;

  //Trasformo il Path modificando le cartelle di sistema (es.{app}\)
  Path := RealizeAliasPath(Application.Handle, Path); //trasformo {app} nella cartella reale
  if not SelectDirectory('', '', Path, [sdShowEdit, sdShowShares, sdValidateDir, sdNewUI]) then
    Exit;

  //Provo a convertire la path del file selezionato in path di sistema
  RealizeSystemPath(Application.Handle, Path);

  if Sender is TLabeledButtonEdit then
    TLabeledButtonEdit(Sender).Text := Path
  else if Sender is TLabeledDBButtonEdit then
    TLabeledDBButtonEdit(Sender).SetFieldValue(Path);
end;

procedure DrawButtonIntoControl(const AWinControl: TWinControl; const ACanvas: TCanvas;
  ACurrentPPI: Integer);
var
  ImageIndex: Integer;
  LColor: TColor;
  LButtonEditStyle: TButtonEditStyle;
  LButtonDown: Boolean;
  LButtonRect: TRect;
  LMouseOverButton: Boolean;
  LEnabled: Boolean;
  LTextIndicatorImages: TImageList;
  LCalendarIndicatorImages: TImageList;
  ElementDetails: TThemedElementDetails;
begin
  if AWinControl is TLabeledButtonEdit then
  begin
    LButtonEditStyle := TLabeledButtonEdit(AWinControl).ButtonEditStyle;
    LButtonDown := TLabeledButtonEdit(AWinControl).FButtonDown;
    LButtonRect := TLabeledButtonEdit(AWinControl).FButtonRect;
    LMouseOverButton := TLabeledButtonEdit(AWinControl).FMouseOverButton;
    LEnabled := TLabeledButtonEdit(AWinControl).Enabled;
    LTextIndicatorImages := TLabeledButtonEdit(AWinControl).FTextIndicatorImages;
    LCalendarIndicatorImages := TLabeledButtonEdit(AWinControl).FCalendarIndicatorImages;
  end
  else
  begin
    LButtonEditStyle := TLabeledDBButtonEdit(AWinControl).ButtonEditStyle;
    LButtonDown := TLabeledDBButtonEdit(AWinControl).FButtonDown;
    LButtonRect := TLabeledDBButtonEdit(AWinControl).FButtonRect;
    LMouseOverButton:= TLabeledDBButtonEdit(AWinControl).FMouseOverButton;
    LEnabled := TLabeledDBButtonEdit(AWinControl).Enabled;
    LTextIndicatorImages := TLabeledDBButtonEdit(AWinControl).FTextIndicatorImages;
    LCalendarIndicatorImages := TLabeledDBButtonEdit(AWinControl).FCalendarIndicatorImages;
  end;

  if TStyleManager.IsCustomStyleActive then
  begin
    case LButtonEditStyle of
      besEllipsis, besFind, besCalc, besFolder, besFile, besColor:
        begin
          if not AWinControl.Enabled then
            ElementDetails := StyleServices.GetElementDetails(tsiTextDisabled)
          else if LButtonDown then
            ElementDetails := StyleServices.GetElementDetails(tsiTextPressed)
          else if LMouseOverButton then
            ElementDetails := StyleServices.GetElementDetails(tsiTextHot)
          else
            ElementDetails := StyleServices.GetElementDetails(tsiTextNormal);

          StyleServices.DrawElement(ACanvas.Handle, ElementDetails, LButtonRect,
            LButtonRect {$IF CompilerVersion > 32}, ACurrentPPI{$IFEND});
        end;
      besDate, besDateHHMM, besDateHHMMSS, besTime, besTimeHHMMSS:
        begin
          if not AWinControl.Enabled then
            ElementDetails := StyleServices.GetElementDetails(tcDropDownButtonDisabled)
          else if LButtonDown then
            ElementDetails := StyleServices.GetElementDetails(tcDropDownButtonPressed)
          else if LMouseOverButton then
            ElementDetails := StyleServices.GetElementDetails(tcDropDownButtonHot)
          else
            ElementDetails := StyleServices.GetElementDetails(tcDropDownButtonNormal);

          StyleServices.DrawElement(ACanvas.Handle, ElementDetails, LButtonRect,
            LButtonRect {$IF CompilerVersion > 32}, ACurrentPPI{$IFEND});
        end;
    end;
  end
  else // No Styles
  begin
    //Disegno il pulsante
    if not LEnabled then
    begin
      ImageIndex := 3;
      LColor := TStyleManager.ActiveStyle.GetStyleColor(scButtonDisabled);
    end
    else if LButtonDown then
    begin
      ImageIndex := 2;
      LColor := TStyleManager.ActiveStyle.GetStyleColor(scButtonPressed);
    end
    else if LMouseOverButton then
    begin
      ImageIndex := 1;
      LColor := TStyleManager.ActiveStyle.GetStyleColor(scButtonDisabled);
    end
    else
    begin
      ImageIndex := 0;
      LColor := TStyleManager.ActiveStyle.GetStyleColor(scEdit)
    end;
    ACanvas.Brush.Color := LColor;

    ACanvas.FillRect(LButtonRect);

    case LButtonEditStyle of
      besEllipsis, besFind, besCalc, besFolder, besFile, besColor:
      //besDate, besDateHHMM, besDateHHMMSS, besTime, besTimeHHMMSS:
      begin
        LTextIndicatorImages.Draw(ACanvas, LButtonRect.Left, LButtonRect.Top, ImageIndex);
      end;
      besDate, besDateHHMM, besDateHHMMSS, besTime, besTimeHHMMSS:
      begin
        LCalendarIndicatorImages.Draw(ACanvas, LButtonRect.Left, LButtonRect.Top, 0);
      end;
    end;
  end;
end;

procedure DoPaintButton(const AWinControl: TWinControl;
  var AButtonRect: TRect; var Msg: TWMNCPaint;
  ACurrentPPI: Integer);
var
  DC: HDC;
  LOffset: Integer;
  LCanvas: TCanvas;
  LButtonWidth: Integer;
  LBorderStyle: TBorderStyle;
  LCtl3D: Boolean;
begin
  if AWinControl is TLabeledButtonEdit then
  begin
    LButtonWidth := TLabeledButtonEdit(AWinControl).FButtonWidth;
    LCanvas := TLabeledButtonEdit(AWinControl).FCanvas;
    LBorderStyle := TLabeledButtonEdit(AWinControl).BorderStyle;
    LCtl3D := TLabeledButtonEdit(AWinControl).Ctl3D;
  end
  else
  begin
    LButtonWidth := TLabeledDBButtonEdit(AWinControl).FButtonWidth;
    LCanvas := TLabeledDBButtonEdit(AWinControl).FCanvas;
    LBorderStyle := TLabeledDBButtonEdit(AWinControl).BorderStyle;
    LCtl3D := TLabeledDBButtonEdit(AWinControl).Ctl3D;
  end;

  LOffset := 2;
  if LBorderStyle = bsNone then
    Dec(LOffset);
  if LCtl3D then
    Dec(LOffset);

  DC := GetWindowDC(AWinControl.Handle);
  LCanvas.Handle := DC;
  try
    GetWindowRect(AWinControl.Handle, AButtonRect);
    OffsetRect(AButtonRect, -AButtonRect.Left, -AButtonRect.Top);

    InflateRect(AButtonRect, -LOffset, -LOffset);
    if not AWinControl.UseRightToLeftAlignment then
      AButtonRect.Left := AButtonRect.Right - LButtonWidth
    else
      AButtonRect.Right := AButtonRect.Left + LButtonWidth;
    IntersectClipRect(LCanvas.Handle, AButtonRect.Left, AButtonRect.Top, AButtonRect.Right, AButtonRect.Bottom);

    DrawButtonIntoControl(AWinControl, LCanvas, ACurrentPPI);
    Msg.Result := 0;
  finally
    LCanvas.Handle := 0;
    ReleaseDC(AWinControl.Handle, DC);
  end;
end;

{ TLabeledButtonEditStyleHook Methods }

constructor TLabeledButtonEditStyleHook.Create(AControl: TWinControl);
begin
  inherited;
end;

procedure TLabeledButtonEditStyleHook.WMNCCalcSize(var Msg: TWMNCCalcSize);
var
  W: Integer;
begin
  Handled := False;
  if (Control is TLabeledButtonEdit) then
    W := TLabeledButtonEdit(Control).ButtonWidth
  else if (Control is TLabeledDBButtonEdit) then
    W := TLabeledDBButtonEdit(Control).ButtonWidth
  else
    Exit;

  if not Control.UseRightToLeftAlignment then
    Dec(Msg.CalcSize_Params^.rgrc[0].Right, W)
  else
    Inc(Msg.CalcSize_Params^.rgrc[0].Left, W);

  InflateRect(Msg.CalcSize_Params^.rgrc[0], -2, -2);
  Handled := True;
end;

procedure TLabeledButtonEditStyleHook.PaintNC(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  ControlRect, EditRect, BtnRect: TRect;
  BtnWidth: Integer;
begin
  if StyleServices.Available then
  begin
    // Draw border of control
    if Control.Focused then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollFocused)
    else if MouseInControl then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollHot)
    else if Control.Enabled then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollNormal)
    else
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);

    ControlRect := Rect(0, 0, Control.Width, Control.Height);

    EditRect := ControlRect;
    InflateRect(EditRect, -2, -2);
    if Control is TLabeledButtonEdit then
      BtnWidth := TLabeledButtonEdit(Control).ButtonWidth
    else
      BtnWidth := TLabeledDBButtonEdit(Control).ButtonWidth;

    if not Control.UseRightToLeftAlignment then
      Dec(EditRect.Right, BtnWidth)
    else
      Inc(EditRect.Left, BtnWidth);

    // Exclude the editing area
    ExcludeClipRect(Canvas.Handle, EditRect.Left, EditRect.Top, EditRect.Right, EditRect.Bottom);

    StyleServices.DrawElement(Canvas.Handle, Details, ControlRect, ControlRect
      {$IF CompilerVersion > 32}, TLabeledButtonEdit(Control).FCurrentPPI{$IFEND});

    // Draw the button
    BtnRect := ControlRect;
    InflateRect(BtnRect, -2, -2);

    if not Control.UseRightToLeftAlignment then
      BtnRect.Left := BtnRect.Right - BtnWidth
    else
      BtnRect.Right := BtnRect.Left + BtnWidth;
    IntersectClipRect(Canvas.Handle, BtnRect.Left, BtnRect.Top, BtnRect.Right, BtnRect.Bottom);

    if Control is TLabeledButtonEdit then
    begin
      TLabeledButtonEdit(Control).FButtonRect := BtnRect;
      TLabeledButtonEdit(Control).DrawButton(Canvas);
    end
    else
    begin
      TLabeledDBButtonEdit(Control).FButtonRect := BtnRect;
      TLabeledDBButtonEdit(Control).DrawButton(Canvas);
    end;
  end;
end;

{ TLabeledButtonEdit }
procedure TLabeledButtonEdit.ButtonEditClick;
begin
  //Se il controllo non è enabled non lancia l'onclick
  if not Enabled or ReadOnly then exit;
  if Assigned(FOnButtonEditClick) then
    FOnButtonEditClick(Self) else
  begin
    if ButtonEditStyle in [besDate, besDateHHMM, besDateHHMMSS] then
    begin
      //crea la form di scelta della data
      //CalendarViewPopUp(self, ButtonEditStyle); TODO
    end
    else if ButtonEditStyle in [besTime, besTimeHHMMSS] then
    begin
      //crea la form di scelta dell'ora
      //ClockPopUp(self, ButtonEditStyle); //TODO
    end
    else if ButtonEditStyle in [besFile] then
      SelectFileByDialog(self, BeforeDialogExecute, DEFAULT_DIALOG_TITLE, ALL_FILES_FILTER)
    else if ButtonEditStyle in [besFolder] then
      SelectDirByDialog(self)
    else if ButtonEditStyle in [besColor] then
      SelectColorByDialog(self, BeforeDialogExecute);
  end;
end;

class constructor TLabeledButtonEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TLabeledButtonEdit, TLabeledButtonEditStyleHook);
end;

class destructor TLabeledButtonEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TLabeledButtonEdit, TLabeledButtonEditStyleHook);
end;

constructor TLabeledButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FTextIndicatorImages := TImageList.Create(nil);
  FTextIndicatorImages.ColorDepth := cd32Bit;
  FTextIndicatorImages.DrawingStyle := dsTransparent;

  FCalendarIndicatorImages := TImageList.Create(nil);
  FCalendarIndicatorImages.ColorDepth := cd32Bit;
  FCalendarIndicatorImages.DrawingStyle := dsTransparent;

  LoadImagesIntoControl(Self,1,1);

  ButtonEditStyle := besEllipsis;
  FButtonWidth := DEFAULT_BUTTON_WIDTH;
  FShortCut := VK_F3;
end;

destructor TLabeledButtonEdit.Destroy;
begin
  FCanvas.Free;
  FTextIndicatorImages.Free;
  FCalendarIndicatorImages.Free;
  inherited;
end;

procedure TLabeledButtonEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DOWN) and (Shift = [ssAlt])) or //Alt-down
    (Key = FShortCut) or  //Shortcut definito
    ((Key = VK_SPACE) and Collapsed) then //Spazio se collapsed
    ButtonEditClick
  else if ((Key = VK_DELETE) or (Key = VK_BACK)) and (Collapsed) then
    Text := ''
  else
    inherited KeyDown(Key, Shift);
end;

procedure TLabeledButtonEdit.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FButtonWidth := MulDiv(ButtonWidth, M, D);
  LoadImagesIntoControl(Self,M,D);
  inherited;
end;

procedure TLabeledButtonEdit.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    RecreateWnd;
  end;
end;

procedure TLabeledButtonEdit.SetButtonEditStyle(Value: TButtonEditStyle);
begin
  if FButtonEditStyle <> Value then
  begin
    FButtonEditStyle := Value;
    RecreateWnd;
    UpdateEditMask;
  end;
end;

procedure TLabeledButtonEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  RepaintButton;
end;

procedure TLabeledButtonEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOverButton := False;
end;

procedure TLabeledButtonEdit.WMNCPaint(var Msg: TWMNCPaint);
begin
  inherited;
  DoPaintButton(Self, FButtonRect, Msg, FCurrentPPI);
end;

procedure TLabeledButtonEdit.MouseCancel;
begin
  if GetCapture = Handle then
    ReleaseCapture;

  FButtonDown := False;
  RepaintButton;
end;

procedure TLabeledButtonEdit.KeyPress(var Key: Char);
begin
  inherited;
  if (Ord(Key) = vk_Return) and (FButtonEditStyle = besEllipsis) then
  begin
    Key := #0;
    ButtonEditClick;
  end;
end;

procedure TLabeledButtonEdit.WMNCHitTest(var Msg: TMessage);
begin
  inherited;

  if Msg.Result = WinApi.Windows.HTNOWHERE then
  begin
    FMouseOverButton := True;
    Msg.Result := HTCLIENT;
  end
  else
    FMouseOverButton := False;
end;

procedure TLabeledButtonEdit.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  if not UseRightToLeftAlignment then
    Dec(Msg.CalcSize_Params^.rgrc[0].Right, FButtonWidth)
  else
    Inc(Msg.CalcSize_Params^.rgrc[0].Left, FButtonWidth);
  inherited;
end;

procedure TLabeledButtonEdit.RepaintButton;
begin
  if HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TLabeledButtonEdit.DrawButton(const Canvas: TCanvas);
begin
  DrawButtonIntoControl(Self, Canvas, FCurrentPPI);
end;

procedure TLabeledButtonEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  MouseCancel;
end;

procedure TLabeledButtonEdit.WMLButtonDown(var Msg: TMessage);
begin
  if FMouseOverButton then
  begin
    if not Focused then
      SetFocus;
    FButtonDown := True;
    RepaintButton;
    SetCapture(Handle);
    Msg.Result := 0;
  end
  else
  begin
    inherited;
    if not Focused then
      MouseCancel;
  end;
end;

procedure TLabeledButtonEdit.DoExit;
begin
  //Se la data inputata è "vuota" sbianco il controllo di edit
  if isDateTimeInput then
  begin
    if isEmpty then
    begin
      Text := '';
      ClearEditMask;
    end
    else
    begin
      Try
        case FButtonEditStyle of
          besDate       : Text := FormatDateTime(DATE_FORMAT,StrToDateTime(Text));
          besTime       : Text := Copy(FormatDateTime(TIME_HH_MM_SS_FORMAT,StrToDateTime(Text)),1,5);
          besDateHHMM   : Text := FormatDateTime(DATE_TIME_HH_MM_FORMAT,StrToDateTime(Text));
          besDateHHMMSS : Text := FormatDateTime(DATE_TIME_HH_MM_SS_FORMAT,StrToDateTime(Text));
        end;
      Except
        SetFocus;
        raise;
      End;
    end;
  end;
  inherited;
  ExitEditCtrl(Self, BoundLabel);
//  //Invalidate;
end;

procedure TLabeledButtonEdit.ClearEditMask;
begin
  EditMask := '';
end;

procedure TLabeledButtonEdit.UpdateEditMask;
begin
  //imposta la editmask
  case FButtonEditStyle of
    besDate : EditMask := DATE_MASK;
    besDateHHMM : EditMask := DATE_TIME_HH_MM_MASK;
    besDateHHMMSS : EditMask := DATE_TIME_HH_MM_SS_MASK;
    besTime : EditMask := TIME_HH_MM_MASK;
    besColor : EditMask := COLOR_MASK;
  end;
end;

procedure TLabeledButtonEdit.WMLButtonUp(var Msg: TWMLButtonUp);
var
  P: TPoint;
  R: TRect;
begin
  MouseCancel;
  inherited;

  P := Msg.Pos;
  R := FButtonRect;
  if UseRightToLeftAlignment then
  begin
    R.Left := 0;
    P.X := R.Right + P.X;
  end;
  if PtInRect(R, P) then
    ButtonEditClick;
end;

procedure TLabeledButtonEdit.WMLButtonDblClk(var Msg: TMessage);
begin
  if FMouseOverButton then
    WMLButtonDown(Msg)
  else
    inherited;
end;

procedure TLabeledButtonEdit.WMRButtonDown(var Msg: TMessage);
begin
  if FMouseOverButton then
    Msg.Result := 0
  else
    inherited;
end;

procedure TLabeledButtonEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if FMouseOverButton then
    Msg.HitTest := Windows.HTNOWHERE;

  inherited;
end;

procedure TLabeledButtonEdit.DoEnter;
begin
  UpdateEditMask;
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

function TLabeledButtonEdit.GetIsEmpty: boolean;
begin
  Result := KillChars(Text,[' ',FormatSettings.DateSeparator,FormatSettings.TimeSeparator,FormatSettings.ThousandSeparator,FormatSettings.DecimalSeparator])='';
end;

function TLabeledButtonEdit.isColorInput: boolean;
begin
  Result := FButtonEditStyle = besColor;
end;

function TLabeledButtonEdit.isDateTimeInput: boolean;
begin
  Result := FButtonEditStyle in [besDate,besDateHHMM,besDateHHMMSS, besTime];
end;

procedure TLabeledButtonEdit.SetCollapsed(const Value: boolean);
begin
  FCollapsed := Value;
  if FCollapsed then
    Width := CollapsedWidth;
end;

procedure TLabeledButtonEdit.SetDateTimeValue(const Value: TDateTime);
begin
  Text := DateTimeToStr(Value);
end;

procedure TLabeledButtonEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Collapsed then
    AWidth := CollapsedWidth;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TLabeledButtonEdit.CollapsedWidth: integer;
begin
  if Ctl3D then
    Result := ButtonWidth + 4
  else
    Result := ButtonWidth + 2;
end;

function TLabeledButtonEdit.GetDateTimeValue: TDateTime;
begin
  if isDateTimeInput then
  begin
    if not TryStrToDateTime(Text, Result) then
      Result := 0;
  end
  else
    Result := 0;
end;

{ TLabeledDBButtonEdit }

procedure TLabeledDBButtonEdit.ButtonEditClick;
var
  Title, DialogFilter, FileName: string;
begin
  //Se il controllo non è enabled non lancia l'onclick ma apre il file, se è valorizzato
  if not Enabled or ReadOnly then
  begin
    if (ButtonEditStyle = besFile) and isFileNameField(Field) then
    begin
      FileName := Field.Value;
      //Apro il file
      if FileName <> '' then
        ShowFileByShellExecute(Handle, FileName);
    end;
    Exit;
  end;

  if Assigned(FOnButtonEditClick) then
    FOnButtonEditClick(Self) else
  begin
    //Forza il ButtonEditStyle in base al tipo di campo
    if (Field <> nil) and (Field.EditMask = DATE_TIME_HH_MM_MASK) then
      ButtonEditStyle := besDateHHMM;
    //Forza il ButtonEditStyle in base al tipo di campo
    if (Field <> nil) and (Field.EditMask = DATE_TIME_HH_MM_SS_MASK) then
      ButtonEditStyle := besDateHHMMSS;

    if ButtonEditStyle in [besDate, besDateHHMM, besDateHHMMSS] then
    begin
      //crea la form di scelta della data
      CalendarViewPopUp(self, ButtonEditStyle);
    end
    else if ButtonEditStyle in [besTime, besTimeHHMMSS] then
    begin
      //crea la form di scelta dell'ora
      ClockPopUp(self, ButtonEditStyle);
    end
    else if ButtonEditStyle = besFile then
    begin
      if Field is TCBStringField then
      begin
        Title := TCBStringField(Field).DisplayLabel;
        DialogFilter := TCBStringField(Field).DialogFilter;
      end
      else if Field is TCBWideStringField then
      begin
        Title := TCBWideStringField(Field).DisplayLabel;
        DialogFilter := TCBWideStringField(Field).DialogFilter;
      end
      else
      begin
        Title := Field.DisplayLabel;
        DialogFilter := ALL_FILES_FILTER;
      end;
      SelectFileByDialog(self, BeforeDialogExecute, Title, DialogFilter);
    end
    else if ButtonEditStyle = besFolder then
      SelectDirByDialog(Self)
    else if ButtonEditStyle in [besColor] then
      SelectColorByDialog(self, BeforeDialogExecute);
  end;
end;

class constructor TLabeledDBButtonEdit.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TLabeledDBButtonEdit, TLabeledButtonEditStyleHook);
end;

class destructor TLabeledDBButtonEdit.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TLabeledDBButtonEdit, TLabeledButtonEditStyleHook);
end;

constructor TLabeledDBButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FTextIndicatorImages := TImageList.Create(nil);
  FTextIndicatorImages.ColorDepth := cd32Bit;
  FTextIndicatorImages.DrawingStyle := dsTransparent;

  FCalendarIndicatorImages := TImageList.Create(nil);
  FCalendarIndicatorImages.ColorDepth := cd32Bit;
  FCalendarIndicatorImages.DrawingStyle := dsTransparent;

  LoadImagesIntoControl(Self,1,1);

  FButtonEditStyle := besEllipsis;
  FButtonWidth := DEFAULT_BUTTON_WIDTH;
  FShortCut := VK_F3;

  InitCBEditorStyle(Self);
end;

destructor TLabeledDBButtonEdit.Destroy;
begin
  FCanvas.Free;
  FTextIndicatorImages.Free;
  FCalendarIndicatorImages.Free;
  inherited;
end;

procedure TLabeledDBButtonEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = TASTO_DOWN) and (Shift = [ssAlt])) or //Alt-down
    (Key = FShortCut) or  //Shortcut definito
    ((Key = TASTO_SPACE) and Collapsed) then //Spazio se collapsed
    ButtonEditClick
  else if (Field <> nil) and (Field.CanModify) and ((Key = TASTO_DELETE) or (Key = TASTO_BACKSPACE)) and (Collapsed) then
    Field.Clear
  else
    inherited KeyDown(Key, Shift);
end;

procedure TLabeledDBButtonEdit.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FButtonWidth := MulDiv(ButtonWidth, M, D);
  LoadImagesIntoControl(Self,M,D);
  inherited;
end;

procedure TLabeledDBButtonEdit.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    RecreateWnd;
  end;
end;

procedure TLabeledDBButtonEdit.SetButtonEditStyle(Value: TButtonEditStyle);
begin
  if FButtonEditStyle <> Value then
  begin
    FButtonEditStyle := Value;
    RecreateWnd;
  end;
end;

procedure TLabeledDBButtonEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  RepaintButton;
end;

procedure TLabeledDBButtonEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOverButton := False;
end;

procedure TLabeledDBButtonEdit.WMNCPaint(var Msg: TWMNCPaint);
begin
  inherited;
  DoPaintButton(Self, FButtonRect, Msg, FCurrentPPI);
end;

procedure TLabeledDBButtonEdit.MouseCancel;
begin
  if GetCapture = Handle then
    ReleaseCapture;

  FButtonDown := False;
  RepaintButton;
end;

procedure TLabeledDBButtonEdit.KeyPress(var Key: Char);
begin
  inherited;
  if (Ord(Key) = vk_Return) and (FButtonEditStyle = besEllipsis) then
  begin
    Key := #0;
    ButtonEditClick;
  end;
end;

procedure TLabeledDBButtonEdit.WMNCHitTest(var Msg: TMessage);
begin
  inherited;

  if Msg.Result = Windows.HTNOWHERE then
  begin
    FMouseOverButton := True;
    Msg.Result := HTCLIENT;
  end
  else
    FMouseOverButton := False;
end;

procedure TLabeledDBButtonEdit.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  if not UseRightToLeftAlignment then
    Dec(Msg.CalcSize_Params^.rgrc[0].Right, FButtonWidth)
  else
    Inc(Msg.CalcSize_Params^.rgrc[0].Left, FButtonWidth);
  inherited;
end;

procedure TLabeledDBButtonEdit.RepaintButton;
begin
  if HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TLabeledDBButtonEdit.DrawButton(const Canvas: TCanvas);
begin
  DrawButtonIntoControl(Self, Canvas, FCurrentPPI);
end;

procedure TLabeledDBButtonEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  MouseCancel;
end;

procedure TLabeledDBButtonEdit.WMLButtonDown(var Msg: TMessage);
begin
  if FMouseOverButton then
  begin
    if not Focused then
      SetFocus;
    FButtonDown := True;
    RepaintButton;
    SetCapture(Handle);
    Msg.Result := 0;
  end
  else
  begin
    inherited;
    if not Focused then
      MouseCancel;
  end;
end;

function TLabeledDBButtonEdit.CanChangeData: boolean;
begin
  Result := (Field.DataSet.State in [dsEdit, dsInsert]) or
  ((DataSource <> nil) and (DataSource.AutoEdit));
end;
procedure TLabeledDBButtonEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, BoundLabel);
end;

procedure TLabeledDBButtonEdit.WMLButtonUp(var Msg: TWMLButtonUp);
var
  P: TPoint;
  R: TRect;
begin
  MouseCancel;
  inherited;

  P := Msg.Pos;
  R := FButtonRect;
  if UseRightToLeftAlignment then
  begin
    R.Left := 0;
    P.X := R.Right + P.X;
  end;
  if PtInRect(R, P) then
    ButtonEditClick;
end;

procedure TLabeledDBButtonEdit.WMLButtonDblClk(var Msg: TMessage);
begin
  if FMouseOverButton then
    WMLButtonDown(Msg)
  else
    inherited;
end;

procedure TLabeledDBButtonEdit.WMRButtonDown(var Msg: TMessage);
begin
  if FMouseOverButton then
    Msg.Result := 0
  else
    inherited;
end;

procedure TLabeledDBButtonEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if FMouseOverButton then
    Msg.HitTest := Windows.HTNOWHERE;

  inherited;
end;

procedure TLabeledDBButtonEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

function TLabeledDBButtonEdit.GetDataField: string;
begin
  Result := inherited DataField;
end;

function TLabeledDBButtonEdit.GetIsEmpty: Boolean;
begin
  Result := (Field <> nil) and Field.IsNull;
end;

procedure TLabeledDBButtonEdit.SetDataField(const Value: string);
begin
  inherited DataField := Value;
  if (Field <> nil) then
  begin
    case Field.DataType of
      ftDate, ftDateTime : ButtonEditStyle := besDate;
    end;
  end;
end;

procedure TLabeledDBButtonEdit.SetFieldValue(const Value: Variant);
begin
  //Tenta di mandare in edit il dataset del campo se il suo datasource è autoedit
  if CanChangeData then
  begin
    if not (DataSource.DataSet.State in [dsEdit, dsInsert]) then
      DataSource.DataSet.Edit;
    Field.Value := Value;
  end;
end;

function TLabeledDBButtonEdit.isColorInput: boolean;
begin
  Result := FButtonEditStyle = besColor;
end;

function TLabeledDBButtonEdit.isDateTimeInput: boolean;
begin
  Result := FButtonEditStyle in [besDate,besDateHHMM,besDateHHMMSS, besTime];
end;

procedure TLabeledDBButtonEdit.SetCollapsed(const Value: boolean);
begin
  FCollapsed := Value;
  if FCollapsed then
    Width := CollapsedWidth;
end;

procedure TLabeledDBButtonEdit.SetDateTimeValue(const Value: TDateTime);
begin
  Text := DateTimeToStr(Value);
end;

procedure TLabeledDBButtonEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Collapsed then
    AWidth := CollapsedWidth;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TLabeledDBButtonEdit.CollapsedWidth: integer;
begin
  if Ctl3D then
    Result := ButtonWidth + 4
  else
    Result := ButtonWidth + 2;
end;

function TLabeledDBButtonEdit.GetDateTimeValue: TDateTime;
begin
  if isDateTimeInput then
  begin
    if not TryStrToDateTime(Text, Result) then
      Result := 0;
  end
  else
    Result := 0;
end;

end.
