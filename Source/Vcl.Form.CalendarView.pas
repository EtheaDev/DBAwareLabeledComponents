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
unit Vcl.Form.CalendarView;

interface

uses
  //Delphi
  System.SysUtils
  , System.Classes
  , System.Types
  //Labeled controls
  , Vcl.LabeledExtCtrls
  , Vcl.LabeledCtrls
  , Vcl.LabeledButtonEdit
  , Vcl.LabeledMask
  //VCL
  , WinApi.Windows
  , Vcl.Forms
  , Vcl.StdCtrls
  , Vcl.Controls
  , Vcl.Mask
  , Vcl.Graphics
  , Vcl.ExtCtrls
  , Vcl.WinXCalendars
  , Winapi.Messages;

type
  TCanChange = function(): boolean of object;

  TCalendarViewEx = class(TCalendarView)
  private
    FOnCanChangeDate : TCanChange;
  protected
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  public
    property OnCanChangeDate: TCanChange read FOnCanChangeDate write FOnCanChangeDate;
    constructor Create(AOwner: TComponent); override;
  published
    property ParentFont stored True;
  end;

  // Calendar Form Type Definition
  TCalendarViewPopupForm = class( TForm )
    procedure FormCancel;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure meKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FScaleFactor: Single;
    EditOwner: TCustomEdit;
    EditStyle : TButtonEditStyle;
    PanelEdit: TPanel;
    me: TLabeledMaskEdit;
    CV: TCalendarViewEx;
    FormResult : integer;
    isGestioneOra : boolean;
    m_CurrentDay, m_CurrentYear, m_CurrentMonth : Word;
    m_CurrentHour, m_CurrentMin, m_CurrentSec, m_CurrentMsec : Word;
    m_CurrentDateSelected: TDateTime;
    CALENDAR_COLOR: TColor;
    DateRect : TRect;
    FDropped : Boolean;
    function GetEditFont : TFont;
    function CanChangeDate : boolean;
    procedure CalendarDrawDayItem(Sender: TObject; DrawParams: TDrawViewInfoParams;
    CalendarViewViewInfo: TCellItemViewInfo);
    procedure CalendarViewChange(Sender: TObject);
    procedure ImpostaDataOra;
    function GetDayColor(DayOfWeek : integer) : TColor;
  public
    procedure ScaleForPPI(NewPPI: Integer); override;
    procedure InitForm( Owner: TCustomEdit; ButtonEditStyle : TButtonEditStyle = besDate );
    constructor Create(AOwner : TComponent); override;
  end;

  procedure CalendarViewPopUp(EditControl : TCustomEdit;
    ButtonEditStyle : TButtonEditStyle);

implementation

{$R *.dfm}

uses
  //RTL
  Data.DB
  //VCL
  , Vcl.DBCtrls
  , Vcl.Themes
  , System.UITypes
  //Labeled components
  , Vcl.BoundLabel
  , Vcl.DbAwareLabeledUtils
  , Vcl.DbAwareLabeledConsts
  ;

{ TCalendarViewEx }
constructor TCalendarViewEx.Create(AOwner: TComponent);
begin
  inherited;
  ParentFont := True;
  HeaderInfo.Font.Assign(Font);
  FirstDayOfWeek := TDaysOfWeek.dwMonday;
end;

procedure TCalendarViewEx.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if Assigned(FOnCanChangeDate) then
  begin
    if FOnCanChangeDate then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure CalendarViewPopUp(EditControl : TCustomEdit;
  ButtonEditStyle : TButtonEditStyle);
var
  FCalendForm : TCalendarViewPopupForm;
begin
  if (not EditControl.CanFocus) or not (EditControl.Visible) then exit;
  Application.CreateForm(TCalendarViewPopupForm, FCalendForm);
  FCalendForm.InitForm( EditControl, ButtonEditStyle );
  FCalendForm.Show;
end;

{ TCalendarViewPopupForm }

procedure TCalendarViewPopupForm.CalendarDrawDayItem(Sender: TObject; DrawParams: TDrawViewInfoParams;
    CalendarViewViewInfo: TCellItemViewInfo);
begin
  DrawParams.ForegroundColor := GetDayColor(DayOfWeek(CalendarViewViewInfo.Date));

  if (DateRect.Left = 0) or (DateRect.Left > DrawParams.DrawRect.Left) then
    DateRect.Left := DrawParams.DrawRect.Left;

  if (DateRect.Right = 0) or (DateRect.Right < DrawParams.DrawRect.Right) then
    DateRect.Right := DrawParams.DrawRect.Right;

  if (DateRect.Top = 0) or (DateRect.Top > DrawParams.DrawRect.Top) then
    DateRect.Top := DrawParams.DrawRect.Top;

  if (DateRect.Bottom = 0) or (DateRect.Bottom < DrawParams.DrawRect.Bottom) then
    DateRect.Bottom := DrawParams.DrawRect.Bottom;

end;

procedure TCalendarViewPopupForm.CalendarViewChange(Sender: TObject);
begin
  if FDropped then
  begin

    if (DateToStr(CV.Date) = '00/00/0000') then exit;

    ImpostaDataOra;
  end;
end;

function TCalendarViewPopupForm.CanChangeDate: boolean;
var
  EditControl : TCustomEdit;
begin
  EditControl := TCustomEdit( EditOwner );
  if EditControl is TLabeledDBButtonEdit then
    Result := TLabeledDBButtonEdit(EditControl).CanChangeData
  else if EditControl <> nil then
    Result := EditControl.Enabled
  else
    Result := False;
end;

constructor TCalendarViewPopupForm.Create(AOwner: TComponent);
begin
  inherited;
  Left := 343;
  Top := 212;
  AutoScroll := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Caption := ' ';
  ClientHeight := 200;
  ClientWidth := 190;
  Color := clWhite;
  KeyPreview := True;
  OnKeyDown := FormKeyDown;
  OnDeactivate := FormDeactivate;
  OnClose := FormClose;
  PixelsPerInch := 96;
end;

procedure TCalendarViewPopupForm.FormCancel;
begin
  FormResult := mrCancel;
  Close;
end;

procedure TCalendarViewPopupForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  FDisplayFormat: string;
  FEditText : string;

  function GetTextDateTime : string;
  begin
    //Funzione utilizzata per formattare l'output quando non c'è una display format sul campo
    //o quando si tratta di un TCBXEditButton semplice
    if not (isGestioneOra) then
      Result := datetostr(m_CurrentDateSelected)
    else if EditStyle = besDateHHMM then
      Result := DateTimeHHMMtoStr(m_CurrentDateSelected)
    else
      Result := DateTimeHHMMSStoStr(m_CurrentDateSelected);
  end;

begin
  if FormResult = mrOk	then
  begin
    if EditOwner is TLabeledDBButtonEdit then
    with TLabeledDBButtonEdit(EditOwner) do
    begin
      if CanChangeData then
      begin
        //Tenta di mandare in edit il dataset del campo se il suo datasource è autoedit
        if not (Field.DataSet.State in [dsEdit, dsInsert]) then
          Field.DataSet.Edit;
      end
      else
        Exit; //Non sono in edit: non modifica il testo del campo

      if Field is TDateTimeField then
        FDisplayFormat := TDateTimeField(Field).DisplayFormat
      else if Field is TSQLTimeStampField then
        FDisplayFormat := TSQLTimeStampField(Field).DisplayFormat
      else
        FDisplayFormat := '';

      if (FDisplayFormat <> '') then
      begin
        DateTimeToString(FEditText, FDisplayFormat, m_CurrentDateSelected);
        EditOwner.Text := FEditText;
      end
      else
      begin
        EditOwner.Text := GetTextDateTime;
      end;
    end
    else
    begin
      EditOwner.Text := GetTextDateTime;
    end;

    EditOwner.SelectAll;
    EditOwner.SetFocus;
  end;
  action := caFree;
end;

procedure TCalendarViewPopupForm.FormDeactivate(Sender: TObject);
begin
  FormResult := mrCancel;
  Close;
end;

procedure TCalendarViewPopupForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift <> []) then exit;
  Case key of
    VK_RETURN:
    begin
      if DateToStr(CV.Date) = '00/00/0000' then exit;

      ImpostaDataOra;
    end;
    VK_ESCAPE: FormCancel;

  end;
end;

function TCalendarViewPopupForm.GetDayColor(DayOfWeek: integer): TColor;
  function CalcDayColor: TColor;
  begin
    case DayOfWeek of
      1 :  //domenica
      begin
        if ColorToRgb(CALENDAR_COLOR) <> ColorToRgb(clRed) then
          Result := clRed else
          Result := GetStyledColor(clGrayText);
      end;
      7 : //sabato
      begin
        if ColorToRgb(CALENDAR_COLOR) <> ColorToRgb(clBlue) then
          Result := clBlue else
        Result := GetStyledColor(clGrayText);
      end;
    else
      Result := GetStyledColor(clWindowText); //giorni feriali
    end;
  end;

begin
  if IsVCLStyled then
  begin
    if (DayOfWeek = 1)  then
      Result := GetStyledColor(clHighlight) //Domenica
    else if (DayOfWeek = 7) then
      Result := GetStyledColor(clHighlight) // Sabato
    else
      Result := GetStyledColor(clWindowText); //giorni feriali
  end
  else
    Result := CalcDayColor;
end;

function TCalendarViewPopupForm.GetEditFont: TFont;
begin
  if editOwner is TLabeledDBButtonEdit then
    Result := TLabeledDBButtonEdit(editOwner).Font
  else if editOwner is TLabeledButtonEdit then
    Result := TLabeledButtonEdit(editOwner).Font
  else
    Raise EComponentError.Create('la Funzione TfrmCalPop.GetEditFont non è in grado di restituire un valore valido');
end;

procedure TCalendarViewPopupForm.ImpostaDataOra;
begin
  if isGestioneOra then
  begin
    DecodeTime(StrToTime(me.Text), m_CurrentHour, m_CurrentMin, m_CurrentSec, m_CurrentMsec );
    if EditStyle = besDateHHMMSS then
      m_CurrentDateSelected := CV.Date+EncodeTime(m_CurrentHour,m_CurrentMin,m_CurrentSec,0)
    else
      m_CurrentDateSelected := CV.Date+EncodeTime(m_CurrentHour,m_CurrentMin,0,0);
  end
  else
    m_CurrentDateSelected := CV.Date;

  if CanChangeDate then
    FormResult := mrOk
  else
    FormResult := mrCancel;
  Close;

end;

procedure TCalendarViewPopupForm.InitForm(Owner: TCustomEdit;
  ButtonEditStyle: TButtonEditStyle);
var
   rectPlace: TRect;
   ptUpper, ptLower: TPoint;
   EditFont : TFont;
  //VCL
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  FDropped := False;
  EditStyle := ButtonEditStyle;
  EditOwner := Owner;
  EditFont := GetEditFont;
  isGestioneOra := ButtonEditStyle in [besDateHHMM, besDateHHMMSS];
  CALENDAR_COLOR := GetStyledColor(clWindow);

  Self.Constraints.MinWidth := EditOwner.Width;
  
  DateRect := Rect(0,0,0,0);
  Font.Assign(EditFont);
  CV := TCalendarViewEx.Create(Self);

  with CV do
  begin
    Parent := Self;
    HeaderInfo.Font := Font;
    HeaderInfo.Font.Style := [fsBold];
    HeaderInfo.Font.Size := HeaderInfo.Font.Size + 2;
    HeaderInfo.DaysOfWeekFont := Font;
    OnDrawDayItem := CalendarDrawDayItem;
    OnChange := CalendarViewChange;
    OnCanChangeDate := CanChangeDate;

    //VCL: questo pezzo è copiato da TCustomEdit.AdjustHeight per calcolare l'altezza
    //Standard di un controllo di edit come fa la VCL
    DC := GetDC(0);
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, HeaderInfo.Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);

    CV.Height := Metrics.tmHeight * 13; //13 l'ho determinato a mano!
    CV.Width := (Metrics.tmAveCharWidth * 21) + Metrics.tmMaxCharWidth  ; //31 l'ho determinato a mano!

    Self.Width := CV.Width;
    Align := alTop;

  end;

  {Initialize form Height & Width based on Font }
  Height := CV.Height;

  if isGestioneOra then
  begin
    Height := Height + 20;
    PanelEdit := TPanel.Create(Self);
    PanelEdit.Parent := Self;
    PanelEdit.Align := alBottom;
    CV.Align := alClient;
    me := TLabeledMaskEdit.Create(self);
    with me do
    begin
      Parent := PanelEdit;
      Font.Assign(EditFont);
      Width := Round(70 * FScaleFactor);
      ReadOnly := False;
      Left := Round(70 * FScaleFactor);
      Top := Round(7 * FScaleFactor);
      if ButtonEditStyle = besDateHHMM then
      begin
        EditMask := TIME_HH_MM_MASK;
        MaxLength := 5;
      end
      else
      begin
        EditMask := TIME_MASK;
        MaxLength := 8;
      end;
      TabOrder := 0;
      Text := '  =  ';
      OnKeyDown := meKeyDown;
      BoundCaption := TIME_LABEL;
      BoundLabel.Font.Assign(EditFont);
      BoundLabel.LabelPosition := lpLeftMiddle;
      BoundLabel.LabelSpacing := 3;
    end;
  end;

  { Dynamically set the size and position }
  rectPlace := editOwner.ClientRect;
  ptUpper.X := rectPlace.Left;
  ptUpper.Y := rectPlace.Top;
  ptUpper := editOwner.ClientToScreen( ptUpper );
  ptLower.X := rectPlace.Right;
  ptLower.Y := rectPlace.Bottom;
  ptLower := editOwner.ClientToScreen( ptLower );

  { If too far down, pop the calendar above the control }
  if ptUpper.X + 1 + Width > Screen.Width then
     Left := Screen.Width - Width - 1
  else
     Left := ptUpper.X - 1;
  if ptLower.Y + 1 + Height > Screen.Height then
     Top := ptUpper.Y - Height
  else
     Top := ptLower.Y + 1;

  { define initial date }
  if not TryStrToDateTime(TCustomEdit( EditOwner ).Text, m_CurrentDateSelected) then
  begin
    if not isGestioneOra then
      m_CurrentDateSelected := Date
    else
      m_CurrentDateSelected := Now;
  end;

  {Extract date Components}
  DecodeDate( m_CurrentDateSelected, m_CurrentYear, m_CurrentMonth, m_CurrentDay );
  CV.Date := EncodeDate(m_CurrentYear, m_CurrentMonth, m_CurrentDay);

  {Extract Time Components}
  if isGestioneOra then
  begin
    DecodeTime(m_CurrentDateSelected, m_CurrentHour, m_CurrentMin, m_CurrentSec, m_CurrentMsec );
    //inizializzo editmask dell'ora e minuti
    me.Text := PadL(IntToStr(m_CurrentHour),2,'0')+FormatSettings.TimeSeparator+PadL(IntToStr(m_CurrentMin),2,'0');
    //aggiungo eventualmente i secondi
    if ButtonEditStyle = besDateHHMMSS then
      me.Text := me.Text + FormatSettings.TimeSeparator+PadL(IntToStr(m_CurrentSec),2,'0');
  end;
  FDropped := True;
end;

procedure TCalendarViewPopupForm.meKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_LEFT) or (key = VK_RIGHT) or (Key = VK_UP) or (Key=VK_DOWN) then
    Key := 0;

  if (key = VK_RETURN) then
  begin
    if DateToStr(CV.Date) = '00/00/0000' then exit;
    ImpostaDataOra;
  end;

end;

procedure TCalendarViewPopupForm.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
  FScaleFactor := NewPPI / PixelsPerInch;
end;

end.
