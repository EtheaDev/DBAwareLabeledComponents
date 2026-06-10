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
unit Vcl.SelectOptionsForm;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Data.DB,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  Vcl.DBGrids,
  Vcl.Buttons,
  Vcl.DBCtrls,
  Vcl.Samples.Spin,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ImgList;

const
  MAX_SELECTION = 20;

resourcestring
  CHANGE_STYLE = 'Change style';
  STR_OPTIONS =  'Options';
  STR_SELECT_OPTIONS = 'Select option';
  STR_SELECT = 'Select';
  STR_EXIT = 'Exit';
  STR_HELP = 'Help';

type
  /// <summary>
  ///  Modal form that presents a list of mutually exclusive options in a radio
  ///  group and lets the user pick one, with optional OK/Cancel and Help buttons.
  /// </summary>
  TfmSelectOption = class(TForm)
    paBottom: TPanel;
    paButtons: TPanel;
    bbOK: TButton;
    bbCancel: TButton;
    gb: TRadioGroup;
    paHelp: TPanel;
    btHelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FScaleFactor: Single;
  public
  {$IFDEF D10_3+}
    /// <summary>
    ///  Overridden to track the current scaling factor (NewPPI relative to the
    ///  design-time PixelsPerInch) so the form is sized correctly for the active DPI.
    /// </summary>
    /// <param name="NewPPI">The new pixels-per-inch value the form is being scaled to.</param>
    procedure ScaleForPPI(NewPPI: Integer); override;
  {$ENDIF}
    /// <summary>
    ///  Creates and initializes the selection form: assigns the font, computes the
    ///  client size (enlarging it to fit the buttons when Resize is True), fills the
    ///  radio group with the options, selects SelectedIndex, sets the caption and
    ///  enables the Help button when a help context is supplied.
    /// </summary>
    /// <param name="AOwner">Owner component for the form.</param>
    /// <param name="NewFont">Font to use; when nil the application default font is used.</param>
    /// <param name="Resize">When True, the window is enlarged to fit the bottom panel and all buttons.</param>
    /// <param name="AOptions">List of option captions to display in the radio group.</param>
    /// <param name="Title">Text shown in the form caption.</param>
    /// <param name="NewWidth">Requested client width in design-time pixels (scaled by the DPI factor).</param>
    /// <param name="NewHeight">Requested client height in design-time pixels (scaled by the DPI factor).</param>
    /// <param name="HelpContext">Help context id; when non-zero the Help panel is shown.</param>
    /// <param name="SelectedIndex">Index of the option initially selected in the radio group.</param>
    Constructor CreateSelectOption( AOwner : TComponent; NewFont : Tfont; Resize : boolean;
       AOptions : TStringList; Title : string; NewWidth : integer = 0; NewHeight : integer = 0;
       HelpContext : integer = 0; SelectedIndex : integer = 0);
    /// <summary>
    ///  Destroys the form instance.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  ///  Displays the option-selection form modally and returns the index of the
  ///  chosen option. AOptions is the newline-separated list of options; the form
  ///  size and number of columns are computed automatically from the option text
  ///  and font.
  /// </summary>
  /// <param name="AOptions">Newline-separated text containing one option per line.</param>
  /// <param name="Font">Font used to size the form; when nil the application default font is used.</param>
  /// <param name="Resize">When True, the window is resized to fit its content and buttons.</param>
  /// <param name="Title">Caption shown on the form.</param>
  /// <param name="HelpContext">Help context id; when non-zero the Help button is shown.</param>
  /// <param name="SelectedIndex">Index of the option initially selected.</param>
  /// <returns>The zero-based index of the selected option, or -1 if the user cancels.</returns>
  function CBSelectOption(AOptions : string; Font : Tfont; Resize : boolean;
    Title : string = ''; HelpContext : integer = 0; SelectedIndex : integer = 0) : integer;

  /// <summary>
  ///  Prompts the user to pick a VCL style from the sorted list of registered
  ///  style names, preselecting the currently active style.
  /// </summary>
  /// <param name="SourceFont">Font used to size the selection form.</param>
  /// <returns>The name of the chosen style; raises EAbort if the user cancels the selection.</returns>
  function CBSelectStyleName(SourceFont: TFont): string;
  /// <summary>
  ///  Computes the standard size of a single-line edit control for the given font,
  ///  mirroring the VCL's TCustomEdit.AdjustHeight calculation.
  /// </summary>
  /// <param name="Font">Font whose text metrics drive the size calculation.</param>
  /// <param name="Ctl3D">Whether the control uses the 3D border style (affects the height).</param>
  /// <param name="Width">Returns the computed average character width plus padding.</param>
  /// <param name="Height">Returns the computed standard control height.</param>
  procedure CalcStandardEditSize(Font : TFont;
    Ctl3D : boolean; out Width, Height : integer);

implementation

{$R *.DFM}

uses
  System.Math,
  Vcl.Themes;

procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);
var
  //VCL
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  //VCL: this section is copied from TCustomEdit.AdjustHeight to compute the
  //standard height of an edit control the same way the VCL does
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
  Width := Metrics.tmAveCharWidth + 2; //the 2 was determined manually!
end;

function CBSelectOption(AOptions : string; Font : Tfont; Resize : boolean;
  Title : string = ''; HelpContext : integer = 0; SelectedIndex : integer = 0) : integer;
var
  FScelta : TfmSelectOption;
  Lista : TStringList;
  Altezza : Integer;
  Larghezza : Integer;
  LColumns: Integer;
  i : integer;
  LFontWidth, LFontHeight: Integer;
begin
  Lista := nil;
  FScelta := nil;
  Result := -1;
  Try
    Lista := TStringList.Create;
    Lista.Text := AOptions;

    if Assigned(Font) then
      CalcStandardEditSize(Font, False, LFontWidth, LFontHeight)
    else
      CalcStandardEditSize(Application.DefaultFont, False, LFontWidth, LFontHeight);
    LFontWidth := LFontWidth + 4;
    LColumns := Trunc((Lista.Count-1) / MAX_SELECTION + 1);
    Altezza := Trunc(LFontHeight * (Lista.Count / LColumns));
    Larghezza := (LFontWidth * length(Title)) * LColumns;
    for i := 0 to Lista.Count - 1 do
      Larghezza := max(Larghezza, (LFontWidth * length(Lista.strings[i]) * LColumns));
    FScelta := TfmSelectOption.CreateSelectOption(
      nil, Font, True, Lista, Title, Larghezza, Altezza, HelpContext, SelectedIndex );
    if Fscelta.ShowModal <> mrOK then
      exit;
    Result := FScelta.gb.ItemIndex;
  Finally
    Lista.Free;
    FScelta.Free;
  End;
end;

function CBSelectStyleName(SourceFont: TFont): string;
var
  i, SelectedIndex: integer;
  LOptions: TStringList;
  LActiveStyleName: string;
begin
  SelectedIndex := -1;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  LOptions := TStringList.Create;
  try
    for i := 0 to High(TStyleManager.StyleNames) do
      LOptions.Add(TStyleManager.StyleNames[i]);
    LOptions.Sort;
    for i := 0 to LOptions.Count -1 do
    begin
      if SameText(LOptions.Strings[i], LActiveStyleName)  then
        SelectedIndex := i;
    end;
    i := CBSelectOption(LOptions.Text, SourceFont, True,
      CHANGE_STYLE, 0, SelectedIndex);
    if i >= 0 then
      Result := LOptions.Strings[i]
    else
      Abort;
  finally
    LOptions.Free;
  end;
end;

procedure TfmSelectOption.FormShow(Sender: TObject);
begin
  if gb.CanFocus then
    gb.SetFocus;
end;

{$IFDEF D10_3+}
procedure TfmSelectOption.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
  FScaleFactor := NewPPI / PixelsPerInch;
end;
{$ENDIF}

constructor TfmSelectOption.CreateSelectOption( AOwner : TComponent;
  NewFont : Tfont; Resize : boolean;
  AOptions : TStringList; Title : string; NewWidth : integer = 0; NewHeight : integer = 0;
  HelpContext : integer = 0; SelectedIndex : integer = 0);
begin
  inherited Create(AOwner);
  //FNewFont := NewFont;
  if Assigned(NewFont) then
    Font.Assign(NewFont)
  else
    Font.Assign(Application.DefaultFont);
  ClientWidth := Round(NewWidth * FScaleFactor);
  ClientHeight := Round(NewHeight * FScaleFactor);
  if Resize then
  begin
    //Increase the height to fit the Bottom panel and some space above it
    ClientHeight := ClientHeight + Round(paBottom.Height * FScaleFactor) * 2;
    //Widen the window at least enough to fit all the buttons
    ClientWidth := max(ClientWidth,
      paButtons.Width+Round(paHelp.Width+10 * FScaleFactor));
  end;
  gb.Items.Text := AOptions.Text;
  gb.ItemIndex := SelectedIndex;
  gb.Columns := Trunc((AOptions.Count-1) / MAX_SELECTION + 1);
  self.Caption := Title;
  if HelpContext <> 0 then
  begin
    paHelp.Visible := True;
    self.HelpContext := HelpContext;
  end;
end;

destructor TfmSelectOption.Destroy;
begin
  inherited;
end;

procedure TfmSelectOption.btHelpClick(Sender: TObject);
begin
  Application.HelpContext(self.HelpContext);
end;

procedure TfmSelectOption.FormCreate(Sender: TObject);
begin
  //Localize the form's caption strings
  gb.Caption := STR_OPTIONS;
  bbOK.Caption := STR_SELECT;
  bbCancel.Caption := STR_EXIT;
  btHelp.Caption := STR_HELP;
  gb.Caption := STR_OPTIONS;
end;

end.
