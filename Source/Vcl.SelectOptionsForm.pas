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
unit Vcl.SelectOptionsForm;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  SysUtils, Variants, Classes, DB,
  Messages, Windows, Forms, Graphics, Controls, Dialogs, ToolWin, ComCtrls, ActnList, ExtCtrls,
  DBGrids, Buttons, DBCtrls, Spin, Grids, StdCtrls, ImgList;

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
    procedure ScaleForPPI(NewPPI: Integer); override;
  {$ENDIF}
    Constructor CreateSelectOption( AOwner : TComponent; NewFont : Tfont; Resize : boolean;
       AOptions : TStringList; Title : string; NewWidth : integer = 0; NewHeight : integer = 0;
       HelpContext : integer = 0; SelectedIndex : integer = 0);
    destructor Destroy; override;
  end;

  function CBSelectOption(AOptions : string; Font : Tfont; Resize : boolean;
    Title : string = ''; HelpContext : integer = 0; SelectedIndex : integer = 0) : integer;

  function CBSelectStyleName(SourceFont: TFont): string;
  procedure CalcStandardEditSize(Font : TFont;
    Ctl3D : boolean; out Width, Height : integer);

implementation

{$R *.DFM}

uses
  Math, Themes;

procedure CalcStandardEditSize(Font : TFont;
  Ctl3D : boolean; out Width, Height : integer);
var
  //VCL
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  //VCL: questo pezzo è copiato da TCustomEdit.AdjustHeight per calcolare l'altezza
  //Standard di un controllo di edit come fa la VCL
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
  Width := Metrics.tmAveCharWidth + 2; //2 l'ho determinato a mano!
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
    //Allungo l'altezza per farci stare il pannello Bottom e un po' di spazio sopra
    ClientHeight := ClientHeight + Round(paBottom.Height * FScaleFactor) * 2;
    //Allargo la finestra almeno per farci stare tutti i pulsanti
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
  //Traduzione in lingua delle stringhe della form
  gb.Caption := STR_OPTIONS;
  bbOK.Caption := STR_SELECT;
  bbCancel.Caption := STR_EXIT;
  btHelp.Caption := STR_HELP;
  gb.Caption := STR_OPTIONS;
end;

end.
