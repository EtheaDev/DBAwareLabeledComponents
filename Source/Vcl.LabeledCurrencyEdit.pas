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
unit Vcl.LabeledCurrencyEdit;

interface

{$I 'DBAwareLabeledComponents.inc'}

uses
  WinApi.Windows
  , System.SysUtils
  , System.Classes
  , WinApi.Messages
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Graphics
  , Vcl.BoundLabel
  , Vcl.LabeledCtrls
  , Vcl.LabeledDBCtrls
  ;

const
  MAX_LEN_DEFAULT = 15;

type
  TNumberValue = (nvNegative, nvPositive);
  TNumberValues = set of TNumberValue;

  {TLabeledCurrencyEdit}
  TLabeledCurrencyEdit = class(TLabeledEditEx)
  private
    FDeleteBuffer: Boolean; //per gestire azzeramento valore se il primo tasto premuto è un numero
    FDecimalOffset: 0..1; //per gestire se esiste o meno la virgola nel numero imputato
    FMaxLength: integer; //per gestire la dimensione massima del numero
    FDecimals: integer; //numero di cifre decimali
    FNumberValues : TNumberValues; //valori accettati in input (pos/neg o entrambi)
    FFormatString: string;
    FSigla: string; //Sigla della divisa
    FAlignment : TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure Reformat;
    procedure SetMaxLength(const Value: integer);
    procedure SetNumberValues(const Value: TNumberValues);
    procedure SetSigla(const Value: string); //Formattazione applicata al valore
    function DeleteSigla( const MyTesto, MySigla : string) : string;
    procedure AddSigla;
    function GetTextAsFloat: Extended;
    procedure SetTextAsFloat( Value: Extended );
    procedure SetDecimals(const Value: integer);
    function InitialPos: integer;
  protected
    function GetIsEmpty: Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
    property AutoSelect default False;
    property MaxLength: integer read FMaxLength write SetMaxLength default MAX_LEN_DEFAULT;
    property NumberValues: TNumberValues read FNumberValues write SetNumberValues default [nvPositive];
    property TextAsFloat: Extended read GetTextAsFloat write SetTextAsFloat;
    property Sigla : string read FSigla write SetSigla;
    property Decimals: integer read FDecimals write SetDecimals default 2;
  end;

  {TLabeledDBCurrencyEdit}
  TLabeledDBCurrencyEdit = class(TLabeledDBEdit)
  private
    FDeleteBuffer: Boolean; //per gestire azzeramento valore se il primo tasto premuto è un numero
    FDecimalOffset: 0..1; //per gestire se esiste o meno la virgola nel numero imputato
    FMaxLength: integer; //per gestire la dimensione massima del numero
    FDecimals: integer; //numero di cifre decimali
    FNumberValues : TNumberValues; //valori accettati in input (pos/neg o entrambi)
    FFormatFloat: string; //Formato di editing
    procedure Reformat;
    procedure SetMaxLength(const Value: integer);
    procedure SetNumberValues(const Value: TNumberValues); //Formattazione applicata al valore
    procedure CheckFieldEmpty;
    procedure SetFieldFormat;
    function GetTextAsFloat: Extended;
    function InitialPos: integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; override;
    property TextAsFloat : Extended read GetTextAsFloat;
  published
    property AutoSelect default False;
    property NumberValues: TNumberValues read FNumberValues write SetNumberValues default [nvPositive];
    property MaxLength: integer read FMaxLength write SetMaxLength default MAX_LEN_DEFAULT;
  end;

implementation

uses
  Data.DB
  , Vcl.DbAwareLabeledConsts
  , Vcl.DbAwareLabeledUtils
  ;

function isKeyDownNumber(Key: Word) : boolean;
begin
  Result := ((Key >= Ord('0')) and (Key <= Ord('9')))
    or ((Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9));
end;

{ TLabeledDBCurrencyEdit }

function TLabeledDBCurrencyEdit.InitialPos : integer;
begin
  Result := Length(Text) - FDecimals - FDecimalOffset;
end;

procedure TLabeledDBCurrencyEdit.CheckFieldEmpty;
begin
  //recupera la maschera dal field
  if (Assigned(Field)) then
  begin
    SetFieldFormat;
    if (Text = '') then
    begin
      Reformat;
      SelStart := InitialPos;
    end;
  end;
end;

constructor TLabeledDBCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited;
  inherited AutoSelect := False;
  FDecimalOffset := 1;
  FDecimals := 2;
  FMaxLength := MAX_LEN_DEFAULT;
  FNumberValues := [nvPositive];
end;

procedure TLabeledDBCurrencyEdit.DoEnter;
begin
  inherited;
  if Assigned(Field) and not (Field.isnull) then
  begin
    CheckFieldEmpty;
    SelLength := 0;
    SelStart := InitialPos;
  end;
  FDeleteBuffer := True;
end;


{$IFNDEF CBCLX}
procedure TLabeledDBCurrencyEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or Alignments[taRightJustify];
  end;
end;
{$ENDIF}

function TLabeledDBCurrencyEdit.GetTextAsFloat: Extended;
begin
  //elimina i punti decimali
  Result := StrToFloat(StripCommas(Text, FDecimals));
end;

procedure TLabeledDBCurrencyEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  CursorPos: integer;
  Buffer: string;
  B1,B2 : string;
begin
  inherited KeyDown(Key,Shift);

  //Se il numero è il primo tasto premuto e il cursore è sulla virgola prima azzera il valore precedente
  if (FDeleteBuffer) and isKeyDownNumber(Key) and
    ((Text = '') or (SelStart = InitialPos)) then
  begin
    if EditCanModify then
    begin
      CheckFieldEmpty;
      FDeleteBuffer := False;
      Text := '';
      Reformat;
      SelStart := InitialPos;
    end;
  end
  //Se c'è una selezione estesa prima cancella il contenuto
  else if (SelLength > 0) and (Shift=[]) and
    ((isKeyDownNumber(Key)) or (Key = VK_DELETE)) then
  begin
    if EditCanModify then
    begin
      B1 := Copy(Text,0,SelStart);
      B2 := Copy(Text,SelStart+SelLength+1,1000);
      if Key = VK_DELETE then Key := 0;
      CursorPos := SelStart;
      FDeleteBuffer := False;
      Text := B1+B2;
      Reformat;
      Buffer := Text;
      if StrToFloat(StripCommas(Buffer, FDecimals))=0 then
        SelStart := initialPos else
        SelStart := CursorPos;
      SelLength := 0;
    end;  
  end
  //Se viene premuto Canc cancella una cifra
  else if Key = VK_DELETE then
  begin
    if EditCanModify then
    begin
      Buffer := Text;
      CursorPos := Length(Buffer)-SelStart-SelLength;
      if CursorPos > FDecimals+FDecimalOffset then
        begin
          if FDecimals=0 then CursorPos := CursorPos +1;
          if (CursorPos-FDecimals-1) mod 4 = 0 then Dec(CursorPos);
          if FDecimals=0 then CursorPos := CursorPos -1;
          Delete( Buffer, Length(Buffer)-CursorPos+1, 1 );
          Dec(CursorPos);
        end
      else
      begin
        if CursorPos = FDecimals+FDecimalOffset then Dec(CursorPos);
        if CursorPos > 0 then
        begin
          Delete( Buffer, Length(Buffer)-CursorPos+FDecimalOffset, 1 );
          Insert( '0', Buffer, Length(Buffer)-CursorPos+FDecimalOffset+1 );
          Dec(CursorPos);
        end;
      end;
      Key := 0;
      Text := Buffer;
      Reformat;
      SelStart := Length(Text)-CursorPos;
    end  
  end
  else
    FDeleteBuffer := False;
end;

procedure TLabeledDBCurrencyEdit.KeyPress(var Key: char);
var
  Buffer: string;
  CursorPos: integer;
begin
  if (Key='.') or (Key=',') then
    Key := FormatSettings.DecimalSeparator;

  inherited KeyPress(Key);
  {$IFDEF DXE+}
  if CharInSet(Key, ['0'..'9',#8,'.',',','-','+',#0]) and EditCanModify then
  {$ELSE}
  if (Key in ['0'..'9',#8,'.',',','-','+',#0]) and EditCanModify then
  {$ENDIF}
  begin
    CheckFieldEmpty;

    Buffer := Text;
    CursorPos := Length(Buffer)-SelStart-SelLength;
    case Key of
      '0'..'9':
      begin
        if CursorPos >= FDecimals+FDecimalOffset then
        begin
          if Length(Buffer) < FMaxLength  then
            Insert( Key, Buffer, Length(Buffer)-CursorPos+1 );
          end
        else if CursorPos >= 1 then
        begin
          Delete( Buffer, Length(Buffer)-CursorPos+1 , 1 );
          Insert( Key, Buffer, Length(Buffer)-CursorPos+2 );
          if 1-CursorPos <= 0 then Dec(CursorPos);
        end;
      end;
      #8: {BACKSPACE}
      begin
        if CursorPos > FDecimals then
        begin
          if FDecimals=0 then CursorPos := CursorPos +1;
          if (CursorPos-FDecimals) mod 4 = 0 then Inc(CursorPos);
          if FDecimals=0 then CursorPos := CursorPos -1;
          Delete( Buffer, Length(Buffer)-CursorPos, 1 );
        end
        else
        begin
          if (CursorPos = FDecimals) and (FDecimals<>0) then Inc(CursorPos);
          Delete( Buffer, Length(Buffer)-CursorPos, 1 );
          if (CursorPos <= FDecimals) and (FDecimals<>0) then
          begin
            Insert( '0', Buffer, Length(Buffer)-CursorPos+1 );
            Inc(CursorPos);
          end;
        end;
      end;
      '.',',':
      begin
        CursorPos := FDecimals;
      end;
      '-':
        if (nvNegative in FNumberValues) then
        begin
          //aggiunge il segno meno
          if (pos('-',Buffer) = 0) then
          begin
            if Length(Buffer) < FMaxLength then Buffer := '-'+Buffer;
          end;
        end;
      '+':
        if (nvPositive in FNumberValues) then
        begin
          //toglie il segno meno
          if (pos('-',Buffer) <> 0) then
          begin
            Buffer := KillChar(Buffer,'-');
          end;
        end;
      #0: {ESCAPE}
        CursorPos := FDecimals + FDecimalOffset;
    end;
    Key := #0;
    Text := Buffer;
    Reformat;
    SelStart := Length(Text)-CursorPos;
  end;  
end;

procedure TLabeledDBCurrencyEdit.Reformat;
Var
  Negative : Boolean;
  TempText : string;
begin
  //modifica il testo del money-edit solo se la tabella è editabile
  if (Field <> nil) and (Field.CanModify) then
  begin
    TempText := Text;
    Negative := (Pos('-',TempText)<>0) or not (nvPositive in FNumberValues);
    if Negative then
      TempText := KillChar(TempText,'-');
    if TempText = '' then TempText := '0';
    //Elimino eventuali separatori decimali
    TempText := KillChar(TempText,FormatSettings.ThousandSeparator);
    //Elimino eventuale simbolo monetario
    TempText := KillSubString(FormatSettings.CurrencyString,TempText);
    //Elimino evetuali spazi
    TempText := KillChar(TempText,' ');
    if FFormatFloat <> '' then
      TempText := FormatFloat(FFormatFloat, StrToFloat(TempText));
    if Negative and (TempText[1]<>'-') then TempText := '-'+TempText;
    if (TempText <> Text) then
      Text := TempText;
  end;
end;

procedure TLabeledDBCurrencyEdit.SetFieldFormat;
var
  FEditformat : string;
begin
  //Come priorità utilizzo la EditMask del campo
  if Field.EditMask <> '' then
  begin
    FEditformat := Copy(Field.EditMask,1,length(Field.EditMask)-4);
  end
  else
  begin
    //per i campi numerici ho a disposizione anche EditFormat e DisplayFormat
    if (Field is TNumericField) then
    with TNumericField(Field) do
    begin
      //Se non esiste EditMask sul field uso la EditFormat
      if (FEditFormat = '') and (EditFormat <> '') then
      begin
        FEditformat := EditFormat;
      end
      //Se non esiste ne EditMask ne EditFormst uso la DisplayFormat
      else if (FEditFormat = '') and (DisplayFormat <> '') then
      begin
        FEditformat := DisplayFormat;
      end
    end;
  end;

  //tolgo dalla formatstring tutti i caratteri non validi alla formattazione float
  FFormatFloat := GetValidChars(FEditFormat,['#','0'..'9','-','+','.',',']);

  if pos('.', FFormatFloat) <> 0 then
  begin
    FDecimals := integer(StrLen( PChar(FFormatFloat) )) - pos('.', FFormatFloat);
    FDecimalOffset := 1;
  end
  else
  begin
    FDecimals := 0;
    FDecimalOffset := 0;
  end;

//  if Field.AsVariant = NULL then
  Reformat;
end;

procedure TLabeledDBCurrencyEdit.SetMaxLength(const Value: integer);
begin
  if Value <> FMaxLength then
  begin
    if Value > 0 then
      FMaxLength := Value else
      raise EComponentError.Create(ERR_MAX_LENGTH);
  end;
end;

procedure TLabeledDBCurrencyEdit.SetNumberValues(const Value: TNumberValues);
begin
  if FNumberValues <> Value then
  begin
    if Value <> [] then
      FNumberValues := Value else
      raise EComponentError.Create(ERR_NUM_VALUES);
  end;
end;

procedure TLabeledDBCurrencyEdit.ValidateEdit;
begin
  //Pulisco sempre i separatori delle migliaia dal testo
  Text := KillChar(Text,FormatSettings.ThousandSeparator);
  inherited;
end;

{ TLabeledCurrencyEdit }

function TLabeledCurrencyEdit.InitialPos : integer;
begin
  Result := Length(Text) - FDecimals - FDecimalOffset;
end;

function TLabeledCurrencyEdit.GetIsEmpty: Boolean;
begin
  Result := Inherited GetIsEmpty or (TextAsFloat = 0);
end;

procedure TLabeledCurrencyEdit.AddSigla;
begin
  if (FSigla <> '') and (pos(Fsigla,Text) = 0) then
    Text := Fsigla+' '+Text;
end;

constructor TLabeledCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF CBCLX}
  inherited Alignment := taRightJustify; //Allineamento di default a destra
{$ELSE}
  FAlignment := taRightJustify;
{$ENDIF}
  inherited AutoSelect := False;
  FDecimalOffset := 1;
  FDecimals := 2;
  FFormatString := CURRENCY_FORMAT; //NB: per la stringa di formattazione si usa sempre il .
  Text := '0'+FormatSettings.decimalSeparator+'00';
  FMaxLength := MAX_LEN_DEFAULT;
  FNumberValues := [nvPositive];
end;

{$IFNDEF CBCLX}
procedure TLabeledCurrencyEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or Alignments[FAlignment];
  end;
end;

procedure TLabeledCurrencyEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;
{$ENDIF}

function TLabeledCurrencyEdit.DeleteSigla(const MyTesto,
  MySigla: string): string;
begin
  Result := MyTesto;
  if pos(MySigla, MyTesto) <> 0 then
  begin
    //elimina la sigla
    delete(Result, Pos(MySigla+' ', Result), Length(MySigla+' '));
  end;
end;

procedure TLabeledCurrencyEdit.DoEnter;
begin
  Text := DeleteSigla(Text, FSigla);
  inherited;
  SelLength := 0;
  SelStart := InitialPos;
  FDeleteBuffer := True;
end;

procedure TLabeledCurrencyEdit.DoExit;
begin
  if FSigla <> '' then
    AddSigla;
  inherited;
end;

function TLabeledCurrencyEdit.GetTextAsFloat: Extended;
var
  Testo : string;
begin
  Testo := Text;
  //elimina eventualmente la sigla
  Testo := DeleteSigla( Testo, FSigla );
  //elimina i punti decimali
  Result := StrToFloat(StripCommas(Testo, FDecimals));
end;

procedure TLabeledCurrencyEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  CursorPos: integer;
  Buffer: string;
  B1,B2 : string;
begin
  inherited KeyDown(Key,Shift);
  if ReadOnly then Exit;

  //Se il numero è il primo tasto premuto e il cursore è sulla virgola prima azzera il valore precedente
  //Se il numero è il primo tasto premuto e il cursore è sulla virgola prima azzera il valore precedente
  if (FDeleteBuffer) and isKeyDownNumber(Key) and
    ((Text = '') or (SelStart = InitialPos)) then
  begin
    FDeleteBuffer := False;
    Text := '';
    Reformat;
    SelStart := InitialPos;
  end
  //Se c'è una selezione estesa prima cancella il contenuto
  else if (SelLength > 0) and (Shift=[]) and
    ((isKeyDownNumber(Key)) or (Key = VK_DELETE)) then
  begin
    B1 := Copy(Text,0,SelStart);
    B2 := Copy(Text,SelStart+SelLength+1,1000);
    if Key = VK_DELETE then Key := 0;
    CursorPos := SelStart;
    FDeleteBuffer := False;
    Text := B1+B2;
    Reformat;
    Buffer := Text;
    if StrToFloat(StripCommas(Buffer, FDecimals))=0 then
      SelStart := InitialPos else
      SelStart := CursorPos;
    SelLength := 0;
  end
  //Se viene premuto Canc cancella una cifra
  else if Key = VK_DELETE then
  begin
    Buffer := Text;
    CursorPos := Length(Buffer)-SelStart-SelLength;
    if CursorPos > FDecimals+FDecimalOffset then
      begin
        if FDecimals=0 then CursorPos := CursorPos +1;
        if (CursorPos-FDecimals-1) mod 4 = 0 then Dec(CursorPos);
        if FDecimals=0 then CursorPos := CursorPos -1;
        Delete( Buffer, Length(Buffer)-CursorPos+1, 1 );
        Dec(CursorPos);
      end
    else
    begin
      if CursorPos = FDecimals+FDecimalOffset then Dec(CursorPos);
      if CursorPos > 0 then
      begin
        Delete( Buffer, Length(Buffer)-CursorPos+FDecimalOffset, 1 );
        Insert( '0', Buffer, Length(Buffer)-CursorPos+FDecimalOffset+1 );
        Dec(CursorPos);
      end;
    end;
    Key := 0;
    Text := Buffer;
    Reformat;
    SelStart := Length(Text)-CursorPos;
  end
  else
    FDeleteBuffer := False;
end;

procedure TLabeledCurrencyEdit.KeyPress(var Key: char);
var
  Buffer: string;
  CursorPos: integer;
begin
  if (Key='.') or (Key=',') then
    Key := FormatSettings.DecimalSeparator;

  inherited KeyPress(Key);
  if ReadOnly then Exit;
  Buffer := Text;
  CursorPos := Length(Buffer)-SelStart-SelLength;
  case Key of
  '0'..'9':
    begin
    if CursorPos >= FDecimals+FDecimalOffset then
      begin
        if Length(Buffer) < FMaxLength  then
          Insert( Key, Buffer, Length(Buffer)-CursorPos+1 );
        end
      else if CursorPos >= 1 then
      begin
        Delete( Buffer, Length(Buffer)-CursorPos+1 , 1 );
        Insert( Key, Buffer, Length(Buffer)-CursorPos+2 );
        if 1-CursorPos <= 0 then Dec(CursorPos);
      end;
    end;
  #8: {BACKSPACE}
    begin
    if CursorPos > FDecimals then
      begin
        if FDecimals=0 then CursorPos := CursorPos +1;
        if (CursorPos-FDecimals) mod 4 = 0 then Inc(CursorPos);
        if FDecimals=0 then CursorPos := CursorPos -1;
        Delete( Buffer, Length(Buffer)-CursorPos, 1 );
      end
    else
      begin
        if (CursorPos = FDecimals) and (FDecimals<>0) then Inc(CursorPos);
        Delete( Buffer, Length(Buffer)-CursorPos, 1 );
        if (CursorPos <= FDecimals) and (FDecimals<>0) then
        begin
          Insert( '0', Buffer, Length(Buffer)-CursorPos+1 );
          Inc(CursorPos);
        end;
      end;
    end;
  '.',',':
    CursorPos := FDecimals;
    '-':
      if (nvNegative in FNumberValues) then
      begin
        //aggiunge il segno meno
        if (pos('-',Buffer) = 0) then
        begin
          if Length(Buffer) < FMaxLength then Buffer := '-'+Buffer;
        end;
      end;
    '+':
      if (nvPositive in FNumberValues) then
      begin
        //toglie il segno meno
        if (pos('-',Buffer) <> 0) then
        begin
          Buffer := KillChar(Buffer,'-');
        end;
      end;
  #27: {ESCAPE}
    CursorPos := FDecimals + FDecimalOffset;
  end;
  Key := #0;
  Text := Buffer;
  Reformat;
  SelStart := Length(Text)-CursorPos;
end;

procedure TLabeledCurrencyEdit.Reformat;
var
  P : integer;
  Negative : Boolean;
  TempText : string;
begin
  TempText := Text;
  P := Pos(FSigla, TempText); //rimuove eventualmente la sigla
  if P <> 0 then
    TempText := Trim(Copy(TempText,1,P-1)+Copy(TempText,P+length(FSigla),Length(TempText)));
  Negative := (Pos('-',TempText)<>0) or not (nvPositive in FNumberValues);
  if Negative then
    TempText := KillChar(TempText,'-');
  if TempText = '' then TempText := '0';
  if FFormatString <> '' then
    TempText := FormatFloat(FFormatString, StrToFloat(KillChar(TempText,FormatSettings.ThousandSeparator)));
  if Negative and (TempText[1]<>'-') then TempText := '-'+TempText;
  //rimette eventualmente la sigla
  Text := TempText;
  if P <> 0 then
    AddSigla;
end;

procedure TLabeledCurrencyEdit.SetDecimals(const Value: integer);
var
  i: integer;
begin
  if ( Value >= 0 ) and ( Value < FMaxLength-1 ) then FDecimals := Value;
  FFormatString := '#,##0';
  if FDecimals > 0 then
  begin
    FFormatString := FFormatString+'.';
    FDecimalOffset := 1;
  end
  else
  begin
    FDecimalOffset := 0;
  end;

  for i := 1 to FDecimals do FFormatString := FFormatString + '0';
  MaxLength := MaxLength;
  Reformat;
end;

procedure TLabeledCurrencyEdit.SetMaxLength(const Value: integer);
begin
  if Value <> FMaxLength then
  begin
    if Value > 0 then
      FMaxLength := Value else
      raise EComponentError.Create(ERR_MAX_LENGTH);
  end;
end;

procedure TLabeledCurrencyEdit.SetNumberValues(const Value: TNumberValues);
begin
  if FNumberValues <> Value then
  begin
    if Value <> [] then
      FNumberValues := Value else
      raise EComponentError.Create(ERR_NUM_VALUES);
  end;
end;

procedure TLabeledCurrencyEdit.SetSigla(const Value: string);
begin
  if FSigla <> Value then
  begin
    Text := DeleteSigla(Text, FSigla);
    FSigla := Value;
    AddSigla;
  end;
end;

procedure TLabeledCurrencyEdit.SetTextAsFloat(Value: Extended);
begin
  Text := FormatFloat(FFormatString, Value);
  AddSigla;
end;

end.
