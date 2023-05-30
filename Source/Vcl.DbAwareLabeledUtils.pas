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
unit Vcl.DbAwareLabeledUtils;

interface

uses
  System.TypInfo
  , Vcl.Graphics
  ;

{$WARN WIDECHAR_REDUCED OFF}
type
  TSetOfChar = set of Char;

function StrSubstChar( OldChar, NewChar: Char; const S: string ) : String;
function ValidPath(const Path : string) : string;
procedure CopyFileEx(const SourceFileName, DestFileName: string; FailIfExists: Boolean = True);
function SamePath(const ASourcePath, ADestPath: string): Boolean;
function CopyToEnd(const S: string; StartIndex: Integer): string;
function GetStringPropCheck(Instance: TObject; const PropName: string ) : string;
procedure SetStringPropCheck(Instance: TObject; const PropName: string; const Value: string);
function StripCommas(AString: string; FDecimals: word): string;
function KillChar( const S : string; Ch : Char ) : string;
function KillSubString( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : string;
function PosChar( Ch : Char; const S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
function PosStr( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
function GetValidChars( const S : string; ValidChars : TSetOfChar ) : string;
function DATE_FORMAT : string;
function TIME_HH_MM_SS_FORMAT : string;
function TIME_HH_MM_FORMAT : string;
function DATE_TIME_HH_MM_FORMAT : string;
function DATE_TIME_HH_MM_SS_FORMAT : string;
function KillChars( const S : string; Chars : TSetOfChar ) : string;
function DateTimeHHMMtoStr(Data : TDateTime) : string;
function DateTimeHHMMSStoStr(Data : TDateTime) : string;
function IsVCLStyled: boolean;
function GetStyledColor(Color: TColor): TColor;
function PadL(Const InString: String; Len: Integer; FChar: Char): String;
function PadR(Const InString: String; Len: Integer; FChar: Char): String;

function LightenColor(Color: TColor; Percentage: Cardinal): TColor;

implementation

uses
  System.SysUtils
  , WinApi.Windows
  , Vcl.DbAwareLabeledConsts
  , Vcl.Controls
  , Vcl.Themes
  , Vcl.GraphUtil
  ;

//sostituisce un carattere in un altro all'interno di una stringa (zero-based)
function StrSubstChar( OldChar, NewChar: Char; const S: string ) : String;
var
  k: integer;
  T: String;
begin
  T := S;
  if OldChar <> NewChar then
  begin
    while true do
    begin
      k := Pos(OldChar, T);
      if k > 0 then
        T[k] := NewChar
      else
        Break;
    end;
  end;
  Result := T;
end;

function ValidPath(const Path : string) : string;
begin
  Result := StrSubstChar('\',PathDelim,Path);
  Result := StrSubstChar('/',PathDelim,Path);
  if (Result <> '') and (Result[length(Result)]<>PathDelim) then
    Result := Result+PathDelim;
end;

procedure CopyFileEx(const SourceFileName, DestFileName: string; FailIfExists: Boolean = True);
begin
  if not CopyFile(PChar(SourceFileName),PChar(DestFileName), FailIfExists) then
    raise Exception.CreateFmt( ERR_COPY_FILE, [SourceFileName, DestFileName] );
end;

function SamePath(const ASourcePath, ADestPath: string): Boolean;
begin
  Result := SameText(ExpandUNCFileName(ASourcePath), ExpandUNCFileName(ADestPath));
end;

function CopyToEnd(const S: string; StartIndex: Integer): string;
begin
  Result := Copy( S, StartIndex, MaxInt );
end;

function GetStringPropCheck(Instance: TObject; const PropName: string ) : string;
begin
  if IsPublishedProp(Instance, PropName) then
    Result := GetStrProp(Instance, PropName)
  else
    Result := '';
end;

procedure SetStringPropCheck(Instance: TObject; const PropName: string; const Value: string);
begin
  if IsPublishedProp(Instance, PropName) then
    SetStrProp(Instance, PropName, Value);
end;

function StripCommas(AString: string; FDecimals: word): string;
begin
  while (Pos(FormatSettings.ThousandSeparator, AString) > 0 ) do
    delete(AString, Pos(FormatSettings.ThousandSeparator, AString), 1);
  while (Pos(FormatSettings.DecimalSeparator, AString) > 0 ) do
    delete(AString, Pos(FormatSettings.DecimalSeparator, AString), 1);
  Try
    if (AString <> '') then
    begin
      strtofloat(AString);
      Insert( FormatSettings.DecimalSeparator, AString, Length(AString)-FDecimals+1 );
      Result := AString;
    end
    else
      Result := '0';
  Except
    On EConvertError do Result := '0' else raise;
  End;
end;

function KillChar( const S : string; Ch : Char ) : string;
var
  I, Count: integer;
begin
  SetLength( Result, Length(S) ); // imposta la lunghezza per prevenire la riallocazione
  Count := 0; // numero di caratteri copiati nella stringa risultato
  for I := 1 to Length(S) do
  begin
    if S[I] <> Ch then // il carattere non è fra quelli da eliminare
    begin
      // aggiunge il carattere alla stringa risultato
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // imposta la lunghezza della stringa con i caratteri effettivamente copiati
end;

function KillSubString( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : string;
var
  I : Integer;
begin
  I := PosStr( SubStr, S, CaseSensitive, StartIndex );
  if I = 0 then // substringa non trovata nella stringa S a partire da StartIndex
  begin
    Result := S;
    Exit;
  end;
  // substringa trovata, copia la parte della stringa prima e dopo la substringa trovata
  Result := Copy(S,1,I-1) + Copy(S,I+Length(SubStr),MaxInt);
end;

function PosChar( Ch : Char; const S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
var
  I : Integer;
  UpC : Char;
begin
  if CaseSensitive then
  begin
    for I := StartIndex to Length(S) do
    begin
      if S[I] = Ch then // carattere trovato nella stringa
      begin
        Result := I;
        Exit;
      end;
    end;
  end
  else
  begin
    UpC := UpCase(Ch); // converte il carattere in maiuscolo
    for I := StartIndex to Length(S) do
    begin
      if UpCase(S[I]) = UpC then // carattere trovato nella stringa
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
  // carattere non trovato nella stringa
  Result := 0;
end;

function PosStr( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
var
  I, J, PosInS : Integer;
  Trovata : Boolean;
begin
  if SubStr = '' then
  begin
    Result := 0;
    Exit;
  end;
  repeat
    // cerca il primo carattere della substringa nella stringa S partendo da StartIndex
    I := PosChar( SubStr[1], S, CaseSensitive, StartIndex );
    if I = 0 then // carattere non trovato
    begin
      Result := 0;
      Exit;
    end;
    // il primo carattere della substringa è stato trovato nella stringa S, verifica che i seguenti caratteri siano uguali
    Trovata := True;
    for J := 2 to Length(SubStr) do
    begin
      PosInS := I+Pred(J); // calcola la posizione nella stringa S del carattere da confrontare
      if PosInS > Length(S) then // è terminata la stringa S
        Trovata := False
      else
      begin
        if CaseSensitive then
        begin
          if SubStr[J] <> S[PosInS] then // uno dei caratteri successivi è diverso
            Trovata := False
        end
        else
        begin
          if UpCase( SubStr[J] ) <> UpCase( S[PosInS] ) then // uno dei caratteri successivi è diverso
            Trovata := False
        end;
      end;
      if not Trovata then
      begin
        StartIndex := Succ(I); // aggiorna l'indice di partenza per la successiva ricerca della substringa nella stringa S
        Break; // interrompe il ciclo di verifica dei caratteri della substringa con quelli della stringa S
      end;
    end;
  until Trovata;
  // substringa trovata
  Result := I;
end;

function GetValidChars( const S : string; ValidChars : TSetOfChar ) : string;
var
  I, Count: integer;
begin
  SetLength( Result, Length(S) ); // imposta la lunghezza per prevenire la riallocazione
  Count := 0; // numero di caratteri copiati nella stringa risultato
  for I := 1 to Length(S) do
  begin
    if (S[I] in ValidChars) then // il carattere è fra quelli da tenere
    begin
      // aggiunge il carattere alla stringa risultato
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // imposta la lunghezza della stringa con i caratteri effettivamente copiati
end;

function DATE_FORMAT : string;
begin
  if LowerCase(FormatSettings.ShortDateFormat[1]) = 'd' then
    Result := 'dd'+FormatSettings.DateSeparator+'mm'
  else
    Result := 'mm'+FormatSettings.DateSeparator+'dd';

  if pos(LowerCase('yyyy'),LowerCase(FormatSettings.ShortDateFormat)) <> 0 then
    Result := Result + FormatSettings.DateSeparator + 'yyyy'
  else
    Result := Result + FormatSettings.DateSeparator + 'yy';
end;

function TIME_HH_MM_SS_FORMAT : string;
begin
  Result := 'hh'+FormatSettings.TimeSeparator+'mm'+FormatSettings.TimeSeparator+'ss';
end;

function TIME_HH_MM_FORMAT : string;
begin
  Result := 'hh'+FormatSettings.TimeSeparator+'mm';
end;

function DATE_TIME_HH_MM_FORMAT : string;
begin
  Result := DATE_FORMAT + ' '+TIME_HH_MM_FORMAT;
end;

function DATE_TIME_HH_MM_SS_FORMAT : string;
begin
  Result := DATE_FORMAT + ' '+TIME_HH_MM_SS_FORMAT;
end;

function KillChars( const S : string; Chars : TSetOfChar ) : string;
var
  I, Count: integer;
begin
  SetLength( Result, Length(S) ); // imposta la lunghezza per prevenire la riallocazione
  Count := 0; // numero di caratteri copiati nella stringa risultato
  for I := 1 to Length(S) do
  begin
    if not (S[I] in Chars) then // il carattere non è fra quelli da eliminare
    begin
      // aggiunge il carattere alla stringa risultato
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // imposta la lunghezza della stringa con i caratteri effettivamente copiati
end;

function DateTimeHHMMtoStr(Data : TDateTime) : string;
var
  DataStr : string;
begin
  DateTimeToString(DataStr, DATE_TIME_HH_MM_FORMAT+FormatSettings.TimeSeparator+'00', Data);
  Result := Copy(DataStr,1,length(DataStr)-3);
end;

function DateTimeHHMMSStoStr(Data : TDateTime) : string;
begin
  DateTimeToString(Result, DATE_TIME_HH_MM_SS_FORMAT, Data);
end;

function IsVCLStyled: boolean;
begin
  Result := StyleServices.Enabled and (TStyleManager.ActiveStyle.Name <> STYLE_WINDOWS);
end;

function GetStyledColor(Color: TColor): TColor;
begin
  if IsVCLStyled then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := Color;
end;

function PadL(Const InString: String; Len: Integer; FChar: Char): String;
begin
  Result := StringOfChar(FChar,Len-Length(InString)) + InString;
end;

function PadR(Const InString: String; Len: Integer; FChar: Char): String;
begin
  Result := InString + StringOfChar(FChar,Len-Length(InString));
end;

function LightenColor(Color: TColor; Percentage: Cardinal): TColor;
var
  rgb: LongInt;
  h, s, l: Word;
begin
  rgb := ColorToRGB(Color);
  ColorRGBToHLS(rgb, h, l, s);
  l := (Cardinal(l) * Percentage) div 100;
  Result := TColor(ColorHLSToRGB(h, l, s));
end;

end.
