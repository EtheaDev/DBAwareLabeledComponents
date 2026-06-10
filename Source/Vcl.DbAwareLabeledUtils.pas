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
unit Vcl.DbAwareLabeledUtils;

interface

uses
  System.TypInfo,
  Vcl.Graphics;

{$WARN WIDECHAR_REDUCED OFF}
type
  /// <summary>
  ///  Set of characters, used by the character filtering helpers (KillChars, GetValidChars).
  /// </summary>
  TSetOfChar = set of Char;

/// <summary>
///  Replaces every occurrence of OldChar with NewChar inside the string S.
/// </summary>
/// <param name="OldChar">The character to be replaced.</param>
/// <param name="NewChar">The character to substitute in place of OldChar.</param>
/// <param name="S">The source string.</param>
/// <returns>A new string with all OldChar occurrences replaced by NewChar.</returns>
function StrSubstChar( OldChar, NewChar: Char; const S: string ) : String;
/// <summary>
///  Normalizes a path by converting both '\' and '/' separators to the platform
///  PathDelim and ensures the result is terminated by a trailing PathDelim.
/// </summary>
/// <param name="Path">The path to normalize.</param>
/// <returns>The normalized path ending with PathDelim, or an empty string if Path is empty.</returns>
function ValidPath(const Path : string) : string;
/// <summary>
///  Copies a file from SourceFileName to DestFileName, raising an exception on failure.
/// </summary>
/// <param name="SourceFileName">Full path of the source file.</param>
/// <param name="DestFileName">Full path of the destination file.</param>
/// <param name="FailIfExists">When True the copy fails if the destination already exists.</param>
procedure CopyFileEx(const SourceFileName, DestFileName: string; FailIfExists: Boolean = True);
/// <summary>
///  Determines whether two paths refer to the same location, comparing their
///  expanded UNC forms case-insensitively.
/// </summary>
/// <param name="ASourcePath">The first path to compare.</param>
/// <param name="ADestPath">The second path to compare.</param>
/// <returns>True if both paths resolve to the same location.</returns>
function SamePath(const ASourcePath, ADestPath: string): Boolean;
/// <summary>
///  Returns the substring of S from StartIndex through the end of the string.
/// </summary>
/// <param name="S">The source string.</param>
/// <param name="StartIndex">The 1-based position to start copying from.</param>
/// <returns>The portion of S starting at StartIndex.</returns>
function CopyToEnd(const S: string; StartIndex: Integer): string;
/// <summary>
///  Reads a published string property by name, returning an empty string when the
///  property does not exist on the instance.
/// </summary>
/// <param name="Instance">The object to read the property from.</param>
/// <param name="PropName">The name of the published property.</param>
/// <returns>The property value, or an empty string if the property is not published.</returns>
function GetStringPropCheck(Instance: TObject; const PropName: string ) : string;
/// <summary>
///  Writes a published string property by name, doing nothing when the property
///  does not exist on the instance.
/// </summary>
/// <param name="Instance">The object whose property is set.</param>
/// <param name="PropName">The name of the published property.</param>
/// <param name="Value">The value to assign.</param>
procedure SetStringPropCheck(Instance: TObject; const PropName: string; const Value: string);
/// <summary>
///  Removes the locale thousand and decimal separators from a numeric string and
///  reinserts the decimal separator FDecimals digits from the end, producing a
///  normalized numeric string.
/// </summary>
/// <param name="AString">The numeric string to process.</param>
/// <param name="FDecimals">The number of decimal digits.</param>
/// <returns>The reformatted numeric string, or '0' when the input is empty or invalid.</returns>
function StripCommas(AString: string; FDecimals: word): string;
/// <summary>
///  Returns a copy of S with every occurrence of the character Ch removed.
/// </summary>
/// <param name="S">The source string.</param>
/// <param name="Ch">The character to remove.</param>
/// <returns>The string without any Ch characters.</returns>
function KillChar( const S : string; Ch : Char ) : string;
/// <summary>
///  Returns a copy of S with the first occurrence of SubStr removed.
/// </summary>
/// <param name="SubStr">The substring to remove.</param>
/// <param name="S">The source string.</param>
/// <param name="CaseSensitive">When True the search is case sensitive.</param>
/// <param name="StartIndex">The 1-based position to start searching from.</param>
/// <returns>The string with the first matching substring removed, or S unchanged if not found.</returns>
function KillSubString( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : string;
/// <summary>
///  Finds the position of the character Ch within S, optionally case sensitive,
///  starting from StartIndex.
/// </summary>
/// <param name="Ch">The character to search for.</param>
/// <param name="S">The string to search in.</param>
/// <param name="CaseSensitive">When True the search is case sensitive.</param>
/// <param name="StartIndex">The 1-based position to start searching from.</param>
/// <returns>The 1-based position of Ch, or 0 if not found.</returns>
function PosChar( Ch : Char; const S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
/// <summary>
///  Finds the position of the substring SubStr within S, optionally case sensitive,
///  starting from StartIndex.
/// </summary>
/// <param name="SubStr">The substring to search for.</param>
/// <param name="S">The string to search in.</param>
/// <param name="CaseSensitive">When True the search is case sensitive.</param>
/// <param name="StartIndex">The 1-based position to start searching from.</param>
/// <returns>The 1-based position of SubStr, or 0 if not found or SubStr is empty.</returns>
function PosStr( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : Integer;
/// <summary>
///  Returns a copy of S keeping only the characters that belong to ValidChars.
/// </summary>
/// <param name="S">The source string.</param>
/// <param name="ValidChars">The set of characters to keep.</param>
/// <returns>The string containing only the allowed characters.</returns>
function GetValidChars( const S : string; ValidChars : TSetOfChar ) : string;
/// <summary>
///  Builds a short date format mask (day/month and two- or four-digit year)
///  based on the current locale FormatSettings.
/// </summary>
/// <returns>The locale-aware date format string.</returns>
function DATE_FORMAT : string;
/// <summary>
///  Returns the locale-aware time format mask with hours, minutes and seconds.
/// </summary>
/// <returns>The 'hh:mm:ss' format string using the locale time separator.</returns>
function TIME_HH_MM_SS_FORMAT : string;
/// <summary>
///  Returns the locale-aware time format mask with hours and minutes.
/// </summary>
/// <returns>The 'hh:mm' format string using the locale time separator.</returns>
function TIME_HH_MM_FORMAT : string;
/// <summary>
///  Returns the combined date and time format mask with hours and minutes.
/// </summary>
/// <returns>The date format followed by the 'hh:mm' time format.</returns>
function DATE_TIME_HH_MM_FORMAT : string;
/// <summary>
///  Returns the combined date and time format mask with hours, minutes and seconds.
/// </summary>
/// <returns>The date format followed by the 'hh:mm:ss' time format.</returns>
function DATE_TIME_HH_MM_SS_FORMAT : string;
/// <summary>
///  Returns a copy of S with every character that belongs to the set Chars removed.
/// </summary>
/// <param name="S">The source string.</param>
/// <param name="Chars">The set of characters to remove.</param>
/// <returns>The string without any of the specified characters.</returns>
function KillChars( const S : string; Chars : TSetOfChar ) : string;
/// <summary>
///  Formats a TDateTime value as a date with hours and minutes, dropping the seconds.
/// </summary>
/// <param name="Data">The date/time value to format.</param>
/// <returns>The formatted date and hh:mm string.</returns>
function DateTimeHHMMtoStr(Data : TDateTime) : string;
/// <summary>
///  Formats a TDateTime value as a date with hours, minutes and seconds.
/// </summary>
/// <param name="Data">The date/time value to format.</param>
/// <returns>The formatted date and hh:mm:ss string.</returns>
function DateTimeHHMMSStoStr(Data : TDateTime) : string;
/// <summary>
///  Indicates whether a non-default (non-Windows) VCL style is currently active.
/// </summary>
/// <returns>True when style services are enabled and the active style is not the default Windows style.</returns>
function IsVCLStyled: boolean;
/// <summary>
///  Returns the color adapted to the active VCL style, or the original color when
///  no custom style is in use.
/// </summary>
/// <param name="Color">The color to resolve.</param>
/// <returns>The style-aware system color, or Color unchanged when not styled.</returns>
function GetStyledColor(Color: TColor): TColor;
/// <summary>
///  Left-pads InString with FChar until it reaches the length Len.
/// </summary>
/// <param name="InString">The string to pad.</param>
/// <param name="Len">The desired total length.</param>
/// <param name="FChar">The padding character.</param>
/// <returns>The left-padded string.</returns>
function PadL(Const InString: String; Len: Integer; FChar: Char): String;
/// <summary>
///  Right-pads InString with FChar until it reaches the length Len.
/// </summary>
/// <param name="InString">The string to pad.</param>
/// <param name="Len">The desired total length.</param>
/// <param name="FChar">The padding character.</param>
/// <returns>The right-padded string.</returns>
function PadR(Const InString: String; Len: Integer; FChar: Char): String;

/// <summary>
///  Lightens the given color by reducing its luminance to the specified percentage,
///  using the HLS color model.
/// </summary>
/// <param name="Color">The base color.</param>
/// <param name="Percentage">The percentage of the original luminance to keep.</param>
/// <returns>The lightened color.</returns>
function LightenColor(Color: TColor; Percentage: Cardinal): TColor;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.DbAwareLabeledConsts,
  Vcl.Controls,
  Vcl.Themes,
  Vcl.GraphUtil;

//Replaces one character with another inside a string (zero-based)
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
  SetLength( Result, Length(S) ); // set the length up front to avoid reallocation
  Count := 0; // number of characters copied into the result string
  for I := 1 to Length(S) do
  begin
    if S[I] <> Ch then // the character is not one of those to be removed
    begin
      // append the character to the result string
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // set the string length to the characters actually copied
end;

function KillSubString( const SubStr, S : string; CaseSensitive : Boolean = False; StartIndex : Integer = 1 ) : string;
var
  I : Integer;
begin
  I := PosStr( SubStr, S, CaseSensitive, StartIndex );
  if I = 0 then // substring not found in S starting from StartIndex
  begin
    Result := S;
    Exit;
  end;
  // substring found, copy the part of the string before and after the matched substring
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
      if S[I] = Ch then // character found in the string
      begin
        Result := I;
        Exit;
      end;
    end;
  end
  else
  begin
    UpC := UpCase(Ch); // convert the character to uppercase
    for I := StartIndex to Length(S) do
    begin
      if UpCase(S[I]) = UpC then // character found in the string
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
  // character not found in the string
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
    // search for the first character of the substring in S starting from StartIndex
    I := PosChar( SubStr[1], S, CaseSensitive, StartIndex );
    if I = 0 then // character not found
    begin
      Result := 0;
      Exit;
    end;
    // the first character of the substring was found in S, verify that the following characters match
    Trovata := True;
    for J := 2 to Length(SubStr) do
    begin
      PosInS := I+Pred(J); // compute the position in S of the character to compare
      if PosInS > Length(S) then // the string S has ended
        Trovata := False
      else
      begin
        if CaseSensitive then
        begin
          if SubStr[J] <> S[PosInS] then // one of the following characters differs
            Trovata := False
        end
        else
        begin
          if UpCase( SubStr[J] ) <> UpCase( S[PosInS] ) then // one of the following characters differs
            Trovata := False
        end;
      end;
      if not Trovata then
      begin
        StartIndex := Succ(I); // update the start index for the next search of the substring in S
        Break; // stop the loop comparing the substring characters with those of S
      end;
    end;
  until Trovata;
  // substring found
  Result := I;
end;

function GetValidChars( const S : string; ValidChars : TSetOfChar ) : string;
var
  I, Count: integer;
begin
  SetLength( Result, Length(S) ); // set the length up front to avoid reallocation
  Count := 0; // number of characters copied into the result string
  for I := 1 to Length(S) do
  begin
    if (S[I] in ValidChars) then // the character is one of those to keep
    begin
      // append the character to the result string
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // set the string length to the characters actually copied
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
  SetLength( Result, Length(S) ); // set the length up front to avoid reallocation
  Count := 0; // number of characters copied into the result string
  for I := 1 to Length(S) do
  begin
    if not (S[I] in Chars) then // the character is not one of those to be removed
    begin
      // append the character to the result string
      Inc(Count);
      Result[Count] := S[I];
    end;
  end;
  SetLength( Result, Count ); // set the string length to the characters actually copied
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
