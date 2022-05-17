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
unit Vcl.LabeledGraphicUtils;

interface

uses
  WinApi.Windows
  , System.SysUtils
  , System.Classes
  , Vcl.Graphics
  ;

type
  TGraphicFileFormat = (gffUnknown, gffBmp, gffTiff, gffJpeg, gffPng,
    gffDcx, gffPcx, gffEmf, gffGif, gffIco);

const
  AGraphicFileFormatExt : Array[TGraphicFileFormat] of string =
    ('', 'bmp', 'tif', 'jpg', 'png', 'dcx', 'pcx', 'emf', 'gif', 'ico');

function ResolveGraphicFileType( AStream: TStream ): TGraphicFileFormat;
function GraphicFileFormatToGraphicClass(GraphicFileFormat : TGraphicFileFormat) : TGraphicClass;
procedure RegisterGraphicClass(GraphicFileFormat : TGraphicFileFormat; AGraphicClass: TGraphicClass);
function LoadPictureFromStream(Source: TStream; Dest: TPicture; RaiseException: boolean): TGraphicFileFormat;
function GetDefaultExt(GraphicFileFormat: TGraphicFileFormat): string;
function GetDialogFilter(GraphicFileFormat: TGraphicFileFormat): string;
function GetExtFilter(GraphicFileFormat: TGraphicFileFormat): string;
function GetRegisteredDialogFilters(IncludeAllPictures: Boolean = False): string;
function HexToColor(Const sColor: string): TColor;
function ColorToHex(Color: TColor): string;
procedure RegisterCustomColors(Colors: TStrings);
function GetCustomColors: TStrings;
function HighlightColor(const Color: TColor; UseBW: Boolean = False): TColor;
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);

implementation

uses
  Vcl.DbAwareLabeledConsts
  ;

var
  GraphicClassList: array[TGraphicFileFormat] OF TGraphicClass;
  CustomColors: TStringList;

function ResolveGraphicFileType(AStream: TStream): TGraphicFileFormat;
const
  SizeOfGraphicHeader = 8;
  MinimumBytesToRead = 10;
var
  P: array [0..MinimumBytesToRead - 1] of Byte;
  StreamLength: Longint;
  BytesRetrieved: Integer;
begin
  Result := gffUnknown;
  if not Assigned(AStream) then
    Exit;
  StreamLength := AStream.Size;
  AStream.Position := 0;
  FillChar(P, SizeOf(P), 0);
  BytesRetrieved := AStream.Read(P[0], SizeOf(P));
  AStream.Position := 0;
  if BytesRetrieved < MinimumBytesToRead then
    Exit;
  // bitmap format
  if (P[0] = 66) and (P[1] = 77) then
    Result := gffBmp
  // tiff format
  else if ((P[0] = 73) and (P[1] = 73) and (P[2] = 42) and (P[3] = 0))
   or ((P[0] = 77) and (P[1] = 77) and (P[2] = 42) and (P[3] = 0)) then
    Result := gffTiff
  // jpg format
  else if (P[6] = 74) and (P[7] = 70) and (P[8] = 73) and (P[9] = 70)
   or (P[6] = 69) and (P[7] = 120) and (P[8] = 105) and (P[9] = 102) then
    Result := gffJpeg
  // png format
  else if (P[0] = 137 ) and (P[1] = 80) and (P[2] = 78) and (P[3] = 71)
   and (P[4] = 13) and (P[5] = 10) and (P[6] = 26) and (P[7] = 10) then
    Result := gffPng
  // dcx format
  else if (P[0] = 177) and (P[1] = 104) and (P[2] = 222) and (P[3] = 58) then
    Result := gffDcx
  // pcx format
  else if p[0] = 10 then
    Result := gffPcx
  // emf format
  else if ((P[0] = 215) and (P[1] = 205) and (P[2] = 198) and (P[3] = 154))
   or ((P[0] = 1) and (P[1] = 0) and (P[2] = 0) and (P[3] = 0)) then
    Result := gffEmf
  // gif format
  else if (P[0] = $47) and (P[1] = $49) and (P[2] = $46) then
    Result := gffGif
  // Ico format
  else if (P[0] = 00) and (P[1] = 00) and (P[2] = 01) and (P[3] = 00) then
    Result := gffIco
  // bitmap format with TGraphicHeader header
  else if (P[0] = 01) and (P[1] = 00) and (P[2] = 00) and (P[3] = 01)
   and (PLongint(@p[4])^ = StreamLength - SizeOfGraphicHeader) then
  begin
    Result := gffBmp;
    AStream.Position := SizeOfGraphicHeader;
  end;
end;

function GraphicFileFormatToGraphicClass(GraphicFileFormat : TGraphicFileFormat) : TGraphicClass;
begin
  Result := GraphicClassList[GraphicFileFormat];
end;

procedure RegisterGraphicClass(GraphicFileFormat : TGraphicFileFormat; AGraphicClass: TGraphicClass);
begin
  GraphicClassList[GraphicFileFormat] := AGraphicClass;
end;

function LoadPictureFromStream(Source: TStream; Dest: TPicture; RaiseException: boolean): TGraphicFileFormat;
var
  Image : TGraphic;
  GraphicFileFormat: TGraphicFileFormat;
  GraphicClass: TGraphicClass;
begin
  GraphicFileFormat := ResolveGraphicFileType(Source);
  if GraphicFileFormat <> gffUnknown then
  begin
    GraphicClass := GraphicFileFormatToGraphicClass(GraphicFileFormat);
    Result := GraphicFileFormat;
    if GraphicClass <> nil then
    begin
      Image := GraphicClass.Create;
      Try
        Image.LoadFromStream(Source);
        Dest.Assign(Image);
      Finally
        Image.Free;
      End;
    end;
  end
  else
    Result := gffUnknown;
  if not (Result=gffUnknown) and raiseException then
    raise EComponentError.Create(UNSUPPORTED_FILE);
end;

function GetDefaultExt(GraphicFileFormat: TGraphicFileFormat): string;
begin
  Result := '.'+AGraphicFileFormatExt[GraphicFileFormat];
end;

function GetDialogFilter(GraphicFileFormat: TGraphicFileFormat): string;
var
  Ext: string;
begin
  Ext := AGraphicFileFormatExt[GraphicFileFormat];
  Result := Format(EXT_FILES_FILTER,[Ext,Ext]);
end;

function GetExtFilter(GraphicFileFormat: TGraphicFileFormat): string;
var
  Ext: string;
begin
  Ext := AGraphicFileFormatExt[GraphicFileFormat];
  Result := Format('*.%s',[Ext]);
end;

function GetRegisteredDialogFilters(IncludeAllPictures: Boolean = False): string;
var
  i : TGraphicFileFormat;
  AllPictures : string;
begin
  Result := '';
  for i := Low(TGraphicFileFormat) to high(TGraphicFileFormat) do
  begin
    if Assigned(GraphicClassList[i]) then
    begin
      if Result <> '' then
        Result := Result + '|';
      Result := Result + GetDialogFilter(i);
      if IncludeAllPictures then
      begin
        if AllPictures <> '' then
          AllPictures := AllPictures + ';';
        AllPictures := AllPictures + GetExtFilter(i);
      end;
    end;
  end;
  if IncludeAllPictures then
    Result := Format(ALL_IMAGES_FILTER,[AllPictures,AllPictures])+'|'+Result;
end;

function ColorToHex(Color : TColor) : string;
begin
   Result :=
     IntToHex(GetRValue(Color), 2) +
     IntToHex(GetGValue(Color), 2) +
     IntToHex(GetBValue(Color), 2) ;
end;

function HexToColor(Const sColor : string) : TColor;
begin
  if Length(Trim(sColor)) = 6 then
      Result :=
       RGB(
         StrToInt('$'+Copy(sColor, 1, 2)),
         StrToInt('$'+Copy(sColor, 3, 2)),
         StrToInt('$'+Copy(sColor, 5, 2))
       )
  else
    Result := clBlack;
end;

procedure RegisterCustomColors(Colors: TStrings);
begin
  if not Assigned(Colors) then
    CustomColors.Free
  else
  begin
    if not Assigned(CustomColors) then
      CustomColors := TStringList.Create;
    CustomColors.Assign(Colors);
  end;
end;

function GetCustomColors: TStrings;
begin
  Result := CustomColors;
end;

function HighlightColor(const Color: TColor; UseBW: Boolean = False): TColor;
var
  r, g, b, yiq: integer;
  LColor: TColor;
begin
  LColor := ColorToRGB(Color);
  if UseBW then
  begin
    //Usa bianco e nero
    r := GetRValue(LColor);
    g := GetGValue(LColor);
  	b := GetBValue(LColor);
	  yiq := ((r*299)+(g*587)+(b*114)) div 1000;
	  if (yiq >= 128) then
      result := clBlack
    else
      result := clWhite;
  end
  else
  begin
    //Usa colore complementare
    result := TColor(WinApi.Windows.RGB(255 - GetRValue(LColor),
                                 255 - GetGValue(LColor),
                                 255 - GetBValue(LColor)));
  end;
end;

procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  buffer: TBitmap;
begin
  buffer := TBitmap.Create;
  try
    buffer.SetSize(NewWidth, NewHeight);
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;

initialization
  GraphicClassList[gffIco] := Vcl.Graphics.TIcon;
  GraphicClassList[gffBmp] := Vcl.Graphics.TBitmap;

finalization
  CustomColors.Free;
  
end.
