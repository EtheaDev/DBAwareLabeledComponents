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
unit Vcl.LabeledGraphicUtils;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

type
  /// <summary>
  ///  Enumerates the graphic file formats recognized by the unit.
  ///  gffUnknown indicates an unrecognized or undetected format.
  /// </summary>
  TGraphicFileFormat = (gffUnknown, gffBmp, gffTiff, gffJpeg, gffPng,
    gffDcx, gffPcx, gffEmf, gffGif, gffIco);

const
  /// <summary>
  ///  Maps each TGraphicFileFormat value to its default file extension
  ///  (without the leading dot). gffUnknown maps to an empty string.
  /// </summary>
  AGraphicFileFormatExt : Array[TGraphicFileFormat] of string =
    ('', 'bmp', 'tif', 'jpg', 'png', 'dcx', 'pcx', 'emf', 'gif', 'ico');

/// <summary>
///  Detects the graphic file format of a stream by inspecting the magic-number
///  signature in its leading bytes, without altering the stream contents.
/// </summary>
/// <param name="AStream">
///  Source stream to inspect. Its Position is reset to 0 on entry; for a bitmap
///  preceded by a TGraphicHeader the Position is advanced past the header.
/// </param>
/// <returns>
///  The detected TGraphicFileFormat, or gffUnknown when the stream is nil,
///  too short, or its signature is not recognized.
/// </returns>
function ResolveGraphicFileType( AStream: TStream ): TGraphicFileFormat;
/// <summary>
///  Returns the TGraphicClass registered for the given file format.
/// </summary>
/// <param name="GraphicFileFormat">The format whose graphic class is requested.</param>
/// <returns>The registered TGraphicClass, or nil when none is registered.</returns>
function GraphicFileFormatToGraphicClass(GraphicFileFormat : TGraphicFileFormat) : TGraphicClass;
/// <summary>
///  Associates a TGraphicClass with a graphic file format so the unit can
///  instantiate the proper graphic when loading streams of that format.
/// </summary>
/// <param name="GraphicFileFormat">The format to register the class for.</param>
/// <param name="AGraphicClass">The graphic class to associate with the format.</param>
procedure RegisterGraphicClass(GraphicFileFormat : TGraphicFileFormat; AGraphicClass: TGraphicClass);
/// <summary>
///  Detects the format of the source stream, creates the matching registered
///  graphic, loads it and assigns it to the destination picture.
/// </summary>
/// <param name="Source">Stream containing the image data to load.</param>
/// <param name="Dest">Picture that receives the loaded image.</param>
/// <param name="RaiseException">
///  When True, raises EComponentError (UNSUPPORTED_FILE) for a recognized format.
/// </param>
/// <returns>The detected TGraphicFileFormat, or gffUnknown when not recognized.</returns>
function LoadPictureFromStream(Source: TStream; Dest: TPicture; RaiseException: boolean): TGraphicFileFormat;
/// <summary>
///  Returns the default file extension for a format, including the leading dot
///  (for example '.bmp').
/// </summary>
/// <param name="GraphicFileFormat">The format whose default extension is requested.</param>
/// <returns>The dotted default extension string.</returns>
function GetDefaultExt(GraphicFileFormat: TGraphicFileFormat): string;
/// <summary>
///  Builds a single Open/Save dialog filter entry for the given format using
///  the EXT_FILES_FILTER resource string (description plus '*.ext' mask).
/// </summary>
/// <param name="GraphicFileFormat">The format the dialog filter is built for.</param>
/// <returns>The formatted dialog filter string for the format.</returns>
function GetDialogFilter(GraphicFileFormat: TGraphicFileFormat): string;
/// <summary>
///  Returns the file mask for the given format in the form '*.ext'.
/// </summary>
/// <param name="GraphicFileFormat">The format whose mask is requested.</param>
/// <returns>The '*.ext' mask string.</returns>
function GetExtFilter(GraphicFileFormat: TGraphicFileFormat): string;
/// <summary>
///  Builds a pipe-separated Open/Save dialog filter combining every currently
///  registered graphic format.
/// </summary>
/// <param name="IncludeAllPictures">
///  When True, prepends an "All pictures" entry (ALL_IMAGES_FILTER) listing the
///  masks of all registered formats.
/// </param>
/// <returns>The combined dialog filter string.</returns>
function GetRegisteredDialogFilters(IncludeAllPictures: Boolean = False): string;
/// <summary>
///  Converts a 6-character hexadecimal RRGGBB string into a TColor.
/// </summary>
/// <param name="sColor">The hexadecimal color string (6 hex digits, RRGGBB).</param>
/// <returns>The corresponding TColor, or clBlack when the input is not 6 characters.</returns>
function HexToColor(Const sColor: string): TColor;
/// <summary>
///  Converts a TColor into its 6-character hexadecimal RRGGBB representation.
/// </summary>
/// <param name="Color">The color to convert.</param>
/// <returns>The RRGGBB hexadecimal string.</returns>
function ColorToHex(Color: TColor): string;
/// <summary>
///  Replaces the unit-wide list of custom colors with a copy of the supplied
///  list; passing nil releases the current list.
/// </summary>
/// <param name="Colors">The colors to store, or nil to clear the custom colors.</param>
procedure RegisterCustomColors(Colors: TStrings);
/// <summary>
///  Returns the unit-wide list of custom colors previously registered.
/// </summary>
/// <returns>The custom colors list, or nil when none has been registered.</returns>
function GetCustomColors: TStrings;
/// <summary>
///  Computes a contrasting color for the given color, suitable for drawing
///  readable text or foreground content over it.
/// </summary>
/// <param name="Color">The background color to contrast against.</param>
/// <param name="UseBW">
///  When True, returns either clBlack or clWhite based on the perceived
///  luminance (YIQ); when False, returns the complementary color.
/// </param>
/// <returns>The contrasting TColor.</returns>
function HighlightColor(const Color: TColor; UseBW: Boolean = False): TColor;
/// <summary>
///  Resizes a bitmap in place to the requested dimensions using a stretched
///  draw through a temporary buffer.
/// </summary>
/// <param name="Bitmap">The bitmap to resize in place.</param>
/// <param name="NewWidth">The target width in pixels.</param>
/// <param name="NewHeight">The target height in pixels.</param>
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);

implementation

uses
  Vcl.DbAwareLabeledConsts;

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
  if (Result=gffUnknown) and raiseException then
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
    //Use black and white
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
    //Use the complementary color
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
