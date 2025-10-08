{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
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
unit Vcl.DBAwareLabeledComponentsRegistry;

{$I 'DBAwareLabeledComponents.inc'}
{$R ..\DbAwareLabeledComponentsSplash.res}

interface

procedure Register;

implementation

uses
  System.Classes,
  System.SysUtils,
  {$IF (CompilerVersion >= 27.0)}BrandingAPI,{$IFEND}
  ToolsAPI,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.LabeledComCtrls,
  Vcl.LabeledCtrls,
  Vcl.LabeledExtCtrls,
  Vcl.LabeledDBCtrls,
  Vcl.LabeledMask,
  Vcl.LabeledDbImage,
  Vcl.LabeledCheckLst,
  Vcl.LabeledColorGrd,
  Vcl.LabeledDBListView,
  {$IFDEF D10_4+}
  Vcl.DbNumberBox,
  Vcl.LabeledNumberBox,
  {$ENDIF}
  {$IFDEF D10_1+}
  Vcl.LabeledButtonEdit,
  {$ENDIF}
  Vcl.LabeledCurrencyEdit,
  Vcl.Imaging.PngImage
  ;

const
  ComponentsVersion = '1.3.6';
  Component_Docs_URL = 'https://ethea.it/docs/dbawarelabeledcomponents/Overview.html';
  {$IFDEF D11+}
  ABOUT_RES_NAME = 'DBAWARESPLASH48PNG';
  SPLASH_RES_NAME = 'DBAWARESPLASH48PNG';
  {$ELSE}
  ABOUT_RES_NAME = 'DBAWARESPLASH24BMP';
  SPLASH_RES_NAME = 'DBAWARESPLASH24BMP';
  {$ENDIF}
  RsAboutTitle = 'Ethea Labeled Components';
  RsAboutDescription = 'Ethea - DbAwareLabeled Components - https://ethea.it/docs/dbawarelabeledcomponents/' + sLineBreak +
    'Delphi-VCL advanced DBGrid plus labeled editors (DB-Aware and Standard) including DbNumberBox';
  RsAboutLicense = 'Apache 2.0 (Free/Opensource)';
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer;

{$IFDEF D11+}
function CreateBitmapFromPngRes(const AResName: string): Vcl.Graphics.TBitmap;
var
  LPngImage: TPngImage;
  LResStream: TResourceStream;
begin
  LPngImage := nil;
  try
    Result := Vcl.Graphics.TBitmap.Create;
    LPngImage := TPngImage.Create;
    LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
    try
      LPngImage.LoadFromStream(LResStream);
      Result.Assign(LPngImage);
    finally
      LResStream.Free;
    end;
  finally
    LPngImage.Free;
  end;
end;

procedure RegisterAboutBox;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LBitmap := CreateBitmapFromPngRes(ABOUT_RES_NAME);
  try
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      RsAboutTitle+' '+ComponentsVersion,
      RsAboutDescription, LBitmap.Handle, False, RsAboutLicense);
  finally
    LBitmap.Free;
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  LBitmap := CreateBitmapFromPngRes(SPLASH_RES_NAME);
  try
    SplashScreenServices.AddPluginBitmap(
      RsAboutTitle+' '+ComponentsVersion,
      LBitmap.Handle, False, RsAboutLicense, '');
  finally
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ABOUT_RES_NAME);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle+' '+ComponentsVersion,
    RsAboutDescription, ProductImage, False, RsAboutLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), SPLASH_RES_NAME);
  SplashScreenServices.AddPluginBitmap(RsAboutTitle, ProductImage,
    False, RsAboutLicense);
end;
{$ENDIF}


procedure Register;
begin
  RegisterWithSplashScreen;

  {$IFDEF D10_1+}
  RegisterComponents('ETHEA LabeledControls', [TLabeledButtonEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBButtonEdit]);
  {$ENDIF}
  {$IFDEF D10_4+}
  RegisterComponents('DbControls', [TDBNumberBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledNumberBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBNumberBox]);
  {$ENDIF}
  RegisterComponents('ETHEA LabeledControls', [TLabeledColorGrid]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledCurrencyEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBCurrencyEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledSpinEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledRichEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledCheckListBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledEditEx]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledComboBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledListBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledMemo]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBLabel]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBComboBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBListBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBMemo]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBImage]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBLookupListBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBLookupComboBox]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDbGrid]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBRichEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBCtrlGrid]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledDBListView]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledMaskEdit]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledRadioGroup]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledImage]);
  RegisterComponents('ETHEA LabeledControls', [TLabeledColorBox]);
end;

initialization
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

end.
