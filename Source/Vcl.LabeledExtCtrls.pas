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
unit Vcl.LabeledExtCtrls;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Types
  , System.SysUtils
  , System.Classes
  , Vcl.Controls
  , Vcl.ExtCtrls
  , Vcl.Graphics
  , Vcl.BoundLabel
  ;

type
  {TLabeledRadioGroup}
  TLabeledRadioGroup = class(TRadioGroup)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  End;

  {TLabeledImage}
  TLabeledImage = class(TImage)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpty stored False;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    property Font;
  end;

  TLabeledColorBox = class(TColorBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
    procedure AdjustItemHeight;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function IsItemHeightStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Style default [cbStandardColors];
  published
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

implementation

{ TLabeledImage }
constructor TLabeledImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledImage.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

function TLabeledImage.GetIsEmpty: Boolean;
begin
  Result := False;
end;

procedure TLabeledImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledImage.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledImage.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledColorBox }
procedure TLabeledColorBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustItemHeight;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopulateList;
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledColorBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledColorBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledColorBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledColorBox.IsItemHeightStored: Boolean;
begin
  Result := False;
end;

procedure TLabeledColorBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledColorBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  //if (Height-2) <> ItemHeight then
  //  ItemHeight := Height - 2;
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledColorBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledColorBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledColorBox.AdjustItemHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
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
  ItemHeight := Metrics.tmHeight + I - 6;
end;

{ TLabeledRadioGroup }

procedure TLabeledRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
    inherited ParentColor := False;
  inherited TabStop := True;
end;

procedure TLabeledRadioGroup.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledRadioGroup.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledRadioGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledRadioGroup.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledRadioGroup.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledRadioGroup.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledRadioGroup.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

end.
