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
unit Vcl.LabeledColorGrd;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Classes
  , Vcl.Controls
  , Vcl.CheckLst
  , Vcl.ColorGrd
  , Vcl.BoundLabel
  ;

Type
  TLabeledColorGrid = Class(TColorGrid)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure SetBoundCaption(const Value: TCaption); virtual;
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
  end;

implementation

{ TLabeledColorGrid }

procedure TLabeledColorGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledColorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledColorGrid.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledColorGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledColorGrid.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledColorGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledColorGrid.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledColorGrid.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

procedure TLabeledColorGrid.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, BoundLabel);
end;

end.
