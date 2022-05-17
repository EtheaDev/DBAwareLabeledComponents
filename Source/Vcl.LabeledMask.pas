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
unit Vcl.LabeledMask;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Types
  , System.SysUtils
  , System.Classes
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Mask
  ,Vcl.BoundLabel
  ;

type
  {TLabeledMaskEdit}
  TLabeledMaskEdit = class(TMaskEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
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
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

implementation

{ TLabeledMaskEdit }

constructor TLabeledMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledMaskEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledMaskEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledMaskEdit.GetIsEmpty: Boolean;
begin
  Result := Text = '';
end;

procedure TLabeledMaskEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledMaskEdit.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledMaskEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledMaskEdit.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

end.
