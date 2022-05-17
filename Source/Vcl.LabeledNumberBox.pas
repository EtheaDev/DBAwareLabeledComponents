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
unit Vcl.LabeledNumberBox;

interface

uses
  WinApi.Windows
  , System.Classes
  , Vcl.Controls
  , Vcl.NumberBox
  , Vcl.DBNumberBox
  , Vcl.BoundLabel
  ;

type
  {TLabeledNumberBox}

  TLabeledNumberBox = class(TNumberBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
  protected
    function GetIsEmpty: Boolean; virtual;
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
    property IsEmpty: Boolean read GetIsEmpty stored False;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledNumberBox}

  TLabeledDbNumberBox = class(TDbNumberBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
  protected
    function GetIsEmpty: Boolean; virtual;
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

{ TLabeledNumberBox }

function TLabeledNumberBox.GetIsEmpty: Boolean;
begin
  Result := (Text = '');
end;

constructor TLabeledNumberBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledNumberBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledNumberBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledNumberBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledNumberBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledNumberBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledNumberBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(self, BoundLabel);
end;

procedure TLabeledNumberBox.DoExit;
begin
  inherited;
  ExitEditCtrl(self, BoundLabel);
end;

{ TLabeledDbNumberBox }

function TLabeledDbNumberBox.GetIsEmpty: Boolean;
begin
  Result := (Text = '');
end;

constructor TLabeledDbNumberBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledDbNumberBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDbNumberBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledDbNumberBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDbNumberBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDbNumberBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDbNumberBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(self, BoundLabel);
end;

procedure TLabeledDbNumberBox.DoExit;
begin
  inherited;
  ExitEditCtrl(self, BoundLabel);
end;

end.
