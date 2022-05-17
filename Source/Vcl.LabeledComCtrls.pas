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
unit Vcl.LabeledComCtrls;

interface

{$I 'DBAwareLabeledComponents.inc'}

uses
  //RTL
  Types,
  SysUtils, Classes, Messages, Forms,
  //CB
  Vcl.BoundLabel,
  //VCL
  Controls, ComCtrls, Spin, Graphics, CategoryButtons;

type
  TAllowExpandNodeEvent = procedure (Node: TTreeNode; var AllowExpand: boolean) of Object;
  TAllowExpandCategoryButtonEvent = procedure (ButtonCategory: TButtonCategory;
    var AllowExpand: boolean) of Object;
  TCloseTabEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;

  {TLabeledRichEdit}
  TLabeledRichEdit = class(TRichEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpy: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property IsEmpty: Boolean read GetIsEmpy;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;


  {TLabeledSpinEdit}
  TLabeledSpinEdit = class(TSpinEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    function GetIsEmpty: Boolean;
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
    property IsEmpty: Boolean read GetIsEmpty;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property BevelEdges;
    property BevelKind;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

implementation


uses
  Vcl.Themes
  , Winapi.Windows
  , System.Math
  ;

{ TLabeledRichEdit }
procedure TLabeledRichEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledRichEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

procedure TLabeledRichEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, BoundLabel);
end;

procedure TLabeledRichEdit.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledRichEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledRichEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

function TLabeledRichEdit.GetIsEmpy: Boolean;
begin
  Result := Trim(Lines.Text) = '';
end;

procedure TLabeledRichEdit.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledSpinEdit }
constructor TLabeledSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledSpinEdit.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledSpinEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledSpinEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledSpinEdit.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledSpinEdit.DoEnter;
begin
  inherited;
  EnterEditCtrl(self, FBoundLabel);
end;

procedure TLabeledSpinEdit.DoExit;
begin
  inherited;
  ExitEditCtrl(self, FBoundLabel);
end;

function TLabeledSpinEdit.GetIsEmpty: Boolean;
begin
  Result := Text = '';
end;

end.
