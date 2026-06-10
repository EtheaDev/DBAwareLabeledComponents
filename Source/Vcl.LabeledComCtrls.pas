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
unit Vcl.LabeledComCtrls;

interface

{$I 'DBAwareLabeledComponents.inc'}

uses
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.BoundLabel,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Samples.Spin,
  Vcl.Graphics,
  Vcl.CategoryButtons;

type
  /// <summary>
  ///  Event signature used to allow or veto the expansion of a tree node.
  /// </summary>
  TAllowExpandNodeEvent = procedure (Node: TTreeNode; var AllowExpand: boolean) of Object;
  /// <summary>
  ///  Event signature used to allow or veto the expansion of a button category.
  /// </summary>
  TAllowExpandCategoryButtonEvent = procedure (ButtonCategory: TButtonCategory;
    var AllowExpand: boolean) of Object;
  /// <summary>
  ///  Event signature raised when a tab is requested to be closed,
  ///  reporting the index of the affected item.
  /// </summary>
  TCloseTabEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;

  /// <summary>
  ///  Rich text editor (TRichEdit) extended with an attached TControlBoundLabel
  ///  that is automatically positioned, sized and kept in sync with the control.
  /// </summary>
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
    /// <summary>
    ///  Creates the control and instantiates its bound label.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Handles focus entry, highlighting the bound label accordingly.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Handles focus exit, restoring the bound label appearance.
    /// </summary>
    procedure DoExit; override;
    /// <summary>
    ///  Sets the control bounds and repositions/redraws the bound label.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Read-only flag that is True when the editor text is empty (ignoring whitespace).
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpy;
    /// <summary>
    ///  Caption displayed by the attached bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The label automatically bound to and positioned alongside this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;


  /// <summary>
  ///  Spin edit (TSpinEdit) extended with an attached TControlBoundLabel
  ///  that is automatically positioned, sized and kept in sync with the control.
  /// </summary>
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
    /// <summary>
    ///  Creates the control and instantiates its bound label.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds and repositions/redraws the bound label.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Read-only flag that is True when the edit text is empty.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
    /// <summary>
    ///  Exposes the inherited inner bevel style.
    /// </summary>
    property BevelInner;
    /// <summary>
    ///  Exposes the inherited outer bevel style.
    /// </summary>
    property BevelOuter;
    /// <summary>
    ///  Exposes the inherited border style.
    /// </summary>
    property BorderStyle;
    /// <summary>
    ///  Exposes the inherited set of beveled edges.
    /// </summary>
    property BevelEdges;
    /// <summary>
    ///  Exposes the inherited bevel kind.
    /// </summary>
    property BevelKind;
    /// <summary>
    ///  Caption displayed by the attached bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The label automatically bound to and positioned alongside this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

implementation


uses
  Winapi.Windows,
  System.Math,
  Vcl.Themes;

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
