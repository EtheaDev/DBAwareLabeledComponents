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
unit Vcl.LabeledNumberBox;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Controls,
  Vcl.NumberBox,
  Vcl.DBNumberBox,
  Vcl.BoundLabel;

type
  /// <summary>
  ///  A TNumberBox descendant with an attached TControlBoundLabel that is kept
  ///  positioned, sized, parented and visibility-synchronized alongside the edit.
  /// </summary>
  TLabeledNumberBox = class(TNumberBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
  protected
    /// <summary>
    ///  Returns True when the edit holds no text (Text is an empty string).
    /// </summary>
    /// <returns>True if the control text is empty; otherwise False.</returns>
    function GetIsEmpty: Boolean; virtual;
    /// <summary>
    ///  Setter for BoundCaption; updates the bound label caption and stores the value.
    /// </summary>
    /// <param name="Value">The new caption text to display on the bound label.</param>
    procedure SetBoundCaption(const Value: TCaption); virtual;
    /// <summary>
    ///  Sets the control parent and reparents the associated bound label accordingly.
    /// </summary>
    /// <param name="AParent">The new parent windowed control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Handles component notifications; clears the bound label reference when it is removed.
    /// </summary>
    /// <param name="AComponent">The component the notification refers to.</param>
    /// <param name="Operation">The operation being performed (insertion or removal).</param>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control while it changes.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Updates the bound label appearance when the control gains focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the control and the associated bound label instance.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds, repositioning the bound label before and redrawing it after.
    /// </summary>
    /// <param name="ALeft">New left coordinate.</param>
    /// <param name="ATop">New top coordinate.</param>
    /// <param name="AWidth">New width.</param>
    /// <param name="AHeight">New height.</param>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Read-only flag indicating whether the edit is currently empty. Not persisted.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty stored False;
    /// <summary>
    ///  The caption text shown on the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The TControlBoundLabel attached to and managed by this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  /// <summary>
  ///  A data-aware TDbNumberBox descendant with an attached TControlBoundLabel that is
  ///  kept positioned, sized, parented and visibility-synchronized alongside the edit.
  /// </summary>
  TLabeledDbNumberBox = class(TDbNumberBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
  protected
    /// <summary>
    ///  Returns True when the edit holds no text (Text is an empty string).
    /// </summary>
    /// <returns>True if the control text is empty; otherwise False.</returns>
    function GetIsEmpty: Boolean; virtual;
    /// <summary>
    ///  Setter for BoundCaption; updates the bound label caption and stores the value.
    /// </summary>
    /// <param name="Value">The new caption text to display on the bound label.</param>
    procedure SetBoundCaption(const Value: TCaption); virtual;
    /// <summary>
    ///  Sets the control parent and reparents the associated bound label accordingly.
    /// </summary>
    /// <param name="AParent">The new parent windowed control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Handles component notifications; clears the bound label reference when it is removed.
    /// </summary>
    /// <param name="AComponent">The component the notification refers to.</param>
    /// <param name="Operation">The operation being performed (insertion or removal).</param>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control while it changes.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Updates the bound label appearance when the control gains focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the control and the associated bound label instance.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds, repositioning the bound label before and redrawing it after.
    /// </summary>
    /// <param name="ALeft">New left coordinate.</param>
    /// <param name="ATop">New top coordinate.</param>
    /// <param name="AWidth">New width.</param>
    /// <param name="AHeight">New height.</param>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  The caption text shown on the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The TControlBoundLabel attached to and managed by this control.
    /// </summary>
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
