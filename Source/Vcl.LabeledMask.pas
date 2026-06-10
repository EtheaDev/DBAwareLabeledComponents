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
unit Vcl.LabeledMask;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.BoundLabel;

type
  /// <summary>
  ///  A VCL TMaskEdit extended with an automatically managed boundary label
  ///  (BoundLabel). The label tracks the edit control's position, parent and
  ///  visibility, and highlights when the edit receives focus.
  /// </summary>
  TLabeledMaskEdit = class(TMaskEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    /// <summary>
    ///  Assigns the parent of the control and propagates the same parent to the
    ///  bound label so it is displayed alongside the edit.
    /// </summary>
    /// <param name="AParent">The new parent window control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Handles component notifications; clears the internal reference when the
    ///  bound label is being removed to avoid dangling pointers.
    /// </summary>
    /// <param name="AComponent">The component the notification refers to.</param>
    /// <param name="Operation">The operation being performed (insertion or removal).</param>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized as the inverse of the
    ///  edit control's visibility while the visible state is changing.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Called when the control gains focus; applies the focused highlight to
    ///  the bound label.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Called when the control loses focus; restores the normal appearance of
    ///  the bound label.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the masked edit control and instantiates its associated
    ///  bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this instance.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control's bounding rectangle and repositions the bound label
    ///  accordingly.
    /// </summary>
    /// <param name="ALeft">The left coordinate.</param>
    /// <param name="ATop">The top coordinate.</param>
    /// <param name="AWidth">The control width.</param>
    /// <param name="AHeight">The control height.</param>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Returns True when the edit contains no text.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
    /// <summary>
    ///  The caption text displayed by the bound label associated with this edit.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label automatically created and managed for this edit control.
    /// </summary>
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
