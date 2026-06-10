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
unit Vcl.LabeledColorGrd;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.CheckLst,
  Vcl.ColorGrd,
  Vcl.BoundLabel;

Type
  /// <summary>
  ///  A VCL TColorGrid extended with an associated boundary label (BoundLabel).
  ///  The label is created automatically, follows the grid's position and font,
  ///  mirrors its visibility, and highlights when the grid receives focus.
  /// </summary>
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
    /// <summary>
    ///  Creates the color grid and instantiates its associated bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds, first recalculating the geometry to make room
    ///  for the bound label, then repositioning and repainting the label.
    /// </summary>
    /// <param name="ALeft">New left coordinate of the control.</param>
    /// <param name="ATop">New top coordinate of the control.</param>
    /// <param name="AWidth">New width of the control.</param>
    /// <param name="AHeight">New height of the control.</param>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Text displayed in the associated bound label. Setting this updates the
    ///  label caption and stores the value.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label control associated with this grid, created and
    ///  managed automatically. Read-only.
    /// </summary>
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
