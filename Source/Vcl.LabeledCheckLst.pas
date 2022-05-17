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
unit Vcl.LabeledCheckLst;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.Classes
  , Vcl.Controls
  , Vcl.CheckLst
  , Vcl.BoundLabel;

type
  TLabeledCheckListBox = class(TCheckListBox)
  strict private
  private
    FReadOnly : boolean;
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure PerformAllCheck(Check: boolean);
    procedure SetBoundCaption(const Value: TCaption);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    procedure UncheckAll;
    procedure CheckAll;
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property ReadOnly: boolean read FReadOnly write FReadOnly default False;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

implementation

uses
  System.Types
  ;

{ TLabeledCheckListBox }

procedure TLabeledCheckListBox.CheckAll;
begin
  PerformAllCheck(True);
end;

procedure TLabeledCheckListBox.UncheckAll;
begin
  PerformAllCheck(False);
end;

procedure TLabeledCheckListBox.KeyPress(var Key: Char);
begin
  //Se è read-only non consento che alla pressione della barra si modifichi il checkbox
  if (Key = ' ') and FReadOnly then
    Key := #0;
  inherited;
end;
procedure TLabeledCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  //In caso di ReadOnly devo prevenire la modifica del checkbox
  if FReadOnly and (Button = mbLeft) then
  begin
    Index := ItemAtPos(Point(X,Y),True);
    if (Index <> -1) and ItemEnabled[Index] then
    begin
      if not UseRightToLeftAlignment then
      begin
        if X - ItemRect(Index).Left < GetCheckWidth then
          Exit;
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          Exit;
      end;
    end;
  end;
  inherited;
end;
procedure TLabeledCheckListBox.PerformAllCheck(Check : boolean);
var
  i : integer;
begin
  for i := 0 to Items.Count -1 do
    Checked[i] := Check;
end;

procedure TLabeledCheckListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
  end;

procedure TLabeledCheckListBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, BoundLabel);
end;

procedure TLabeledCheckListBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, BoundLabel);
end;

procedure TLabeledCheckListBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledCheckListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledCheckListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledCheckListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledCheckListBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

end.
