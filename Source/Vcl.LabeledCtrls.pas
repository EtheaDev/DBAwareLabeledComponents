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
unit Vcl.LabeledCtrls;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , System.SysUtils
  , System.Classes
  , WinApi.Messages
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Graphics
  , Vcl.BoundLabel
  ;

type
  {TLabeledEditEx}
  TLabeledEditEx = class(TEdit)
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

  {TLabeledComboBox}
  TLabeledComboBox = class(TComboBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property IsEmpty: Boolean read GetIsEmpty;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  {TLabeledListBox}
  TLabeledListBox = class(TListBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
    function GetTextDelimited: string;
    procedure SetTextDelimited(const Value: string);
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
    procedure DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  published
    property IsEmpty: Boolean read GetIsEmpty stored False;
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    property TextDelimited : string read GetTextDelimited write SetTextDelimited;
  end;

  {TLabeledMemo}
  TLabeledMemo = class(TMemo)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
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

{ TLabeledEditEx }

function TLabeledEditEx.GetIsEmpty: Boolean;
begin
  Result := (Text = '');
end;

constructor TLabeledEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledEditEx.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledEditEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledEditEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledEditEx.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledEditEx.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledEditEx.DoEnter;
begin
  inherited;
  EnterEditCtrl(self, BoundLabel);
end;

procedure TLabeledEditEx.DoExit;
begin
  inherited;
  ExitEditCtrl(self, BoundLabel);
end;

{ TLabeledComboBox }
procedure TLabeledComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
end;

procedure TLabeledComboBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledComboBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledComboBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledComboBox.GetIsEmpty: Boolean;
begin
  Result := Text = '';
end;

procedure TLabeledComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledComboBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

{ TLabeledListBox }
procedure TLabeledListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
  end;

procedure TLabeledListBox.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledListBox.DefaultDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  OldEvent :  TDrawItemEvent;
begin
  //Rende il metodo visibile al programmatore staccando temporaneamente
  //l'event handler
  OldEvent := OnDrawItem;
  Try
    OnDrawItem := nil;
    inherited DrawItem(Index, Rect, state);
  Finally
    OnDrawItem := OldEvent;
  End;
end;

procedure TLabeledListBox.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledListBox.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledListBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledListBox.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

function TLabeledListBox.GetIsEmpty: Boolean;
begin
  Result := Text = '';
end;

function TLabeledListBox.GetTextDelimited: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Items.Count -1 do
  begin
    if Result <> '' then Result := Result + ';';
    Result := Result + Items.Strings[i];
  end;
end;

procedure TLabeledListBox.SetTextDelimited(const Value: string);
var
  k: integer;
  StrSucc : string;
  itemstr : string;
begin
  items.Clear;
  StrSucc := Value;
  k := Pos(';', StrSucc);
  while k > 0 do
  begin
    itemstr := Copy( StrSucc, 1, k-1 );
    items.Add(itemstr);
    StrSucc := Copy( StrSucc, k+1, Length(StrSucc) );
    k := Pos(';', StrSucc);
  end;
  if StrSucc <> '' then
    items.Add(StrSucc);
end;

{ TLabeledMemo }
procedure TLabeledMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetBounds(Left, Top, Width, Height);
end;

constructor TLabeledMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabel := TControlBoundLabel.Create(self);
  end;

procedure TLabeledMemo.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledMemo.DoEnter;
begin
  inherited;
  EnterEditCtrl(Self, FBoundLabel);
end;

procedure TLabeledMemo.DoExit;
begin
  inherited;
  ExitEditCtrl(Self, FBoundLabel);
end;

function TLabeledMemo.GetIsEmpty: Boolean;
begin
  Result := Trim(Lines.Text) = '';
end;

procedure TLabeledMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

procedure TLabeledMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledMemo.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

end.
