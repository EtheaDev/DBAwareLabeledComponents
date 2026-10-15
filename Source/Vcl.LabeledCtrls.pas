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
unit Vcl.LabeledCtrls;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Vcl.BoundLabel;

type
  /// <summary>
  ///  Standard VCL TEdit extended with an attached boundary label
  ///  (BoundLabel) whose caption is set through BoundCaption.
  /// </summary>
  TLabeledEditEx = class(TEdit)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
  protected
    /// <summary>
    ///  Returns True when the edit Text is an empty string.
    /// </summary>
    /// <returns>True if the control contains no text; otherwise False.</returns>
    function GetIsEmpty: Boolean; virtual;
    /// <summary>
    ///  Updates the bound label caption and stores the new BoundCaption value.
    /// </summary>
    /// <param name="Value">The new caption to assign to the bound label.</param>
    procedure SetBoundCaption(const Value: TCaption); virtual;
    /// <summary>
    ///  Reparents the bound label together with the control.
    /// </summary>
    /// <param name="AParent">The new parent window control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Clears the internal reference when the bound label is being removed.
    /// </summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Highlights the bound label when the control receives focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the control and its associated bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds and repositions the bound label accordingly.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Indicates whether the edit text is empty. Read-only and not streamed.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty stored False;
    /// <summary>
    ///  Caption displayed by the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label automatically attached to this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  /// <summary>
  ///  Standard VCL TComboBox extended with an attached boundary label
  ///  (BoundLabel) whose caption is set through BoundCaption.
  /// </summary>
  TLabeledComboBox = class(TComboBox)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    /// <summary>
    ///  Reparents the bound label together with the control.
    /// </summary>
    /// <param name="AParent">The new parent window control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Clears the internal reference when the bound label is being removed.
    /// </summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control.
    /// </summary>
    procedure VisibleChanging; override;
  public
    /// <summary>
    ///  Creates the control and its associated bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds and repositions the bound label accordingly.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    /// <summary>
    ///  Highlights the bound label when the control receives focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  published
    /// <summary>
    ///  Indicates whether the combo box text is empty. Read-only.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
    /// <summary>
    ///  Caption displayed by the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label automatically attached to this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
  end;

  /// <summary>
  ///  Standard VCL TListBox extended with an attached boundary label
  ///  (BoundLabel) and a semicolon-delimited representation of its items.
  /// </summary>
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
    /// <summary>
    ///  Reparents the bound label together with the control.
    /// </summary>
    /// <param name="AParent">The new parent window control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Clears the internal reference when the bound label is being removed.
    /// </summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Highlights the bound label when the control receives focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the control and its associated bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds and repositions the bound label accordingly.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    /// <summary>
    ///  Draws an item using the inherited list box rendering, temporarily
    ///  detaching the OnDrawItem handler so the default drawing is exposed.
    /// </summary>
    /// <param name="Index">The index of the item to draw.</param>
    /// <param name="Rect">The bounding rectangle of the item.</param>
    /// <param name="State">The owner-draw state of the item.</param>
    procedure DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  published
    /// <summary>
    ///  Indicates whether the list box has no selected/current text. Not streamed.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty stored False;
    /// <summary>
    ///  Caption displayed by the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label automatically attached to this control.
    /// </summary>
    property BoundLabel: TControlBoundLabel read FBoundLabel;
    /// <summary>
    ///  Gets or sets all list items as a single semicolon-delimited string.
    /// </summary>
    property TextDelimited : string read GetTextDelimited write SetTextDelimited;
  end;

  /// <summary>
  ///  Standard VCL TMemo extended with an attached boundary label
  ///  (BoundLabel) whose caption is set through BoundCaption.
  /// </summary>
  TLabeledMemo = class(TMemo)
  private
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetBoundCaption(const Value: TCaption);
    function GetIsEmpty: Boolean;
  protected
    /// <summary>
    ///  Reparents the bound label together with the control.
    /// </summary>
    /// <param name="AParent">The new parent window control.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>
    ///  Clears the internal reference when the bound label is being removed.
    /// </summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>
    ///  Keeps the bound label visibility synchronized with the control.
    /// </summary>
    procedure VisibleChanging; override;
    /// <summary>
    ///  Highlights the bound label when the control receives focus.
    /// </summary>
    procedure DoEnter; override;
    /// <summary>
    ///  Restores the bound label appearance when the control loses focus.
    /// </summary>
    procedure DoExit; override;
  public
    /// <summary>
    ///  Creates the control and its associated bound label.
    /// </summary>
    /// <param name="AOwner">The component that owns this control.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Sets the control bounds and repositions the bound label accordingly.
    /// </summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    /// <summary>
    ///  Indicates whether the memo contains only blank text. Read-only.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
    /// <summary>
    ///  Caption displayed by the associated bound label.
    /// </summary>
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    /// <summary>
    ///  The boundary label automatically attached to this control.
    /// </summary>
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
  //Exposes the drawing method to the developer by temporarily detaching
  //the event handler
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
