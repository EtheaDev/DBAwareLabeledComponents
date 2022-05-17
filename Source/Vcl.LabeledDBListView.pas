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
unit Vcl.LabeledDBListView;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , Winapi.CommCtrl
  , System.SysUtils
  , System.Classes
  , WinApi.Messages
  , Vcl.ComCtrls
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Graphics
  , Vcl.BoundLabel
  , Data.DB
  ;

type
  TLabeledDBListView = class;

  TLabeledDBListItem = class(TListItem)
  private
    FBookmark: TBookmark;
    function GetDataSet: TDataSet;
  protected
    property Bookmark: TBookmark read FBookmark;
  public
    property DataSet : TDataSet read GetDataSet;
  end;

  { TLabeledDBListViewDataLink }
  TLabeledDBListViewDataLink = class(TDataLink)
  private
    FCBXDBListView: TLabeledDBListView;
  protected
    procedure DataEvent(Event: TDataEvent; Info: {$IFNDEF DXE2+}LongInt{$ELSE}NativeInt{$ENDIF}); override;
    procedure EditingChanged; override;
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged; override;
    procedure UpdateData; override;
  public
    constructor Create(CBXDBListView: TLabeledDBListView);
  end;

{ TLabeledDBListView }

  TLVDBNotifyEvent = procedure(Sender: TObject; Item: TListItem; DataSet : TDataSet) of object;
  TLVDBAcceptFieldEvent = procedure(Sender: TObject; Field: TField; var Accept: boolean;
    var ColumnSize: integer) of object;

  TLabeledDBListView = class(TCustomListView)
  private
    Updating : boolean;
    FDataField : string;
    FDataLink : TLabeledDBListViewDataLink;
    FOnUpdateContent: TLVDBNotifyEvent;
    FImageIndexField: string;
    FFileExtList: TStringList;
    FOnAddColumn: TLVDBAcceptFieldEvent;
    function GetDataSource : TDataSource;
    procedure SetDataSource(Value : TDataSource);
    procedure SetDataField(const Value : string);
    procedure SetImageIndexField(const Value: string);
    function GetListItems: TListItems;
    procedure SetListItems(const Value: TListItems);
    procedure GotoItem(Bookmark: TBookmark);
    procedure SetOnUpdateContent(const Value: TLVDBNotifyEvent);
    procedure SynchRecord(Item: TListItem);
    procedure UpdateSubItems(ListItem: TListItem);
    function GetFileExtList: TStringList;
    function GetFileExtImageIndex(const FileExt : string): integer;
    procedure SetOnAddColumn(const Value: TLVDBAcceptFieldEvent);
  protected
    procedure Delete(Item: TListItem); override;
    procedure Change(Item: TListItem; Change: Integer); override;
    function CreateListItem: TListItem; override;
    procedure Edit(const Item: TLVItem); override;
    procedure SetViewStyle(Value: TViewStyle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateContent;
    procedure UpdateColumnsByFields;
    function RegisterFileExtension(const FileExt : string) : integer;
    property FileExtList : TStringList read GetFileExtList;
  published
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property DataField : string read FDataField write SetDataField;
    property ImageIndexField : string read FImageIndexField write SetImageIndexField;

    property Cursor default crHandPoint;
    property Items: TListItems read GetListItems write SetListItems stored False;
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection default False;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default True;
    property RowSelect default True;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle default vsReport;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;

    property OnUpdateContent : TLVDBNotifyEvent read FOnUpdateContent write SetOnUpdateContent;
    property OnAddColumn: TLVDBAcceptFieldEvent read FOnAddColumn write SetOnAddColumn;
  end;

implementation

uses
  Vcl.Forms
  , Vcl.LabeledShellUtils
  ;

{ TLabeledDBListView }

function TLabeledDBListView.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TLabeledDBListView.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
  UpdateContent;
end;

procedure TLabeledDBListView.SetDataField(const Value : string);
begin
  FDataField := Value;
  UpdateContent;
end;

procedure TLabeledDBListView.SetImageIndexField(const Value: string);
begin
  FImageIndexField := Value;
  UpdateContent;
end;

constructor TLabeledDBListView.Create(AOwner: TComponent);
begin
  FDataLink := TLabeledDBListViewDataLink.Create(Self);
  inherited Create(AOwner);
  Cursor := crHandPoint;
  ReadOnly := True;
  RowSelect := True;
  HideSelection := False;
  ViewStyle := vsReport;
end;

destructor TLabeledDBListView.Destroy;
begin
  FDataLink.Free;
  FFileExtList.Free;
  inherited Destroy;
end;

procedure TLabeledDBListView.UpdateContent;
var
  ListItem : TListItem;
  OldBookmark : TBookmark;
  FActive : Boolean;
  DataField : TField;
  ImageIndexField : TField;
begin
  if Updating or //Previene lo ricorsione
    ( FDataField = '') or
    not Assigned(DataSource) or
    not Assigned(DataSource.DataSet) or
    not (DataSource.DataSet.State in [dsBrowse, dsInactive]) then
    Exit
  else
    Updating := True; //Indico lo stato di aggiornamento del list-view
  try
    if ViewStyle = vsReport then
      Visible := False;
    Items.Clear;
    if DataSource.DataSet.State = dsInactive then
      Exit;
    UpdateColumnsByFields;
    FActive := DataSource.DataSet.Active;
    if not FActive then
      Exit;
    DataSource.DataSet.DisableControls;
    try
      OldBookmark := DataSource.DataSet.Bookmark;
      DataSource.DataSet.First;
      DataField := DataSource.DataSet.FindField(FDataField);
      if not Assigned(DataField) then
        Exit;
      if FImageIndexField <> '' then
        ImageIndexField := DataSource.DataSet.FindField(FImageIndexField)
      else
        ImageIndexField := nil;
      //Ciclo sui record per creare gli items  
      while not DataSource.DataSet.EOF do
      begin
        listItem := Items.Add;
        listItem.Caption := DataField.DisplayText;
        (ListItem as TLabeledDBListItem).FBookmark := DataSource.DataSet.Bookmark;
        //Inizializzo imageindex se è stato impostato il campo
        if (ImageIndexField <> nil) then
          listItem.ImageIndex := ImageIndexField.AsInteger;
        //Avvio l'evento di update: è il momento giusto in cui il dataset è allineato
        if Assigned(FOnUpdateContent) then
          FOnUpdateContent(self, ListItem, DataSource.DataSet);
        //Aggiorno le colonne in caso di vsReport
        UpdateSubItems(ListItem);
        //Se inizialmente il record del dataset era posizionato qui imposto l'item
        if DataSource.DataSet.Bookmark = OldBookmark then
          ListItem.Selected := True;
        DataSource.DataSet.Next;
      end;
    finally
      DataSource.DataSet.EnableControls;
      DataSource.DataSet.Bookmark := OldBookmark;
    end;
  finally
    if ViewStyle = vsReport then
      Visible := True;
    Updating := False;
  end;
end;

procedure TLabeledDBListView.UpdateSubItems(ListItem : TListItem);
var
  Field : TField;
  i : integer;
begin
  for i := 1 to Columns.Count -1 do
  begin
    Field := DataSource.DataSet.FindField(Columns[i].Caption);
//    Field := TField(Columns[i].Tag); //Recupero Field dal Tag
    if Field <> nil then
      ListItem.SubItems.Add(Field.DisplayText)
    else
      ListItem.SubItems.Add('');
  end;
end;

function TLabeledDBListView.GetListItems: TListItems;
begin
  Result := inherited Items;
end;

procedure TLabeledDBListView.SetListItems(const Value: TListItems);
begin
  inherited Items := Value;
end;

procedure TLabeledDBListView.Delete(Item: TListItem);
begin
  inherited;
end;

procedure TLabeledDBListView.SynchRecord(Item: TListItem);
begin
  if (csDestroying in componentState) then
    Exit;
  //Sposto il puntatore del dataset
  if not Updating and (DataSource.DataSet.State = dsBrowse) then
  begin
    Updating := True;
    try
      //Sposto il puntatore del dataset
      DataSource.DataSet.Bookmark := TLabeledDBListItem(Item).FBookmark;
    finally
      Updating := False;
    end;
  end;
end;

procedure TLabeledDBListView.Change(Item: TListItem; Change: Integer);
begin
  inherited;
  SynchRecord(Item);
end;

procedure TLabeledDBListView.GotoItem(Bookmark: TBookmark);
var
  i : integer;
  Item: TListItem;
begin
  if Updating then
    Exit;
  for i := 0 to Items.Count -1 do
  begin
    Item := Items[i];
    if (TLabeledDBListItem(Item).FBookmark = Bookmark) then
    begin
      Updating := True;
      try
        Item.Selected := True;
        break;
      finally
        Updating := False;
      end;
    end;
  end;
end;

function TLabeledDBListView.CreateListItem: TListItem;
var
  LClass: TListItemClass;
begin
  LClass := TLabeledDBListItem;
  if Assigned(OnCreateItemClass) then
    OnCreateItemClass(Self, LClass);
  Result := LClass.Create(Items);
end;

procedure TLabeledDBListView.SetOnAddColumn(const Value: TLVDBAcceptFieldEvent);
begin
  FOnAddColumn := Value;
end;

procedure TLabeledDBListView.SetOnUpdateContent(const Value: TLVDBNotifyEvent);
begin
  FOnUpdateContent := Value;
end;

procedure TLabeledDBListView.Edit(const Item: TLVItem);
var
  EditItem: TListItem;
begin
  inherited;
  EditItem := Self.Selected;
  //Aggiorno il record corrispondente
  SynchRecord(EditItem);
  DataSource.DataSet.Edit;
  try
    DataSource.DataSet.FieldByName(DataField).AsString := EditItem.Caption;
    DataSource.DataSet.Post;
  except
    DataSource.DataSet.Cancel;
    raise;
  end;
end;

procedure TLabeledDBListView.UpdateColumnsByFields;
var
  i : integer;
  Field : TField;
  TM: TTextMetric;

  procedure AddColumn(Field : TField);
  var
    ListColumn : TListColumn;
    ColumnWidth : integer;
    MinColWidth : integer;
    Accept: boolean;
  begin
    //Aggiunge solo i campi testuali
    if Field.DataType in ftNonTextTypes then
      Exit;
    Accept := True;
    GetTextMetrics(Canvas.Handle, TM);
    MinColWidth := Field.DisplayWidth;
    ColumnWidth := MinColWidth * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
    //Se è la prima colonna ed esiste una imagelist devo allargarla
    if (Columns.Count = 0) and Assigned(SmallImages) then
      ColumnWidth := ColumnWidth + SmallImages.Width;
    if Assigned(FOnAddColumn) then
      FOnAddColumn(Self,Field,Accept,ColumnWidth);
    if not Accept then
      Exit;
    ListColumn := Columns.Add;
    ListColumn.Caption := Field.FieldName;
    ListColumn.Width := ColumnWidth;
    ListColumn.Alignment := Field.Alignment;
  end;

begin
//  if HandleAllocated then non serve altrimenti non mi mostra i record se il componente non è ancora visibile
  begin
    Columns.Clear;
    if //(ViewStyle <> vsReport) or
       not Assigned(DataSource) or not Assigned(DataSource.DataSet) then
      Exit;
    //Aggiungo come prima colonna il campo del DbListView
    Field := DataSource.DataSet.FindField(DataField);
    if Field <> nil then
      AddColumn(Field);

    for i := 0 to DataSource.DataSet.FieldCount -1 do
    begin
      Field := DataSource.DataSet.Fields[i];
      //Aggiungo gli altri campi visibili del dataset
      if Field.Visible and not SameText(Field.FieldName, DataField) then
        AddColumn(Field);
    end;
  end;  
end;

procedure TLabeledDBListView.SetViewStyle(Value: TViewStyle);
begin
  inherited;
  UpdateContent;
end;

function TLabeledDBListView.GetFileExtImageIndex(const FileExt : string) : integer;
begin
  Result := FileExtList.IndexOf(UpperCase(FileExt));
end;

function TLabeledDBListView.RegisterFileExtension(const FileExt: string) : integer;
var
  LargeIcon, SmallIcon : HICON;
begin
  Result := GetFileExtImageIndex(FileExt);
  if Result < 0 then
  begin
    //Aggiungo l'icona alle imagelist associate
    Result := FileExtList.Add(UpperCase(FileExt));
    if Assigned(LargeImages) or Assigned(SmallImages) then
    begin
      GetAssociatedIcon(Application.Handle, FileExt, @LargeIcon, @SmallIcon);
      if (LargeIcon <> 0) and Assigned(LargeImages) then
        ImageList_AddIcon(LargeImages.Handle, LargeIcon);
      if (SmallIcon <> 0) and Assigned(SmallImages) then
        ImageList_AddIcon(SmallImages.Handle, SmallIcon);
    end;
  end;  
end;

function TLabeledDBListView.GetFileExtList: TStringList;
begin
  if not Assigned(FFileExtList) then
    FFileExtList := TStringList.Create;
  Result := FFileExtList;
end;

{ TLabeledDBListViewDataLink }

procedure TLabeledDBListViewDataLink.ActiveChanged;
begin
  FCBXDBListView.UpdateContent;
  inherited;
end;

constructor TLabeledDBListViewDataLink.Create(CBXDBListView: TLabeledDBListView);
begin
  FCBXDBListView := CBXDBListView;
end;

procedure TLabeledDBListViewDataLink.DataEvent(Event: TDataEvent;
  Info: {$IFNDEF DXE2+}LongInt{$ELSE}NativeInt{$ENDIF});
begin
  inherited;
  if (Info = 0) and (Event = deDataSetChange) and not FCBXDBListView.Updating then
    FCBXDBListView.UpdateContent;
end;

procedure TLabeledDBListViewDataLink.DataSetChanged;
begin
  inherited;
end;

procedure TLabeledDBListViewDataLink.EditingChanged;
begin
  inherited;
end;

procedure TLabeledDBListViewDataLink.FocusControl(Field: TFieldRef);
begin
  inherited;
end;

procedure TLabeledDBListViewDataLink.LayoutChanged;
begin
  inherited;
end;

procedure TLabeledDBListViewDataLink.RecordChanged(Field: TField);
begin
  inherited;
  //Mi posiziono sull'item corrispondente
  FCBXDBListView.GotoItem(DataSet.Bookmark);
end;

procedure TLabeledDBListViewDataLink.UpdateData;
begin
  inherited;
  FCBXDBListView.UpdateContent;
end;

{ TLabeledDBListItem }

function TLabeledDBListItem.GetDataSet: TDataSet;
begin
  Result := (Owner.Owner as TLabeledDBListView).DataSource.DataSet;
end;

end.
