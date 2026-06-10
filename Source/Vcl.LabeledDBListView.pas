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
unit Vcl.LabeledDBListView;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.CommCtrl,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Vcl.BoundLabel;

type
  TLabeledDBListView = class;

  /// <summary>
  ///  List item used by TLabeledDBListView that keeps a bookmark to the
  ///  dataset record it represents.
  /// </summary>
  TLabeledDBListItem = class(TListItem)
  private
    FBookmark: TBookmark;
    function GetDataSet: TDataSet;
  protected
    /// <summary>
    ///  Bookmark of the dataset record associated with this list item.
    /// </summary>
    property Bookmark: TBookmark read FBookmark;
  public
    /// <summary>
    ///  Dataset that owns the record represented by this list item; resolved
    ///  through the owning TLabeledDBListView and its DataSource.
    /// </summary>
    property DataSet : TDataSet read GetDataSet;
  end;

  { TLabeledDBListViewDataLink }
  /// <summary>
  ///  Data link that connects a TLabeledDBListView to a dataset and forwards
  ///  dataset notifications so the list view stays synchronized.
  /// </summary>
  TLabeledDBListViewDataLink = class(TDataLink)
  private
    FCBXDBListView: TLabeledDBListView;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure EditingChanged; override;
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged; override;
    procedure UpdateData; override;
  public
    /// <summary>
    ///  Creates the data link and stores a reference to the owning list view.
    /// </summary>
    /// <param name="CBXDBListView">List view that owns this data link.</param>
    constructor Create(CBXDBListView: TLabeledDBListView);
  end;

{ TLabeledDBListView }

  /// <summary>
  ///  Event fired while an item is being built from the current dataset record,
  ///  allowing custom population of the list item.
  /// </summary>
  TLVDBNotifyEvent = procedure(Sender: TObject; Item: TListItem; DataSet : TDataSet) of object;
  /// <summary>
  ///  Event fired before a field is added as a column, allowing the column to be
  ///  rejected or its width to be customized.
  /// </summary>
  TLVDBAcceptFieldEvent = procedure(Sender: TObject; Field: TField; var Accept: boolean;
    var ColumnSize: integer) of object;

  /// <summary>
  ///  Data-aware list view that displays the records of a dataset, one item per
  ///  record, with optional report columns built from the dataset fields.
  /// </summary>
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
    /// <summary>
    ///  Creates the list view, its internal data link, and applies the default
    ///  presentation (hand cursor, read-only, row select, report view style).
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///  Frees the internal data link and the file-extension list.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///  Rebuilds all list items from the linked dataset: clears the items,
    ///  refreshes the columns, creates one item per record from DataField,
    ///  assigns image indexes, fires OnUpdateContent, and restores the original
    ///  record position and selection. Guarded against re-entrancy.
    /// </summary>
    procedure UpdateContent;
    /// <summary>
    ///  Rebuilds the report columns from the dataset fields, using DataField as
    ///  the first column followed by the other visible text fields. OnAddColumn
    ///  is fired for each candidate field.
    /// </summary>
    procedure UpdateColumnsByFields;
    /// <summary>
    ///  Registers a file extension and, if image lists are assigned, adds its
    ///  associated large/small system icons to them. Returns the image index of
    ///  the extension (existing or newly added).
    /// </summary>
    /// <param name="FileExt">File extension to register (case-insensitive).</param>
    /// <returns>Index of the extension within FileExtList.</returns>
    function RegisterFileExtension(const FileExt : string) : integer;
    /// <summary>
    ///  List of registered file extensions, kept in sync with the image lists.
    ///  Created on first access.
    /// </summary>
    property FileExtList : TStringList read GetFileExtList;
  published
    /// <summary>
    ///  Data source providing the dataset whose records are displayed.
    ///  Setting it refreshes the content.
    /// </summary>
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    /// <summary>
    ///  Name of the dataset field used for the item caption and the first
    ///  column. Setting it refreshes the content.
    /// </summary>
    property DataField : string read FDataField write SetDataField;
    /// <summary>
    ///  Name of the dataset field providing each item's image index. Setting it
    ///  refreshes the content.
    /// </summary>
    property ImageIndexField : string read FImageIndexField write SetImageIndexField;

    /// <summary>
    ///  Mouse cursor; defaults to crHandPoint.
    /// </summary>
    property Cursor default crHandPoint;
    /// <summary>
    ///  Collection of list items; not streamed since it is rebuilt from the
    ///  dataset.
    /// </summary>
    property Items: TListItems read GetListItems write SetListItems stored False;
    /// <summary>
    ///  Action associated with the control.
    /// </summary>
    property Action;
    /// <summary>
    ///  Alignment of the control within its parent.
    /// </summary>
    property Align;
    /// <summary>
    ///  Number of items by which the item array grows when reallocated.
    /// </summary>
    property AllocBy;
    /// <summary>
    ///  Anchors that bind the control's edges to its parent.
    /// </summary>
    property Anchors;
    /// <summary>
    ///  Edges of the control that have a bevel.
    /// </summary>
    property BevelEdges;
    /// <summary>
    ///  Style of the inner bevel.
    /// </summary>
    property BevelInner;
    /// <summary>
    ///  Style of the outer bevel.
    /// </summary>
    property BevelOuter;
    /// <summary>
    ///  Kind of bevel; defaults to bkNone.
    /// </summary>
    property BevelKind default bkNone;
    /// <summary>
    ///  Width in pixels of the bevels.
    /// </summary>
    property BevelWidth;
    /// <summary>
    ///  Bidirectional reading order of the control.
    /// </summary>
    property BiDiMode;
    /// <summary>
    ///  Style of the control's border.
    /// </summary>
    property BorderStyle;
    /// <summary>
    ///  Width in pixels of the control's border.
    /// </summary>
    property BorderWidth;
    /// <summary>
    ///  Indicates whether items display check boxes.
    /// </summary>
    property Checkboxes;
    /// <summary>
    ///  Background color of the control.
    /// </summary>
    property Color;
    /// <summary>
    ///  Report-view columns of the list view.
    /// </summary>
    property Columns;
    /// <summary>
    ///  Indicates whether clicking a column header generates an event.
    /// </summary>
    property ColumnClick;
    /// <summary>
    ///  Size constraints of the control.
    /// </summary>
    property Constraints;
    /// <summary>
    ///  Indicates whether the control uses the 3D style.
    /// </summary>
    property Ctl3D;
    /// <summary>
    ///  Cursor shown while dragging from the control.
    /// </summary>
    property DragCursor;
    /// <summary>
    ///  Kind of drag operation (drag-and-drop or docking).
    /// </summary>
    property DragKind;
    /// <summary>
    ///  Drag mode of the control.
    /// </summary>
    property DragMode;
    /// <summary>
    ///  Indicates whether the control is enabled.
    /// </summary>
    property Enabled;
    /// <summary>
    ///  Font used to render item text.
    /// </summary>
    property Font;
    /// <summary>
    ///  Indicates whether scroll bars use the flat style.
    /// </summary>
    property FlatScrollBars;
    /// <summary>
    ///  Indicates whether the full item is shown while dragging a column.
    /// </summary>
    property FullDrag;
    /// <summary>
    ///  Indicates whether grid lines are drawn in report view.
    /// </summary>
    property GridLines;
    /// <summary>
    ///  Indicates whether the selection is hidden when the control loses focus;
    ///  defaults to False.
    /// </summary>
    property HideSelection default False;
    /// <summary>
    ///  Indicates whether items are highlighted on hover.
    /// </summary>
    property HotTrack;
    /// <summary>
    ///  Visual styles applied to hot-tracked items.
    /// </summary>
    property HotTrackStyles;
    /// <summary>
    ///  Time in milliseconds the pointer must hover before hot tracking.
    /// </summary>
    property HoverTime;
    /// <summary>
    ///  Options controlling icon arrangement.
    /// </summary>
    property IconOptions;
    /// <summary>
    ///  Image list used for large icons.
    /// </summary>
    property LargeImages;
    /// <summary>
    ///  Indicates whether multiple items can be selected.
    /// </summary>
    property MultiSelect;
    /// <summary>
    ///  Indicates whether the control operates in virtual (owner data) mode.
    /// </summary>
    property OwnerData;
    /// <summary>
    ///  Indicates whether items are owner-drawn.
    /// </summary>
    property OwnerDraw;
    /// <summary>
    ///  Indicates whether items can be edited; defaults to True.
    /// </summary>
    property ReadOnly default True;
    /// <summary>
    ///  Indicates whether selecting an item selects the whole row; defaults to
    ///  True.
    /// </summary>
    property RowSelect default True;
    /// <summary>
    ///  Indicates whether BiDiMode is inherited from the parent.
    /// </summary>
    property ParentBiDiMode;
    /// <summary>
    ///  Indicates whether the color is inherited from the parent; defaults to
    ///  False.
    /// </summary>
    property ParentColor default False;
    /// <summary>
    ///  Indicates whether the font is inherited from the parent.
    /// </summary>
    property ParentFont;
    /// <summary>
    ///  Indicates whether ShowHint is inherited from the parent.
    /// </summary>
    property ParentShowHint;
    /// <summary>
    ///  Pop-up menu associated with the control.
    /// </summary>
    property PopupMenu;
    /// <summary>
    ///  Indicates whether column headers are shown in report view.
    /// </summary>
    property ShowColumnHeaders;
    /// <summary>
    ///  Indicates whether work areas are shown.
    /// </summary>
    property ShowWorkAreas;
    /// <summary>
    ///  Indicates whether hints are shown for the control.
    /// </summary>
    property ShowHint;
    /// <summary>
    ///  Image list used for small icons.
    /// </summary>
    property SmallImages;
    /// <summary>
    ///  Sorting mode applied to the items.
    /// </summary>
    property SortType;
    /// <summary>
    ///  Image list used for item state images.
    /// </summary>
    property StateImages;
    /// <summary>
    ///  Tab order of the control within its parent.
    /// </summary>
    property TabOrder;
    /// <summary>
    ///  Indicates whether the control can receive focus via Tab; defaults to
    ///  True.
    /// </summary>
    property TabStop default True;
    /// <summary>
    ///  View style of the list; defaults to vsReport. Changing it refreshes the
    ///  content.
    /// </summary>
    property ViewStyle default vsReport;
    /// <summary>
    ///  Indicates whether the control is visible.
    /// </summary>
    property Visible;
    /// <summary>
    ///  Occurs during advanced custom drawing of the control.
    /// </summary>
    property OnAdvancedCustomDraw;
    /// <summary>
    ///  Occurs during advanced custom drawing of an item.
    /// </summary>
    property OnAdvancedCustomDrawItem;
    /// <summary>
    ///  Occurs during advanced custom drawing of a subitem.
    /// </summary>
    property OnAdvancedCustomDrawSubItem;
    /// <summary>
    ///  Occurs after an item changes.
    /// </summary>
    property OnChange;
    /// <summary>
    ///  Occurs before an item changes.
    /// </summary>
    property OnChanging;
    /// <summary>
    ///  Occurs when the control is clicked.
    /// </summary>
    property OnClick;
    /// <summary>
    ///  Occurs when a column header is clicked.
    /// </summary>
    property OnColumnClick;
    /// <summary>
    ///  Occurs after a column is dragged to a new position.
    /// </summary>
    property OnColumnDragged;
    /// <summary>
    ///  Occurs when a column header is right-clicked.
    /// </summary>
    property OnColumnRightClick;
    /// <summary>
    ///  Occurs when two items are compared during sorting.
    /// </summary>
    property OnCompare;
    /// <summary>
    ///  Occurs when the context pop-up menu is about to be shown.
    /// </summary>
    property OnContextPopup;
    /// <summary>
    ///  Occurs during custom drawing of the control.
    /// </summary>
    property OnCustomDraw;
    /// <summary>
    ///  Occurs during custom drawing of an item.
    /// </summary>
    property OnCustomDrawItem;
    /// <summary>
    ///  Occurs during custom drawing of a subitem.
    /// </summary>
    property OnCustomDrawSubItem;
    /// <summary>
    ///  Occurs when item data is requested in virtual mode.
    /// </summary>
    property OnData;
    /// <summary>
    ///  Occurs when an item is searched for in virtual mode.
    /// </summary>
    property OnDataFind;
    /// <summary>
    ///  Occurs when an item's hint is requested in virtual mode.
    /// </summary>
    property OnDataHint;
    /// <summary>
    ///  Occurs when the virtual data state changes.
    /// </summary>
    property OnDataStateChange;
    /// <summary>
    ///  Occurs when the control is double-clicked.
    /// </summary>
    property OnDblClick;
    /// <summary>
    ///  Occurs when an item is deleted.
    /// </summary>
    property OnDeletion;
    /// <summary>
    ///  Occurs when an owner-drawn item must be drawn.
    /// </summary>
    property OnDrawItem;
    /// <summary>
    ///  Occurs after an item's label has been edited.
    /// </summary>
    property OnEdited;
    /// <summary>
    ///  Occurs when an item's label is about to be edited.
    /// </summary>
    property OnEditing;
    /// <summary>
    ///  Occurs when docking of the control ends.
    /// </summary>
    property OnEndDock;
    /// <summary>
    ///  Occurs when a drag operation ends.
    /// </summary>
    property OnEndDrag;
    /// <summary>
    ///  Occurs when the control receives focus.
    /// </summary>
    property OnEnter;
    /// <summary>
    ///  Occurs when the control loses focus.
    /// </summary>
    property OnExit;
    /// <summary>
    ///  Occurs when an item's image index is requested.
    /// </summary>
    property OnGetImageIndex;
    /// <summary>
    ///  Occurs when a subitem's image index is requested.
    /// </summary>
    property OnGetSubItemImage;
    /// <summary>
    ///  Occurs when a dragged object is dropped on the control.
    /// </summary>
    property OnDragDrop;
    /// <summary>
    ///  Occurs when an object is dragged over the control.
    /// </summary>
    property OnDragOver;
    /// <summary>
    ///  Occurs when an info tip is requested for an item.
    /// </summary>
    property OnInfoTip;
    /// <summary>
    ///  Occurs when an item is inserted.
    /// </summary>
    property OnInsert;
    /// <summary>
    ///  Occurs when a key is pressed down.
    /// </summary>
    property OnKeyDown;
    /// <summary>
    ///  Occurs when a character key is pressed.
    /// </summary>
    property OnKeyPress;
    /// <summary>
    ///  Occurs when a key is released.
    /// </summary>
    property OnKeyUp;
    /// <summary>
    ///  Occurs when a mouse button is pressed.
    /// </summary>
    property OnMouseDown;
    /// <summary>
    ///  Occurs when the mouse pointer moves over the control.
    /// </summary>
    property OnMouseMove;
    /// <summary>
    ///  Occurs when a mouse button is released.
    /// </summary>
    property OnMouseUp;
    /// <summary>
    ///  Occurs when the control is resized.
    /// </summary>
    property OnResize;
    /// <summary>
    ///  Occurs when an item is selected or deselected.
    /// </summary>
    property OnSelectItem;
    /// <summary>
    ///  Occurs when docking of the control starts.
    /// </summary>
    property OnStartDock;
    /// <summary>
    ///  Occurs when a drag operation starts.
    /// </summary>
    property OnStartDrag;

    /// <summary>
    ///  Occurs while each item is being built from the current dataset record,
    ///  allowing custom population of the item.
    /// </summary>
    property OnUpdateContent : TLVDBNotifyEvent read FOnUpdateContent write SetOnUpdateContent;
    /// <summary>
    ///  Occurs before a field is added as a column, allowing the column to be
    ///  rejected or its width to be customized.
    /// </summary>
    property OnAddColumn: TLVDBAcceptFieldEvent read FOnAddColumn write SetOnAddColumn;
  end;

implementation

uses
  Vcl.Forms,
  Vcl.LabeledShellUtils;

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
  if Updating or //Prevents recursion
    ( FDataField = '') or
    not Assigned(DataSource) or
    not Assigned(DataSource.DataSet) or
    not (DataSource.DataSet.State in [dsBrowse, dsInactive]) then
    Exit
  else
    Updating := True; //Flag that the list view is being updated
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
      //Loop over the records to create the items
      while not DataSource.DataSet.EOF do
      begin
        listItem := Items.Add;
        listItem.Caption := DataField.DisplayText;
        (ListItem as TLabeledDBListItem).FBookmark := DataSource.DataSet.Bookmark;
        //Initialize the image index when the field has been set
        if (ImageIndexField <> nil) then
          listItem.ImageIndex := ImageIndexField.AsInteger;
        //Fire the update event: this is the right moment, when the dataset is aligned
        if Assigned(FOnUpdateContent) then
          FOnUpdateContent(self, ListItem, DataSource.DataSet);
        //Update the columns in case of vsReport
        UpdateSubItems(ListItem);
        //If the dataset record was originally positioned here, select the item
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
//    Field := TField(Columns[i].Tag); //Retrieve the Field from the Tag
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
  //Move the dataset cursor
  if not Updating and (DataSource.DataSet.State = dsBrowse) then
  begin
    Updating := True;
    try
      //Move the dataset cursor
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
  //Update the corresponding record
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
    //Add only text fields
    if Field.DataType in ftNonTextTypes then
      Exit;
    Accept := True;
    GetTextMetrics(Canvas.Handle, TM);
    MinColWidth := Field.DisplayWidth;
    ColumnWidth := MinColWidth * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
    //If this is the first column and an image list exists, widen it
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
//  if HandleAllocated then: not needed, otherwise records are not shown when the component is not yet visible
  begin
    Columns.Clear;
    if //(ViewStyle <> vsReport) or
       not Assigned(DataSource) or not Assigned(DataSource.DataSet) then
      Exit;
    //Add the DbListView field as the first column
    Field := DataSource.DataSet.FindField(DataField);
    if Field <> nil then
      AddColumn(Field);

    for i := 0 to DataSource.DataSet.FieldCount -1 do
    begin
      Field := DataSource.DataSet.Fields[i];
      //Add the other visible fields of the dataset
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
    //Add the icon to the associated image lists
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
  Info: NativeInt);
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
  //Move to the corresponding item
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
