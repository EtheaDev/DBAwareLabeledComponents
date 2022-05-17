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
unit Vcl.LabeledDbImage;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  WinApi.Windows
  , System.SysUtils
  , System.Classes
  , Data.DB
  , Vcl.Controls
  , Vcl.ExtCtrls
  , Vcl.Graphics
  , Vcl.DbCtrls
  , Vcl.Dialogs
  , Vcl.ExtDlgs
  , Vcl.BoundLabel
  , Vcl.LabeledExtCtrls
  ;

type
  TLoadImageOption = (liDefault, liNoChangeDir, liCopyImageFromSource);
  TLabeledDbImageLink = class;
  TDbImageLinkEmptyCaptionStyle = (csDisplayLabel, csSize, csNone);

  { TLabeledImageDataLink }
  TLabeledImageDataLink = class(TFieldDataLink)
  private
    FCBXDbImageLink: TLabeledDbImageLink;
  protected
    procedure DataEvent(Event: TDataEvent; Info:{$IFNDEF DXE2+}LongInt{$ELSE}NativeInt{$ENDIF}); override;
    procedure EditingChanged; override;
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged; override;
    procedure UpdateData; override;
  public
    constructor Create(CBXDbImageLink: TLabeledDbImageLink);
  end;

  {TLabeledDbImageLink}
  TLabeledDbImageLink = class(TCustomPanel)
  private
    FDataLink: TLabeledImageDataLink;
    FImage: TImage;
    FImagesPath: string;
    FBoundLabel: TControlBoundLabel;
    FBoundCaption: TCaption;
    FEmptyCaptionStyle: TDbImageLinkEmptyCaptionStyle;
    procedure SetBoundCaption(const Value: TCaption);
    procedure ImageClick(Sender : TObject);
    procedure ImageDblClick(Sender : TObject);
    procedure PanelEnter(Sender: TObject);
    procedure PanelExit(Sender: TObject);
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    procedure SetImagesPath(const Value: string);
    function CalcCaption: string;
    procedure SetImage;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetField: TField;
    function GetDataField: string;
    procedure SetDataField(const Value: string);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    function GetImagesPath: string;
    procedure SetEmptyCaptionStyle(
      const Value: TDbImageLinkEmptyCaptionStyle);
    procedure UpdateHint;
  protected
    procedure VisibleChanging; override;
    property DataLink: TLabeledImageDataLink read FDataLink;
    function CreateImageContainer : TImage; virtual;
    procedure DblClick; override;
    procedure Click; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Resize; override;
    //procedure di risposta al DataLink
    procedure DataLinkDataEvent(Event: TDataEvent; Info: Integer);
    procedure LayoutChanged(Sender : TFieldDataLink); virtual;
    procedure RecordChanged(Sender : TFieldDataLink); virtual;
    procedure DataSetChanged(Sender : TFieldDataLink); virtual;
    procedure LinkActive(Sender : TFieldDataLink; Active : boolean); virtual;
    procedure UpdateData(Sender : TFieldDataLink); virtual;
    procedure LinkEditingChanged(Sender : TFieldDataLink); virtual;
    //metodi di aggiornamento di un datalink
    procedure Change;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ClearImage;
  public
    function ImageFileName : string;
    procedure LoadImageFromFile;
    procedure SetImageField(const FileName : string);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function isEmpty : boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    //Proprietà pubblicate di TCustomPanel
    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentCtl3D;
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;

    //Proprietà di BoundLabel
    property BoundCaption : TCaption read FBoundCaption write SetBoundCaption;
    property BoundLabel: TControlBoundLabel read FBoundLabel;

    //Proprietà per il datalink
    property Field: TField read GetField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property UseDockManager default False;
    property Cursor : TCursor read GetCursor write SetCursor default crHandPoint;
    property ImagesPath : string read GetImagesPath write SetImagesPath;
    property EmptyCaptionStyle : TDbImageLinkEmptyCaptionStyle read FEmptyCaptionStyle write SetEmptyCaptionStyle default csDisplayLabel;
  end;

procedure DrawImage( const FileName : string; Image : TImage; const NoImageName : string);
function LoadImage( var FileName : string; Image : TImage;
  const NoImageName : string; LoadImageOption : TLoadImageOption = liNoChangeDir;
  HelpContext : integer = 2000) : boolean;
procedure RegisterImageLinkPath(const Path : string);

implementation

uses
  Vcl.DbAwareLabeledConsts
  , Vcl.DbAwareLabeledUtils
  ;

var
  ImageLinkPath : string;

procedure RegisterImageLinkPath(const Path : string);
begin
  ImageLinkPath := ValidPath(Path);
end;

function GetImageLinkPath : string;
begin
  Result := ImageLinkPath;
end;

function LoadImage( var FileName : string; Image : TImage; const NoImageName : string;
  LoadImageOption : TLoadImageOption = liNoChangeDir;
  HelpContext : integer = 2000) : boolean;
var
  od : TOpenPictureDialog;
  InitialPath : string;
  DestPath : string;
begin
  od := TOpenPictureDialog.Create(nil);
  Try
    od.FileName := ExtractFileName(FileName);
    InitialPath := ExtractFilePath(FileName);
    od.InitialDir := InitialPath;
    od.Options := [ofHideReadOnly, ofEnableSizing];
    Result := od.Execute;
    if Result then
    begin
      DestPath := ExtractFilePath(od.FileName);
      if (LoadImageOption <> liDefault) and (InitialPath <> '') and
        not SamePath(DestPath, InitialPath) then
      begin
        if LoadImageOption = liNoChangeDir then
          raise EInOutError.CreateHelp(ERR_NO_CHANGE_DIR,HelpContext)
        else if LoadImageOption = liCopyImageFromSource then
        begin
          //Copio l'immagine nella cartella di destinazione: se esiste già dà errore
          CopyFileEx(od.FileName, InitialPath+ExtractFileName(od.FileName));
          //Cambio la path dell'immagine
          FileName := InitialPath+ExtractFileName(od.FileName);
        end;
      end
      else
        FileName := od.FileName;
        
      drawImage(FileName, Image, NoImageName);
    end;
  Finally
    od.Free;
  End;
end;

procedure DrawImage( const FileName : string; Image : TImage; const NoImageName : string);
var
  ImageOK : boolean;
  FileImmagine : string;
begin
  ImageOK := False;
  Try
    FileImmagine := ExtractFileName(FileName);
    //Prelevo l'immagine
    if FileImmagine <> '' then
    begin
      Try
        //tento di caricare l'immagine
        Image.Picture.LoadFromFile(FileName);
        ImageOK := True;
      Except
      End;
    end;
  Finally
    if not ImageOK then
    Try
      Image.Picture.LoadFromFile(NoImageName);
    Except
    End;
  End;
end;

{ TLabeledDbImageLink }
constructor TLabeledDbImageLink.Create(AOwner: TComponent);
begin
  inherited;
  FBoundLabel := TControlBoundLabel.Create(self);
  FDataLink := TLabeledImageDataLink.Create(Self);
  FDataLink.Control := Self;
  //inizializzazioni
  //creazione contenitore immagine
  FImage := CreateImageContainer;
  FImage.Cursor := crHandPoint;
  FImage.OnClick := ImageClick;
  FImage.OnDblClick := ImageDblClick;
  FImage.Parent := self;

  //Inizializzazioni
  ParentBackground := False;
  UseDockManager := False;
  Cursor := crHandPoint;
  ParentColor := True;
  BevelOuter := bvLowered;
  BorderWidth := 4;
  TabStop := True;
  OnEnter := PanelEnter;
  OnExit := PanelExit;
end;

function TLabeledDbImageLink.CreateImageContainer: TImage;
begin
  Result := TImage.Create(Self);
  Result.Align := alClient;
  Result.Center := True;
  Result.Proportional := True;
  Result.Stretch := True;
end;

destructor TLabeledDbImageLink.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

function TLabeledDbImageLink.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TLabeledDbImageLink.ImageClick(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    SetFocus;
end;

procedure TLabeledDbImageLink.LoadImageFromFile;
var
  FileName : string;
  OldImage : TImage;
begin
  //Richiamando DataLink.Edit si tenta di mandare in edit il dataset
  //nel caso sia collegato ad un DataSource in AutoEdit a True!
  if not DataLink.Edit then
    raise EDatabaseError.Create(ERR_NO_EDITING_DATASET);
  FileName := imageFileName;
  if FileName = '' then
    FileName := ImagesPath;
  OldImage := TImage.Create(nil);
  Try
    OldImage.Picture.Assign(FImage.Picture);

    if LoadImage(FileName, FImage, '', liCopyImageFromSource, 0) then
    begin
      Try
        SetImageField(FileName);
        UpdateHint;
      Except
        FImage.Picture.Assign(OldImage.Picture);
        raise;
      End;
    end;
  Finally
    OldImage.Free;
  End;
end;

procedure TLabeledDbImageLink.DblClick;
begin
  LoadImageFromFile;
end;

procedure TLabeledDbImageLink.SetCursor(const Value: TCursor);
begin
  inherited Cursor := Value;
  if not (csDesigning in ComponentState) then
    FImage.Cursor := Value;
end;

procedure TLabeledDbImageLink.ImageDblClick(Sender : TObject);
begin
  LoadImageFromFile;
end;

procedure TLabeledDbImageLink.SetImagesPath(const Value: string);
begin
  if FImagesPath <> Value then
  begin
    FImagesPath := ValidPath(Value);
  end;
end;

procedure TLabeledDbImageLink.PanelEnter(Sender: TObject);
begin
  Color := GetStyledColor(clWindow);
end;

procedure TLabeledDbImageLink.PanelExit(Sender: TObject);
begin
  ParentColor := True;
end;

function TLabeledDbImageLink.CalcCaption: string;
begin
  Result := '';
  case EmptyCaptionStyle of
    csSize : Result := IntToStr((ClientWidth-(BorderWidth*2)))+'x'+IntToStr((ClientHeight-(BorderWidth*2)));
    csDisplayLabel :
    begin
      if Assigned(Field) then
        Result := Field.DisplayLabel
      else
        Result := '';
    end;
  end;
end;

procedure TLabeledDbImageLink.SetParent(AParent: TWinControl);
begin
  inherited;
  if (Caption = '') and (Parent <> nil) and isEmpty then
    Caption := CalcCaption;
  SetParentOfLabel(FBoundLabel,AParent,Self);
end;

procedure TLabeledDbImageLink.Resize;
begin
  inherited;
  if isEmpty then
    Caption := CalcCaption;
end;

procedure TLabeledDbImageLink.ClearImage;
begin
  FImage.Picture.Graphic := nil;
end;

function TLabeledDbImageLink.isEmpty: boolean;
begin
  Result := True;
  if (FImage = nil) or (FImage.Picture = nil) then exit;
  Result := (FImage.Picture.Graphic = nil ) or FImage.Picture.Graphic.Empty;
end;

procedure TLabeledDbImageLink.DataSetChanged(Sender: TFieldDataLink);
begin
  ;
end;

procedure TLabeledDbImageLink.LayoutChanged(Sender: TFieldDataLink);
begin
  ;
end;

procedure TLabeledDbImageLink.LinkActive(Sender: TFieldDataLink;
  Active: boolean);
begin
  if Active then
    SetImage
  else
    ClearImage;
end;

procedure TLabeledDbImageLink.LinkEditingChanged(Sender: TFieldDataLink);
begin
  ;
end;

procedure TLabeledDbImageLink.RecordChanged(Sender: TFieldDataLink);
begin
  ;
end;

procedure TLabeledDbImageLink.UpdateData(Sender: TFieldDataLink);
begin
  ;
end;procedure TLabeledDbImageLink.UpdateHint;
begin
  if ImageFileName <> '' then
    Hint := CalcCaption+sLineBreak+ImageFileName
  else if Assigned(Field) then
    Hint := Field.DisplayLabel
  else
    Hint := '';
  FImage.Hint := Hint;
end;

procedure TLabeledDbImageLink.SetImage;
begin
  //Carica l'immagine a partire dal nome file contenuto nel campo
  if (ImageFileName <> '') then
  begin
    Try
      if FileExists(ImageFileName) then
      begin
        FImage.Picture.LoadFromFile(ImageFileName);
        Caption := '';
      end
      else
      begin
        Caption := Format('?"%s"?',[ExtractFileName(ImageFileName)]);
        ClearImage;
      end;
    Except
      Caption := Format('?"%s"?',[ExtractFileName(ImageFileName)]);
      ClearImage;
    End;
  end
  else
  begin
    ClearImage;
    Caption := CalcCaption;
  end;
  UpdateHint;
end;

function TLabeledDbImageLink.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TLabeledDbImageLink.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> Value then
    begin
      FDataLink.DataSource := Value;
    end;
  end;
end;

function TLabeledDbImageLink.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TLabeledDbImageLink.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TLabeledDbImageLink.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  SetImage;
end;

procedure TLabeledDbImageLink.DataLinkDataEvent(Event: TDataEvent;
  Info: Integer);
begin
  inherited;
  if ((Event = deFieldChange) and (Info<>0) and (TField(Info)=Field)) or
     (Event = deDataSetScroll) or
     (Event = deDataSetChange) or
     (Event = deLayoutChange) then
  begin
    SetImage;
  end;
end;

function TLabeledDbImageLink.ImageFileName: string;
begin
  if (FDataLink.Field <> nil) and (FDataLink.Field.AsString <> '') then
  begin
    if FileExists(FDataLink.Field.AsString) then
      Result := FDataLink.Field.AsString
    else if ImagesPath <> '' then
      Result := ImagesPath+FDataLink.Field.AsString
    else
      Result := FDataLink.Field.AsString;
  end
  else
    ImageFileName := '';
end;

procedure TLabeledDbImageLink.SetImageField(const FileName: string);
begin
  //Se c'è una ImagesPath specificata controllo che corrisponda
  if (ImagesPath <> '') and (FileName <> '') and
    not SamePath(ExtractFilePath(FileName), ImagesPath) then
      raise EInOutError.CreateHelp(ERR_NO_CHANGE_DIR,2000);
  DataLink.Edit;
  if DataLink.Field <> nil then
  begin
    if ImagesPath <> '' then
      DataLink.Field.AsString := ExtractFileName(FileName)
    else
      DataLink.Field.AsString := FileName;
  end;
  Change;
end;

procedure TLabeledDbImageLink.SetReadOnly(const Value: boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TLabeledDbImageLink.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TLabeledDbImageLink.Change;
begin
  FDataLink.Modified;
end;

procedure TLabeledDbImageLink.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) then
  begin
    SetImageField('');
  end
  else if (Key = VK_INSERT) then
  begin
    FDataLink.Edit;
    LoadImageFromFile;
  end;
end;

procedure TLabeledDbImageLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
  if (AComponent = FBoundLabel) and (Operation = opRemove) then
    FBoundLabel := nil;
end;

function TLabeledDbImageLink.GetImagesPath: string;
begin
  if FImagesPath = '' then
    Result := GetImageLinkPath
  else
    Result := FImagesPath;
end;

procedure TLabeledDbImageLink.SetBoundCaption(const Value: TCaption);
begin
  ChangeBoundCaption(Value,FBoundLabel,self);
  FBoundCaption := Value;
end;

procedure TLabeledDbImageLink.VisibleChanging;
begin
  inherited;
  FBoundLabel.Visible := not Visible;
end;

procedure TLabeledDbImageLink.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  RecalculateBounds(ALeft, ATop, AWidth, AHeight, FBoundLabel, Self);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBoundLabel(FBoundLabel, Self );
end;

procedure TLabeledDbImageLink.Click;
begin
  inherited;
end;

procedure TLabeledDbImageLink.SetEmptyCaptionStyle(const Value: TDbImageLinkEmptyCaptionStyle);
begin
  FEmptyCaptionStyle := Value;
  if isEmpty then
    Caption := CalcCaption;
end;

{ TLabeledImageDataLink }

procedure TLabeledImageDataLink.ActiveChanged;
begin
  inherited;
  FCBXDbImageLink.LinkActive(Self, Active);
end;

constructor TLabeledImageDataLink.Create(CBXDbImageLink: TLabeledDbImageLink);
begin
  inherited Create;
  FCBXDbImageLink := CBXDbImageLink;
  VisualControl := True;
end;

procedure TLabeledImageDataLink.DataEvent(Event: TDataEvent; Info:{$IFNDEF DXE2+}LongInt{$ELSE}NativeInt{$ENDIF});
begin
  inherited;
  FCBXDbImageLink.DataLinkDataEvent(Event, Info);
end;

procedure TLabeledImageDataLink.DataSetChanged;
begin
  inherited;
  FCBXDbImageLink.DataSetChanged(Self);
end;

procedure TLabeledImageDataLink.EditingChanged;
begin
  inherited;
  FCBXDbImageLink.LinkEditingChanged(Self);
end;

procedure TLabeledImageDataLink.FocusControl(Field: TFieldRef);
begin
  inherited;
  ;
end;

procedure TLabeledImageDataLink.LayoutChanged;
begin
  inherited;
  FCBXDbImageLink.LayoutChanged(Self);
end;

procedure TLabeledImageDataLink.RecordChanged(Field: TField);
begin
  inherited;
  FCBXDbImageLink.RecordChanged(self);
end;

procedure TLabeledImageDataLink.UpdateData;
begin
  inherited;

end;

initialization
  ImageLinkPath := '';

end.
