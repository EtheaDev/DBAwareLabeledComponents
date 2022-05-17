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
unit Vcl.LabeledShellUtils;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  SysUtils
  , WinApi.Windows
  , WinApi.ShlObj
  , WinApi.ActiveX
  , System.Classes
  , WinApi.ShellAPI
  , System.Win.Registry
  , Vcl.Graphics
  ;

const
  APP_PATH = '{app}';
  DICTIONARY_PATH = '{DictionaryPath}';
  USERTEMP_PATH = '{UserTemp}';
  STORAGE_PATH = '{StoragePath}';

Type
  PHICON = ^HICON;

  TCBSystemFolder = (
    sfApplication,
    sfDictionaryPath,
    sfUserTemp,
    sfStoragePath,
    sfuserdesktop,
    sfinternet,
    sfuserprograms,
    sfcontrols,
    sfprinters,
    sfuserdocs,
    sfuserfavorites,
    sfuserstartup,
    sfuserrecent,
    sfusersendto,
    sfbitbucket,
    sfuserstartmenu,
    sfdesktopDir,
    sfdrives,
    sfnetwork,
    sfnethood,
    sffonts,
    sftemplates,
    sfcommonstartmenu,
    sfcommonprograms,
    sfcommonstartup,
    sfcommondesktop,
    sfcommondocuments,
    sfuserappdata,
    sflocalappdata,
    sfprinthood,
    sfaltstartup,
    sfcommonaltstartup,
    sfcommonfavorites,
    sfinternalcache,
    sfcookies,
    sfhistory,
    sfProgramFiles,
    sfCommonProgramFiles,
    sfSystem);

Const
  ACBSystemFolderId : Array[TCBSystemFolder] of integer = (
    -1                           ,
    -2                           ,
    -3                           ,
    -4                           ,
    CSIDL_DESKTOP                ,
    CSIDL_INTERNET               ,
    CSIDL_PROGRAMS               ,
    CSIDL_CONTROLS               ,
    CSIDL_PRINTERS               ,
    CSIDL_PERSONAL               ,
    CSIDL_FAVORITES              ,
    CSIDL_STARTUP                ,
    CSIDL_RECENT                 ,
    CSIDL_SENDTO                 ,
    CSIDL_BITBUCKET              ,
    CSIDL_STARTMENU              ,
    CSIDL_DESKTOPDIRECTORY       ,
    CSIDL_DRIVES                 ,
    CSIDL_NETWORK                ,
    CSIDL_NETHOOD                ,
    CSIDL_FONTS                  ,
    CSIDL_TEMPLATES              ,
    CSIDL_COMMON_STARTMENU       ,
    CSIDL_COMMON_PROGRAMS        ,
    CSIDL_COMMON_STARTUP         ,
    CSIDL_COMMON_DESKTOPDIRECTORY,
    CSIDL_COMMON_DOCUMENTS       ,
    CSIDL_APPDATA                ,
    CSIDL_LOCAL_APPDATA          ,
    CSIDL_PRINTHOOD              ,
    CSIDL_ALTSTARTUP             ,
    CSIDL_COMMON_ALTSTARTUP      ,
    CSIDL_COMMON_FAVORITES       ,
    CSIDL_INTERNET_CACHE         ,
    CSIDL_COOKIES                ,
    CSIDL_HISTORY                ,
    CSIDL_PROGRAM_FILES          ,
    CSIDL_PROGRAM_FILES_COMMON   ,
    CSIDL_SYSTEM                 );

  ASystemFolderAlias : Array[TCBSystemFolder] of string = (
    APP_PATH,
    DICTIONARY_PATH,
    USERTEMP_PATH,
    STORAGE_PATH,
    '{userdesktop}',
    '{internet}',
    '{userprograms}',
    '{controls}',
    '{printers}',
    '{userdocs}',
    '{userfavorites}',
    '{userstartup}',
    '{userrecent}',
    '{usersendto}',
    '{bitbucket}',
    '{userstartmenu}',
    '{desktopdir}',
    '{drives}',
    '{network}',
    '{nethood}',
    '{fonts}',
    '{templates}',
    '{commonstartmenu}',
    '{commonprograms}',
    '{commonstartup}',
    '{commondesktop}',
    '{commondocs}',
    '{userappdata}',
    '{localappdata}',
    '{printhood}',
    '{altstartup}',
    '{commonaltstartup}',
    '{commonfavorites}',
    '{internalcache}',
    '{cookies}',
    '{history}',
    '{programfiles}',
    '{commonprogramfiles}',
    '{system}');

function GetSystemPath(ApplicationHandle : HWND; Folder: TCBSystemFolder): String;
function GetTempDirectoryDelim: String;
function RealizeAliasPath(ApplicationHandle : HWND; const AliasPath : string) : string;
procedure RealizeSystemPath(ApplicationHandle : HWND; var Path : string);
function RealizeSystemFileName(ApplicationHandle : HWND; const FileName : string) : string;
procedure RegisterDictionaryPath(const Path : string);
procedure RegisterStoragePath(const Path : string);
procedure GlobalLoadFileList(const Path, WildCard: string; FileList: TStringList);

//Copia i files di una cartella in un'altra
function CopyFilesTo(ApplicationHandle : HWND; const SourceFolder, DestFolder, WildCard: string;
  Overwrite : boolean = false; OnlyCheck: boolean = False; const LogFileName: string = ''): Boolean;

//Recupera l'icona associata ad un file
procedure GetAssociatedIcon(ApplicationHandle : HWND;
  const FileName: TFilename; PLargeIcon, PSmallIcon: PHICON);

function InternalExecuteApplication(BatchCommand: string; Visibility: Integer; Wait: Boolean;
  const RetainProcessHandle: Boolean; out ProcessHandle: Cardinal;
  const CurrentDirectory: string = ''): Cardinal;

//Apre il file utilizzando ShellExecute
procedure ShowFileByShellExecute(Handle: HWND; const FileName: string);

//Copiare un file in una destinazione
function CopyFileTo(const SourceFileName, DestFileName: string;
  Overwrite : boolean = false): Boolean;

procedure SetImageLinkPath(const Path : string);
function GetImageLinkPath : string;

procedure SetDefaultBlobFilePath(const Path : string);
function GetDefaultBlobFilePath: string;

function OpenDialogExecute(var FileName : string; const Title : string = ''; const Filter : string = '';
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;

function ColorDialogExecute(var Color : TColor; const Title : string = '';
  CustomColors: TStrings = nil;
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;

implementation

uses
  System.AnsiStrings
  , Vcl.Dialogs
  , Vcl.DbAwareLabeledUtils
  , Vcl.DbAwareLabeledConsts
  ;

var
  DictionaryPath : string;
  StoragePath : string;
  ImageLinkPath : string;
  DefaultBlobfilePath : string;

function CopyFileTo(const SourceFileName, DestFileName: string;
  Overwrite : boolean = false): Boolean;
begin
  Result := CopyFile(PChar(SourceFileName), PChar(DestFileName), not Overwrite);
end;

procedure SetDefaultBlobFilePath(const Path : string);
begin
  DefaultBlobfilePath := ValidPath(Path);
end;

function GetDefaultBlobFilePath: string;
begin
  Result := DefaultBlobfilePath;
end;

procedure SetImageLinkPath(const Path : string);
begin
  ImageLinkPath := ValidPath(Path);
end;

function GetImageLinkPath : string;
begin
  Result := ImageLinkPath;
end;

procedure RegisterDictionaryPath(const Path : string);
begin
  DictionaryPath := IncludeTrailingPathDelimiter(Path);
end;

procedure RegisterStoragePath(const Path : string);
begin
  StoragePath := IncludeTrailingPathDelimiter(Path);
end;

function RealizeAliasPath(ApplicationHandle : HWND; const AliasPath : string) : string;
var
  i : TCBSystemFolder;
  p : integer;
  Alias : string;
  NewPath : string;
begin
  Result := AliasPath;
  for i := Low(TCBSystemFolder) to High(TCBSystemFolder) do
  begin
    Alias := ASystemFolderAlias[i];
    p := pos(UpperCase(Alias),UpperCase(AliasPath));
    if p > 0 then
    begin
      //Sostituisco l'alias con la directory vera:
      //l'alias {app} {Dictionary} sono particolari: non uso GetSystemPath
      if SameText(Alias,APP_PATH) then
      begin
        NewPath := ExtractFilePath(ParamStr(0));
        NewPath := Copy(NewPath,1,length(NewPath)-1);
      end
      else if SameText(Alias,DICTIONARY_PATH) then
      begin
        NewPath := DictionaryPath;
        NewPath := Copy(NewPath,1,length(NewPath)-1);
      end
      else if SameText(Alias,STORAGE_PATH) then
      begin
        NewPath := StoragePath;
        NewPath := Copy(NewPath,1,length(NewPath)-1);
      end
      else if SameText(Alias,USERTEMP_PATH) then
      begin
        NewPath := GetTempDirectoryDelim;
        NewPath := Copy(NewPath,1,length(NewPath)-1);
      end
      else
        NewPath := String(GetSystemPath(ApplicationHandle,i));
      if Copy(NewPath,length(NewPath),1) = PathDelim then
        NewPath := Copy(NewPath,1,length(NewPath)-1);

      Result := Copy(AliasPath,1,p-1)+NewPath+Copy(AliasPath,p+length(Alias),MaxInt);
      break;
    end;
  end;
end;

function GetTempDirectoryDelim: String;
var
   lng: DWORD;
begin
  SetLength(Result, MAX_PATH) ;
  lng := GetTempPath(MAX_PATH, PChar(Result)) ;
  SetLength(Result, lng);
end;

function GetSystemPath(ApplicationHandle : HWND; Folder: TCBSystemFolder): String;
var
  PIDL: PItemIDList;
  Path: LPSTR;
  AMalloc: IMalloc;
  FolderId : integer;
begin
  Path := AnsiStrAlloc(MAX_PATH);
  FolderId := ACBSystemFolderId[Folder];
  SHGetSpecialFolderLocation(ApplicationHandle, FolderId, PIDL);
  if SHGetPathFromIDListA(PIDL, Path) then
    Result := String(Path);
  SHGetMalloc(AMalloc);
  AMalloc.Free(PIDL);
  {$IFDEF DXE6+}
  System.AnsiStrings.StrDispose(Path);
  {$ELSE}
  StrDispose(Path);
  {$ENDIF}
end;

procedure RealizeSystemPath(ApplicationHandle : HWND; var Path : string);
var
  j : integer;
  PartialPath : string;
  PathChanged : boolean;
  PathList : TStringList;

  function ChangePartialPath : boolean;
  var
    i : TCBSystemFolder;
    SystemPath, AliasPath : string;
  begin
    Result := False;
    for i := Low(TCBSystemFolder) to High(TCBSystemFolder) do
    begin
      AliasPath := ASystemFolderAlias[i]+PathDelim;
      SystemPath := RealizeAliasPath(ApplicationHandle,AliasPath);
      if SameText(PartialPath, SystemPath) then
      begin
        PartialPath := AliasPath;
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  //Cerco di convertire la Path passata usando la nomenclatura di ASystemFolderAlias
  //es: Path = 'C:\Documents and Settings\Carlo\Cartella\' => '{userdocs}\Cartella\'
  //Per far ciò devo costruire la Path a "pezzi":
  //es. prima provo con C:\Documents and Settings\ poi con C:\Documents and Settings\Carlo\ e così via.
  PathList := TStringList.Create;
  try
    PathList.Text := StringReplace(Path,PathDelim,sLineBreak,[rfReplaceAll]);
    PartialPath := '';
    PathChanged := False;
    for j := 0 to PathList.Count -1 do
    begin
      PartialPath := PartialPath+PathList.Strings[j]+PathDelim;
      if not PathChanged then
        PathChanged := ChangePartialPath;
    end;
    if PathChanged then
      Path := PartialPath;
  finally
    PathList.Free;
  end;
end;  

function RealizeSystemFileName(ApplicationHandle : HWND; const FileName : string) : string;
var
  Path: string;
begin
  Path := ExtractFilePath(FileName);
  RealizeSystemPath(ApplicationHandle, Path);
  Result := Path+ExtractFileName(FileName);
end;

procedure GlobalLoadFileList(const Path, WildCard: string; FileList: TStringList);
var
  SearchRec: TSearchRec;
  R: Integer;
  PathWithWildCards: string;
begin
  PathWithWildCards := IncludeTrailingPathDelimiter(Path) + WildCard;
  //Find the first file
  R := SysUtils.FindFirst(PathWithWildCards, faAnyFile, SearchRec);
  try
    while R = 0 do // file found!
    begin
      FileList.Append(SearchRec.Name); // Add file to list
      R := SysUtils.FindNext(SearchRec); // Find next file
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

//Copiare tutti i files di una cartella in un'altra
function CopyFilesTo(ApplicationHandle : HWND; const SourceFolder, DestFolder, WildCard: string;
  Overwrite : boolean = false; OnlyCheck: boolean = False; const LogFileName: string = ''): Boolean;
var
  FileList : TStringList;
  i : integer;
  FileSource, FileDest : string;
  DirSource, DirDest : string;
  f: TextFile;

  function Logging: boolean;
  begin
    Result := not OnlyCheck and (LogFileName <> '');
  end;

begin
  Result := not OnlyCheck;
  DirSource := IncludeTrailingPathDelimiter(SourceFolder);
  DirDest := IncludeTrailingPathDelimiter(DestFolder);

  FileList := TStringList.Create;
  try
    if Logging then
    begin
      AssignFile(f, LogFileName);
      if FileExists(LogFileName) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, Format('%s - CopyFiles: Source Folder=''%s'' Dest Folder=''%s'' WildCard=''%s'' Overwrite=%s',
        [DateTimeToStr(Now), SourceFolder, DestFolder, WildCard, BoolToStr(Overwrite)]));
    end;

    GlobalLoadFileList(DirSource, WildCard,FileList);
    for i := 0 to FileList.Count -1 do
    begin
      FileSource := DirSource+FileList.Strings[i];
      if FileExists(FileSource) then
      begin
        FileDest := RealizeAliasPath(ApplicationHandle,DirDest)+ExtractFileName(FileSource);
        if OnlyCheck and not FileExists(FileDest) then
        begin
          Result := True;
          break;
        end;
        if not FileExists(FileDest) or Overwrite then
        begin
          if CopyFile(PChar(FileSource), PChar(FileDest), not Overwrite) then
          begin
            if Logging then
              WriteLn(f, Format('SUCCESS: %s - File Copied: FileName=''%s''',
                [DateTimeToStr(Now), ExtractFileName(FileDest)]));
          end
          else
          begin
            Result := False; //Almeno un file non è stato copiato
            if Logging then
              WriteLn(f, Format('ERROR: %s - File not copied: FileName=''%s''',
                [DateTimeToStr(Now), ExtractFileName(FileDest)]));
          end;
        end
        else if FileExists(FileDest) then
        begin
          if Logging then
            WriteLn(f, Format('WARNING: %s - File already Exists: FileName=''%s''',
              [DateTimeToStr(Now), ExtractFileName(FileDest)]));
        end;
      end;
    end;
  finally
    FileList.Free;
    if Logging then
      CloseFile(f);
  end;
end;

procedure GetAssociatedIcon(ApplicationHandle : HWND;
  const FileName: TFilename; PLargeIcon, PSmallIcon: PHICON);
var
  IconIndex: integer;
  FileExt, FileType: string;
  Reg: TRegistry;
  p: integer;
  p1, p2: pchar;
  IconFileName : TFilename;
  IconRegKey : string;
begin
  PLargeIcon^ := 0;
  PSmallIcon^ := 0;
  IconIndex := 0;
  // Recupero l'estensione
  FileExt := UpperCase(ExtractFileExt(FileName));
  if ((FileExt = '.EXE') or (FileExt = '.ICO')) and
    FileExists(FileName) then
  begin
    // se il file è un exe o una icona recupero l'immagine da se stesso
    IconFileName := FileName;
  end
  else
  begin
    // altrimenti cerco il file associato ad esso
    Reg := nil;
    try
      Reg := TRegistry.Create(KEY_QUERY_VALUE);
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if FileExt = '.EXE' then FileExt := '.COM';
      if Reg.OpenKeyReadOnly(FileExt) then
        try
          FileType := Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;

      if (FileType <> '') and Reg.OpenKeyReadOnly(FileType + '\CurVer') then
      begin
        try
          IconRegKey := Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;
      end
      else
        IconRegKey := FileType;

      if (FileType <> '') and Reg.OpenKeyReadOnly(IconRegKey+'\DefaultIcon') then
        try
          IconFileName := Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;
    finally
      Reg.Free;
    end;
  end;

  if IconFileName <> '' then
  begin
    // Recupera il filename e l'indice dell'icona
    p1 := PChar(IconFileName);
    p2 := StrRScan(p1, ',');
    if p2 <> nil then begin
      p := p2 - p1 + 1; // Posizione della virgola
      IconIndex := StrToInt(Copy(IconFileName, p + 1, Length(IconFileName) - p));
      SetLength(IconFileName, p - 1);
    end;
  end;

  if (IconFileName <> '') then
    ExtractIconEx(pchar(IconFileName), IconIndex, PLargeIcon^, PSmallIcon^, 1);
  // Tenta di recuperare l'icona
  if (IconFileName = '') or ((IconIndex = 0) and (PLargeIcon^ = 0) and (PLargeIcon^ = 0)) then
  begin
    // Operazione fallita: il file non ha associazioni
    // Cerca di prendere le icone di default da SHELL32.DLL
    IconFileName := String(GetSystemPath(ApplicationHandle, sfSystem))+
      PathDelim+'shell32.dll';
    // Determina le icone di default per alcuni tipi di dati
    if      (FileExt = '.DOC')
         or (FileExt = '.DOT')
         or (FileExt = '.ODT')
         or (FileExt = '.OTT')
         or (FileExt = '.SXW')
         or (FileExt = '.STW')
         or (FileExt = '.RTM') then IconIndex := 1 //Associo l'icona del documento a Word, LibreOffice/OpenOffice e ReportBuilder
    else if (FileExt = '.EXE')
         or (FileExt = '.COM') then IconIndex := 2
    else if (FileExt = '.HLP') then IconIndex := 23
    else if (FileExt = '.INI')
         or (FileExt = '.INF') then IconIndex := 63
    else if (FileExt = '.TXT') then IconIndex := 64
    else if (FileExt = '.BAT') then IconIndex := 65
    else if (FileExt = '.DLL')
         or (FileExt = '.SYS')
         or (FileExt = '.VBX')
         or (FileExt = '.OCX')
         or (FileExt = '.VXD') then IconIndex := 66
    else if (FileExt = '.FON') then IconIndex := 67
    else if (FileExt = '.TTF') then IconIndex := 68
    else if (FileExt = '.FOT') then IconIndex := 69
    else
      IconIndex := 0;
    // Attempt to get the icon.
    ExtractIconEx(pchar(IconFileName), IconIndex, PLargeIcon^, PSmallIcon^, 1);
  end;
end;

// Lancia l'applicazione indicata.
// FileName è il nome, completo di percorso, dell'eseguibile da lanciare.
// Visibility è una delle costanti SW_XXX definite in windows.pas.
// Se Wait = True, aspetta che il processo lanciato termini e ne ritorna l'exit code,
// oppure -1 in caso di errori.
// Se Wait = False, ritorna 0 se tutto è ok o -1 in caso di errori.
// Se RetainProcessHandle è True, l'handle del processo lanciato è restituito
// in ProcessHandle. ATTENZIONE: in questo caso è responsabilità del chiamante
// liberare l'handle, non appena smette di averne bisogno, tramite CloseHandle().
// Se la CurrentDirectory è indicata, viene usata come cartella di avvio per
// l'applicazione, altrimenti la cartella di avvio è quella predefinita (cioè
// di norma quella del processo chiamante).
function InternalExecuteApplication(BatchCommand: string; Visibility: Integer; Wait: Boolean;
  const RetainProcessHandle: Boolean; out ProcessHandle: Cardinal;
  const CurrentDirectory: string = ''): Cardinal;
var
  zAppName: array[0..511] of Char;
  zCurDir: array[0..511] of Char;
  PCurDir: PChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CreateProcessReturnValue: Boolean;
  IntResult: DWORD;
begin
  ProcessHandle := 0;
  StrPCopy(zAppName, BatchCommand);
  if CurrentDirectory <> '' then begin
    StrPCopy(zCurDir, CurrentDirectory);
    PCurDir := zCurDir;
  end
  else
    PCurDir := nil;
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  CreateProcessReturnValue :=
    CreateProcess(nil,
    zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    False, { handle inheritance flag }
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, { creation flags }
    nil, { pointer to new environment block }
    PCurDir, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo { return information }
    );
  if CreateProcessReturnValue then begin
    // (ND 09/11/1999) Occorre decrementare il conteggio dei riferimenti dei
    // due handle ricevuti, in modo che questi vengano eliminati al termine
    // del processo. Il thread handle lo chiudo subito, il process handle solo
    // dopo averlo utilizzato.
    CloseHandle(ProcessInfo.hThread);
    if Wait and not RetainProcessHandle then begin
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, IntResult);
      Result := IntResult;
    end
    else
      Result := 0;
    if RetainProcessHandle then
      ProcessHandle := ProcessInfo.hProcess
    else
      CloseHandle(ProcessInfo.hProcess);
  end
  else
    Result := 1;
end;

procedure ShowFileByShellExecute(Handle: HWND; const FileName: string);
var
  FileNameEx: string;
begin
  //Correggo eventuali alias contenuti nel filename
  FileNameEx := RealizeAliasPath(Handle, FileName);
  //Lancio ShellExecute in modalità Open e Show
  ShellExecute( Handle, 'open' , PChar(FileNameEx), nil, nil, SW_SHOW );
end;

function OpenDialogExecute(var FileName : string; const Title : string = ''; const Filter : string = '';
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;
var
  od : TOpenDialog;
begin
  Result := False;
  od := TOpenDialog.Create(nil);
  Try
    if Filter = '' then
      od.Filter := ALL_FILES_FILTER
    else
      od.Filter := Filter;
    if Title = '' then
      od.Title := DEFAULT_DIALOG_TITLE
    else
      od.Title := Title;
    od.InitialDir := ExtractFilePath(FileName);
    od.FileName := ExtractFileName(FileName);
    od.DefaultExt := ExtractFileExt(FileName);
    if Assigned(BeforeDialogExecute) then
      BeforeDialogExecute(od);
    if od.Execute then
    begin
      FileName := od.FileName;
      Result := True;
    end;
  Finally
    od.Free;
  End;
end;

function ColorDialogExecute(var Color : TColor; const Title : string = '';
  CustomColors: TStrings = nil;
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;
var
  cd : TColorDialog;
begin
  Result := False;
  cd := TColorDialog.Create(nil);
  Try
    cd.Options := [cdAnyColor];

    cd.Color := Color;
    if CustomColors <> nil then
      cd.CustomColors.Assign(CustomColors);
(*
    if Title = '' then
      cd.Title := DEFAULT_COLORDIALOG_TITLE
    else
      cd.Title := Title;
*)
    if Assigned(BeforeDialogExecute) then
      BeforeDialogExecute(cd);
    if cd.Execute then
    begin
      Color := cd.Color;
      Result := True;
    end;
  Finally
    cd.Free;
  End;
end;

initialization
  DictionaryPath := '';
  ImageLinkPath := '';
  DefaultBlobfilePath := '';

end.
