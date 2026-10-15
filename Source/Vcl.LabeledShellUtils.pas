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
unit Vcl.LabeledShellUtils;

{$I 'DBAwareLabeledComponents.inc'}

interface

uses
  Winapi.Windows,
  Winapi.ShlObj,
  Winapi.ActiveX,
  Winapi.ShellAPI,
  System.Win.Registry,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

const
  APP_PATH = '{app}';
  DICTIONARY_PATH = '{DictionaryPath}';
  USERTEMP_PATH = '{UserTemp}';
  STORAGE_PATH = '{StoragePath}';

Type
  /// <summary>
  ///  Pointer to a Windows HICON handle, used to return icon handles by reference.
  /// </summary>
  PHICON = ^HICON;

  /// <summary>
  ///  Enumeration of supported system/special folders. The first four values
  ///  (sfApplication, sfDictionaryPath, sfUserTemp, sfStoragePath) are custom
  ///  application-level locations; the remaining values map to Windows CSIDL
  ///  special-folder identifiers.
  /// </summary>
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
  /// <summary>
  ///  Maps each TCBSystemFolder value to its Windows CSIDL identifier. The four
  ///  custom application folders use negative sentinel values (-1..-4) since they
  ///  are not resolved through the Windows shell.
  /// </summary>
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

  /// <summary>
  ///  Maps each TCBSystemFolder value to its textual alias placeholder (for
  ///  example '{userdocs}'). These aliases can be embedded in paths and later
  ///  expanded to the real folder location by RealizeAliasPath.
  /// </summary>
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

/// <summary>
///  Returns the full file-system path of the requested system/special folder.
/// </summary>
/// <param name="ApplicationHandle">Window handle used as owner for the shell call.</param>
/// <param name="Folder">The system folder to resolve.</param>
/// <returns>The absolute path of the requested folder, or an empty string if it cannot be resolved.</returns>
function GetSystemPath(ApplicationHandle : HWND; Folder: TCBSystemFolder): String;
/// <summary>
///  Returns the current user's temporary directory, including the trailing path delimiter.
/// </summary>
/// <returns>The temporary directory path terminated by a path delimiter.</returns>
function GetTempDirectoryDelim: String;
/// <summary>
///  Expands the first folder alias placeholder (for example '{app}' or '{userdocs}')
///  found in the supplied path into its real file-system location.
/// </summary>
/// <param name="ApplicationHandle">Window handle used as owner for shell folder lookups.</param>
/// <param name="AliasPath">A path that may contain an alias placeholder.</param>
/// <returns>The path with the first matching alias replaced; the unchanged path if no alias is found.</returns>
function RealizeAliasPath(ApplicationHandle : HWND; const AliasPath : string) : string;
/// <summary>
///  Performs the reverse of RealizeAliasPath: scans the given path and, when it
///  starts with a known system folder, replaces that portion with the matching
///  alias placeholder. The path is updated in place.
/// </summary>
/// <param name="ApplicationHandle">Window handle used as owner for shell folder lookups.</param>
/// <param name="Path">The path to convert; modified in place when a match is found.</param>
procedure RealizeSystemPath(ApplicationHandle : HWND; var Path : string);
/// <summary>
///  Converts the directory portion of a file name to alias-based notation using
///  RealizeSystemPath, leaving the file name component unchanged.
/// </summary>
/// <param name="ApplicationHandle">Window handle used as owner for shell folder lookups.</param>
/// <param name="FileName">The fully qualified file name to convert.</param>
/// <returns>The file name with its directory portion expressed using folder aliases when applicable.</returns>
function RealizeSystemFileName(ApplicationHandle : HWND; const FileName : string) : string;
/// <summary>
///  Registers the global dictionary path used to resolve the '{DictionaryPath}' alias.
/// </summary>
/// <param name="Path">The dictionary directory to register; a trailing delimiter is added automatically.</param>
procedure RegisterDictionaryPath(const Path : string);
/// <summary>
///  Registers the global storage path used to resolve the '{StoragePath}' alias.
/// </summary>
/// <param name="Path">The storage directory to register; a trailing delimiter is added automatically.</param>
procedure RegisterStoragePath(const Path : string);
/// <summary>
///  Loads the names of all files in the given directory matching the wildcard
///  into the supplied string list.
/// </summary>
/// <param name="Path">The directory to scan.</param>
/// <param name="WildCard">The file mask to match (for example '*.txt').</param>
/// <param name="FileList">The list to which the matching file names are appended.</param>
procedure GlobalLoadFileList(const Path, WildCard: string; FileList: TStringList);

/// <summary>
///  Copies all files matching the wildcard from a source folder to a destination
///  folder, with optional overwrite, check-only and logging behaviour.
/// </summary>
/// <param name="ApplicationHandle">Window handle used to resolve any alias in the destination folder.</param>
/// <param name="SourceFolder">The folder to copy files from.</param>
/// <param name="DestFolder">The destination folder; may contain a folder alias.</param>
/// <param name="WildCard">The file mask selecting which files to copy.</param>
/// <param name="Overwrite">When True, existing destination files are overwritten.</param>
/// <param name="OnlyCheck">When True, no copy is performed; returns True if at least one file would need copying.</param>
/// <param name="LogFileName">Optional log file path; when set, copy results are appended to it.</param>
/// <returns>In check-only mode, True if a file is missing at the destination; otherwise True when all files were copied successfully.</returns>
//Copies the files of one folder into another
function CopyFilesTo(ApplicationHandle : HWND; const SourceFolder, DestFolder, WildCard: string;
  Overwrite : boolean = false; OnlyCheck: boolean = False; const LogFileName: string = ''): Boolean;

/// <summary>
///  Retrieves the large and small icon handles associated with a file, falling
///  back to default SHELL32.DLL icons for common file types when no association exists.
/// </summary>
/// <param name="ApplicationHandle">Window handle used to resolve the system folder when loading SHELL32.DLL.</param>
/// <param name="FileName">The file whose associated icon is requested.</param>
/// <param name="PLargeIcon">Receives the large icon handle.</param>
/// <param name="PSmallIcon">Receives the small icon handle.</param>
//Retrieves the icon associated with a file
procedure GetAssociatedIcon(ApplicationHandle : HWND;
  const FileName: TFilename; PLargeIcon, PSmallIcon: PHICON);

/// <summary>
///  Launches an external application via CreateProcess, optionally waiting for it
///  to finish and optionally returning the process handle to the caller.
/// </summary>
/// <param name="BatchCommand">The full command line of the executable to launch.</param>
/// <param name="Visibility">One of the SW_XXX window-show constants.</param>
/// <param name="Wait">When True (and the handle is not retained), waits for the process to terminate and returns its exit code.</param>
/// <param name="RetainProcessHandle">When True, the process handle is returned in ProcessHandle and the caller is responsible for closing it.</param>
/// <param name="ProcessHandle">Receives the process handle when RetainProcessHandle is True; otherwise 0.</param>
/// <param name="CurrentDirectory">Optional working directory for the launched process; when empty the default is used.</param>
/// <returns>The process exit code when waiting; 0 on success without waiting; 1 if the process could not be created.</returns>
function InternalExecuteApplication(BatchCommand: string; Visibility: Integer; Wait: Boolean;
  const RetainProcessHandle: Boolean; out ProcessHandle: Cardinal;
  const CurrentDirectory: string = ''): Cardinal;

/// <summary>
///  Opens a file with its default associated application using ShellExecute,
///  resolving any folder alias contained in the file name beforehand.
/// </summary>
/// <param name="Handle">Window handle used as owner for the shell operation.</param>
/// <param name="FileName">The file to open; may contain a folder alias.</param>
//Opens the file using ShellExecute
procedure ShowFileByShellExecute(Handle: HWND; const FileName: string);

/// <summary>
///  Copies a single file to a destination, optionally overwriting an existing file.
/// </summary>
/// <param name="SourceFileName">The file to copy.</param>
/// <param name="DestFileName">The destination file name.</param>
/// <param name="Overwrite">When True, an existing destination file is overwritten.</param>
/// <returns>True if the file was copied successfully.</returns>
//Copies a file to a destination
function CopyFileTo(const SourceFileName, DestFileName: string;
  Overwrite : boolean = false): Boolean;

/// <summary>
///  Sets the global default path used for image-link files.
/// </summary>
/// <param name="Path">The image-link directory; normalized via ValidPath.</param>
procedure SetImageLinkPath(const Path : string);
/// <summary>
///  Returns the global default path used for image-link files.
/// </summary>
/// <returns>The currently configured image-link path.</returns>
function GetImageLinkPath : string;

/// <summary>
///  Sets the global default path used when saving or loading BLOB files.
/// </summary>
/// <param name="Path">The default BLOB-file directory; normalized via ValidPath.</param>
procedure SetDefaultBlobFilePath(const Path : string);
/// <summary>
///  Returns the global default path used when saving or loading BLOB files.
/// </summary>
/// <returns>The currently configured default BLOB-file path.</returns>
function GetDefaultBlobFilePath: string;

/// <summary>
///  Displays a standard Open File dialog and returns the file selected by the user.
/// </summary>
/// <param name="FileName">On entry, the initial file/folder; on success, receives the selected file name.</param>
/// <param name="Title">Optional dialog caption; a default title is used when empty.</param>
/// <param name="Filter">Optional file filter; an all-files filter is used when empty.</param>
/// <param name="BeforeDialogExecute">Optional callback invoked with the TOpenDialog before it is shown.</param>
/// <returns>True if the user selected a file and confirmed the dialog.</returns>
function OpenDialogExecute(var FileName : string; const Title : string = ''; const Filter : string = '';
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;

/// <summary>
///  Displays a standard Color selection dialog and returns the chosen color.
/// </summary>
/// <param name="Color">On entry, the initial color; on success, receives the selected color.</param>
/// <param name="Title">Optional dialog caption (currently unused).</param>
/// <param name="CustomColors">Optional set of custom colors to preload into the dialog.</param>
/// <param name="BeforeDialogExecute">Optional callback invoked with the TColorDialog before it is shown.</param>
/// <returns>True if the user selected a color and confirmed the dialog.</returns>
function ColorDialogExecute(var Color : TColor; const Title : string = '';
  CustomColors: TStrings = nil;
  BeforeDialogExecute : TNotifyEvent = nil) : boolean;

implementation

uses
  System.AnsiStrings,
  Vcl.Dialogs,
  Vcl.DbAwareLabeledUtils,
  Vcl.DbAwareLabeledConsts;

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
      //Replace the alias with the real directory:
      //the {app} and {Dictionary} aliases are special: GetSystemPath is not used for them
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
  //Try to convert the given Path using the ASystemFolderAlias naming convention
  //e.g.: Path = 'C:\Documents and Settings\Carlo\Folder\' => '{userdocs}\Folder\'
  //To do this the Path must be built up "piece by piece":
  //e.g. first try C:\Documents and Settings\ then C:\Documents and Settings\Carlo\ and so on.
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
  R := System.SysUtils.FindFirst(PathWithWildCards, faAnyFile, SearchRec);
  try
    while R = 0 do // file found!
    begin
      FileList.Append(SearchRec.Name); // Add file to list
      R := System.SysUtils.FindNext(SearchRec); // Find next file
    end;
  finally
    System.SysUtils.FindClose(SearchRec);
  end;
end;

//Copies all the files of one folder into another
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
            Result := False; //At least one file could not be copied
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
  // Retrieve the extension
  FileExt := UpperCase(ExtractFileExt(FileName));
  if ((FileExt = '.EXE') or (FileExt = '.ICO')) and
    FileExists(FileName) then
  begin
    // if the file is an exe or an icon, take the image from the file itself
    IconFileName := FileName;
  end
  else
  begin
    // otherwise look up the file associated with it
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
    // Retrieve the file name and the icon index
    p1 := PChar(IconFileName);
    p2 := StrRScan(p1, ',');
    if p2 <> nil then begin
      p := p2 - p1 + 1; // Position of the comma
      IconIndex := StrToInt(Copy(IconFileName, p + 1, Length(IconFileName) - p));
      SetLength(IconFileName, p - 1);
    end;
  end;

  if (IconFileName <> '') then
    ExtractIconEx(pchar(IconFileName), IconIndex, PLargeIcon^, PSmallIcon^, 1);
  // Attempt to retrieve the icon
  if (IconFileName = '') or ((IconIndex = 0) and (PLargeIcon^ = 0) and (PLargeIcon^ = 0)) then
  begin
    // Operation failed: the file has no associations
    // Try to take the default icons from SHELL32.DLL
    IconFileName := String(GetSystemPath(ApplicationHandle, sfSystem))+
      PathDelim+'shell32.dll';
    // Determine the default icons for some data types
    if      (FileExt = '.DOC')
         or (FileExt = '.DOT')
         or (FileExt = '.ODT')
         or (FileExt = '.OTT')
         or (FileExt = '.SXW')
         or (FileExt = '.STW')
         or (FileExt = '.RTM') then IconIndex := 1 //Assign the document icon to Word, LibreOffice/OpenOffice and ReportBuilder
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

// Launches the specified application.
// FileName is the full path name of the executable to launch.
// Visibility is one of the SW_XXX constants defined in windows.pas.
// If Wait = True, waits for the launched process to terminate and returns its exit code,
// or -1 in case of errors.
// If Wait = False, returns 0 if everything is ok or -1 in case of errors.
// If RetainProcessHandle is True, the handle of the launched process is returned
// in ProcessHandle. WARNING: in this case it is the caller's responsibility to
// release the handle, as soon as it is no longer needed, by calling CloseHandle().
// If CurrentDirectory is specified, it is used as the startup folder for the
// application; otherwise the startup folder is the default one (that is,
// normally the one of the calling process).
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
    // (ND 09/11/1999) The reference count of the two received handles must be
    // decremented, so that they are released when the process terminates.
    // The thread handle is closed immediately, the process handle only
    // after it has been used.
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
  //Resolve any alias contained in the file name
  FileNameEx := RealizeAliasPath(Handle, FileName);
  //Invoke ShellExecute in Open and Show mode
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
