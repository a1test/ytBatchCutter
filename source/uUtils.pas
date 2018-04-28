unit uUtils;

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Windows, Rtti, TypInfo,
  JclStringLists, System.IniFiles;

const
  INPUT_QUERY_FORCE_PASSWORD = #0;

type

  TPercent = Double;

  TIntPercent = 0..100;

  TNonRefCountedInterfacedObject = TSingletonImplementation;

  TLockablePersistent = class (TPersistent)
  private
    FLock: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  TStringsHelper = class helper for TStrings
  public
    function Delete(const S: string): Boolean; overload;
    function Contains(const S: string): Boolean;
    function ContainsName(const Name: string): Boolean;
    function IndexOfNameOrStr(const S: string): Integer;
    function IndexOfValue(const S: string): Integer;
    function TryGetString(AIndex: Integer): string;
    function AddNameValue(AName: string; AValue: string): Integer;
    function AddFmt(AString: string;const AArgs: array of const): Integer;
    procedure RemoveValues;
    procedure LoadFromFileShared(const FileName: string);
    function IsEmpty: Boolean;


  end;

  TMemIniFileHelper = class helper for TMemIniFile
    procedure ReloadValues;
  end;

  ExceptionHelper = class helper for Exception
  public
    function AddMessageLine(const S: string): string;
    function AddMessage(const S: string): string;
  end;


  TDLLDuplicator = class
  private
    FOriginalDLLPath: string;
    FDllCount: Integer;
    function GetUnlinkedDllPath: string;
  public
    function LoadUnlinkedDll: THandle;
    function FreeDll(AHandle: THandle): Boolean;

    constructor Create(ADLLFile: string);
  end;

  TCustomAttributeClass = class of TCustomAttribute;

  TRttiObjectHelper = class Helper for TRttiObject
    function GetAttribute<T: TCustomAttribute>(out AAttribute: T): Boolean;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
  end;

  TObjectHelper = class helper for TObject
    function TryCast(AClass: TClass; out Obj): Boolean;
    function HasAttributeByPropInfo<T: TCustomAttribute>(APropInfo: PPropInfo): Boolean;
    function HasAttribute<T: TCustomAttribute>(APropName: string): Boolean;
    function GetClassAttribute<T: TCustomAttribute>(out AAttribute: T): Boolean;
  end;


  NameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

const
  cnChanged = 3;

type
  TChangeableObjectList <T: class> = class (TObjectList<T>)
  public
    procedure Changed(Item: T);

  end;

  TDictionaryHelper = class
  public
    class procedure KeyChanged <K, V>(Dictionary: TDictionary <K, V>;
      Key: K); overload;
  end;

  TDictionaryAccess<TKey,TValue> = class (TDictionary<TKey,TValue>)

  end;

type
  TCheckRef = reference to function: Boolean;

function SleepUntil(Milliseconds: Integer; Check: TCheckRef): Boolean;

function StringToPAnsiChar(const S: string): PAnsiChar;
function GetProcAddressAndCheck(hModule: HMODULE; lpProcName: LPCWSTR): FARPROC;
function FindStringIndex(out AType; AArray: array of string; AName: string): Boolean;
function AddNameValue(AStrings: IJclStringList; AName: string; AValue: string): Integer;

procedure WriteAllTextToFile(APath: string; AContents: string);
function RealAllTextFromFile(APath: string): string;

function GetSecondsIdle: Integer;

procedure PatchInstanceClass(Instance: TObject; NewClass: TClass);

function GetExeFilename: TFilename;
function GetExePath: string;
function RemoveFileExt(FileName: string): string;
function AddToFileName(FileName: TFilename; Add: string): string;
function ExtractDirName(const Path: string): string;

implementation

uses
  IOUtils, StrUtils, System.Diagnostics;

function SleepUntil(Milliseconds: Integer; Check: TCheckRef): Boolean;
var
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  try
    repeat
      if Check then
        Exit(True)
      else if Stopwatch.Elapsed.TotalMilliseconds > Milliseconds then
        Exit(False);
      Sleep(1);

    until False;
  finally
    //TraceMsg('SleepUntil elapsed = ' + Stopwatch.Elapsed.Milliseconds.ToString());
  end;
end;

function GetExeFilename: TFilename;
begin
  Result := ParamStr(0);
end;

function GetExePath: string;
begin
  Result := ExtractFilePath(GetExeFilename);
end;

function RemoveFileExt(FileName: string): string;
begin
  Result := ChangeFileExt(FileName, '');
end;

function AddToFileName(FileName: TFilename; Add: string): string;
begin
  Result := ChangeFileExt(Filename, Add + ExtractFileExt(Filename));
end;

function ExtractDirName(const Path: string): string;
begin
  Result := ExtractFileName(ExcludeTrailingPathDelimiter(Path));
end;

procedure PatchInstanceClass(Instance: TObject; NewClass: TClass);
type
  PClass = ^TClass;
begin
  if Assigned(Instance) and Assigned(NewClass)
    and NewClass.InheritsFrom(Instance.ClassType)
    and (NewClass.InstanceSize = Instance.InstanceSize) then
  begin
    PClass(Instance)^ := NewClass;
  end;
end;

procedure WriteAllTextToFile(APath: string; AContents: string);
var
  FS: TFileStream;
  Buff: TBytes;
  Encoding: TEncoding;
begin
  FS := TFile.Open(APath,
    TFileMode.fmOpenOrCreate, TFileAccess.faWrite, TFileShare.fsReadWrite);
  try
    Encoding := TEncoding.UTF8;
    Buff := Encoding.GetPreamble;
    FS.WriteBuffer(Buff, Length(Buff));

    Buff := Encoding.GetBytes(AContents);
    FS.WriteBuffer(Buff, Length(Buff));
  finally
    FS.Free;
  end;
end;

function RealAllTextFromFile(APath: string): string;
begin
  Result := TFile.ReadAllText(APath);
end;

function StringToPAnsiChar(const S: string): PAnsiChar;
begin
  Result := PAnsiChar(AnsiString(S));
end;


function GetProcAddressAndCheck(hModule: HMODULE; lpProcName: LPCWSTR): FARPROC;
begin
  Result := GetProcAddress(hModule, lpProcName);
  Win32Check(Assigned(Result));
end;

function FindStringIndex(out AType; AArray: array of string; AName: string): Boolean;
var
  I: Integer;
begin
  for I := Low(AArray) to High(AArray) do
    if SameText(AName, AArray[I]) then
    begin
      Byte(AType) := I;
      Exit(True);
    end;
  Byte(AType) := Low(Byte);
  Result := False;
end;

function AddNameValue(AStrings: IJclStringList; AName: string; AValue: string): Integer;
var
  Strings: TStrings;
begin
  Strings := AStrings as TStrings;
  Result := Strings.AddNameValue(AName, AValue);
end;

function GetSecondsIdle: Integer;
var
  liInfo: TLastInputInfo;
begin
  liInfo.cbSize := SizeOf(TLastInputInfo) ;
  GetLastInputInfo(liInfo) ;
  Result := (GetTickCount - liInfo.dwTime) div 1000;

end;

function GetAttribute(RttiObj: TRttiObject; AAttributeClass: TCustomAttributeClass;
  out AAttribute: TCustomAttribute): Boolean; overload;
var
  Attr: TCustomAttribute;
begin
  try
    for Attr in RttiObj.GetAttributes do
    begin
      if Attr is AAttributeClass then
      begin
        AAttribute := Attr;
        Exit(True);
      end;
    end;
  except
    // AV
  end;
  Exit(False);

end;

{ TStringsHelper }

function TStringsHelper.ContainsName(const Name: string): Boolean;
begin
  Result := IndexOfName(Name) >= 0;
end;

function TStringsHelper.Delete(const S: string): Boolean;
var
  I: Integer;
begin
  I := IndexOf(S);
  Result := I >= 0;
  if Result then
    Delete(I);
end;

function TStringsHelper.IndexOfNameOrStr(const S: string): Integer;
begin
  Result := IndexOfName(S);
  if Result = -1 then
    Result := IndexOf(S);
end;

function TStringsHelper.IndexOfValue(const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Count -1 do
    if ValueFromIndex[I] = S then
      Exit(I);
  Exit(-1);
end;

function TStringsHelper.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TStringsHelper.LoadFromFileShared(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStringsHelper.RemoveValues;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      if ValueFromIndex[I] <> '' then
        Strings[I] := Names[I];
  finally
    EndUpdate;
  end;

end;

function TStringsHelper.TryGetString(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Strings[AIndex]
  else
    Result := '';
end;

function TStringsHelper.AddFmt(AString: string; const AArgs: array of const): Integer;
begin
  Result := Add(Format(AString, AArgs));
end;

function TStringsHelper.AddNameValue(AName, AValue: string): Integer;
begin
  Result := Add(AName + NameValueSeparator + AValue);
end;

function TStringsHelper.Contains(const S: string): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

{ TObjectHelper }

function TObjectHelper.HasAttributeByPropInfo<T>(APropInfo: PPropInfo): Boolean;
var
  Context : TRttiContext;
  Prop  : TRttiProperty;
  ObjType: TRttiType;
begin
  Result := False;
  Context := TRttiContext.Create;
  try
    ObjType := Context.GetType(Self.ClassType);
    while Assigned(ObjType) do
    begin
      Prop := ObjType.GetProperty(string(APropInfo.Name));
      if Prop = nil then
        Exit
      else if Prop.HasAttribute<T> then
        Exit(True);

      Break;
    end;
  finally
    Context.Free;
  end;

end;

function TObjectHelper.TryCast(AClass: TClass; out Obj): Boolean;
begin
  Result := Assigned(Self) and (Self is AClass);
  if Result then
    TObject(Obj) := Self as AClass
  else
    TObject(Obj) := nil;
  Result := TObject(Obj) <> nil;
end;

{ TLockableObject }

constructor TLockablePersistent.Create;
begin
  inherited;
  FLock := TObject.Create;
end;

destructor TLockablePersistent.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TLockablePersistent.Lock;
begin
  TMonitor.Enter(FLock);
end;

procedure TLockablePersistent.Unlock;
begin
  TMonitor.Exit(FLock);
end;


{ TDLLDuplicator }

constructor TDLLDuplicator.Create(ADLLFile: string);
begin
  inherited Create;
  FOriginalDLLPath := ADLLFile;
  FDllCount := 1;
end;

function TDLLDuplicator.FreeDll(AHandle: THandle): Boolean;
begin
  Result := FreeLibrary(AHandle);
  if Result then
    Dec(FDllCount);
end;

function TDLLDuplicator.GetUnlinkedDllPath: string;
begin
  if FDllCount > 1 then
  begin
    Result := TPath.GetFileNameWithoutExtension(FOriginalDllPath)
      + Format(' (%d)', [FDllCount]);
    Result := Result + ExtractFileExt(FOriginalDllPath);
    if not FileExists(Result) then
      TFile.Copy(FOriginalDllPath, Result);
  end
  else
    Result := FOriginalDllPath;
end;

function TDLLDuplicator.LoadUnlinkedDll: THandle;
begin
  Result := SafeLoadLibrary(GetUnlinkedDllPath);
  Win32Check(Result <> 0);
  Inc(FDllCount);
end;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttribute<T>(out AAttribute: T): Boolean;
var
  Attr: TCustomAttribute;
begin
  try
    for Attr in Self.GetAttributes do
    begin
      if Attr is T then
      begin
        AAttribute := Attr as T;
        Exit(True);
      end;
    end;
  except
    // AV
  end;
  Exit(False);
end;


function TRttiObjectHelper.HasAttribute<T>: Boolean;
var
  Dummy: T;
begin
  Result := GetAttribute<T>(Dummy);
end;

function TObjectHelper.GetClassAttribute<T>(out AAttribute: T): Boolean;
var
  Context : TRttiContext;
begin
  Context := TRttiContext.Create;
  try
    Result := Context.GetType(Self.ClassType).GetAttribute<T>(AAttribute);
  finally
   Context.Free;
  end;

end;

function TObjectHelper.HasAttribute<T>(APropName: string): Boolean;
var
  Context : TRttiContext;
begin
  Context := TRttiContext.Create;
  try
    Result := (Context.GetType(Self.ClassType).GetProperty(APropName)).HasAttribute<T>;
  finally
   Context.Free;
  end;
end;

{ TChangeableObjectList<T> }

procedure TChangeableObjectList<T>.Changed(Item: T);
begin
  Notify(Item, TCollectionNotification(cnChanged));
end;

{ ExceptionHelper }

function ExceptionHelper.AddMessage(const S: string): string;
begin
  Message := S + ': ' + Message;
  Result := Message;
end;

function ExceptionHelper.AddMessageLine(const S: string): string;
begin
  Message := S + '.' + sLineBreak + Message;
  Result := Message;
end;

{ TNameAttribute }

constructor NameAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TMemIniFileHelper }

procedure TMemIniFileHelper.ReloadValues;
begin
  Rename(FileName, True);
end;

{ TChangeableDictionary<K, V> }


class procedure TDictionaryHelper.KeyChanged<K, V>(
  Dictionary: TDictionary<K, V>; Key: K);
begin
  TDictionaryAccess<K, V>(Dictionary).KeyNotify(Key, TCollectionNotification(cnChanged));
end;

end.
