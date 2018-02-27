unit uIPCUtils;

interface

uses
  Messages, Windows, Classes, JclSynch, SyncObjs, JclAppInst;

type

  TMessageReceiver = class
  private
    FWindowHandle: HWND;
  protected
    procedure WndProc(var Msg: TMessage); virtual;
    function HandleMessage(var Msg: TMessage): Boolean; virtual; abstract;
    property WindowHandle: HWND read FWindowHandle;
  public
    constructor Create(AWindowText: string = '');
    destructor Destroy; override;
  end;

  // ! Forms.TMessageEvent
  TMessageEvent = procedure (var AMessage: TMessage) of object;
  
  TMessageNotifier = class(TMessageReceiver)
  private
    FOnCatch: TMessageEvent;
    FMessageID: Cardinal;
  protected
    function HandleMessage(var Msg: TMessage): Boolean; override;
  public
    function PostMessage(wParam: WPARAM; lParam: LPARAM): Boolean;
    property OnCatch: TMessageEvent read FOnCatch write FOnCatch;
    property MessageID: Cardinal read FMessageID write FMessageID;
    property WindowHandle;

  end;

  TStringReceivedEvent = procedure (ADataKind: TJclAppInstDataKind;
    AString: string; var AResult: LRESULT) of object;
  
  TStringMessageReceiver = class
  private
    FCatcher: TMessageNotifier;
    FOnStringReceived: TStringReceivedEvent;
    procedure WMCopyDataCatched(var AMessage: TMessage);
  public
    constructor Create(AWindowText: string);
    destructor Destroy; override;
    property OnStringReceived: TStringReceivedEvent read FOnStringReceived
      write FOnStringReceived;
  end;

  TProcessWaiter = class(TThread)
  private
    FPID: DWORD;
    FReleasingEvent: TJclEvent;
    FOnProcessClosed: TNotifyEvent;
    procedure CallOnProcessClosed;
    function GetWaitingError: Integer;
  protected
    procedure Execute; override;    
  public
    constructor Create(AProcessID: DWORD);
    destructor Destroy; override;
    property WaitingError: Integer read GetWaitingError;
    property PID: DWORD read FPID;
    property OnProcessClosed: TNotifyEvent read FOnProcessClosed write FOnProcessClosed;
  end;  

  TProcessCreator = class
  private
    FApplicationName: string;
    FParameters: string;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    FAdjustCmdLine: Boolean;
    FProcessDispatcher: TJclDispatcherObject;
    FProcessHandle: THandle;
    FRunElevated: Boolean;
    FCurrentDirectory: string;
    FWnd: THandle;
    FCreationFlags: Cardinal;
  function GetProcessHandle: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; overload;
    class procedure Execute(AApplicationName: string; AParams: string = ''); overload;
    class function RunElevatedInstance: Boolean;
    function WaitFor: Cardinal;
    function IsRunning: Boolean;
    property ApplicationName : string read FApplicationName write FApplicationName;
    property Parameters : string read FParameters write FParameters;
    property StartupInfo: TStartupInfo read FStartupInfo write FStartupInfo;
    property ProcessHandle: THandle read FProcessHandle;
    property AdjustCmdLine: Boolean read FAdjustCmdLine write FAdjustCmdLine;
    procedure SetRunElevated(AEnable: Boolean; AWnd: THandle);
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property CreationFlags: Cardinal read FCreationFlags write FCreationFlags;
  end;

function RefreshRunningProcesses(FullPath: Boolean = True): TStringList;
function IsProcessRunning(FileName: string; FullPath: Boolean = True): Boolean;

var
  RunningProcesses: TStringList;

implementation

uses
  SysUtils, ShellApi, ActiveX,
  {$IFNDEF CONSOLE} Forms, {$ENDIF}
  JclSysInfo;

var
  ThreadReleaser: TJclEvent;

function RefreshRunningProcesses(FullPath: Boolean = True): TStringList;
begin
  RunningProcesses.Clear;
  RunningProcessesList(RunningProcesses, FullPath);
  Result := RunningProcesses;
end;

function IsProcessRunning(FileName: string; FullPath: Boolean = True): Boolean;
begin
  RefreshRunningProcesses(FullPath);
  Result := RunningProcesses.IndexOf(FileName) >= 0;
end;

function StrToPChar(const AStr: string): PChar;
begin
  if AStr = '' then
    Result := nil
  else
    Result := PChar(AStr);
end;

function Win32CheckExcept(AExceptErrors: array of Integer; RetVal: BOOL): BOOL;
var
  LastError: Integer;
  I: Integer;
begin
  Result := RetVal;
  if not Result then
  begin
    LastError := GetLastError;
    for I := Low(AExceptErrors) to High(AExceptErrors) do
      if LastError = AExceptErrors[I] then
        Exit;
    RaiseLastOSError;
  end  
end;  

function ShellExecute(const AWnd: HWND; const AOperation, AFileName: String;
  const AParameters: String = ''; const ADirectory: String = ''; 
  const AShowCmd: Integer = SW_SHOWNORMAL): THandle;
var
  ExecInfo: TShellExecuteInfo;
begin
  Assert(AFileName <> '');

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);
 
    ExecInfo.Wnd := AWnd;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI
      or SEE_MASK_NOCLOSEPROCESS;
    Win32Check(ShellExecuteEx(@ExecInfo));
    Result := ExecInfo.hProcess;
  finally
    CoUninitialize;
  end;
end;

  
{ TMessageReceiver }

constructor TMessageReceiver.Create(AWindowText: string = '');
begin
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  if AWindowText <> '' then
    Win32Check(SetWindowText(WindowHandle, PChar(AWindowText)));
end;

destructor TMessageReceiver.Destroy;
begin
  Classes.DeallocateHWnd(FWindowHandle);
  inherited;
end;

procedure TMessageReceiver.WndProc(var Msg: TMessage);
begin
  if not HandleMessage(Msg) then
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);      
end;

{ TMessageNotifier }

function TMessageNotifier.HandleMessage(var Msg: TMessage): Boolean;
begin
  Result := (Msg.Msg = FMessageID) and Assigned(FOnCatch);
  if Result then
    FOnCatch(Msg);
end;  


function TMessageNotifier.PostMessage(wParam: WPARAM; lParam: LPARAM): Boolean;
begin
  Result := Windows.PostMessage(WindowHandle, FMessageID, wParam, lParam);
end;

{ TProcessWaiter }

procedure TProcessWaiter.CallOnProcessClosed;
begin
  if Assigned(FOnProcessClosed) then
    FOnProcessClosed(Self);
end;

constructor TProcessWaiter.Create(AProcessID: DWORD);
const
  EVENT_MANUAL_RESET = True;
  EVENT_SINGALED = False;
begin  
  inherited Create(True);
  FPID := AProcessID;
//  FReleasingEvent := TJclEvent.Create(nil, EVENT_MANUAL_RESET, EVENT_SINGALED, '');  
end;                            

destructor TProcessWaiter.Destroy;
begin
  // предотвращаем повторный вызов деструктора из ThreadProc
  FreeOnTerminate := False;
//  FReleasingEvent.SetEvent;
  inherited;
//  FreeAndNil(FReleasingEvent); 
end;

procedure TProcessWaiter.Execute;
var
  hProcess: THandle;
  ProcessDispatcher: TJclDispatcherObject;
  WaitResult: Cardinal;
begin
  inherited;
  hProcess := OpenProcess(Windows.SYNCHRONIZE, False, FPID);
  if hProcess = 0 then
  begin
    ReturnValue := GetLastError;
    Exit;
  end;
    
  ProcessDispatcher := TJclDispatcherObject.Attach(hProcess);
  try
    WaitResult := WaitForMultipleObjects([ThreadReleaser, ProcessDispatcher], False, INFINITE);
    case WaitResult of
      WAIT_FAILED:
        ReturnValue := GetLastError;
      WAIT_OBJECT_0 + 1:
        if Assigned(FOnProcessClosed) then
          Synchronize(CallOnProcessClosed);
    end;
  finally
    FreeAndNil(ProcessDispatcher);
  end;
end;

function TProcessWaiter.GetWaitingError: Integer;
begin
  Result := ReturnValue;
end;

{ TProcessCreator }

constructor TProcessCreator.Create;
begin
  inherited;
  FAdjustCmdLine := True;
end;

destructor TProcessCreator.Destroy;
begin
  FreeAndNil(FProcessDispatcher);
  if FProcessInfo.hThread <> 0 then
    CloseHandle(FProcessInfo.hThread);
      
  inherited;
end;

procedure TProcessCreator.Execute;
const
  PROCESS_SECURITY_ATTRIBUTES = nil;
  THREAD_SECURITY_ATTRIBUTES = nil;
  INHERIT_HANDLES = False;
  ENVIRONMENT = nil;
  ERROR_ELEVATION_REQUIRED = 740;
var
  QuotedAppName: string;
  Params: string;
begin
  Params := FParameters;
  if FAdjustCmdLine and (Params <> '') then
    Params := Format('"%s" %s', [FApplicationName, Params]);

  FStartupInfo.cb := SizeOf(FStartupInfo);
  SetLastError(ERROR_INVALID_PARAMETER);

  if not FRunElevated and Win32CheckExcept([ERROR_ELEVATION_REQUIRED],
    CreateProcess(
      StrToPChar(FApplicationName),
      StrToPChar(Params),
      PROCESS_SECURITY_ATTRIBUTES,
      THREAD_SECURITY_ATTRIBUTES,
      INHERIT_HANDLES,
      FCreationFlags,
      ENVIRONMENT,
      StrToPChar(FCurrentDirectory),
      FStartupInfo,
      FProcessInfo)) then
    FProcessHandle := FProcessInfo.hProcess
  else
    FProcessHandle :=
      ShellExecute(FWnd, 'runas', '"' + FApplicationName + '"', FParameters,
        FCurrentDirectory, SW_SHOWNORMAL);
      // ShellExecute(0, nil, pchar('"' + FApplicationName + '"'), pchar(FParams), nil, SW_SHOWNORMAL);
  FProcessDispatcher := TJclDispatcherObject.Attach(FProcessHandle);  
  
end;

class procedure TProcessCreator.Execute(AApplicationName: string; AParams: string = '');
begin
  with Self.Create do
  try
    FApplicationName := AApplicationName;
    FParameters := AParams;
    Execute;
  finally
    Free;
  end;
end;

class function TProcessCreator.RunElevatedInstance: Boolean;
var
  ElevatedInstance: TProcessCreator;
begin
  Result := True;
  ElevatedInstance := Self.Create;
  try
    ElevatedInstance.SetRunElevated(True, 0); { TODO : pass handle }
    ElevatedInstance.Parameters := GetCommandLine;
    //ElevatedInstance.AdjustCmdLine := False; { TODO : check }
    ElevatedInstance.ApplicationName := ParamStr(0); //Application.ExeName;
    try
      ElevatedInstance.Execute;
    except on E: EOSError do
      if E.ErrorCode = ERROR_CANCELLED then
        raise //Result := False
      else
        raise
    end;
  finally
    FreeAndNil(ElevatedInstance);
  end;
end;

procedure TProcessCreator.SetRunElevated(AEnable: Boolean; AWnd: THandle);
begin
  FRunElevated := AEnable;
  FWnd := AWnd;
end;

function TProcessCreator.GetProcessHandle: THandle;
begin
  Result := FProcessInfo.hProcess;
end;

function TProcessCreator.IsRunning: Boolean;
var
  ExitCode: Cardinal;
begin
  Result := (GetProcessHandle <> 0) 
    and GetExitCodeProcess(GetProcessHandle, ExitCode)
    and (ExitCode = STILL_ACTIVE);
end;

function TProcessCreator.WaitFor: Cardinal;
var
  hProcess: THandle;
begin
  Result := 0;
  if IsRunning then
  begin
    hProcess := FProcessDispatcher.Handle;
    while True do
    begin
      case MsgWaitForMultipleObjects(1, hProcess, False, INFINITE, QS_ALLEVENTS) of
        WAIT_OBJECT_0:
          Break;
      {$IFNDEF CONSOLE}
        WAIT_OBJECT_0 + 1:
          Application.HandleMessage;
      {$ENDIF}
      else
        Exit;
      end;
    {$IFNDEF CONSOLE}
      if Application.Terminated then
        Exit;
    {$ENDIF}
    end;
  end;
  GetExitCodeProcess(FProcessDispatcher.Handle, Result);
  // check STILL_ACTIVE ?
end;

{ TStringMessageNotifier }

procedure TStringMessageReceiver.WMCopyDataCatched(var AMessage: TMessage);
var
  DataKind: TJclAppInstDataKind;
  ReceivedStr: string;
begin
  if Assigned(FOnStringReceived) then
  begin
    DataKind := ReadMessageCheck(AMessage, FCatcher.WindowHandle);
    if DataKind <> AppInstDataKindNoData then
    begin
      ReadMessageString(AMessage, ReceivedStr);
      FOnStringReceived(DataKind, ReceivedStr, AMessage.Result);
    end;
  end;
end;

constructor TStringMessageReceiver.Create(AWindowText: string);
begin
  inherited Create;
  FCatcher := TMessageNotifier.Create(AWindowText);
  FCatcher.MessageID := WM_COPYDATA;
  FCatcher.OnCatch := WMCopyDataCatched;
end;

destructor TStringMessageReceiver.Destroy;
begin
  FreeAndNil(FCatcher);
  inherited;
end;

initialization
  ThreadReleaser := TJclEvent.Create(nil, True, False, '');
  RunningProcesses := TStringList.Create;

finalization
  ThreadReleaser.SetEvent;
  FreeAndNil(ThreadReleaser);
  FreeAndNil(RunningProcesses);


end.
