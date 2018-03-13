program ytMultiCut;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}
{$R *.res}
{$R *.dres}

uses
  System.SysUtils,
  Classes,
  IOUtils,
  Windows,
  uIPCUtils,
  System.Generics.Collections,
  ShellApi,
  ActiveX,
  Types,
  uUtils,
  StrUtils,
  JclStringLists,
  Math,
  JclFileUtils,
  DateUtils,
  Variants;

const
  EXITCODE_ABORT = 1;
  EXITCODE_EXCEPTION = 2;

type

  TVideoPart = record
    InPoint: string;
    OutPoint: string;
  end;

  TVideoFileInfo = class
  private
    FUrl: string;
    FOriginalUrl: string;
    FParts: TList<TVideoPart>;
    FComment: string;
    FVideoID: string;
    FDuration: TDateTime;
    FFilename: TFilename;
    FModifiedFile: TFilename;
    procedure SetUrl(const Value: string);
    function GetPureUrl: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseTimes(var Line: string);
    property Url: string read FUrl write SetUrl;
    property PureUrl: string read GetPureUrl;
    property Comment: string read FComment write FComment;
    property VideoID: string read FVideoID write FVideoID;
    property Parts: TList<TVideoPart> read FParts;
    property Duration: TDateTime read FDuration write FDuration;
    property OriginalFile: TFilename read FFilename write FFilename;
    property ModifiedFile: TFilename read FModifiedFile write FModifiedFile;
  end;

type
  TParamType = (ptConcatAll, ptYoutubeDL, ptFFMpegCut, ptFFMpegConcat,
    ptFFMpegReEncoding, ptWriteDuration, ptTotalDuration);
  TParamStrArray = array [TParamType] of string;

var
  FParams: TParamStrArray; // moving this to class makes Ctrl+Space IMPORSSIBRU11

type
  TYtMultiCut = class
  private const

    YoutubeDL = 'youtube-dl';
    YoutubeDLExe = YoutubeDL + '.exe';
    FFMpeg = 'ffmpeg';
    FFMpegExe = FFMpeg + '.exe';

    CONCAT_DEMUXER_FILE = 'concat.txt';
    LENGTH_PARAM = '        length = ';
    VIDEO_EXT = '.mp4';

    ParamNames: array [TParamType] of string = ('concat-all', YoutubeDL,
      FFMpeg + '-cut', FFMpeg + '-concat', FFMpeg + '-reencoding',
      'write-length', 'total-length');
    ParamActions: array [TParamType] of string = ('Concatenating all videos' ,
      'Downloading', 'Cutting', 'Parts concatenating', 'Re-encoding',
      'Getting durations', 'Total duration');
    DontDisplayParams: set of TParamType = [ptTotalDuration];

    procedure SetDestinationDir(const Value: string);

  var
    FVideos: TObjectList<TVideoFileInfo>;
    FDestinationDir: string;
    FIgnoreErrors: array [TParamType] of Boolean;
    FHasErrors: Boolean;
    function GetDownloadsDir: string;
    function GetConcatVidsDir: string;
    procedure LoadVideoList(Lines: IJclStringList);
    procedure SetConfigParams(Lines: IJclStringList);
    function WarnIfEmptyParam(Param: TParamType): Boolean;
    procedure FindDownloadedFiles;
    procedure AskIgnoreErrors(Param: TParamType);
    procedure CheckError(ErrorCode: Integer; Action: TParamType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DownloadVideos;
    function GetAndWriteVideoDurations(InputData: IJclStringList): Boolean;
    procedure LoadInputData(InputData: IJclStringList);
    function CutAndConcatVideos: TFilename;
    procedure ShowReadmeText;
    property DestinationDir: string read FDestinationDir
      write SetDestinationDir;
    property HasErrors: Boolean read FHasErrors;
  end;

procedure WritelnFmt(const Msg: string; const Args: array of const);
begin
  Writeln(Format(Msg, Args));
end;

function ReadChar(const Chars: array of Char): Char;
var
  Input: string;
begin
  repeat
    Readln(Input);
  until (Input.Length = 1) and (Input.Trim.IndexOfAny(Chars) >= 0);
  Exit(Input.Trim.ToLower[1]);
end;

function AskConfirmation(const Msg: string): Boolean;
begin
  Write(Msg);
  Writeln(' [y/n]');
  Result := ReadChar(['y', 'n']) = 'y';
end;


procedure AskContinue(Msg: string; const Args: array of const); overload;
begin
  if not Msg.IsEmpty then
    Msg := Format(Msg + '. ', Args);
  if not AskConfirmation(Msg + 'Continue?') then
  begin
    ExitCode := EXITCODE_ABORT;
    Abort;
  end;
end;

procedure AskContinue; overload;
begin
  AskContinue('', []);
end;

var
  FmtDotDecimalSeparator: TFormatSettings;

function StrTimeLengthToTime(Str: string): TDateTime;

  procedure Add00ToTime;
  begin
    Str := '00' + FormatSettings.TimeSeparator + Str;
  end;
begin
  // "XX" and "X:XX" writing must be changed to 00:XX:XX format to correctly converting.
  // Delphi handles 00:00 format as HH:MM while ffmpeg handles it as MM:SS
  case Str.CountChar(FormatSettings.TimeSeparator) of
    1:
      Add00ToTime;
    0:
      begin
        Add00ToTime;
        Add00ToTime;
      end;
  end;

  if not TryStrToTime(Str, Result)
    and not TryStrToTime(Str, Result, FmtDotDecimalSeparator) then
  begin
    AskContinue('Unable to convert "%s" to time', [Str]);
  end
end;

function TimeLengthToStr(TimeLength: TDateTime; var Fmt: TFormatSettings): string;
begin
  if HoursBetween(0, TimeLength) = 0 then
    Fmt.LongTimeFormat := 'n:ss'
  else
    Fmt.LongTimeFormat := FormatSettings.LongTimeFormat;
  Result := TimeToStr(TimeLength, Fmt);
end;


function SortFilesToNewer(List: IJclStringList;
  Index1, Index2: Integer): Integer;
var
  D1, D2: TDateTime;
begin
  FileAge(List[Index1], D1);
  FileAge(List[Index2], D2);
  Result := CompareValue(D1, D2);
end;

function ConsistOf(const S: string; Chars: TSysCharSet): Boolean;
var
  C: Char;
begin
  for C in S do
  begin
    if not CharInSet(C, Chars) then
      Exit(False);
  end;

  Exit(True);

end;


{ TVideoFile }

constructor TVideoFileInfo.Create;
begin
  inherited;
  FParts := TList<TVideoPart>.Create;
end;

destructor TVideoFileInfo.Destroy;
begin
  FreeAndNil(FParts);
  inherited;
end;

function TVideoFileInfo.GetPureUrl: string;
begin
  Result := FUrl.Split(['&'])[0];
end;

procedure TVideoFileInfo.ParseTimes(var Line: string);

  procedure ShowError;
  begin
    AskContinue('Unable to parse time of line "%s"', [Line]);
  end;

var
  Words: IJclStringList;
  Part: TVideoPart;
  I: Integer;
  S: string;
  Word: string;
  NewParts: TList<TVideoPart>;
begin
  Line := Line.Replace('-', '');

  Words := JclStringList;
  Words.DelimitedText := Line;
  if Words.Count < 2 then
    ShowError;

  NewParts := TList<TVideoPart>.Create;
  try

    for I := 0 to Words.Count -1 do
    begin
      S := Words[I];
      if ConsistOf(S, ['0' .. '9', ':']) then
      begin
        if not S.Contains(':') and (S.Length >= 3) then
          S := S.Insert(S.Length - 2, ':');
        if Part.InPoint.IsEmpty then
          Part.InPoint := S
        else
          Part.OutPoint := S;
        if I = Words.Count - 1 then
        begin
          if Part.OutPoint.IsEmpty then
          begin
            WritelnFmt('Skipped time "%s" of line "%s"', [Part.InPoint, Line]);
            AskContinue;
          end
          else
            NewParts.Add(Part);
        end;

      end
      else if not Part.InPoint.IsEmpty  then
      begin
        if not Part.OutPoint.IsEmpty  then
        begin
          NewParts.Add(Part);
        end
        else
        begin
          WritelnFmt('Skipped time "%s" of line "%s"', [Part.InPoint, Line]);
          AskContinue;
        end;

        Part.InPoint := '';
        Part.OutPoint := '';

      end
    end;
    if NewParts.Count > 1 then
    begin
      Line := '#' + Line;
      for Part in NewParts do
        Line := Line + sLineBreak + Part.InPoint + ' ' + Part.OutPoint;
    end;
    FParts.AddRange(NewParts);

  finally
    FreeAndNil(NewParts);
  end;


end;

procedure TVideoFileInfo.SetUrl(const Value: string);
const
  VideoIDParams: array [0 .. 2] of string = ('v=', '/v/', 'youtu.be/');
var
  I: Integer;
  S: string;
begin
  for S in VideoIDParams do
  begin
    I := Value.IndexOf(S);
    if I <> -1 then
    begin
      Inc(I, S.Length);
      Break;
    end;
  end;
  if I = -1 then
    AskContinue('Not found video id param in URL "%s"', [Value]);

  FOriginalUrl := Value;
  // Removing trailing params e.g. &t=999s
  FUrl := Value;
  FVideoID := GetPureUrl.Substring(I);
end;

{ TYoutubeMultiCutter }

procedure TYtMultiCut.AskIgnoreErrors(Param: TParamType);
var
  PT: TParamType;
begin
  if FIgnoreErrors[Param] then
    Exit;

  Writeln('Ignore such errors? [y/n/a] Type "a" for ignore all errors');
  case ReadChar(['y', 'n', 'a']) of
    'y':
      FIgnoreErrors[Param] := True;
    'a':
      for PT := Low(TParamType) to High(TParamType) do
        FIgnoreErrors[PT] := True
  end;
end;

procedure TYtMultiCut.CheckError(ErrorCode: Integer;
  Action: TParamType);
begin
  if ErrorCode = 0 then
    Exit;

  FHasErrors := True;

  case Action of
    ptFFMpegCut, ptFFMpegConcat, ptFFMpegReEncoding, ptWriteDuration:
      AskIgnoreErrors(Action);
    ptYoutubeDL:
      AskContinue;
    ptConcatAll:
      ;
  end;


end;

constructor TYtMultiCut.Create;
begin
  inherited Create;
  FVideos := TObjectList<TVideoFileInfo>.Create;
  FParams[ptYoutubeDL] := YoutubeDLExe + ' -f best';
  FParams[ptFFMpegCut] := FFMpegExe + ' -i "%s" -ss %s -to %s -n part%d' + VIDEO_EXT;
  FParams[ptFFMpegConcat] := FFMpegExe + ' -f concat -safe 0 -i ' +
    CONCAT_DEMUXER_FILE + ' -c copy "%s" -n';
  FParams[ptFFMpegReEncoding] := FFMpegExe + ' -i "%s" "%s" -n';

end;

destructor TYtMultiCut.Destroy;
begin
  FreeAndNil(FVideos);
  inherited;
end;

procedure TYtMultiCut.FindDownloadedFiles;
var
  V: TVideoFileInfo;
  Files: IJclStringList;
  S: string;
begin
  for V in FVideos do
  begin
    Files := TJclStringList.Create.Files(GetDownloadsDir + '*-' +
      V.VideoID + '*.*');
    Files.Sort(SortFilesToNewer);

    for S in Files do
      if not MatchText(ExtractFileExt(S), ['.txt', '.part']) then
      begin
        V.OriginalFile := S;
        Break;
      end;
    if V.OriginalFile = '' then
    begin
      AskContinue
        ('Not found video file with id "%s" in folder "%s". Comment "%s"',
        [V.VideoID, GetDownloadsDir, V.Comment]);
      Continue;
    end;
  end;
end;

procedure TYtMultiCut.DownloadVideos;
var
  V: TVideoFileInfo;
  YoutubeDLProcess: TProcessCreator;
begin
  if WarnIfEmptyParam(ptYoutubeDL) then
    Exit;

  YoutubeDLProcess := TProcessCreator.Create;
  try
    // first parameter in CreateProcess must specify full path to exe if not empty
    YoutubeDLProcess.ApplicationName := '';
    YoutubeDLProcess.AdjustCmdLine := False;
    YoutubeDLProcess.Parameters := FParams[ptYoutubeDL];
    for V in FVideos do
      YoutubeDLProcess.Parameters := YoutubeDLProcess.Parameters + ' ' + V.Url;
    TDirectory.CreateDirectory(GetDownloadsDir);
    YoutubeDLProcess.CurrentDirectory := GetDownloadsDir;
    // YoutubeDLProcess.CreationFlags := CREATE_NEW_CONSOLE;
    Writeln('Creating process: ' + YoutubeDLProcess.ApplicationName + ' ' +
      YoutubeDLProcess.Parameters);
    YoutubeDLProcess.Execute;
    CheckError(YoutubeDLProcess.WaitFor, ptYoutubeDL);
  finally
    FreeAndNil(YoutubeDLProcess);
  end;

end;

procedure TYtMultiCut.SetConfigParams(Lines: IJclStringList);

  procedure SetParamAndDeleteLine(Param: TParamType);
  var
    I: Integer;
  begin
    I := Lines.IndexOfName(ParamNames[Param]);
    if I >= 0 then
    begin
      FParams[Param] := Lines.ValueFromIndex[I];
      if Param in DontDisplayParams then
        Exit;

      Writeln(Lines[I]);
    end
    else
    begin
      if Param in DontDisplayParams then
        Exit;

      Writeln(ParamNames[Param] + ' is not specified. Using default value:');
      if FParams[Param].IsEmpty then
        Writeln('[empty]')
      else
        Writeln(FParams[Param]);
      Writeln('');
    end;

  end;

var
  Param: TParamType;
begin
  for Param := Low(TParamType) to High(TParamType) do
    SetParamAndDeleteLine(Param);
end;

function TYtMultiCut.GetDownloadsDir: string;
begin
  Result := PathAddSeparator(FDestinationDir + '_downloads');
end;

function TYtMultiCut.WarnIfEmptyParam(Param: TParamType): Boolean;
begin
  Result := FParams[Param].IsEmpty;
  if Result then
    Writeln(Format('Empty %s param. %s skipped', [ParamNames[Param],
      ParamActions[Param]]));

end;


function TYtMultiCut.GetAndWriteVideoDurations(InputData: IJclStringList): Boolean;

var
  TotalDuration: TDateTime;
  Vids: TObjectList<TVideoFileInfo>;

  procedure GetDurations;
  const
    DURATION_FILENAME = 'duration.tmp';
  var
    V: TVideoFileInfo;
    YoutubeDLProcess: TProcessCreator;
    DurationFile: TFilename;
    DurationStr: string;
  begin
    YoutubeDLProcess := TProcessCreator.Create;
    try
      YoutubeDLProcess.AdjustCmdLine := False;
      YoutubeDLProcess.CurrentDirectory := FDestinationDir;
      for V in Vids do
      begin
        if V.Duration > 0  then
          Continue;

        YoutubeDLProcess.Parameters := 'cmd /c ' + YoutubeDL + ' ' + V.PureUrl
          + ' --get-duration > ' + DURATION_FILENAME;

        Writeln('Creating process: ' + YoutubeDLProcess.ApplicationName + ' ' +
          YoutubeDLProcess.Parameters);
        YoutubeDLProcess.Execute;
        CheckError(YoutubeDLProcess.WaitFor, ptWriteDuration);

        DurationFile := YoutubeDLProcess.CurrentDirectory + DURATION_FILENAME;
        try
          DurationStr := TFile.ReadAllText(DurationFile).Trim;
          Writeln(DurationStr);
          V.Duration := StrTimeLengthToTime(DurationStr);
        except on E: Exception do
          if E is EAbort then
            raise
          else
            AskContinue('Failed to read video duration from file "%s": %s',
              [DURATION_FILENAME, E.Message]);
        end;


      end;
      if FileExists(DurationFile) then
      try
        TFile.Delete(DurationFile);
      except
        WritelnFmt('Failed to delete "%s" file', [DurationFile]);
      end;

    finally
      FreeAndNil(YoutubeDLProcess);
    end;
  end;

  procedure WriteDurations;
  const
    MODIFIED_INPUTFILE = 'modified_input.txt';
  var
    V: TVideoFileInfo;
    Lines: IJclStringList;
    I: Integer;
    Format: TFormatSettings;
    BkpFile: TFileName;
  begin
    Format := TFormatSettings.Create;

    Lines := InputData;
    for V in Vids do
    begin
      if V.Duration = 0 then
      begin
        AskContinue('Can''t determine video duration for "%s" (%s)',
          [V.Url, V.Comment]);
        Continue;
      end;

      I := Lines.IndexOf(V.Url);
      if I >= 0 then
      begin
        Lines.Insert(I + 1, LENGTH_PARAM + TimeLengthToStr(V.Duration, Format));
        TotalDuration := TotalDuration + V.Duration;
      end
      else
        AskContinue('Writing video length failed: can''t find video with url "%s" ', [V.Url]);

    end;

    if Lines.IndexOfName(ParamNames[ptTotalDuration]) < 0 then
    begin
      if Lines.Last = '' then
        Lines.Delete(Lines.Count - 1);
      Lines.Add('');
    end;
    Lines.Values[ParamNames[ptTotalDuration]] := '    ' + TimeLengthToStr(TotalDuration, Format);

  end;

var
  I: Integer;
  Part: TVideoPart;
  V: TVideoFileInfo;
begin
  TotalDuration := 0;

  Vids := TObjectList<TVideoFileInfo>.Create(False);
  try

    for I := FVideos.Count - 1 downto 0 do
    begin
      V := FVideos[I];
      TotalDuration := TotalDuration + V.Duration;
      if V.Duration = 0 then
        Vids.Add(V);

      for Part in V.Parts do
      begin
        V.Duration := V.Duration + StrTimeLengthToTime(Part.OutPoint)
          - StrTimeLengthToTime(Part.InPoint);
      end;

    end;

    Result := Vids.Count > 0;

    Write(ParamActions[ptWriteDuration]);
    if Result then
      Write(Format(' for %d video(s)', [Vids.Count]));
    Writeln;

    GetDurations;
    WriteDurations;
  finally

  end;
  Writeln('Done');

end;

function TYtMultiCut.GetConcatVidsDir: string;
begin
  Result := PathAddSeparator(FDestinationDir + '_concatenated');
end;

procedure TYtMultiCut.LoadInputData(InputData: IJclStringList);
begin
  SetConfigParams(InputData);
  LoadVideoList(InputData);

end;

procedure TYtMultiCut.LoadVideoList(Lines: IJclStringList);

  function IsParam(Line: string): Boolean;
  var
    Param: TParamType;
  begin
    for Param := Low(TParamType) to High(TParamType) do
      if Line.StartsWith(ParamNames[Param]) then
        Exit(True);
    Exit(False);
  end;

var
  Video: TVideoFileInfo;
  Line: string;
  PrevLineEmpty: Boolean;
  I, Dummy: Integer;
  DelCount: Integer;
begin
  Writeln('Loading video list...');

  PrevLineEmpty := True;
  Video := nil;
  for I := 0 to Lines.Count - 1 do
  begin
    Line := Lines[I].Trim;
    if Line.IsEmpty then
    begin
      PrevLineEmpty := True;
      Continue;
    end;

    if IsParam(Line) then
      Continue;


    if PrevLineEmpty then
    begin
      Video := TVideoFileInfo.Create;
      FVideos.Add(Video);
    end;

    PrevLineEmpty := False;
    if Line.StartsWith('#') then
      Continue;
    if Line.StartsWith('http') then
    begin
      Video.Url := Line;
      Lines[I] := Line;
    end
    else if Line.Contains(':\') and TFile.Exists(Line) then
      Video.OriginalFile := Line
    else if Lines[I].StartsWith(LENGTH_PARAM) then
      Video.Duration := StrTimeLengthToTime(Lines[I].Substring(LENGTH_PARAM.Length))
    else if TryStrToInt(Line.Split([' ', ':'])[0], Dummy) then
    begin
      Video.ParseTimes(Line);
      Lines[I] := Line;
    end
    else
      Video.FComment := Line;

  end;

  DelCount := 0;
  for I := FVideos.Count - 1 downto 0 do
  begin
    Video := FVideos[I];
    if (Video.Url + Video.OriginalFile).IsEmpty then
    begin
      if not Video.Comment.IsEmpty then
        WritelnFmt('Text "%s" is not associated with any URL', [Video.Comment]);

      FVideos.Delete(I);
      Inc(DelCount);
    end;
  end;

  Writeln('Loaded videos from file: ' + FVideos.Count.ToString);
  if DelCount > 0 then
    AskContinue('Found %d unrelated texts', [DelCount]);

end;

function TYtMultiCut.CutAndConcatVideos: TFilename;

var
  FFMpeg: TProcessCreator;
  NewFilesDemuxer: IJclStringList;

  procedure CutAndConcat(V: TVideoFileInfo);
  var
    WorkDir: string;
    PartsDemuxer: IJclStringList;
    I: Integer;
    T: TVideoPart;
  begin
    PartsDemuxer := JclStringList;

    WorkDir := FDestinationDir + PathAddSeparator
      (RemoveFileExt(ExtractFileName(V.OriginalFile)));

    V.ModifiedFile := GetConcatVidsDir + ExtractDirName(WorkDir) + ExtractFileExt(V.OriginalFile);
    NewFilesDemuxer.Add('file ' + QuotedStr(ExtractFileName(V.ModifiedFile)));
    TDirectory.CreateDirectory(GetConcatVidsDir);

    // Cutting
    if V.Parts.Count > 0 then
    begin
      TDirectory.CreateDirectory(WorkDir);
      FFMpeg.CurrentDirectory := WorkDir;
      PartsDemuxer.Add('# ' + V.Comment);

      I := 1;
      for T in V.Parts do
      begin
        if FParams[ptFFMpegCut] <> '' then
        begin
          FFMpeg.Parameters := Format(FParams[ptFFMpegCut],
            [V.OriginalFile, T.InPoint, T.OutPoint, I]);
          FFMpeg.Execute;
          CheckError(FFMpeg.WaitFor, ptFFMpegCut);
        end;
        PartsDemuxer.GetStringsRef.AddFmt('file part%d' + VIDEO_EXT, [I]);
        Inc(I);
      end;

      if FParams[ptFFMpegCut] <> '' then
        PartsDemuxer.SaveToFile(WorkDir + CONCAT_DEMUXER_FILE);

      // Concatenating parts
      if FParams[ptFFMpegConcat] <> '' then
      begin
        FFMpeg.Parameters := Format(FParams[ptFFMpegConcat], [V.ModifiedFile]);
        FFMpeg.Execute;
        CheckError(FFMpeg.WaitFor, ptFFMpegConcat);
      end;

    end
    else // parts = 0
    begin
      WarnIfEmptyParam(ptFFMpegReEncoding);

      if FParams[ptFFMpegReEncoding].IsEmpty then
        TFile.Copy(V.OriginalFile, V.ModifiedFile, True)
      else
      begin
        // reencoding is needed to properly concatenate this video with other cutted videos
        WritelnFmt('%s "%s"', [ParamActions[ptFFMpegReEncoding],
          ExtractFileName(V.OriginalFile)]);
        FFMpeg.CurrentDirectory := FDestinationDir;
        FFMpeg.Parameters := Format(FParams[ptFFMpegReEncoding],
          [V.OriginalFile, V.ModifiedFile]);
        FFMpeg.Execute;
        CheckError(FFMpeg.WaitFor, ptFFMpegReEncoding);
      end;
    end;

  end;

  procedure ConcatAllVideos;
  var
    FinalVideo: TFilename;
  begin
    Writeln('');
    Writeln(ParamActions[ptConcatAll]);
    NewFilesDemuxer.SaveToFile(GetConcatVidsDir + CONCAT_DEMUXER_FILE);
    FinalVideo := '..\' + FParams[ptConcatAll];
    if not ExtractFileExt(FinalVideo).EndsWith(VIDEO_EXT, True) then
      FinalVideo := FinalVideo + VIDEO_EXT;
    FFMpeg.Parameters := Format(FParams[ptFFMpegConcat], [FinalVideo]);
    FFMpeg.CurrentDirectory := GetConcatVidsDir;
    FFMpeg.Execute;
    CheckError(FFMpeg.WaitFor, ptConcatAll);
    TFile.Delete(GetConcatVidsDir + CONCAT_DEMUXER_FILE);
  end;

var
  V: TVideoFileInfo;
  HasCutting: Boolean;
  DownloadsDirName: string;
begin
  Writeln('');
  Writeln('Start cutting and concatenating');
  WarnIfEmptyParam(ptFFMpegCut);
  WarnIfEmptyParam(ptFFMpegConcat);
  NewFilesDemuxer := JclStringList;
  DownloadsDirName := ExtractFileName(PathRemoveSeparator(GetDownloadsDir));

  HasCutting := False;
  for V in FVideos do
    if V.Parts.Count > 0 then
    begin
      HasCutting := True;
      Break;
    end;

  FFMpeg := TProcessCreator.Create;
  try
    FFMpeg.ApplicationName := '';
    FFMpeg.AdjustCmdLine := False;

    for V in FVideos do
      if HasCutting then
        CutAndConcat(V)
      else
        NewFilesDemuxer.Add('file '
          + QuotedStr(DownloadsDirName.QuotedString + PathDelim + ExtractFileName(V.OriginalFile))
        );

    if not FParams[ptConcatAll].IsEmpty then
      ConcatAllVideos;

    Writeln('Done.');
  finally
    FreeAndNil(FFMpeg);
  end;
end;

procedure TYtMultiCut.SetDestinationDir(const Value: string);
begin
  FDestinationDir := PathAddSeparator(Value);
end;

procedure TYtMultiCut.ShowReadmeText;
var
  ResStream: TResourceStream;
  Text: IJclStringList;
  S: string;
  P: TParamType;
begin
  ResStream := TResourceStream.Create(hInstance, 'UsageHelp', RT_RCDATA);
  try
    Text := JclStringList.LoadFromStream(ResStream);

    for S in Text do
      Writeln(S);

    Writeln('');
    Writeln('Default parameters:');
    for P := Low(TParamType) to High(TParamType) do
      Writeln(ParamNames[P] + '=' + FParams[P]);

  finally
    FreeAndNil(ResStream);
  end;
end;

procedure WaitForEnterKeyForExit;
begin
  Writeln('Press ENTER for exit');
  Readln;
end;

var
  Cutter: TYtMultiCut;
  InputFile: TFilename;
  InputLines: IJclStringList;
begin
  FmtDotDecimalSeparator := TFormatSettings.Create;
  FmtDotDecimalSeparator.DecimalSeparator := '.';

  try
    Cutter := TYtMultiCut.Create;
    try
      if ParamCount = 0 then
      begin
        Writeln('Need an input file param');
        Writeln;
        Cutter.ShowReadmeText;
        Abort;
      end;
      InputFile := ExpandFileName(ParamStr(1));

      try
        InputLines := JclStringList.LoadFromFile(InputFile);
      except
        on E: Exception do
        begin
          E.Message := Format('Failed to load video list file "%s": %s',
            [InputFile, E.Message]);
          raise;
        end;
      end;


      Cutter.LoadInputData(InputLines);
      Cutter.DestinationDir := ExtractFilePath(InputFile);

      Cutter.GetAndWriteVideoDurations(InputLines);

      if InputLines.Text.Trim <> TFile.ReadAllText(InputFile).Trim then
        InputLines.SaveToFile(InputFile);

      if not AskConfirmation('Skip downloading?') then
        Cutter.DownloadVideos;
      Cutter.FindDownloadedFiles;
      Cutter.CutAndConcatVideos;

      if Cutter.HasErrors then
        WaitForEnterKeyForExit;

    finally
      FreeAndNil(Cutter);
    end;

  except
    on E: Exception do
      if E is EAbort then
        Exit
      else
      begin
        ExitCode := EXITCODE_EXCEPTION;
        Writeln(E.ClassName, ': ', E.Message);
        Writeln('');
        WaitForEnterKeyForExit;
      end;
  end;

end.



