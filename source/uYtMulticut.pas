unit uYtMulticut;

interface

uses
  System.SysUtils,
  Classes,
  System.Generics.Collections,
  JclStringLists,
  uIPCUtils;

procedure Run;

type

  TVideoPart = record
  private
    FInPoint: string;
    FOutPoint: string;
    procedure SetInPoint(const Value: string);
    procedure SetOutPoint(const Value: string);
    procedure CheckPoints;
    function GetInPointTime: TDateTime;
    function GetOutPointTime: TDateTime;
  public
    property  InPoint: string read FInPoint write SetInPoint;
    property OutPoint: string read FOutPoint write SetOutPoint;
    property InPointTime: TDateTime read GetInPointTime;
    property OutPointTime: TDateTime read GetOutPointTime;
    function GetDuration: TDateTime;
  end;

  TVideoFileInfo = class
  private
    FUrl: string;
    FOriginalUrl: string;
    FParts: TList<TVideoPart>;
    FComment: string;
    FVideoID: string;
    FDuration: TDateTime;
    FOriginalFile: string;
    FModifiedFile: TFilename;
    procedure SetUrl(const Value: string);
    function GetPureUrl: string;
    function GetDuration: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseTimes(var Line: string);
    property Url: string read FUrl write SetUrl;
    property PureUrl: string read GetPureUrl;
    property Comment: string read FComment write FComment;
    property VideoID: string read FVideoID write FVideoID;
    property Parts: TList<TVideoPart> read FParts;
    property Duration: TDateTime read GetDuration write FDuration;
    property OriginalFile: string read FOriginalFile write FOriginalFile;
    property ModifiedFile: TFilename read FModifiedFile write FModifiedFile;
  end;

type
  TParamType = (ptConcatAll, ptYoutubeDL, ptYoutubeDLDownload, ptFFMpeg, ptFFMpegConcat,
    ptFFMpegFilterComplex, ptYoutubeDLGetDuration, ptTotalDuration,
    ptUseConcatDemuxer, ptVideoFormat);
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
    VIDEO_TEMP_PART_FILENAME = 'part_tmp';

    DEFAULT_VIDEO_FORMAT = 'mp4';
    DEFAULT_USE_CONCAT_DEMUXER = False;

    ParamNames: array [TParamType] of string = ('concat-all', YoutubeDL, YoutubeDL + '-download', FFMpeg,
    FFMpeg + '-concat', FFMpeg + '-filter-complex',
      'get-length', 'total-length', 'use-concat-demuxer', 'video-format');
    ParamActions: array [TParamType] of string = ('Concatenating all videos' ,
      'Youtube-DL', 'Downloading', 'FFMpeg', 'Concatenating', 'Trimming',
      'Getting durations', 'Total duration', 'Use concat demuxer', 'Video format');
    OutputParams: set of TParamType = [ptTotalDuration];

    procedure SetDestinationDir(const Value: string);

  var
    FVideos: TObjectList<TVideoFileInfo>;
    FDestinationDir: string;
    FIgnoreErrors: array [TParamType] of Boolean;
    FHasErrors: Boolean;
    function GetDownloadsDir: string;
    function GetConcatVidsDir: string;
    procedure LoadVideoList(Lines: IJclStringList);
    procedure ReadConfigParams(Lines: IJclStringList);
    function WarnIfEmptyParam(Param: TParamType): Boolean;
    procedure FindDownloadedFiles;
    procedure AskIgnoreErrors(Param: TParamType);
    procedure ExecuteAndWait(ProcessCreator: TProcessCreator; Action: TParamType);
    function ParamVarTemplate(Param: TParamType): string;
    function UseConcatDemuxer: Boolean;
    function GetVideoExt: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DownloadVideos;
    procedure GetAndWriteVideoDurations(InputData: IJclStringList);
    procedure LoadInputData(InputData: IJclStringList);
    function TrimVideos: TFilename;
    procedure ShowReadmeText;
    property DestinationDir: string read FDestinationDir
      write SetDestinationDir;
    property HasErrors: Boolean read FHasErrors;
  end;


implementation

uses
  IOUtils,
  Windows,
  ShellApi,
  ActiveX,
  Types,
  uUtils,
  StrUtils,
  Math,
  JclFileUtils,
  DateUtils,
  Variants,
  JclSysInfo;

const
  EXITCODE_ABORT = 1;
  EXITCODE_EXCEPTION = 2;


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
    Input := Input.Trim;
  until (Input.Length = 1)
    and ((Input.ToLower.IndexOfAny(Chars) >= 0)
    or (Input.ToUpper.IndexOfAny(Chars) >= 0));
  Exit(Input.ToLower[1]);
end;

function AskConfirmation(const Msg: string): Boolean;
begin
  Write(Msg);
  Writeln(' [y/n]');
  Result := ReadChar(['y', 'n']) = 'y';
end;


procedure AskContinue(Msg: string; const Args: array of const; DontStopOnDebug: Boolean = False); overload;
begin
  if not Msg.IsEmpty then
    Msg := Format(Msg + '. ', Args);

  if DontStopOnDebug then
    WritelnFmt(Msg, Args)
  else if not AskConfirmation(Msg + 'Continue?') then
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

function StrDurationToTime(Str: string): TDateTime;

  procedure Add00ToTime;
  begin
    Str := '00' + FormatSettings.TimeSeparator + Str;
  end;
begin
  if Str = '' then
    Exit(0);

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

function TVideoFileInfo.GetDuration: TDateTime;
var
  Part: TVideoPart;
begin
  Result := 0;

  if FParts.Count = 0 then
    Result := FDuration
  else
    for Part in FParts do
      Result := Result + Part.GetDuration;

end;

function TVideoFileInfo.GetPureUrl: string;
begin
  if FUrl.IsEmpty then
    Result := ''
  else
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
  Index: Integer;
  NewParts: TList<TVideoPart>;
begin
  Line := Line.Replace('-', '');
  Words := JclStringList;

  I := Line.IndexOf('#');
  if I < 0 then
    I := Line.Length;
  Words.DelimitedText := Line.Substring(0, I);

  I := Words.DelimitedText.IndexOf('#');
  if I >= 0 then
    Words.DelimitedText := Words.DelimitedText.Substring(0, I);

  if Words.Count < 2 then
    ShowError;

  NewParts := TList<TVideoPart>.Create;
  try

    for I := 0 to Words.Count -1 do
    begin
      S := Words[I];
      if ConsistOf(S, ['0' .. '9', ':', '.']) then
      begin
        if not S.Contains(':') and (S.Length >= 3) then
        begin
          Index := S.IndexOf('.');
          if Index = -1 then
            Index := S.Length;
          S := S.Insert(Index - 2, ':');
          if Index in [5 .. 6] then
            S := S.Insert(Index - 4, ':')
          else if Index > 6 then
            ShowError;
        end;
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

  Writeln('Ignore such errors? [y/n/a] Type "a" for ignore all ffmpeg or youtube-dl errors');
  case ReadChar(['y', 'n', 'a']) of
    'y':
      FIgnoreErrors[Param] := True;
    'a':
      for PT := Low(TParamType) to High(TParamType) do
        FIgnoreErrors[PT] := True
  end;
end;

procedure TYtMultiCut.ExecuteAndWait(ProcessCreator: TProcessCreator;
  Action: TParamType);
begin
  Writeln(ProcessCreator.Parameters);
  Writeln('');
  ProcessCreator.Parameters := ProcessCreator.Parameters.Replace(sLineBreak, '');
  ProcessCreator.Execute;
  if ProcessCreator.WaitFor = 0 then
    Exit;

  FHasErrors := True;

  case Action of
    ptYoutubeDLGetDuration:
      AskIgnoreErrors(Action);
    ptYoutubeDLDownload:
      AskContinue;
    ptConcatAll:
      ;
  else
    AskContinue;
  end;


end;

constructor TYtMultiCut.Create;
begin
  inherited Create;
  FVideos := TObjectList<TVideoFileInfo>.Create;
  FParams[ptYoutubeDL] := YoutubeDLExe;
  FParams[ptYoutubeDLDownload] := ParamVarTemplate(ptYoutubeDL) + ' -f best';
  FParams[ptYoutubeDLGetDuration] := 'cmd /c ' + ParamVarTemplate(ptYoutubeDL) + ' %s --get-duration > %s';
  FParams[ptFFMpeg] := FFmpegExe + ' -hide_banner -loglevel info';
  FParams[ptVideoFormat] := DEFAULT_VIDEO_FORMAT;
  FParams[ptFFMpegFilterComplex] := ParamVarTemplate(ptFFMpeg)
    + ' %s  -filter_complex "%s" -map "[v]" -map "[a]" -vcodec libx264 "%s"';

end;

destructor TYtMultiCut.Destroy;
begin
  FreeAndNil(FVideos);
  inherited;
end;

procedure TYtMultiCut.FindDownloadedFiles;
var
  V, V2: TVideoFileInfo;
  Files: IJclStringList;
  S: string;
begin
  Writeln('Finding downloaded files...');

  for V in FVideos do
  begin
    if V.VideoID.IsEmpty then
      if FileExists(V.OriginalFile) then
        Continue
      else
      begin
        AskContinue('Video file "%s" not exists', [V.OriginalFile]);
      end;


    Files := TJclStringList.Create.Files(GetDownloadsDir + '*-' +
      V.VideoID + '*.*');
    Files.Sort(SortFilesToNewer);

    for S in Files do
      if not MatchText(ExtractFileExt(S), ['.txt', '.part']) then
      begin

        for V2 in FVideos do
          if V2.OriginalFile = S then
            AskContinue('More 1 videos has same ID = ' + V2.VideoID, []);


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
  if WarnIfEmptyParam(ptYoutubeDLDownload) then
    Exit;

  YoutubeDLProcess := TProcessCreator.Create;
  try
    // first parameter in CreateProcess must specify full path to exe if not empty
    YoutubeDLProcess.ApplicationName := '';
    YoutubeDLProcess.AdjustCmdLine := False;
    YoutubeDLProcess.Parameters := FParams[ptYoutubeDLDownload];
    for V in FVideos do
      YoutubeDLProcess.Parameters := YoutubeDLProcess.Parameters + ' ' + V.Url;
    TDirectory.CreateDirectory(GetDownloadsDir);
    YoutubeDLProcess.CurrentDirectory := GetDownloadsDir;
    // YoutubeDLProcess.CreationFlags := CREATE_NEW_CONSOLE;
    ExecuteAndWait(YoutubeDLProcess, ptYoutubeDL);
  finally
    FreeAndNil(YoutubeDLProcess);
  end;

end;

procedure TYtMultiCut.ReadConfigParams(Lines: IJclStringList);

  procedure SetParam(Param: TParamType);
  var
    I: Integer;
  begin
    I := Lines.IndexOfName(ParamNames[Param]);
    if I >= 0 then
    begin
      FParams[Param] := Lines.ValueFromIndex[I];
      if Param in OutputParams then
        Exit;

      Writeln(Lines[I]);
    end
    else
    begin
      if Param in OutputParams then
        Exit;

      Writeln(ParamNames[Param] + '=[DEFAULT VALUE]=' + IfThen(FParams[Param].IsEmpty, '[EMPTY]', FParams[Param]));
    end;

  end;

var
  P, P2: TParamType;
begin
  Writeln('Reading config params...');
  for P := Low(TParamType) to High(TParamType) do
    SetParam(P);

  { TODO : check replacing order }
  for P := Low(TParamType) to High(TParamType) do
    for P2 := Low(TParamType) to High(TParamType) do
      if FParams[P].Contains(ParamVarTemplate(P2)) then
        FParams[P] := FParams[P].Replace(ParamVarTemplate(P2), FParams[P2]);

end;

function TYtMultiCut.GetDownloadsDir: string;
begin
  Result := PathAddSeparator(FDestinationDir + '_downloads');
end;

function TYtMultiCut.GetVideoExt: string;
begin
  Result := '.' + FParams[ptVideoFormat];
end;

function TYtMultiCut.WarnIfEmptyParam(Param: TParamType): Boolean;
begin
  Result := FParams[Param].IsEmpty;
  if Result then
    Writeln(Format('Empty %s param. %s skipped', [ParamNames[Param],
      ParamActions[Param]]));

end;


procedure TYtMultiCut.GetAndWriteVideoDurations(InputData: IJclStringList);


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
      for V in FVideos do
      begin
        if (V.Duration > 0) or V.Url.IsEmpty  then
          Continue;

        YoutubeDLProcess.Parameters := Format(FParams[ptYoutubeDLGetDuration],
           [V.PureUrl, DURATION_FILENAME]);

        ExecuteAndWait(YoutubeDLProcess, ptYoutubeDLGetDuration);

        DurationFile := YoutubeDLProcess.CurrentDirectory + DURATION_FILENAME;
        try
          DurationStr := TFile.ReadAllText(DurationFile).Trim;
          Writeln(DurationStr);
          V.Duration := StrDurationToTime(DurationStr);
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
    DurationStr: string;
    TotalDuration: TDateTime;
  begin
    Format := TFormatSettings.Create;
    TotalDuration := 0;
    Lines := InputData;
    for V in FVideos do
    begin
      if V.Url.IsEmpty and string(V.OriginalFile).IsEmpty then
        Continue;

      if V.Duration = 0 then
      begin
        if not V.Url.IsEmpty then
          AskContinue('Can''t determine video duration for "%s"', [V.Url]);
        Continue;
      end;

      TotalDuration := TotalDuration + V.Duration;
      if not V.Url.IsEmpty then
        I := Lines.IndexOf(V.Url)
      else if not V.OriginalFile.IsEmpty then
        I := Lines.IndexOf(V.OriginalFile)
      else
        Continue;

      if I >= 0 then
      begin
        DurationStr := LENGTH_PARAM + TimeLengthToStr(V.Duration, Format);
        if I = Lines.Count - 1 then
          Lines.Add(DurationStr)
        else
        begin
          if Lines[I + 1].StartsWith(LENGTH_PARAM) then
            Lines[I + 1] := DurationStr
          else
            Lines.Insert(I + 1, DurationStr);
        end;
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
    Writeln('Total duration ~ ' + TimeLengthToStr(TotalDuration, Format));


  end;

var
  I: Integer;
  Part: TVideoPart;
  V: TVideoFileInfo;
  PartDuration: TDateTime;
begin
{ TODO :
Getting this error if url is local file path:
Writing video length failed: can't find video with url "" . Continue? [y/n] }
  try

    Write(ParamActions[ptYoutubeDLGetDuration]);
    Write(Format(' for %d video(s)', [FVideos.Count]));
    Writeln;

    GetDurations;
    WriteDurations;
  finally

  end;

end;

function TYtMultiCut.GetConcatVidsDir: string;
begin
  Result := PathAddSeparator(FDestinationDir + '_concatenated');
end;

procedure TYtMultiCut.LoadInputData(InputData: IJclStringList);
begin
  ReadConfigParams(InputData);
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
    begin
      Video.Duration := StrDurationToTime(Lines[I].Substring(LENGTH_PARAM.Length))
    end
    else if TryStrToInt(Line.Split([' ', ':', '.'])[0], Dummy) then
    begin
      Video.ParseTimes(Line);
      Lines[I] := Line;
    end
    else
      Video.FComment := Line;

  end;

  WritelnFmt('Loaded %d videos', [FVideos.Count]);
  if FVideos.Count = 1  then
    WritelnFmt('Parts count = %d', [FVideos[0].Parts.Count]);


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

  if DelCount > 0 then
    AskContinue('Found %d unrelated texts', [DelCount], True);

end;

function TYtMultiCut.ParamVarTemplate(Param: TParamType): string;
begin
  Result := '%' + ParamNames[Param] + '%';
end;

function TYtMultiCut.TrimVideos: TFilename;

var
  FFMpeg: TProcessCreator;
  NewFilesDemuxer: IJclStringList;

  procedure TrimViaFilterComplex(Videos: array of TVideoFileInfo; OutputVideo: TFilename);
  var
    FilterInput: IJclStringList;
    FilterVideosScale, FilterVideosTrim: IJclStringList;
    FilterAudios: IJclStringList;
    FilterConcat: string;
    InputFileNo: Integer;
    OutNo: Integer;
    AIn, AOut, VIn, VOut: string;

    procedure AddInput(Video: TVideoFileInfo);
    var
      VStreamSource: string;
    begin
      FilterInput.GetStringsRef.AddFmt('-i "%s" ', [Video.OriginalFile]);
      Inc(InputFileNo);

      AIn := InputFileNo.ToString + ':' + 'a'; // [0:a]
      VIn := Format('%dv', [InputFileNo]); //[0v]
      VStreamSource := InputFileNo.ToString + ':' + 'v'; // [0:v]
      AOut := AIn;
      VOut := VIn;
      if InputFileNo = 0 then
        // using setsar in case of some videos with SAR = 0:1 (!) with which scale2ref is useless
        FilterVideosScale.GetStringsRef.AddFmt('[%s]setsar=1[%s]; ', [VStreamSource, VIn])
      else
      begin
        FilterVideosScale.GetStringsRef.AddFmt('[%s][0v]scale2ref[s%s][0v]; ', [VStreamSource, VIn]);
        VStreamSource := VIn;
        FilterVideosScale.GetStringsRef.AddFmt('[s%s]setsar=1[%s]; ', [VStreamSource, VIn]);
      end;
    end;

    procedure AddOut;
    begin
      FilterConcat := FilterConcat + Format('[%s][%s]', [VOut, AOut]);
      Inc(OutNo);
    end;

  const
    SET_PTS = 'setpts=PTS-STARTPTS';
  var
    T: TVideoPart;
    Trim: string;
    V: TVideoFileInfo;
    PrevTime: TDateTime;
  begin
    if  WarnIfEmptyParam(ptFFMpegFilterComplex) then
      Exit;

    FilterInput := JclStringList;

    FilterVideosScale := JclStringList;
    FilterVideosTrim := JclStringList;
    FilterAudios := JclStringList;

    FilterVideosScale.Add('');
    OutNo := 0;

    TDirectory.CreateDirectory(GetConcatVidsDir);
    InputFileNo := -1;
    for V in Videos do
    begin

      AddInput(V);

      PrevTime := 0;
      if V.Parts.Count = 0 then
        AddOut
      else
        for T in V.Parts do
        begin
          //if T.InPointTime < PrevTime then
          if PrevTime <> 0 then
            AddInput(V); // otherwise it leads to Buffer queue overflow, dropping

          Trim := T.InPoint.Replace(':', '\:').QuotedString + ':' + T.OutPoint.Replace(':', '\:').QuotedString;

          VOut := Format('%dv%d', [InputFileNo, OutNo]); //[v0-0]
          AOut := Format('%da%d', [InputFileNo, OutNo]); //[v0-0]
          FilterVideosTrim.GetStringsRef.AddFmt('[%s]trim=%s,%s[%s]; ', [VIn, Trim, SET_PTS, VOut]);
          FilterAudios.GetStringsRef.AddFmt('[%s]atrim=%s,a%s[%s]; ', [AIn, Trim, SET_PTS, AOut]);

          PrevTime := T.InPointTime; // or OutPoint ? Check

          AddOut;
        end;

    end;

    FilterConcat := FilterConcat + Format('concat=n=%d:v=1:a=1[v][a]', [OutNo]);
    FFMpeg.Parameters := Format(FParams[ptFFMpegFilterComplex],
      [FilterInput.Text, FilterVideosScale.Text + FilterVideosTrim.Text + FilterAudios.Text  + FilterConcat, OutputVideo]);
    ExecuteAndWait(FFMpeg, ptFFMpegFilterComplex)

  end;

  procedure ConcatAllVideos(OutputVideo: TFilename);
  begin
    if WarnIfEmptyParam(ptFFMpegConcat) then
      Exit;
    Writeln('');
    Writeln(ParamActions[ptConcatAll]);
    NewFilesDemuxer.SaveToFile(GetConcatVidsDir + CONCAT_DEMUXER_FILE);
    FFMpeg.CurrentDirectory := GetConcatVidsDir;
    FFMpeg.Parameters := Format(FParams[ptFFMpegConcat], [OutputVideo]);
    ExecuteAndWait(FFMpeg, ptConcatAll);
    TFile.Delete(GetConcatVidsDir + CONCAT_DEMUXER_FILE);
  end;

const
  KillFFMpegTimeout = 2000;
  KillFFMpegWaitAfter = 500;
var
  V: TVideoFileInfo;
  HasCutting: Boolean;
  DownloadsDirName: string;
  I: Integer;
  FinalVideo: TFilename;
  PID: THandle;
begin
  Writeln('');
  Writeln('Start trimming');

  if DebugHook <> 0 then
  begin
    PID := GetPidFromProcessName(FFMpegExe);
    if PID <> INVALID_HANDLE_VALUE then
    begin
      WritelnFmt('Terminating "%s"...', [FFMpegExe]);
      if TerminateApp(PID, KillFFMpegTimeout) = taKill  then
        Sleep(KillFFMpegWaitAfter);
    end;
  end;


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
    //FFMpeg.CreationFlags := CREATE_NEW_CONSOLE;
    FFMpeg.CurrentDirectory := FDestinationDir;

    I := 0;

    FinalVideo := IfThen(FParams[ptConcatAll].IsEmpty, 'final', FParams[ptConcatAll]);
    if not ExtractFileExt(FinalVideo).EndsWith(GetVideoExt, True) then
      FinalVideo := FinalVideo + GetVideoExt;
    if TFile.Exists(FFMpeg.CurrentDirectory + FinalVideo) then
      TFile.Delete(FFMpeg.CurrentDirectory + FinalVideo);

    TrimViaFilterComplex(FVideos.ToArray, FinalVideo);

// { TODO : check }
//
// if not HasCutting then
//    for V in FVideos do
//        NewFilesDemuxer.Add('file '
//          + QuotedStr(DownloadsDirName.QuotedString + PathDelim + ExtractFileName(V.OriginalFile))
//        );
//
//    ConcatAllVideos('..\' +  FinalVideo);

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



function TYtMultiCut.UseConcatDemuxer: Boolean;
begin
  if not TryStrToBool(FParams[ptUseConcatDemuxer], Result) then
    Result := DEFAULT_USE_CONCAT_DEMUXER;
end;

procedure WaitForEnterKeyForExit;
begin
  Writeln('Press ENTER for exit');
  Readln;
end;


procedure Run;
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
      Cutter.TrimVideos;

      //if Cutter.HasErrors then
        //WaitForEnterKeyForExit;

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
end;

{ TVideoPart }

procedure TVideoPart.CheckPoints;
begin
  if not FOutPoint.IsEmpty and not FInPoint.IsEmpty and  (GetDuration < 0) then
    AskContinue('Inpoint "%s" > oupoint "%s"', [FInPoint, FOutPoint]);
end;

function TVideoPart.GetDuration: TDateTime;
begin
  Result := GetOutPointTime - GetInPointTime;
end;


function TVideoPart.GetInPointTime: TDateTime;
begin
  Result := StrDurationToTime(FInPoint);
end;

function TVideoPart.GetOutPointTime: TDateTime;
begin
  Result := StrDurationToTime(FOutPoint);
end;

procedure TVideoPart.SetInPoint(const Value: string);
begin
  FInPoint := Value;
  CheckPoints;
end;

procedure TVideoPart.SetOutPoint(const Value: string);
begin
  FOutPoint := Value;
  CheckPoints;
end;

end.
