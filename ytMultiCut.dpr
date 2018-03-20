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
  Variants,
  uYTMulticut in 'uYTMulticut.pas';

const
  EXITCODE_ABORT = 1;
  EXITCODE_EXCEPTION = 2;


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

end.



