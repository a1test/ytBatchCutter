program ytBatchCutter;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}
{$R *.res}
{$R *.dres}

uses
  uYtBatchCutter in 'source\uYtBatchCutter.pas';

begin
{ TODO : Add ability to repeatedly insert parts of the same file }
{ TODO : Merge videos with different sizes }
  Run;

end.



