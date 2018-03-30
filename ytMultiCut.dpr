program ytMultiCut;

{$APPTYPE CONSOLE}
{$WARN SYMBOL_PLATFORM OFF}
{$R *.res}
{$R *.dres}

uses
  uYtMulticut in 'source\uYtMulticut.pas';

begin
{ TODO : Add ability to repeatedly insert parts of the same file }
{ TODO : Merge videos with different sizes }
  Run;

end.



