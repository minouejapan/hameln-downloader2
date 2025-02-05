program hamelndlw;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$TYPEINFO OFF}

uses
{$IFnDEF FPC}
  Vcl.Forms,
{$ELSE}
  Forms, Interfaces,
{$ENDIF}
  nvdllib, dlmainunit;

{$R *.res}

begin
{$IFDEF FPC}
  Application.Scaled:=True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(THameln, Hameln);
  Application.Run;
end.

