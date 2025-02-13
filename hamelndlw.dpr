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

var
  hMutex: THandle;
	smem: ^TShareMem;

begin
{$IFDEF FPC}
{$ENDIF}
  Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(THameln, Hameln);
  Application.Run;
end.

