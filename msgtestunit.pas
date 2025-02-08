unit msgtestunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Windows, LazUTF8;

type
  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;//Handle of the Window that passed the data
    CopyDataStruct: PCopyDataStruct; //data passed
    Result: Longint;//Use it to send a value back to the "Sender"
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  protected
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;


  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  PrevWndProc: WNDPROC;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  str: string;
begin

  if uMsg = WM_COPYDATA then
  begin
    str := PChar(PCopyDataStruct(LParam).lpData);
    Form1.Edit1.Text := str;
    Exit;
  end;

  result := CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  CDS: TCopyDataStruct;
  str: string;
  len: integer;
  Wnd: THandle;
begin
  Wnd := StrToInt(Edit2.Text);
  str := '123TEST';
  len := UTF8Length(str) + 2;
  Cds.dwData := 0;
  Cds.cbData := len;
  Cds.lpData := Pointer(str);
  SendMessage(Wnd, WM_COPYDATA, Handle, LPARAM(@Cds));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit3.Text := IntToStr(Handle);
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrUInt(@WndCallback)));
end;

procedure TForm1.WMCopyData(var Message: TWMCopyData);
var
  str: string;
begin
  str := PChar(Message.CopyDataStruct.lpData);
  Edit1.Text := str;
end;

end.

