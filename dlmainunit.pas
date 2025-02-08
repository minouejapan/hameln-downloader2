(*
  ハーメルン小説ダウンローダー

  1.31     02/08  担保園ではないが１話しかない作品をダウンロード出来なかった不具合を修正した
  1.3 2025/02/08  作者名を取得出来ない場合があった不具合を修正した
                  ログファイルの書式を他の外部ダウンローダーに合わせた
                  実行時引数にURLを指定しても自動実行しなかった不具合を修正した
                  挿絵URLの抽出処理が甘かった不具合を修正した
                  短編の処理がおかしかった不具合を修正した
  1.2 2025/02/06  各話タイトルを正しく取得出来ない場合があった不具合を修正した
  1.1 2025/02/01  本文が完全に取得出来ない場合があった不具合を修正した
                  文字コードにエスケープされた文字を戻せない場合があった不具合を修正した
                  特殊タグの除去が不十分だったものを修正した
  1.0 2025/01/28  WinINetで設定したCookieが有効でなくなったためTEdgeBrowserベースに移植した

  ビルドに必要：
    WebView4Delphi  https://github.com/salvadordf/WebView4Delphi
    TRegExpr        https://github.com/andgineer/TRegExpr
*)

unit dlmainunit;

{$IFDEF FPC}
  {$MODE Delphi}
  {$CODEPAGE utf8}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, LazUTF8,
{$ELSE}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Lazutf8wrap,
{$ENDIF}
  RegExpr, WinINet,
  uWVBrowserBase, uWVBrowser,uWVWindowParent, uWVTypes, uWVTypeLibrary, uWVLoader;

type

  { THameln }

  THameln = class(TForm)
    Panel1: TPanel;
    WV2: TWVBrowser;
    Timer1: TTimer;
    Panel2: TPanel;
    CancelBtn: TButton;
    StartBtn: TButton;
    NvTitle: TLabel;
    URL: TEdit;
    Elapsed: TLabel;
    Status: TLabel;
    Label1: TLabel;
    OCBtn: TSpeedButton;
    WVWindowParent1: TWVWindowParent;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure WV2AfterCreated(Sender: TObject);
    procedure WV2ExecuteScriptCompleted(Sender: TObject;
      aErrorCode: HRESULT; const aResultObjectAsJson: wvstring;
      aExecutionID: integer);
    procedure WV2InitializationError(Sender: TObject;
      aErrorCode: HRESULT; const aErrorMessage: wvstring);
    procedure WV2NavigationCompleted(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2NavigationCompletedEventArgs);
    procedure WV2RetrieveHTMLCompleted(Sender: TObject;
      aResult: boolean; const aHTML: wvstring);
    procedure WV2SourceChanged(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2SourceChangedEventArgs);
    procedure Timer1Timer(Sender: TObject);
    procedure OCBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    Done,
    Cancel,
    Busy,
    WVCreated: boolean;
    TextBuff,
    PrevURL,
    NextURL: string;
    StartTime: TTime;
    FmHt: integer;
    IsCalled,
    NShort: Boolean;
    JsCnt,
    WVMode: integer;
    function ParsePage(Page: string): Boolean;
    procedure LoadEachPage;
    procedure ParseShort(Page: string);
    procedure ParseChapter(MainPage: string);
    function GetNovelStatus(MainPage: string): string;
    function GetHTMLSrc(aURL: string; Mode: integer): string;
  public

  end;

var
  Hameln: THameln;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}
{$R verinfow.res}

{ THameln }

uses
  nvdllib;

const
  // バージョン
  VERSION  = 'ver1.2 2025/2/6';
  NVSITE   = 'https://syosetu.org';
  // データ抽出用の識別タグ(正規表現バージョン)
  // トップページ
  STITLE   = '<span .*?itemprop="name">.*?</span>';                                 // タイトル
  SAUTHER  = '<div align="right">作者：<span itemprop="author">.*?</a></span></div>';   // 作者
  SAUTHER2 = '<div align="right">作者：<span itemprop="author">.*?</span></div>';   // 作者
  SHEADER  = '<div class="ss">.*?<hr.*?></div>';                                    // 前書き部分
  SCONTENT = '<div class="ss">.*?<table .*?>.*?</div>';                             // 目次部分
  SCHAPTER = '<tr><td .*?><strong>.*?</strong></td></tr>';                          // 章(ないこともある)
  SSECTION = '<span id=".*?">.*?</span>';                                           // 話
  SSHORT   = '<div style=.*?>1 / 1</div>';                                          // 短編判別用
  // 短編
  SSTITLE  = '<span .*?><a href=\./>.*?</a></span>';                                // タイトル
  SSAUTHER = '作：<a href=".*?">.*?</a>';                                           // 作者
  SSMAEGAKI= '</div>'#13#10'<div class="ss">.*?<hr';                                // 前書き部分
  SSATOGAKI= '<div id="atogaki">.*?><br>.*?</div>';
  SSSECT   = '<span style="font-size:120%"> .*?</span>';                            // 話
  SSBODY   = '<div id="honbun">.*?</div>';                                          // 短編本文
  // 各話ページ
  SHEAD    = '<p><span.*?><a href=\./>.*?</a></span>';                              // タイトルまで
  SMAEGAKI = '<div id="maegaki">.*?</div>';                                         // 前書き
  SATOGAKI = '<div id="atogaki">.*?</div>';                                         // 後書き
  SPTITLE  = '<span style="font-size:120%">.*?</span>';                             // 各話タイトル
  SBODY    = '<div id="honbun">.*?</div>';                                          // 本文
  SLINEB   = '<p id=".*?">';                                                        // 行始まり
  SLINEE   = '</p>';                                                                // 行終わり
  SIMAGE   = '<a href=".*?" alt="挿絵" name="img">.*?</a>';                                               // 挿絵


// ユーザメッセージID
  WM_DLINFO  = WM_USER + 30;


var
  TextPage,
  PageList,
  TitleList,
  LogFile: TStringList;
  URLadr,
  Path,
  NvStat,
  FileName,
  StartPage: string;
  RegEx: TRegExpr;
  hWnd: THandle;
  CDS: TCopyDataStruct;
  StartN: integer;

// WinINetを用いたHTMLファイルのダウンロード
function LoadFromHTML(URLadr: string): string;
var
  hSession    : HINTERNET;
  hService    : HINTERNET;
  dwBytesRead : DWORD;
  dwFlag      : DWORD;
  lpBuffer    : PChar;
  RBuff       : TMemoryStream;
  TBuff       : TStringList;
begin
  Result   := '';
  // ハーメルンサイトのR18作品アクセス用Cookie
  if not InternetSetCookie(PChar('https://syosetu.org'{URLadr}), PChar('over18'), PChar('off')) then
    Writeln(#13#10'Cookieの設定に失敗しました.');

  hSession := InternetOpen('WinINet', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(hSession) then
  begin
    dwFlag   := INTERNET_FLAG_RELOAD;
    hService := InternetOpenUrl(hSession, PChar(URLadr), nil, 0, dwFlag, 0);
    if Assigned(hService ) then
    begin
      RBuff := TMemoryStream.Create;
      try
        lpBuffer := AllocMem(65536);
        try
          dwBytesRead := 65535;
          while True do
          begin
            if InternetReadFile(hService, lpBuffer, 65535,{SizeOf(lpBuffer),}dwBytesRead) then
            begin
              if dwBytesRead = 0 then
                break;
              RBuff.WriteBuffer(lpBuffer^, dwBytesRead);
            end else
              break;
          end;
        finally
          FreeMem(lpBuffer);
        end;
        TBuff := TStringList.Create;
        try
          RBuff.Position := 0;
          TBuff.LoadFromStream(RBuff, TEncoding.UTF8);
          Result := TBuff.Text;
        finally
          TBuff.Free;
        end;
      finally
        RBuff.Free;
      end;
    end;
    InternetCloseHandle(hService);
  end;
end;

// HTML内のJavaScript実行が完了したことを判定する
function IsScriptDone(Src: string): boolean;
begin
  Result := Src <> '';
end;

// 本文の改行タグを改行コードに変換する
function ChangeBRK(Base: string): string;
var
  str: string;
begin
  str    := UTF8StringReplace(Base, '<br />', #13#10, [rfReplaceAll]);
  str    := UTF8StringReplace(str, '<br/>',   #13#10, [rfReplaceAll]);
  Result := UTF8StringReplace(str, '<br>',    #13#10, [rfReplaceAll]);
end;

// 本文の文字装飾を除去する
function EliminateDeco(Base: string): string;
var
  tmp: string;
begin
  tmp := Base;
  tmp := ReplaceRegExpr('<span .*?>', tmp, '');
  tmp := ReplaceRegExpr('</span>', tmp, '');
  tmp := ReplaceRegExpr('<div .*?>', tmp, '');
  tmp := ReplaceRegExpr('</div>', tmp, '');
  tmp := ReplaceRegExpr('<table .*?>', tmp, '');
  tmp := ReplaceRegExpr('<th .*?>', tmp, '');
  tmp := ReplaceRegExpr('<tr .*?>', tmp, '');
  tmp := ReplaceRegExpr('<td .*?>', tmp, '');
  tmp := ReplaceRegExpr('<tr>', tmp, '');
  tmp := ReplaceRegExpr('</tr>', tmp, '');
  tmp := ReplaceRegExpr('<hr>', tmp, AO_HR);

  Result := tmp;
end;

// 本文の傍点を青空文庫形式に変換する
function ChangeBouten(Base: string): string;
var
  tmp: string;
begin
  RegEx.Expression  := '<span class="\.sesame">.*?</span>';  // 傍点指定範囲
  RegEx.InputString := Base;
  while RegEx.Exec do
  begin
    tmp := RegEx.Match[0];
    tmp := ReplaceRegExpr('<span class="\.sesame">', tmp, '');
    tmp := ReplaceRegExpr('</span>', tmp, '');
    tmp := ReplaceRegExpr('<ruby><rb>', tmp, '');
    tmp := ReplaceRegExpr('</rb>.*?</ruby>', tmp, '');
    UTF8Delete(Base, RegEx.MatchPos[0], RegEx.MatchLen[0]);         // 傍点範囲の文字列を削除
    UTF8Insert(AO_EMB + tmp + AO_EME, Base, RegEx.MatchPos[0]);  // 変換後の文字列を挿入
    RegEx.InputString := Base;
  end;
  // 本文中の余計なタグを除去する
  Base := ReplaceRegExpr('<style>.*?</style>', Base, '');
  Base := ReplaceRegExpr('<script>.*?</script>', Base, '');

  Result := Base;
end;

// 本文のルビタグを青空文庫形式に変換する
function ChangeRuby(Base: string): string;
var
  tmp: string;
begin
  // <rp>タグを除去
  tmp := UTF8StringReplace(Base, '<rp>(</rp>', '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rp>)</rp>', '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rp>（</rp>', '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rp>）</rp>', '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rb>', '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '</rb>', '', [rfReplaceAll]);
  // rubyタグを青空文庫形式に変換
  tmp := UTF8StringReplace(tmp,  '<ruby>', AO_RBI, [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rt>',   AO_RBL, [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '</rt></ruby>', AO_RBR, [rfReplaceAll]);

  Result := tmp;
end;

// 埋め込まれた画像リンクを青空文庫形式に変換する
function ChangeImage(Base: string): string;
var
  str: string;
begin
  RegEx.Expression  := SIMAGE;
  RegEx.InputString := Base;
  while RegEx.Exec do
  begin
    str := RegEx.Match[0];
    str := ReplaceRegExpr('<a href="', str, AO_PIB);
    str := ReplaceRegExpr('" alt="挿絵" name="img">.*?</a>', str, AO_PIE);
    UTF8Delete(Base, RegEx.MatchPos[0], RegEx.MatchLen[0]);
    UTF8Insert(str, Base, RegEx.MatchPos[0]);
    RegEx.InputString := Base;
  end;
  Result := Base;
end;

// 本文のリンクタグを除去する
function Delete_tags(Base: string): string;
begin
  // リンクタグ
  Base := ReplaceRegExpr('<a href=".*?">', Base, '');
  Base := ReplaceRegExpr('</a>', Base, '');
  // 行タグ
  Base := ReplaceRegExpr('<p id=".*?">', Base, '');  // 各行を整形
  Base := ReplaceRegExpr('</p>', Base, #13#10);

  Result := Base;
end;

// タグ類処理(順番を間違えると誤作動する)
function ProcTags(Str: string): string;
var
  tmp: string;
begin
  tmp := ChangeAozoraTag(Str);
  tmp := ChangeBrk(tmp);
  tmp := EliminateDeco(tmp);
  tmp := ChangeBouten(tmp);
  tmp := Restore2RealChar(tmp);
  tmp := ChangeImage(tmp);
  tmp := ChangeRuby(tmp);
  Result := Delete_tags(tmp);
end;

// 小説本文をHTMLから抜き出して整形する
function THameln.ParsePage(Page: string): Boolean;
var
  sp, i, mp, ml: integer;
  header, footer, chapt, sect, body, tmp: string;
  lines: TStringList;
begin
  Result := True;
  // 前書き
  header := '';
  RegEx.Expression  := SMAEGAKI;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    header := RegEx.Match[0];
    mp := RegEx.MatchPos[0];
    ml := RegEx.MatchLen[0];
    header := ReplaceRegExpr('<div id="maegaki">', header, '');
    header := ReplaceRegExpr('</div>', header, '');
    header := ProcTags(header);
  end;
  // 章・話タイトル
  chapt := ''; sect := '';

  sp := Pos('<li class="novelmokuzi">', Page);
  if sp > 0 then
    Delete(Page, 1, sp);

  body := '';
  RegEx.Expression  := SPTITLE;    //<span style=.*?>.*?</span>
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    sect := RegEx.Match[0];
    mp := RegEx.MatchPos[0];
    ml := RegEx.MatchLen[0];
    sect := ReplaceRegExpr('<span .*?>', sect, '');
    sect := ReplaceRegExpr('</span>', sect, '');
    sect := ProcTags(sect);
    // 章タイトルが含まれていれば分離する
    sp := UTF8Pos(#13#10, sect);
    if sp > 0 then
    begin
      chapt := Trim(UTF8Copy(sect, 1, sp - 1));
      sect  := Trim(UTF8Copy(sect, sp + 2, Length(sect)));
    end else
      sect := Trim(sect);
    UTF8Delete(Page, 1, mp + ml - 1);
  end else
    Result:= False;
  // 本文
  body := '';
  RegEx.InputString := Page;
  // honbunタグと本文開始の<p id="0">の間に文章があれば抽出する
  RegEx.Expression  := '<div id="honbun">.*?<p id="0">';
  if RegEx.Exec then
  begin
    tmp := RegEx.Match[0];
    tmp := ReplaceRegExpr('<div id="honbun">', tmp, '');
    tmp := ReplaceRegExpr('<p id="0">', tmp, '');
    body := tmp;
  end;
  // 装飾用特殊タグで本文部分を誤認識しないように一行ずつ抽出する
  RegEx.Expression  := '<p id="\d*?">.*?</p>';//SBODY;
  while RegEx.Exec do
  begin
    body := body + RegEx.Match[0];
    UTF8Delete(Page, 1, RegEx.MatchPos[0]);
    RegEx.InputString := Page;
  end;
  body := ChangeAozoraTag(body);
  body := ProcTags(body);
  // 全角空白が64個以上連続していた場合はダミーと判断して全て除去する
  lines := TStringList.Create;
  try
    lines.Text := body;
    RegEx.Expression := ' *';
    for i := 0 to lines.Count - 1 do
    begin
      RegEx.InputString := lines.Strings[i];
      if RegEx.Exec then
      begin
        if (RegEx.MatchPos[0] = 1) and (RegEx.MatchLen[0] > 10) then
        begin
          lines.Strings[i] := ReplaceRegExpr(' *', lines.Strings[i], '');
        end;
      end;
    end;
    body := lines.Text;
  finally
    lines.Free;
  end;
  UTF8Delete(Page, 1, RegEx.MatchPos[0] + RegEx.MatchLen[0] - 1);
  // 後書き
  footer := '';
  RegEx.Expression  := SATOGAKI;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    footer := RegEx.Match[0];
    footer := ReplaceRegExpr('<div id="atogaki">', footer, '');
    footer := ReplaceRegExpr('</div>', footer, '');
    footer := ProcTags(footer);
    UTF8Delete(Page, 1, RegEx.MatchPos[0] + RegEx.MatchLen[0] - 1);
  end;
  if chapt <> '' then
    TextPage.Add(AO_CPB + chapt + AO_CPE);
  TextPage.Add(AO_SEB + sect + AO_SEE);
  if header <> '' then
    TextPage.Add(AO_KKL + header + #13#10 + AO_KKR);
  if body <> '' then
    TextPage.Add(body)
  else
    TextPage.Add('★HTMLページ読み込みエラー');
  if footer <> '' then
    TextPage.Add(AO_KKL + footer + #13#10 + AO_KKR);
  TextPage.Add('');
  TextPage.Add(AO_PB2);
  TextPage.Add('');

  if body = '' then
  begin
    Result := False;
  end;
end;

// 各話URLリストをもとに各話ページを読み込んで本文を取り出す
procedure THameln.LoadEachPage;
var
  i, n, cnt, sc, rt: integer;
  line, stat, urla: string;
begin
  cnt := PageList.Count;
  if StartN > 0 then
    i := StartN - 1
  else
    i := 0;
  n := 1;
  sc := cnt - i;
  // 最初のアクセスが空振りするのでダミでーアクセスしておく
  urla := PageList.Strings[0];
  line := GetHTMLSrc(urla, 0);
  while i < cnt do
  begin
    urla := PageList.Strings[i];
    URL.Text := urla;
    PrevURL := '';
    NextURL := '';
    // TEgdeBrowserはNavigate(URL)してもページを更新してくれない場合があるため
    // エピソードページの取得を判定するために前後ページへのリンクURLを保存する
    if cnt > 1 then
    begin
      if i > 0 then
        PrevURL := '<a href="./' + IntToStr(n - 1)  + '.html"><< 前の話</a>';
      if i < cnt then
        NextURL := '<a href="./' + IntToStr(n + 1) + '.html" class="next_page_link">次の話 >></a>';
    end;
    line := GetHTMLSrc(urla, 0);
    rt := 1;
    // 前後ページへのリンクURLがあるかチェックしてない場合は再取得を繰り返す
    // 尚、5回繰り返しても取得出来なければエラーとする
    if cnt > 1 then
    begin
      while ((i = 0) and (UTF8Pos(NextURL, line) = 0))
         or ((i > 0) and (UTF8Pos(PrevURL, line) = 0)) do
      begin
        Status.Caption := 'リトライ中(' + IntToStr(rt) + ')';
        // リトライを5回行っても駄目だった場合はエラーとする
        if rt = 5 then
        begin
          TextPage.Add('★エラー：リトライ回数超過');
          line := '';
          Break;
        end;
        if Cancel then
          Break;
        Inc(rt);
        Sleep(500);
        line := GetHTMLSrc(urla, 0);
      end;
    end;

    if line <> '' then
    begin
      if not ParsePage(line) then
        Break;
      stat := '各話を取得中 [' + Format('%3d', [i + 1]) + '/' + Format('%3d', [cnt]) + '(' + Format('%d', [(n * 100) div sc]) + '%)]';
      Status.Caption := stat;
      Elapsed.Caption := '経過時間：' + FormatDateTime('nn:ss', Now - StartTime);
      Application.ProcessMessages;
      if hWnd <> 0 then
        SendMessage(hWnd, WM_DLINFO, n, 1);
      if Cancel then
        Break;
      // サーバーへの負担を減らすため1秒のインターバルを入れる
      Sleep(1000);   // Sleep処理を削除したり、この数値を小さくすることを禁止します
    end;
    Inc(i);
    Inc(n);
  end;
end;

// 短編専用処理
procedure THameln.ParseShort(Page: string);
var
  title, auther, authurl, header, sect, body, footer: string;
begin
  title := ''; auther := ''; authurl := '';
  // タイトル
  RegEx.Expression  := SSTITLE;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    title := RegEx.Match[0];
    title := ReplaceRegExpr('<span .*?><a href=\./>', title, '');
    title := ReplaceRegExpr('</a></span>', title, '');
    title := ProcTags('【短編】' + title);
    // ファイル名を準備する
    if FileName = '' then
      FileName := Path + PathFilter(title) + '.txt';
  end;
  // 作者・作者URL
  RegEx.Expression  := SSAUTHER;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    auther := RegEx.Match[0];
    authurl:= auther;
    auther := ReplaceRegExpr('作：<a href=".*?">', auther, '');
    auther := ReplaceRegExpr('</a>', auther, '');
    auther := ProcTags(auther);
    authurl:= ReplaceRegExpr('作：<a href="', authurl, '');
    authurl:= ReplaceRegExpr('">.*?</a>', authurl, '');
    if authurl <> '' then
      authurl:= 'https:' + authurl;
  end;
  // 前書き
  header := '';
  RegEx.Expression  := SSMAEGAKI;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    header := RegEx.Match[0];
    header := ReplaceRegExpr('</div>'#13#10'<div class="ss">', header, '');
    header := ReplaceRegExpr('<hr', header, '');
    header := ProcTags(header);
  end;
  // 話タイトル
  sect := '';
  RegEx.Expression  := SSSECT;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    sect := RegEx.Match[0];
    sect := ReplaceRegExpr('<span style="font-size:120%">', sect, '');
    sect := ReplaceRegExpr('</span>', sect, '');
    sect := ProcTags(Trim(sect));
  end;
  // 本文
  body := '';
  RegEx.Expression  := SSBODY;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    body := RegEx.Match[0];
    body := ReplaceRegExpr('<div id="honbun">', body, '');
    body := ReplaceRegExpr('</div>', body, '');
    body := ReplaceRegExpr('<div id="honbun">', body, '');
    body := ReplaceRegExpr('</div>', body, '');
    body := ChangeAozoraTag(body);
    body := ReplaceRegExpr('<p id=".*?">', body, '');  // 各行を整形
    body := ReplaceRegExpr('</p>', body, #13#10);
    body := ProcTags(body);
  end;
  // 後書き
  footer := '';
  RegEx.Expression  := SSATOGAKI;
  RegEx.InputString := Page;
  if RegEx.Exec then
  begin
    footer := RegEx.Match[0];
    footer := ReplaceRegExpr('<div id="atogaki">', footer, '');
    footer := ReplaceRegExpr('</div>', footer, '');
    footer := ProcTags(footer);
  end;
  TextPage.Add(title);
  TextPage.Add(auther);
  TextPage.Add(AO_PB2);
  if header <> '' then
  begin
    TextPage.Add(AO_KKL + URL.Text + #13#10 + header + #13#10 + AO_KKR);
    TextPage.Add(AO_PB2);
  end;
  TextPage.Add(AO_SEB + sect + AO_SEE);
  TextPage.Add(body);
  if footer <> '' then
    TextPage.Add(AO_KKL + footer + #13#10 + AO_KKR);
  TextPage.Add(AO_PB2);

  LogFile.Add(URL.Text);
  LogFile.Add('タイトル：' + title);
  if authurl <> '' then
    LogFile.Add('作者　　：' + auther + '(https:' + authurl + ')')
  else
    LogFile.Add('作者  ：' + auther);
  LogFile.Add('あらすじ：');
  LogFile.Add(header);
  LogFile.Add('');
end;

// トップページからタイトル、作者、前書き、各話情報を取り出す
procedure THameln.ParseChapter(MainPage: string);
var
  sp, sc, sl, pn: integer;
  title, auther, authurl, header, cont, sendstr, aurl: string;
  conhdl: THandle;
  ws: WideString;
begin
  title := ''; auther := ''; authurl := ''; header := ''; cont := '';
  // 短編かどうかをチェックする
  RegEx.Expression  := SSHORT;
  RegEx.InputString := MainPage;
  NShort := RegEx.Exec;
  if NShort then
  begin
    ParseShort(MainPage);
  end else begin
    // タイトル名
    RegEx.Expression  := STITLE;
    //RegEx.InputString := MainPage;
    if RegEx.Exec then
    begin
      sp := RegEx.MatchPos[0] + RegEx.MatchLen[0];
      title := RegEx.Match[0];
      // タイトルの前後のタグを除去する
      title := ReplaceRegExpr('<span .*?itemprop="name">', title, '');
      title := ReplaceRegExpr('</span>', title, '');
      title := ProcTags(title);
      UTF8Delete(MainPage, 1, sp - 1);
      NvTitle.Caption := '作品タイトル：' + title;
      // ファイル名を準備する
      if FileName = '' then
        FileName := Path + PathFilter(title) + '.txt';
      // 作者名
      RegEx.Expression := SAUTHER;
      RegEx.InputString:= MainPage;
      if RegEx.Exec then
      begin
        sp := RegEx.MatchPos[0] + RegEx.MatchLen[0];
        auther := RegEx.Match[0];
        // 作者名の前後のタグを除去する
        auther := ReplaceRegExpr('<div align="right">作者：<span itemprop="author">', auther, '');
        auther := ReplaceRegExpr('</span></div>', auther, '');
        UTF8Delete(MainPage, 1, sp - 1);
        RegEx.InputString := auther;
        RegEx.Expression  := '<a href=.*?>';
        if RegEx.Exec then
        begin
          authurl := RegEx.Match[0];
          authurl := ReplaceRegExpr('<a href="', authurl, '');
          authurl := ReplaceRegExpr('">', authurl, '');
          auther := ReplaceRegExpr('<.*?>', auther, '');
        end;
        auther := ProcTags(auther);
      end else begin
        RegEx.Expression := SAUTHER2;
        RegEx.InputString:= MainPage;
        if RegEx.Exec then
        begin
          sp := RegEx.MatchPos[0] + RegEx.MatchLen[0];
          auther := RegEx.Match[0];
          // 作者名の前後のタグを除去する
          auther := ReplaceRegExpr('<div align="right">作者：<span itemprop="author">', auther, '');
          auther := ReplaceRegExpr('</span></div>', auther, '');
          UTF8Delete(MainPage, 1, sp - 1);
          RegEx.InputString := auther;
          RegEx.Expression  := '<a href=.*?>';
          if RegEx.Exec then
          begin
            authurl := RegEx.Match[0];
            authurl := ReplaceRegExpr('<a href="', authurl, '');
            authurl := ReplaceRegExpr('">', authurl, '');
            auther := ReplaceRegExpr('<.*?>', auther, '');
          end;
          auther := ProcTags(auther);
        end;
      end;
      // 前書き部分
      RegEx.Expression := SHEADER;
      RegEx.InputString:= MainPage;
      if RegEx.Exec then
      begin
        sp := RegEx.MatchPos[0] + RegEx.MatchLen[0];
        header := RegEx.Match[0];
        // 前書きの前後のタグを除去する
        header := ReplaceRegExpr('<div class="ss">', header, '');
        header := ReplaceRegExpr('<hr.*?></div>', header, '');
        header := ProcTags(header);
        UTF8Delete(MainPage, 1, sp - 1);
      end;
      // 目次部分
      pn := 1;
      RegEx.Expression := SCONTENT;
      RegEx.InputString:= MainPage;
      if RegEx.Exec then
      begin
        cont := RegEx.Match[0];
        // 作者名の前後のタグを除去する
        cont := ReplaceRegExpr('<div class="ss">', cont, '');
        cont := ReplaceRegExpr('</div>', cont, '');
      end;
      // 目次を取り出す
      aurl := URL.Text;
      if aurl[UTF8Length(aurl)] = '/' then
        UTF8Delete(aurl, UTF8Length(aurl), 1);
      while True do
      begin
        RegEx.Expression := SSECTION;
        RegEx.InputString:= cont;
        // ここでは目次ああるかどうかだけをチェックして各話URLを簡易的に登録していく
        if RegEx.Exec then
        begin
          sc := RegEx.MatchPos[0];
          sl := RegEx.MatchLen[0];
          PageList.Add(aurl + '/' + IntToStr(pn) + '.html');
          Inc(pn);
          UTF8Delete(cont, 1, sc + sl - 1);
        end else
          Break;
      end;
      // タイトル名に"完結"が含まれていなければ先頭に小説の連載状況を追加する
      if UTF8Pos('完結', title) = 0 then
        title := NvStat + title;
      TextPage.Add(title);
      TextPage.Add(auther);
      TextPage.Add(AO_PB2);
      TextPage.Add(AO_KKL + URL.Text + #13#10 + header + #13#10 + AO_KKR);
      TextPage.Add(AO_PB2);

      LogFile.Add(URL.Text);
      LogFile.Add('タイトル：' + title);
      if authurl <> '' then
        LogFile.Add('作者　　：' + auther + '(https:' + authurl + ')')
      else
        LogFile.Add('作者　　：' + auther);
      LogFile.Add('あらすじ：');
      LogFile.Add(header);
      LogFile.Add('');

      // Naro2mobiから呼び出された場合は進捗状況をSendする
      if hWnd <> 0 then
      begin
        conhdl := GetStdHandle(STD_OUTPUT_HANDLE);
        sendstr := title + ',' + auther;
        // 送信する文字列をUTF-16にする
        ws := UTF8ToUTF16(sendstr);
        Cds.dwData := PageList.Count - StartN + 1;
        Cds.cbData := ByteLength(ws) + 2;
        Cds.lpData := PWideChar(ws);
        SendMessage(hWnd, WM_COPYDATA, conhdl, LPARAM(Addr(Cds)));
      end;
    end else
      Status.Caption := 'トップページから情報を取得出来ませんでした.';
  end;
end;

// 小説の連載状況をチェックする
function THameln.GetNovelStatus(MainPage: string): string;
var
  str: string;
begin
  Result := '';
  if MainPage <> '' then
  begin
    // トップページから作品情報ページURLを取得して連載状況を確認する
    RegEx.Expression  := '<li><a href="//syosetu.org/\?mode=ss_detail&nid=.*?">小説情報</a></li>';
    RegEx.InputString := MainPage;
    if RegEx.Exec then
    begin
      str := RegEx.Match[0];
      str := UTF8StringReplace(str, '<li><a href="', '', [rfReplaceAll]);
      str := UTF8StringReplace(str, '">小説情報</a></li>', '', [rfReplaceAll]);
      str := LoadFromHTML('https:' + str);
      if UTF8Pos('連載(完結)', str) > 0 then
        Result := '【完結】'
      else if UTF8Pos('連載(連載中)', str) > 0 then
        Result := '【連載中】';
    end;
  end;
end;

// HTMLページダウンロード
// TEdgeViewで指定したURLからHTMLをロードした結果を受け取る
function THameln.GetHTMLSrc(aURL:string; Mode: integer): string;
var
  cnt: integer;
  tout: boolean;
begin
  Result := '';
  WVMode := Mode;
  Done := False;
  tout := False;
  JsCnt := 0;

  WV2.Navigate(aURL);
  Application.ProcessMessages;
  while WV2.IsNavigating do
  begin
    Sleep(10);
    Application.ProcessMessages;
    if Cancel then
      Break;
  end;
  cnt := 0;
  while not Done do
  begin
    Application.ProcessMessages;
    Sleep(100);
    Inc(cnt);
    if cnt > 50 then
    begin
      tout := True;
      Break;
    end;
  end;
  if not tout then
    Result := Restore2RealChar(TextBuff);
end;

procedure THameln.WV2AfterCreated(Sender: TObject);
begin
  WVWindowParent1.UpdateSize;
end;

procedure THameln.FormCreate(Sender: TObject);
var
  cfg, opt, op, ver: string;
  f: TextFile;
  i: integer;
begin
  WVCreated := False;
  Done      := False;
  TextPage  := TStringList.Create;
  LogFile   := TStringList.Create;
  PageList  := TStringList.Create;
  TitleList := TStringList.Create;
  RegEx     := TRegExpr.Create;
  FmHt      := Height;
  IsCalled  := False;
  FileName  := '';
  hWnd      := 0;

  // 保存されたパラメータを読み込む(IniFilesを用いるほどではないので原始的な方法で)
  // Turbo PASCAL時代に戻った感じ
  cfg := ChangeFileExt(Application.ExeName, '.cfg');
  if FileExists(cfg) then
  begin
    try
      AssignFile(f, cfg);
      Reset(f);
      Readln(f, opt);
      if opt = '' then
        opt := '1';
      if ParamCount = 0 then
      begin
        Readln(f, Opt);
        if Opt = '' then
          Opt := '300';
        Left := StrToInt(Opt);
        Readln(f, Opt);
        if Opt = '' then
          Opt := '200';
        Top := StrToInt(Opt);
      end;
    finally
      CloseFile(f);
    end;
  end;
  // ファイルバージョンをキャプションに表示する
  ver := GetVersionInfo(Application.ExeName);
  Caption := 'ハーメルン小説ダウンローダー [' + ver + ']';

  if ParamCount = 0 then
  begin
    Exit;
  end;
  // オプション引数取得
  for i := 0 to ParamCount - 1 do
  begin
    op := ParamStr(i + 1);
    // Naro2mobiのWindowsハンドル
    if UTF8Pos('-h', op) = 1 then
    begin
      UTF8Delete(op, 1, 2);
      try
        hWnd := StrToInt(op);
      except
        ExitCode := -1;
        Exit;
      end;
    // DL開始ページ番号
    end else if UTF8Pos('-s', op) = 1 then
    begin
      UTF8Delete(op, 1, 2);
      StartPage := op;
      try
        StartN := StrToInt(op);
      except
        ExitCode := -1;
        Exit;
      end;
    // 作品URL
    end else if UTF8Pos('https:', op) = 1 then
    begin
      URLadr := op;
      if UTF8Pos('https://', URLadr) = 0 then
      begin
        MessageDlg('URLが違います.', mtWarning, [mbOK], 0);
      end else begin
        URL.Text := URLadr;
        IsCalled := True;
      end;
    // それ以外であれば保存ファイル名
    end else begin
      FileName := TrimSpace(op);
      if UTF8UpperCase(ExtractFileExt(op)) <> '.TXT' then
        FileName := FileName + '.txt';
    end;
  end;
end;

procedure THameln.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Cancel := True;
end;

procedure THameln.FormShow(Sender: TObject);
begin
  inherited;
  if (URLadr <> '') and (FileName  <> '') then
  begin
    Left   := -1000;
    Top    := 0;
    Height := 1000;
  end;
  if GlobalWebView2Loader.InitializationError then
    showmessage(GlobalWebView2Loader.ErrorMessage)
  else if GlobalWebView2Loader.Initialized then
    WV2.CreateBrowser(WVWindowParent1.Handle)
  else
    Timer1.Enabled := True;
end;

procedure THameln.OCBtnClick(Sender: TObject);
begin
  // 閉じた状態であればブラウザを開く
  if Height = FmHt then
  begin
    Height := 1000;
    if URL.Text = '' then
      WV2.Navigate(NVSITE)
    else
      WV2.Navigate(URL.Text);

    OCBtn.Caption := '▲';
  // ブラウザを閉じる
  end else begin
    Height := FmHt;
    OCBtn.Caption := '▼';
    Sleep(100);
  end;
end;

procedure THameln.StartBtnClick(Sender: TObject);
label
  Quit;
begin
  if UTF8Pos('https://', URL.Text) = 0 then
  begin
    Status.Caption := '状態：URLをセットしてください.';
    Exit;
  end;
  Cancel := False;
  StartBtn.Enabled := False;
  CancelBtn.Enabled := True;
  URL.Enabled := False;
  TextPage.Clear;
  LogFile.Clear;
  PageList.Clear;
  TitleList.Clear;
  NvTitle.Caption := '作品タイトル：';
  StartTime := Now;
  Busy := True;
  PrevURL := '';  // エピソードページを取得出来たか判定するために前後ページのURLを用いる
  NextURL := '';
  // トップページ情報を取得する
  TextBuff := LoadFromHTML(URL.Text);
  if TextBuff <> '' then
  begin
    NvStat := GetNovelStatus(TextBuff);
    ParseChapter(TextBuff);
  end;
  if (TextBuff = '') or ((PageList.Count = 0) and (not NShort)) then
  begin
    LogFile.Add('エラー：トップページ情報を取得出来ませんでした.');
    Cancel := True;
    Status.Caption :='エラー：トップページ情報を取得出来ませんでした.';
    Goto Quit;
  end;

  if (PageList.Count >= StartN) or NShort then
  begin
    if not NShort then                  // 短編でなければ各話情報を取得
    begin
      LoadEachPage;
    // 短編を保存する
    end else begin
      TextPage.WriteBOM := True;      // DelphiとLazarusでデフォルトの定義が違うため明示的に指定する
      LogFile.WriteBOM  := True;
      TextPage.SaveToFile(Filename, TEncoding.UTF8);
      LogFile.SaveToFile(ChangeFileExt(FileName, '.log'), TEncoding.UTF8);
      Status.Caption := Status.Caption + '・・完了';
      Goto Quit;
    end;
    if FileName <> '' then
    begin
      try
        TextPage.WriteBOM := True;      // DelphiとLazarusでデフォルトの定義が違うため明示的に指定する
        LogFile.WriteBOM  := True;
        TextPage.SaveToFile(Filename, TEncoding.UTF8);
        LogFile.SaveToFile(ChangeFileExt(FileName, '.log'), TEncoding.UTF8);
        if Cancel then
          Status.Caption := Status.Caption + '・・中止'
        else
          Status.Caption := Status.Caption + '・・完了';
      except
        ExitCode := -1;
        Status.Caption := 'ファイルの保存に失敗しました.';
      end;
    end else
      Status.Caption := '小説情報を取得できませんでした.';
  end else begin
    Status.Caption := '小説情報を取得できませんでした.';
    ExitCode := -1;
  end;
  Elapsed.Caption := '経過時間：' + FormatDateTime('nn:ss', Now - StartTime);
Quit:
  URL.Enabled := True;
  StartBtn.Enabled := True;
  CancelBtn.Enabled := False;
  Busy := False;
end;

procedure THameln.FormActivate(Sender: TObject);
begin
  if (URLadr <> '') {and (FileName  <> '')} then
  begin
    while Timer1.Enabled do
      Sleep(100);
    StartBtnClick(nil);
    Close;
  end;
end;

procedure THameln.FormClose(Sender: TObject; var Action: TCloseAction);
var
  cfg, opt: string;
  f: TextFile;
begin
  // パラメータを保存する(IniFilesを用いるほどではないので原始的な方法で)
  cfg := ChangeFileExt(Application.ExeName, '.cfg');
  try
    AssignFile(f, cfg);
    opt := '';
    Rewrite(f);
    Writeln(f, opt);
    if not isCalled then
    begin
      if Left > 0 then
        opt := IntToStr(Left)
      else
        opt := '300';
      Writeln(f, opt);
      if Top > 0 then
        opt := IntToStr(Top)
      else
        opt := '200';
      Writeln(f, opt);
    end else begin
      opt := '300';
      Writeln(f, opt);
      opt := '200';
      Writeln(f, opt);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure THameln.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Busy then
    CanClose := False
  else begin
    WV2.Stop;
    TitleList.Free;
    PageList.Free;
    TextPage.Free;
    LogFile.Free;
    FreeAndNil(RegEx);
  end;
end;

procedure THameln.CancelBtnClick(Sender: TObject);
begin
  Cancel := True;
end;

procedure THameln.WV2ExecuteScriptCompleted(Sender: TObject;
  aErrorCode: HRESULT; const aResultObjectAsJson: wvstring;
  aExecutionID: integer);
begin
  if aResultObjectAsJson <> '' then
  begin
    WV2.RetrieveHTML;
  end else begin
    WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)');
  end;
end;

procedure THameln.WV2InitializationError(Sender: TObject;
  aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  showmessage(string(aErrorMessage));
end;

procedure THameln.WV2NavigationCompleted(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2NavigationCompletedEventArgs);
begin
  if not Done then
    WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)');
end;

// JavaScriptの実行が終わるとコールされる
procedure THameln.WV2RetrieveHTMLCompleted(Sender: TObject;
  aResult: boolean; const aHTML: wvstring);
var
  src: string;
begin
  if aHTML <> '' then
  begin
    src := aHTML;

    if not Done then
    begin
      if WVMode = 0 then
      begin
        if Pos('var script3 = document.createElement( ''script'' );', src) = 0 then
          WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)')
        else begin
          TextBuff := src;
          Done := True;
        end;
      end else begin
        if Pos('</script></body></html>', src) = 0 then
          WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)')
        else begin
          TextBuff := src;
          Done := True;
        end;
    end;
    end;
  end else begin
    SetActiveWindow(Handle);
    WVWindowParent1.SetFocus;
    WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)');
  end;
end;

procedure THameln.WV2SourceChanged(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2SourceChangedEventArgs);
begin
  WV2.ExecuteScript('encodeURI(document.documentElement.outerHTML)');
end;

procedure THameln.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if GlobalWebView2Loader.Initialized then
    WV2.CreateBrowser(WVWindowParent1.Handle)
  else
    Timer1.Enabled := True;
end;

initialization
  GlobalWebView2Loader := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := wvstring(ExtractFileDir(Application.ExeName));
  GlobalWebView2Loader.StartWebView2;


end.

