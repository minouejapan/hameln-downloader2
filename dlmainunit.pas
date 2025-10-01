(*
  ハーメルン小説ダウンローダー

  2.1 2025/10/01  短編の処理がおかしかった不具合を修正した
  2.0 2025/09/27  HTML構文解析を力技からSHParser(SimpleHTMLParser)による解析に変更した
                  本文の一部を取得出来ない場合があった不具合を修正した
                  前書き・後書きにある脚注を取得出来ていなかった不具合を修正した
                  ソースコードの視認性向上のため要素抽出用判定タグを定数から埋め込みに変更した
  1.9 2025/09/06  脚注が複数ある場合でもすべて取得出来るように処理を変更した
                  前書きの青空文庫タグ処理を修正した
  1.8 2025/08/31  脚注の処理を追加した
                  挿絵処理がうまく出来ない場合があった不具合を修正した
  1.7 2025/05/13  挿し絵以外のリンクも挿し絵として誤変換する対策として挿絵リンク検索パターンを厳格化した
  1.6 2025/03/13  作品ステータスが連載(未完)と短編の場合に連載状況を取得出来なかった不具合を臭瀬下
                  保存ファイル名にも連載状況を付加するようにした
  1.5 2025/02/20  Naro2mobiから呼び出すと正常にダウンロード出来ない場合がある不具合を修正した
  1.41     02/13  短編のあらすじと本文の前書きを連結していたのを本文の前に移動した
  1.4 2025/02/12  単体で起動した際に連続で作品をDLすると次の作品にも前のファイル名が使わ
                  れる不具合を修正した
                  短編に前書きがある場合抽出されなかった不具合を修正した
                  実行環境によってはNaro2mobiとのSendMessageハンドシェイクが確立されず、
                  フリーズしたような状態になるためSendMessageの後にProcessMessageを入れる
                  ようしにた
  1.32     02/11  タイトルに《》があると青空文庫タグに変換されて見づらくなるため【】に
                  置換するようにした
  1.31     02/08  短編ではないが１話しかない作品をダウンロード出来なかった不具合を修正した
                  Naro2mobiからの呼び出し時に短編のダウンロードが停止する不具合を修正した
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
  RegExpr, SHParser, UniHTML,
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
    WVMode, PageN: integer;
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
  VERSION  = 'ver2.0 2025/9/26';
  NVSITE   = 'https://syosetu.org';

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
  SaveName,
  StartPage: string;
  RegEx: TRegExpr;
  hWnd: THandle;
  CDS: TCopyDataStruct;
  StartN: integer;


// HTML内のJavaScript実行が完了したことを判定する
function IsScriptDone(Src: string): boolean;
begin
  Result := Src <> '';
end;

// 本文の文字装飾を除去する
function EliminateDeco(Base: string): string;
var
  tmp: string;
begin
  tmp := Base;
  tmp := ReplaceRegExpr('<span .*?>', tmp, '');
  tmp := ReplaceRegExpr('</span>', tmp, '');
  tmp := ReplaceRegExpr('<table .*?>', tmp, '');
  tmp := ReplaceRegExpr('<th .*?>', tmp, '');
  tmp := ReplaceRegExpr('<tr .*?>', tmp, '');
  tmp := ReplaceRegExpr('<td .*?>', tmp, '');
  tmp := ReplaceRegExpr('<tr>', tmp, '');
  tmp := ReplaceRegExpr('</tr>', tmp, '');

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
    UTF8Delete(Base, RegEx.MatchPos[0], RegEx.MatchLen[0]);      // 傍点範囲の文字列を削除
    UTF8Insert(AO_EMB + tmp + AO_EME, Base, RegEx.MatchPos[0]);  // 変換後の文字列を挿入
    RegEx.InputString := Base;
  end;

  Result := Base;
end;

// 本文のルビタグを青空文庫形式に変換する
function ChangeRuby(Base: string): string;
var
  tmp: string;
begin
  tmp := Base;
  // <rp>タグを除去
  tmp := UTF8StringReplace(tmp, '<rp>(</rp>', '', [rfReplaceAll]);
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

// 埋め込まれた画像リンク/URLリンクを青空文庫形式に変換する
function ChangeImage(Base: string): string;
var
  str, tmp: string;
begin
  tmp := Base;
  RegEx.Expression  := '<a href=".{6,60}" alt="挿絵" name=.*?>【挿絵表示】</a>';
  RegEx.InputString := tmp;
  while RegEx.Exec do
  begin
    str := RegEx.Match[0];
    str := ReplaceRegExpr('<a href="', str, AO_PIB);
    str := ReplaceRegExpr('" alt="挿絵" name=.*?>【挿絵表示】</a>', str, AO_PIE);
    tmp := ReplaceRegExpr(RegEx.Match[0], tmp, str);
    RegEx.Expression  := '<a href=".{6,60}" alt="挿絵" name=.*?>【挿絵表示】</a>';
    RegEx.InputString := tmp;
  end;
  Result := tmp;
end;

// 本文のリンクタグを除去・置換する
function Delete_tags(Base: string): string;
var
  tmp: string;
begin
  tmp := Base;
  // script
  tmp := ReplaceRegExpr('<script type="text/javascript">[\s\S]*', tmp, '');
  tmp := ReplaceRegExpr('<script[\s\S]*?/script>', tmp, '');
  // 本文中の余計なタグを除去する
  tmp := ReplaceRegExpr('<style>[\s\S]*?</style>', tmp, '');
  tmp := ReplaceRegExpr('<div id="maegaki_open">[\s\S]*', tmp, '');
  tmp := ReplaceRegExpr('<div id="atogaki_open">[\s\S]*', tmp, '');
  // 水平線に置換する
  tmp := ReplaceRegExpr('<hr>', tmp, #13#10 + AO_HR + #13#10);
  tmp := ReplaceRegExpr('<div class="footnote">', tmp, #13#10 + AO_HR + #13#10);
  tmp := ReplaceRegExpr('</p>', tmp, #13#10);

  Result := tmp;
end;

// タグ類処理(順番を間違えると誤作動する)
function ProcTags(Base: string): string;
var
  tmp: string;
begin
  tmp := Base;
  tmp := ChangeAozoraTag(tmp);
  tmp := Restore2RealChar(tmp);
  tmp := ChangeRuby(tmp);
  tmp := ChangeBouten(tmp);
  tmp := ChangeImage(tmp);
  tmp := EliminateDeco(tmp);
  tmp := Delete_tags(tmp);
  Result := tmp;
end;

// 小説本文をHTMLから抜き出して整形する
function THameln.ParsePage(Page: string): Boolean;
var
  sp, mp, ml: integer;
  header, footer, chapt, sect, body, htmlsrc: string;
  shp: TSHParser;
begin
  Result := True;
  htmlsrc := Page;
  // 検索速度を上げるため<body>～</body>部分だけを切り出す
  RegEx.InputString := Page;
  RegEx.Expression  := '<body[\s\S]*?</body>';
  if RegEx.Exec then
    htmlsrc := RegEx.Match[0];
  htmlsrc := ReplaceRegExpr('<script[\s\S]*?</script>', htmlsrc, '');
  body := ''; header := ''; footer := ''; chapt := ''; sect := '';
  shp := TSHParser.Create(htmlsrc);
  //MessageBoxW(Handle, PWideChar('ノード: ' + IntToStr(shp.NodeComp)), '', 0);
  try
    // hameln専用変換フィルタを登録する
    shp.OnBeforeGetText:= @ProcTags;
    // 本文
    body   := shp.Find('div', 'id', 'honbun');
    // 前書き
    header := shp.FindRegex('<div id="maegaki">', '<div id="maegaki_open">');
    // 後書き
    footer := shp.FindRegex('<div id="atogaki">', '<div id="atogaki_open">');
    // 章・話タイトル
    sect := shp.FindRegex('<div style=.*?>\d{1,5} / \d{1,5}</div><span style="font-size:120%">', '</span>');
    if sect = '' then  // シンプルなタグ構成の場合がある
      sect := shp.FindRegex('</div><span style="font-size:120%">', '</span><span');
    if sect <> '' then
    begin
      // 章タイトルが含まれていれば分離する
      sp := UTF8Pos(#13#10, sect);
      if sp > 0 then
      begin
        chapt := Trim(UTF8Copy(sect, 1, sp - 1));
        sect  := Trim(UTF8Copy(sect, sp + 2, Length(sect)));
      end else
        sect := Trim(sect);
    end else
      Result := False;
   finally
    shp.Free;
  end;

  if chapt <> '' then
    TextPage.Add(AO_CPB + chapt + AO_CPE);
  TextPage.Add(AO_SEB + sect + AO_SEE);
  if header <> '' then
    TextPage.Add(AO_KKL + #13#10 + header + #13#10 + AO_KKR + #13#10);
  if body <> '' then
    TextPage.Add(body)
  else
    TextPage.Add('★HTMLページ読み込みエラー');
  if footer <> '' then
    TextPage.Add(AO_KKL + #13#10 + footer + #13#10 + AO_KKR + #13#10);
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
      begin
        SendMessage(hWnd, WM_DLINFO, n, sc{1});
        Application.ProcessMessages;
      end;
      if Cancel then
        Break;
      // サーバーへの負担を減らすため1秒のインターバルを入れる
      Sleep(500);   // Sleep処理を削除したり、この数値を小さくすることを禁止します
    end;
    Application.ProcessMessages;
    Inc(i);
    Inc(n);
  end;
end;

// 短編専用処理
procedure THameln.ParseShort(Page: string);
var
  shp: TSHParser;
  title, auther, authurl,
  htmlsrc, sendstr, sshead, sect,
  body, header, footer: string;
  ws: WideString;
  fl: TFoundList;
begin
  htmlsrc := Page;
  // HTMLソースから必要な部分だけを切り出す
  shp := TSHParser.Create(Page);
  try
    htmlsrc := shp.Find('div', 'class', 'ss', False);
    htmlsrc := ReplaceRegExpr('<script[\s\S]*?</script>', htmlsrc, '');
  finally
    shp.Free;
  end;
  title := ''; auther := ''; authurl := '';
  body := ''; sshead := ''; header := ''; footer := ''; sect := '';
  shp := TSHParser.Create(htmlsrc);
  try
    // hameln専用変換フィルタを登録する
    shp.OnBeforeGetText:= @ProcTags;
    // タイトル
    title := '【短編】' + shp.FindRegex('<span .*?><a href=.*?>', '</a></span>');
     // ファイル名を準備する
    NvTitle.Caption := '作品タイトル：' + title;
    if FileName = '' then
      FileName := Path + PathFilter(title) + '.txt';
    // 作者
    auther := shp.FindRegex('作：<a href=.*?>', '</a>');
    authurl := shp.FindRegex('作：<a href="', '">.*?</a>');
    if authurl <> '' then
      authurl := 'https:' + authurl;
    // 短編前書き
    sshead := shp.FindRegex('<div class="ss">', '<hr style="margin:20px 0px;">');
    // 話タイトル
    sect := shp.FindRegEx('/ \d{1,6}</div>[\s\S]*?<span style="font-size:120%">', '</span>');
    // 本文
    body   := shp.Find('div', 'id', 'honbun');
    // 前書き
    header := shp.FindRegex('<div id="maegaki">', '<div id="maegaki_open">');
    // 後書き
    footer := shp.FindRegex('<div id="atogaki">', '<div id="atogaki_open">');
   finally
    shp.Free;
  end;

  // Naro2mobiから呼び出された場合は進捗状況をSendする
  if hWnd <> 0 then
  begin
    sendstr := title + ',' + auther;
    // 送信する文字列をUTF-16にする
    ws := UTF8ToUTF16(sendstr);
    Cds.dwData := PageList.Count - StartN + 1;
    Cds.cbData := ByteLength(ws) + 2;
    Cds.lpData := PWideChar(ws);
    SendMessage(hWnd, WM_COPYDATA, Handle, LPARAM(Addr(Cds)));
    Application.ProcessMessages;
  end;

  TextPage.Add(title);
  TextPage.Add(auther);
  TextPage.Add(AO_PB2);
  if sshead <> '' then
  begin
    TextPage.Add(AO_KKL + #13#10 + sshead + #13#10 + AO_KKR + #13#10);
    TextPage.Add(AO_PB2);
  end;
  TextPage.Add(AO_SEB + sect + AO_SEE);
  if HEADER <> '' then
    TextPage.Add(AO_KKL + #13#10 + HEADER + #13#10 + AO_KKR + #13#10);
  TextPage.Add(body);
  if footer <> '' then
    TextPage.Add(AO_KKL + #13#10 + footer + #13#10 + AO_KKR + #13#10);
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
  shp: TSHParser;
  i: integer;
  htmlsrc, title, auther, authurl,
  header, sendstr, aurl: string;
  ws: WideString;
begin
  title := ''; auther := ''; authurl := ''; header := '';
  aurl := URL.Text;
  if aurl[UTF8Length(aurl)] = '/' then
    UTF8Delete(aurl, UTF8Length(aurl), 1);
  // 短編は連載状況が短編でも複数ページの作品があるためページ数でチェックする
  RegEx.Expression  := '<div style=.*?>1 / 1</div>';
  RegEx.InputString := MainPage;
  NShort := RegEx.Exec;
  // 短編専用処理
  if NShort then
  begin
    ParseShort(MainPage);
  // そうでなければ連載作品として処理
  end else begin
    // HTMLソースから余分な情報を削除する
    htmlsrc := MainPage;
    RegEx.InputString:= MainPage;
    RegEx.Expression := '<body[\s\S]*?</body>';
    if RegEx.Exec then
      htmlsrc := RegEx.Match[0];
    htmlsrc := ReplaceRegExpr('<script[\s\S]*?</script>', htmlsrc, '');

    shp := TSHParser.Create(htmlsrc);
    try
      // タイトル名
      title := shp.FindRegex('<span .*?itemprop="name">', '</span>');
      // タイトル名に"完結"が含まれていなければ先頭に小説の連載状況を追加する
      if UTF8Pos('完結', title) = 0 then
        title := NvStat + title;
      NvTitle.Caption := '作品タイトル：' + title;
      // ファイル名を準備する
      if FileName = '' then
        FileName := Path + PathFilter(title) + '.txt';
      // 作者名
      auther := shp.FindRegex('<span itemprop="author"><a href=.*?>', '</a></span>');
      if auther = '' then
        auther := shp.FindRegex('<span itemprop="author">', '</span>')
      else begin// 作者URLあり
        authurl := shp.FindRegex('<span itemprop="author"><a href="', '">.*?</a></span>');
        if authurl <> '' then
          authurl := 'https:' + authurl;
      end;
      // 前書き
      header := shp.FindRegex('</div><div class="ss">', '<hr style="margin:20px 0px;"></div>');
      //目次
      for i := 1 to PageN do
        PageList.Add(aurl + '/' + IntToStr(i) + '.html');

      if PageN = 0 then
        Status.Caption := 'トップページから情報を取得出来ませんでした.'
      else begin
        TextPage.Add(title);
        TextPage.Add(auther);
        TextPage.Add(AO_PB2);
        TextPage.Add(AO_KKL + #13#10 + header + #13#10 + AO_KKR + #13#10);
        TextPage.Add(AO_PB2);

        LogFile.Add(URL.Text);
        LogFile.Add('タイトル：' + title);
        if authurl <> '' then
          LogFile.Add('作者  ：' + auther + '(https:' + authurl + ')')
        else
          LogFile.Add('作者  ：' + auther);
        LogFile.Add('あらすじ：');
        LogFile.Add(header);
        LogFile.Add('');

        // Naro2mobiから呼び出された場合は進捗状況をSendする
        if hWnd <> 0 then
        begin
          sendstr := title + ',' + auther;
          // 送信する文字列をUTF-16にする
          ws := UTF8ToUTF16(sendstr);
          Cds.dwData := PageList.Count - StartN + 1;
          Cds.cbData := ByteLength(ws) + 2;
          Cds.lpData := PWideChar(ws);
          SendMessage(hWnd, WM_COPYDATA, Handle, LPARAM(Addr(Cds)));
          Application.ProcessMessages;
        end;
      end;
    finally
     shp.Free;
    end;
  end;
end;

// 小説の連載状況をチェックする
function THameln.GetNovelStatus(MainPage: string): string;
var
  str, pns: string;
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
      str := GetHTML('https:' + str, 'over18', 'off');
      if UTF8Pos('連載(完結)', str) > 0 then
        Result := '【完結】'
      else if UTF8Pos('連載(連載中)', str) > 0 then
        Result := '【連載中】'
      else if UTF8Pos('連載(未完)', str) > 0 then
        Result := '【連載中】'
      else if UTF8Pos('短編', str) > 0 then
        Result := '【短編】';
      // 作品話数
      RegEx.InputString := str;
      RegEx.Expression  := '<td class="label">話数</td><td.*?>.*? \d{1,5}話</td>';
      if RegEx.Exec then
      begin
        pns := RegEx.Match[0];
        pns := ReplaceRegExpr('話</td>', ReplaceRegExpr('<td class="label">話数</td><td.*?>.*? ', pns, ''), '');
        try
          PageN := StrToInt(pns);
        except
          PageN := 0;
        end;
      end;
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
  SaveName  := '';
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
      SaveName := TrimSpace(op);
      if UTF8UpperCase(ExtractFileExt(op)) <> '.TXT' then
        SaveName := SaveName + '.txt';
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

  if (URLadr <> '') and (SaveName  <> '') then
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
  if Height < 200 {= FmHt} then
  begin
    FmHt := Height;
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
  Status.Caption  := '状態';
  StartTime := Now;
  Busy := True;
  if SaveName <> '' then
    FileName := SaveName
  else
    FileName := '';
  PrevURL := '';  // エピソードページを取得出来たか判定するために前後ページのURLを用いる
  NextURL := '';
  // トップページ情報を取得する
  TextBuff := GetHTML(URL.Text, 'over18', 'off');
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
    // Windowの生成が完了しないうちにダウンロードを開始するとNaro2mobiとのメッセージ
    // ハンドリングがうまくいかず処理が不完全になる場合があるため1.5秒待つ
    Sleep(1500);
    Application.ProcessMessages;
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
    src := string(aHTML);

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

