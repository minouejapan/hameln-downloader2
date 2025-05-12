(*
  Web小説作品ダウンローダー共通ライブラリ nvdllib.pas

  これらの処理にバグがあった場合、各ダウンローダーを個別に修正する必要があり
  バグ修正忘れが出たりしていたため、共有ユニットとすることで一回の修正で済ま
  せるようにした
  このことによって各ダウンローダーのソースコードがコンパクトになった


	1.1	 2025/03/03 PathFilterに処理漏れの"<>を追加した
	1.0  2024/08/19 初版

*)

unit nvdllib;

{$IFDEF FPC}
  {$MODE Delphi}
  {$codepage utf8}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, LazUTF8,
{$ELSE}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.StdCtrls,
  Lazutf8wrap,
{$ENDIF}
  RegExpr;


function PosN(SubStr, Str: string; StartPos: integer): integer;
function ElimCRLF(Base: string): string;
function TrimSpace(Base: string): string;
function ChangeBRK(Base: string): string;
function ChangeAozoraTag(Base: string): string;
function Restore2RealChar(Base: string): string;
function PathFilter(PassName: string): string;
function GetVersionInfo(const AFileName:string): string;

const
  // 青空文庫形式
  AO_RBI = '｜';							// ルビのかかり始め(必ずある訳ではない)
  AO_RBL = '《';              // ルビ始め
  AO_RBR = '》';              // ルビ終わり
  AO_TGI = '［＃';            // 青空文庫書式設定開始
  AO_TGO = '］';              //        〃       終了
  AO_CPI = '［＃「';          // 見出しの開始
  AO_CPT = '」は大見出し］';	// 章
  AO_SEC = '」は中見出し］';  // 話
  AO_PRT = '」は小見出し］';

  AO_CPB = '［＃大見出し］';        // 2022/12/28 こちらのタグに変更
  AO_CPE = '［＃大見出し終わり］';
  AO_SEB = '［＃中見出し］';
  AO_SEE = '［＃中見出し終わり］';
  AO_PRB = '［＃小見出し］';
  AO_PRE = '［＃小見出し終わり］';

  AO_DAI = '［＃ここから';		// ブロックの字下げ開始
  AO_DAO = '［＃ここで字下げ終わり］';
  AO_DAN = '字下げ］';
  AO_PGB = '［＃改丁］';			// 改丁と会ページはページ送りなのか見開き分の
  AO_PB2 = '［＃改ページ］';	// 送りかの違いがあるがどちらもページ送りとする
  AO_BED = '［＃本文終わり］';// 本文終わり(Body End)
  AO_SM1 = '」に傍点］';			// ルビ傍点
  AO_SM2 = '」に丸傍点］';		// ルビ傍点 どちらもsesami_dotで扱う
  AO_EMB = '［＃丸傍点］';        // 横転開始
  AO_EME = '［＃丸傍点終わり］';  // 傍点終わり
  AO_KKL = '［＃ここから罫囲み］' ;     // 本来は罫囲み範囲の指定だが、前書きや後書き等を
  AO_KKR = '［＃ここで罫囲み終わり］';  // 一段小さい文字で表記するために使用する
  AO_END = '底本：';          // ページフッダ開始（必ずあるとは限らない）
  AO_PIB = '［＃リンクの図（';          // 画像埋め込み
  AO_PIE = '）入る］';        // 画像埋め込み終わり
  AO_CVB = '［＃表紙の図（';  // 表紙画像指定
  AO_CVE = '）入る］';        // 終わり
  AO_RAB = '［＃右寄せ］';
  AO_RAE = '［＃右寄せ終わり］';
  AO_HR  = '［＃水平線］';    // 水平線<hr />


implementation

// 自分自身のファイルバージョンを取得する
function GetVersionInfo(const AFileName:string): string;
var
  InfoSize:DWORD;
  SFI:string;
  Buf,Trans,Value:Pointer;
begin
  Result := '';
  if AFileName = '' then Exit;
  InfoSize := GetFileVersionInfoSize(PChar(AFileName),InfoSize);
  if InfoSize <> 0 then
  begin
    GetMem(Buf,InfoSize);
    try
      if GetFileVersionInfo(PChar(AFileName),0,InfoSize,Buf) then
      begin
        if VerQueryValue(Buf,'\VarFileInfo\Translation',Trans,InfoSize) then
        begin
          SFI := Format('\StringFileInfo\%4.4x%4.4x\FileVersion',
                 [LOWORD(DWORD(Trans^)),HIWORD(DWORD(Trans^))]);
          if VerQueryValue(Buf,PChar(SFI),Value,InfoSize) then
            Result := PChar(Value)
          else Result := 'UnKnown';
        end;
      end;
    finally
      FreeMem(Buf);
    end;
  end;
end;

// Delphi XE2ではPos関数に検索開始位置を指定出来ないための代替え
function PosN(SubStr, Str: string; StartPos: integer): integer;
var
  tmp: string;
  p: integer;
begin
  tmp := UTF8Copy(Str, StartPos, UTF8Length(Str));
  p := UTF8Pos(SubStr, tmp);
  if p > 0 then
    Result := p + StartPos - 1  // 1ベーススタートのため1を引く
  else
    Result := 0;
end;

// HTMLテキスト内のCR/LF(#$0D#$0A)を除去しTAB文字を半角スペースに変換する
function ElimCRLF(Base: string): string;
var
  tmp: string;
begin
  tmp := UTF8StringReplace(Base, #$0D, '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  #$0A, '', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  #$09, ' ', [rfReplaceAll]);
  Result := tmp;
end;

// 指定された文字列の前と後のスペース(' '/'　'/#$20/#$09/#$0D/#$0A)を除去する
// '　　文字　列　　' → '文字　列'
function TrimSpace(Base: string): string;
var
  p: integer;
  c: string;
begin
  while True do
  begin
    if UTF8Length(Base) = 0 then
      Break;
    c := UTF8Copy(Base, 1, 1);
    if UTF8Pos(c, ' 　'#$09#$0D#$0A) > 0 then
      UTF8Delete(Base, 1, 1)
    else
      Break;
  end;
  while True do
  begin
    p := UTF8Length(Base);
    if p = 0 then
      Break;
    c := UTF8Copy(Base, p, 1);
    if UTF8Pos(c, ' 　'#$09#$0D#$0A) > 0 then
      UTF8Delete(Base, p, 1)
    else
      Break;
  end;
  Result := Base;
end;

// 本文の改行タグを削除する
function ChangeBRK(Base: string): string;
var
	tmp: string;
begin
  tmp    := UTF8StringReplace(Base, '<br />', '', [rfReplaceAll]);
  Result := UTF8StringReplace(tmp, '<br>', '', [rfReplaceAll]);
end;

// 本文の青空文庫ルビ指定に用いられる文字があった場合誤作動しないように青空文庫代替表記に変換する(2022/3/25)
function ChangeAozoraTag(Base: string): string;
var
  tmp: string;
begin
  tmp := UTF8StringReplace(Base, '<rp>《</rp>', '<rp>(</rp>', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '<rp>》</rp>', '<rp>)</rp>', [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp, '《', '※［＃始め二重山括弧、1-1-52］',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '》', '※［＃終わり二重山括弧、1-1-53］',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '｜', '※［＃縦線、1-1-35］',   [rfReplaceAll]);
  Result := tmp;
end;

// HTML特殊文字の処理
// 1)エスケープ文字列 → 実際の文字
// 2)&#x????; → 通常の文字
function Restore2RealChar(Base: string): string;
var
  tmp, cd: string;
  w, mp, ml: integer;
  ch: Char;
  wch: WideChar;
  r: TRegExpr;
begin
  // エスケープされた文字
  tmp := UTF8StringReplace(Base, '&lt;',      '<',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&gt;',      '>',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&quot;',    '"',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&nbsp;',    ' ',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&yen;',     '\',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&brvbar;',  '|',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&copy;',    '©',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&amp;',     '&',  [rfReplaceAll]);
  // &#????;にエンコードされた文字をデコードする(2023/3/19)
  // 正規表現による処理に変更した(2024/3/9)
  r := TRegExpr.Create;
  try
    r.Expression  := '&#.*?;';
    r.InputString := tmp;
    if r.Exec then
    begin
      repeat
        cd := r.Match[0];
        mp := r.MatchPos[0];
        ml := r.MatchLen[0];
        UTF8Delete(tmp, mp, ml);
        UTF8Delete(cd, 1, 2);           // &#を削除する
        UTF8Delete(cd, UTF8Length(cd), 1);  // 最後の;を削除する
        if cd[1] = 'x' then         // 先頭が16進数を表すxであればDelphiの16進数接頭文字$に変更する
          cd[1] := '$';
        try
          w := StrToInt(cd);
          ch := Char(w);
        except
          ch := '?';
        end;
        UTF8Insert(ch, tmp, mp);
        r.InputString := tmp;
      until not r.Exec;
    end;
    // unicodeエスケープ文字(\uxxxx)
    r.Expression  := '\\u[0-9A-Fa-f]{4}';
    r.InputString := tmp;
    if r.Exec then
    begin
      repeat
        UTF8Delete(tmp, r.MatchPos[0], r.MatchLen[0]);
        cd := r.Match[0];
        UTF8Delete(cd, 1, 2);   // \uを削除する
        UTF8Insert('$', cd, 1); // 先頭に16進数接頭文字$を追加する
        try
          w := StrToInt(cd);
          wch := WideChar(w);
        except
          wch := '？';
        end;
        UTF8Insert(UTF16ToUTF8(wch), tmp, r.MatchPos[0]);
      until not r.ExecNext;
    end;
  finally
    r.Free;
  end;
  Result := tmp;
end;

// タイトル名にファイル名として使用出来ない文字を'-'に置換する
// Lazarus(FPC)とDelphiで文字コード変換方法が異なるためコンパイル環境で
// 変換処理を切り替える
function PathFilter(PassName: string): string;
var
  path: string;
  tmp: WideString;
begin
  // ファイル名を一旦ShiftJISに変換して再度Unicode化することでShiftJISで使用
  // 出来ない文字を除去する
{$IFDEF FPC}
  tmp  := UTF8ToUTF16(PassName);
  path := UTF16ToUTF8(tmp);      // これでUTF-8依存文字は??に置き換わる
{$ELSE}
  tmp  := WideString(PassName);
	path := string(tmp);
{$ENDIF}
  // ファイル名として使用できない文字を'-'に置換する
  path := ReplaceRegExpr('[\\/:;\*\?\+,."<>|\.\t ]', path, '-');

  Result := path;
end;


end.
