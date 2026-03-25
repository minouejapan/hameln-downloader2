# hameln-downloader2
ハーメルン小説テキストダウンローダー新版(Windowsアプリ版)

コンソール版hamelndl.exe ver1.1.0.0の後継バージョンになります（hamelndl.exeは今後は更新しません）。


### 動作環境
Windows10/11上で動作します(Windows11を推奨)。

### 実行ファイルの作り方

Lazarus (3.0以降)のプロジェクトを開くからhamelndlw.lpiを開いてビルドしてください。<br>
尚、ビルドするためにはTRegExprとWebView4Delphi、ShimpleHTMLParserが必要です。<br>
+ TRegExprは https://github.com/andgineer/TRegExpr から取得してください。その上でsrcフォルダー内のregexpr.pas、regexpr_compilers.inc、regexpr_unicodedata.pasの3つのファイルをライブラリパスが通ったフォルダもしくはhamelndlwプロジェクトソースファイルと同じフォルダにコピーして下さい。
+ WebView4Delphiは https://github.com/salvadordf/WebView4Delphi から取得して、パッケージファイル(.lpk)を開くからインストールして下さい。
+ ShimpleHTMLParserはhttps://github.com/minouejapan/SimpleHTMLParserから取得して下さい。

#### バージョン情報を編集したい場合
　verinfow.rcファイルをテキストファイルとして開いて編集してください。尚、編集後は文字コードをShift-JISとして保存する必要があります。<br>
　編集後はコマンドラインから、rc verinfo.rcを実行すればバージョン情報リソースファイルverinfo.resが作成されます。rc.exeはDelphiやVisual Studioをインストールしていればパスが通たフォルダー内に存在しているはずです。<br>


### 使い方
ファイルエクスプローラー等からhamelndlw.exeを開き小説TOPページURLにダウンロードしたいハーメルン小説トップページのURLを入力してダウンロードボタンをクリックします。正常に実行されればhamelndlw.exeがあるフォルダにダウンロードした小説が青空文庫形式のテキストファイルで保存されます。
![hamelndlw](https://github.com/user-attachments/assets/bfb6ce3e-51d8-42e6-a69d-fc21af4c8880)

### 注意事項(2026/3/25追記)
ハーメルンへのアクセスが人間かどうかを検証するツールが導入されたとの情報があります。hameldlwによる作品のダウンロードが人間によるアクセスではないと判定された場合、アクセスが拒否されてダウンロードが出来なくなる可能性があるようです。
ver2.5で追加した機能である「追加インターバル」に大きな値を指定することで判定が緩和される可能性はあるものの、この方法で対処できるかどうかは不明です。もしハーメルンからアクセス拒否された場合は、以降hamelndlwの使用を控えることをお勧めします。


### 禁止事項
1. hamelndlを用いてWeb小説サイトからダウンロードしたテキストファイルの第三者への販売や不特定多数への配信。 
2. ダウンロードしたオリジナル作品を著作者の了解なく加工（文章の流用や作品の翻訳等）しての再公開。 
3. その他、著作者の権利を踏みにじるような行為。 
4. ソースコード中のSleep(1000)を削除または数値を小さくしてダウンロードを高速化し、ダウンロード時にhamelnサーバーに負荷をかける行為。


### ライセンス
MIT
