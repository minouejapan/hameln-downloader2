# hameln-downloader2
ハーメルン小説テキストダウンローダー新版(Windowsアプリ版)

コンソール版hamelndl.exe ver1.1.0.0の後継バージョンになります（hamelndl.exeは今後は更新しません）。


### 動作環境
Windows10/11上で動作します。

### 実行ファイルの作り方

Lazarus (3.0以降)のプロジェクトを開くからhamelndlw.lpiを開いてビルドしてください。

尚、ビルドするためにはTRegExprとWebView4Delphiが必要です。

TRegExprは https://github.com/andgineer/TRegExpr から取得してください。その上でsrcフォルダー内のregexpr.pas、regexpr_compilers.inc、regexpr_unicodedata.pasの3つのファイルをkakuyomudlプロジェクトソースファイルと同じフォルダにコピーして下さい。

WebView4Delphiは https://github.com/salvadordf/WebView4Delphi から取得して、パッケージファイル(.lpk)を開くからインストールして下さい。


#### バージョン情報を編集したい場合
　verinfow.rcファイルをテキストファイルとして開いて編集してください。尚、編集後は文字コードをShift-JISとして保存する必要があります。

　編集後はコマンドラインから、rc verinfo.rcを実行すればバージョン情報リソースファイルverinfo.resが作成されます。rc.exeはDelphiやVisual Studioをインストールしていればパスが通たフォルダー内に存在しているはずです。


### 使い方
ファイルエクスプローラー等からhamelndlw.exeを開き小説TOPページURLにダウンロードしたいハーメルン小説トップページのURLを入力してダウンロードボタンをクリックします。正常に実行されればhamelndlw.exeがあるフォルダにダウンロードした小説が青空文庫形式のテキストファイルで保存されます。
![hamelndlw](https://github.com/user-attachments/assets/bfb6ce3e-51d8-42e6-a69d-fc21af4c8880)


尚、R18作品をダウンロードするためにはハーメルンサイトへのCookie登録が必要です。

対象URLを入力して、hamelndlwウィンドウ右下の▼をクリックすると対象ページが表示されます。
![hamelndlw2](https://github.com/user-attachments/assets/8ac832a7-e30d-4bda-858d-4729fab3aef4)


あなたは18歳以上ですか？の質問に対して「はい」をクリックすると対象作品のトップページが開かれますので、hamelndlwウィンドウ右下の▲をクリックしてページを閉じます。
![hamelndlw1](https://github.com/user-attachments/assets/724d3340-0b28-4d75-a7d0-650d7d84c87f)


その後ダウンロードボタンをクリックすれば対象作品のダウンロードが始まります。

尚、この一度設定したCookieは保存されますので、次回以降はこの作業は不要となります（一定期間経過後にCookieがリセットされてR18作品がダウンロード出来なくなった場合は再度Cookieをセットし直してください）。



### 禁止事項
1.hamelndlを用いてWeb小説サイトからダウンロードしたテキストファイルの第三者への販売や不特定多数への配信。 

2.ダウンロードしたオリジナル作品を著作者の了解なく加工（文章の流用や作品の翻訳等）しての再公開。 

3.その他、著作者の権利を踏みにじるような行為。 

4.ソースコード中のSleep(1000)を削除または数値を小さくしてダウンロードを高速化し、ダウンロード時にhamelnサーバーに負荷をかける行為。


### ライセンス
MIT
