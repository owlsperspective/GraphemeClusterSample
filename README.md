# GraphemeClusterSample
  https://github.com/owlsperspective/GraphemeClusterSample

## 概要
- DelphiでUnicode Standard Annex #11 East Asian Width(UAX #11)を実装します。
- EastAsianWidthConverterはUnicode Inc.の提供するデータベースファイル(`EastAsianWidth.txt`)をDelphiのコード上から参照できるように変換します。
- GraphemeClusterTestはDelphiの正規表現ライブラリ(PCRE)およびSkia4Delphiによる書記素クラスタの分割と、表示文字幅の計算を行うサンプルプログラムです。

## 実行環境
- サンプルプログラムはVCLで作られており、Windows上で動作します。
- `Unicode.EastAsianWidth`ユニットおよび生成された`EastAsianWidth.inc`はDelphiが動作するすべてのプラットフォームで動作します。

## 開発環境
- Embarcadero RAD Stusio (Delphi)
  - https://www.embarcadero.com/jp/products/delphi
    - Unicodeとレコードヘルパに対応するバージョン(Delphi 2009以降)で動作します。
- Skia4Delphi
  - https://skia4delphi.org/
  - https://github.com/skia4delphi/skia4delphi
    - RAD Studio 11 Alexandriaまたはそれ以前のバージョンでサンプルプログラムを実行するためにはSkia4Delphiをインストールする必要があります。
    - `Unicode.EastAsianWidth`ユニットを使用するだけであればSkia4Delphiは必要ありません。

## 使用方法
### EastAsianWidth.incの生成
- DelphiのIDEで`EastAsianWidthConverter.dpr`を開きます。
- Unicodeのwebサイトから`EastAsianWidth.txt`をダウンロードしてプロジェクトフォルダの直下に配置します。最新版は以下のURLにあります。
  - https://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt
- プロジェクトEastAsianWidthConverterをコンパイルし、実行します。
- Source、Outputが正しく設定されていることを確認し、"Convert"ボタンをクリックします。
- `EastAsianWidth.txt`から`EastAsianWidth.inc`が生成され、その内容がMemoに表示されます。

### サンプルプログラムの実行
- Delphi 11 Alexandriaおよびそれ以前のバージョンではあらかじめSkia4Delphiをインストールしておきます。
- DelphiのIDEで`GraphemeClusterTest.dpr`を開きます。
- プロジェクトツールウィンドウでプロジェクトを右クリック→"Skiaを有効化"を選択します。
- コンパイル、実行します。
- RegExボタンをクリックすると正規表現ライブラリ(PCRE)で、SkiaボタンをクリックするとSkia4Delphiで、書記素クラスタの分割が行われ、文字列のコードポイント数(L)と表示幅(W)が表示されます。

### Unicode.EastAsianWidthユニットの使用
- `Unicode.EastAsianWidth.pas`と`EastAsianWidth.inc`は同じフォルダに配置します。
- 使用したいプロジェクトに`Unicode.EastAsianWidth.pas`を追加し、参照するユニットの`uses`に`Unicode.EastAsianWidth`を追加します。
- `class function TEastAsianWidth.GetEastAsianWidth`にUCS4で文字を渡すと`TEastAsianWidth`が返されます。
- `function TEastAsianWidth.GetWidth`で文字幅を取得できます。
- "Ambiguous"を欧文フォントなどで半角幅として扱いたいときは`class property TEastAsianWidth.EastAsian`を`False`に、日本語フォントなどで全角幅として扱いたいときは`True`に設定します。

## ライセンス
このプログラムに固有のコードについてはMITライセンスが適用されます。

Copyright 2023 Owl's Perspective

The MIT License – Open Source Initiative<br />
https://opensource.org/license/mit/

(日本語参考訳)<br />
https://licenses.opensource.jp/MIT/MIT.html

Skia4DelphiはMITライセンスの元で公開されています。<br />
https://github.com/skia4delphi/skia4delphi/blob/main/LICENSE

UnicodeのデータベースはUNICODE LICENSE V3の元で公開されています。<br />
https://www.unicode.org/license.txt

## 更新履歴
- 2023-12-27
  新規作成

