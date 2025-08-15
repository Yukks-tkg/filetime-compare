---
title: "README"
output: html_document
---

# R ファイルタイムスタンプ比較ツール

A/B ディレクトリのファイルを（ベース名一致 / 簡易ルール / 正規表現）でペアにして mtime を比較します。

## デモの使い方（shinyapps.io）

初期値で `sample/A` と `sample/B` が選ばれているので、そのまま「実行」で動作します。

## ローカル実行

```{r}
shiny::runApp() # このフォルダで実行
```
