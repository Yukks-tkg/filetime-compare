# app.R — Rファイルタイムスタンプ比較ツール（ベース名一致・簡易・正規表現 / 比較条件切替）
# 機能: 比較条件(<, <=, ==, >=, >) / 拡張子フィルタ（A・B個別）/ TXT・CSV出力
# サマリ行に A対象・B対象 の件数を表示
# 必要パッケージ：shiny, DT, fs, dplyr, stringr, readr, lubridate, tibble, purrr, tools

library(shiny)
library(DT)
library(fs)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tibble)
library(purrr)
library(tools)

# ---- ユーティリティ ----
normalize_key <- function(x, case_mode = c("auto", "sensitive", "insensitive")) {
  case_mode <- match.arg(case_mode)
  if (case_mode == "auto") {
    if (Sys.info()[["sysname"]] %in% c("Windows")) tolower(x) else x
  } else if (case_mode == "insensitive") {
    tolower(x)
  } else {
    x
  }
}

parse_exts <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) return(character(0))
  exts <- unlist(strsplit(s, "[,;\\s]+"))
  exts <- trimws(exts)
  exts <- exts[nzchar(exts)]
  exts <- gsub("^\\.", "", exts)
  exts
}

# 先頭/末尾トークン（複数可）を分割: カンマ/セミコロン/空白
parse_tokens <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) return(character(0))
  x <- unlist(strsplit(s, "[,;\\s]+"))
  x <- trimws(x)
  x[nzchar(x)]
}

scan_files <- function(dir, recurse = TRUE, case_mode = "auto") {
  stopifnot(dir_exists(dir))
  info <- fs::dir_info(dir, recurse = recurse, type = "file", fail = FALSE)
  if (nrow(info) == 0) {
    return(tibble(
      abspath = character(), relpath = character(), rel_dir = character(),
      file = character(), base = character(), ext = character(), ext_norm = character(),
      mtime = as.POSIXct(character()), size = numeric(), key = character(),
      rel_with_ext = character()
    ))
  }
  rel <- fs::path_rel(info$path, start = dir)
  file <- fs::path_file(info$path)
  ext  <- fs::path_ext(file)
  base <- tools::file_path_sans_ext(file)
  rel_dir <- fs::path_dir(rel)
  rel_dir[rel_dir == "."] <- ""
  # 拡張子の比較用正規化
  ext_norm <- if (case_mode == "sensitive" ||
                  (case_mode == "auto" && !(Sys.info()[["sysname"]] %in% c("Windows")))) {
    ext
  } else {
    tolower(ext)
  }
  key <- normalize_key(file.path(rel_dir, base), case_mode)
  
  tibble(
    abspath = as.character(info$path),
    relpath = as.character(rel),
    rel_with_ext = gsub("^[\\/]+", "", ifelse(rel_dir == "", file, file.path(rel_dir, file))),
    rel_dir = rel_dir,
    file = file,
    base = base,
    ext = ext,
    ext_norm = ext_norm,
    mtime = as.POSIXct(info$modification_time, tz = Sys.timezone()),
    size = as.numeric(info$size),
    key = key
  )
}

# ---- 対応付け（ベース名一致）----
pair_by_base_name <- function(dfA, dfB) {
  dupA <- dfA %>% count(key) %>% filter(n > 1) %>% pull(key)
  dupB <- dfB %>% count(key) %>% filter(n > 1) %>% pull(key)
  pairs <- full_join(
    dfA %>% select(key, relpath_A = rel_with_ext, abspath_A = abspath, mtime_A = mtime, ext_A = ext),
    dfB %>% select(key, relpath_B = rel_with_ext, abspath_B = abspath, mtime_B = mtime, ext_B = ext),
    by = "key"
  ) %>%
    mutate(
      pair_key = key,
      exists_A = !is.na(abspath_A),
      exists_B = !is.na(abspath_B),
      dup_A = key %in% dupA,
      dup_B = key %in% dupB
    )
  pairs
}

# ---- 簡易ルール：先頭/末尾の固定文字を無視してキー化 ----
remove_prefix_suffix_vector <- function(x, prefixes = character(0), suffixes = character(0), case_mode = "auto") {
  if (length(x) == 0) return(x)
  insensitive <- (case_mode == "insensitive") || (case_mode == "auto" && Sys.info()[["sysname"]] %in% c("Windows"))
  norm <- function(s) if (insensitive) tolower(s) else s
  
  res <- x
  if (length(prefixes) > 0) {
    p_norm <- norm(prefixes)
    xn <- norm(res)
    for (i in seq_along(prefixes)) {
      hit <- startsWith(xn, p_norm[i])
      res[hit] <- substring(res[hit], nchar(prefixes[i]) + 1)
      xn <- norm(res)
    }
  }
  if (length(suffixes) > 0) {
    s_norm <- norm(suffixes)
    xn <- norm(res)
    for (i in seq_along(suffixes)) {
      hit <- endsWith(xn, s_norm[i])
      res[hit] <- substr(res[hit], 1, nchar(res[hit]) - nchar(suffixes[i]))
      xn <- norm(res)
    }
  }
  res
}

derive_pair_key_simple <- function(df, pre_tokens = character(0), suf_tokens = character(0),
                                   include_rel_dir = TRUE, case_mode = "auto") {
  core <- remove_prefix_suffix_vector(df$base, pre_tokens, suf_tokens, case_mode = case_mode)
  rel_dir <- df$rel_dir; rel_dir[rel_dir == "."] <- ""
  key <- if (include_rel_dir) {
    ifelse(nzchar(rel_dir), file.path(rel_dir, core), core)
  } else core
  normalize_key(key, case_mode)
}

pair_by_simple_rule <- function(dfA, dfB) {
  dupA <- dfA %>% filter(!is.na(pair_key)) %>% count(pair_key) %>% filter(n > 1) %>% pull(pair_key)
  dupB <- dfB %>% filter(!is.na(pair_key)) %>% count(pair_key) %>% filter(n > 1) %>% pull(pair_key)
  
  pairs <- full_join(
    dfA %>% select(pair_key, core_base_A, relpath_A = rel_with_ext, abspath_A = abspath, mtime_A = mtime, ext_A = ext),
    dfB %>% select(pair_key, core_base_B, relpath_B = rel_with_ext, abspath_B = abspath, mtime_B = mtime, ext_B = ext),
    by = "pair_key"
  ) %>%
    mutate(
      exists_A = !is.na(abspath_A),
      exists_B = !is.na(abspath_B),
      dup_A = pair_key %in% dupA,
      dup_B = pair_key %in% dupB
    )
  pairs
}

# ---- 上級（正規表現）----
derive_pair_key_regex <- function(df, pattern, group_index = 1, include_rel_dir = TRUE, case_mode = "auto") {
  if (is.null(pattern) || !nzchar(pattern)) return(rep(NA_character_, nrow(df)))
  m <- stringr::str_match(df$file, pattern)  # col1=全体, col2..=グループ
  if (is.null(m)) return(rep(NA_character_, nrow(df)))
  col <- 1 + as.integer(group_index)
  key_part <- if (ncol(m) >= col) m[, col] else NA_character_
  key_part[!nzchar(key_part)] <- NA_character_
  rel_dir <- df$rel_dir; rel_dir[rel_dir == "."] <- ""
  key <- if (include_rel_dir) {
    ifelse(nzchar(rel_dir), file.path(rel_dir, key_part), key_part)
  } else key_part
  normalize_key(key, case_mode)
}

pair_by_regex <- function(dfA, dfB) {
  dupA <- dfA %>% filter(!is.na(pair_key)) %>% count(pair_key) %>% filter(n > 1) %>% pull(pair_key)
  dupB <- dfB %>% filter(!is.na(pair_key)) %>% count(pair_key) %>% filter(n > 1) %>% pull(pair_key)
  
  pairs <- full_join(
    dfA %>% select(pair_key, relpath_A = rel_with_ext, abspath_A = abspath, mtime_A = mtime, ext_A = ext),
    dfB %>% select(pair_key, relpath_B = rel_with_ext, abspath_B = abspath, mtime_B = mtime, ext_B = ext),
    by = "pair_key"
  ) %>%
    mutate(
      exists_A = !is.na(abspath_A),
      exists_B = !is.na(abspath_B),
      dup_A = pair_key %in% dupA,
      dup_B = pair_key %in% dupB
    )
  pairs
}

# ---- 検証（比較条件切替対応）----
validate_pairs <- function(pairs, tolerance_sec = 0, comparator = "<") {
  res <- pairs %>%
    mutate(
      comparator = comparator,
      tolerance_sec = tolerance_sec,
      diff_sec = as.numeric(difftime(mtime_B, mtime_A, units = "secs")),
      cmp_ok = dplyr::case_when(
        comparator == "<"  ~ diff_sec >  tolerance_sec,
        comparator == "<=" ~ diff_sec >= -tolerance_sec,
        comparator == "==" ~ abs(diff_sec) <= tolerance_sec,
        comparator == ">=" ~ diff_sec <=  tolerance_sec,
        comparator == ">"  ~ diff_sec <  -tolerance_sec,
        TRUE ~ FALSE
      ),
      fail_reason = case_when(
        !exists_A & !exists_B ~ "NOT_FOUND_A_AND_B",
        !exists_A ~ "NOT_FOUND_A",
        !exists_B ~ "NOT_FOUND_B",
        dup_A | dup_B ~ "DUPLICATE_IN_INPUT",
        is.na(diff_sec) ~ "READ_ERROR",
        !cmp_ok ~ "MTIME_CONDITION_FAILED",
        TRUE ~ NA_character_
      ),
      result = if_else(is.na(fail_reason), "PASS", "FAIL")
    ) %>%
    mutate(pair_id = row_number(), .before = 1)
  res
}

# ---- レポート ----
make_txt_report <- function(df, dir_A, dir_B) {
  total <- nrow(df)
  pass_n <- sum(df$result == "PASS", na.rm = TRUE)
  fail_n <- sum(df$result == "FAIL", na.rm = TRUE)
  header <- c(
    sprintf("# Validation Report (A %s B, tolerance=%s)", unique(df$comparator)[1] %||% "<", unique(df$tolerance_sec)[1] %||% 0),
    sprintf("Dir A: %s", dir_A),
    sprintf("Dir B: %s", dir_B),
    sprintf("Total: %d | PASS: %d | FAIL: %d", total, pass_n, fail_n),
    "",
    "| pair_id | pair_key | relpath_A | ext_A | relpath_B | ext_B | mtime_A | mtime_B | diff_sec | result | reason |",
    "|--------:|----------|-----------|:-----:|-----------|:-----:|---------|---------|---------:|:-------|:-------|"
  )
  body <- pmap_chr(
    df %>% select(pair_id, pair_key, relpath_A, ext_A, relpath_B, ext_B, mtime_A, mtime_B, diff_sec, result, fail_reason),
    function(pair_id, pair_key, relpath_A, ext_A, relpath_B, ext_B, mtime_A, mtime_B, diff_sec, result, fail_reason) {
      sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
              pair_id %||% "",
              pair_key %||% "",
              relpath_A %||% "",
              ext_A %||% "",
              relpath_B %||% "",
              ext_B %||% "",
              if (!is.na(mtime_A)) format(mtime_A, "%Y-%m-%d %H:%M:%S") else "",
              if (!is.na(mtime_B)) format(mtime_B, "%Y-%m-%d %H:%M:%S") else "",
              if (!is.na(diff_sec)) sprintf("%.0f", diff_sec) else "",
              result %||% "",
              fail_reason %||% "")
    }
  )
  c(header, body)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) b else a

# ---- UI ----
ui <- fluidPage(
  titlePanel("Rファイルタイムスタンプ比較ツール（ベース名一致・簡易・正規表現）"),
  sidebarLayout(
    sidebarPanel(
      textInput("dirA", "Aディレクトリ", value = "C:/proj/A", placeholder = "例: C:/proj/A"),
      textInput("dirB", "Bディレクトリ", value = "C:/proj/B", placeholder = "例: C:/proj/B"),
      
      # 拡張子フィルタ
      textInput("extA", "A側で対象とする拡張子（空=すべて）", placeholder = "例: r; csv 或いは .r .csv"),
      textInput("extB", "B側で対象とする拡張子（空=すべて）", placeholder = "例: txt; xpt 或いは .txt .xpt"),
      
      checkboxInput("recurse", "サブディレクトリを含める", value = TRUE),
      selectInput("case", "大文字小文字の扱い", choices = c("自動" = "auto", "区別する" = "sensitive", "区別しない" = "insensitive"), selected = "auto"),
      
      # 比較条件
      selectInput("cmp", "比較条件（A ? B）", choices = c("<","<=","==",">=",">"), selected = "<"),
      numericInput("tol", "許容誤差（秒）", value = 0, min = 0, step = 1),
      
      hr(),
      radioButtons("pair_mode", "対応付け方法", inline = TRUE,
                   choices = c("ベース名一致" = "base",
                               "簡易ルール（先頭/末尾の固定文字を無視）" = "simple",
                               "上級（正規表現）" = "regex"),
                   selected = "base"),
      
      # 簡易ルール
      conditionalPanel(
        'input.pair_mode == "simple"',
        checkboxInput("include_rel_simple", "サブディレクトリをキーに含める（rel_dir/コア名）", value = TRUE),
        tags$b("A側：無視する固定文字"),
        textInput("simpleA_pre", "A側 先頭", placeholder = "例: run_, T_, SAMPLE-（複数可：カンマ/空白区切り）"),
        textInput("simpleA_suf", "A側 末尾", placeholder = "例: _src, _raw, _old（複数可）"),
        tags$b("B側：無視する固定文字"),
        textInput("simpleB_pre", "B側 先頭", placeholder = "例: out_, RESULT-, T_（複数可）"),
        textInput("simpleB_suf", "B側 末尾", placeholder = "例: _log, _v2, -final（複数可）"),
        helpText("例: T_1.R と T_1_log.txt → B側 末尾に「_log」を指定。")
      ),
      
      # 上級（正規表現）
      conditionalPanel(
        'input.pair_mode == "regex"',
        checkboxInput("include_rel_regex", "サブディレクトリをキーに含める（rel_dir/キー）", value = TRUE),
        tags$b("A側のキー抽出（正規表現）"),
        textInput("patA", "A側 正規表現（キーにしたい部分を ( ... ) で囲む）",
                  value = "^(.+)\\.R$", placeholder = "^(.+)\\.R$ など"),
        numericInput("grpA", "A側 使うキャプチャ群番号", value = 1, min = 1, step = 1),
        tags$b("B側のキー抽出（正規表現）"),
        textInput("patB", "B側 正規表現（キーにしたい部分を ( ... ) で囲む）",
                  value = "^(.+)_log\\.txt$", placeholder = "^(.+)_log\\.txt$ など"),
        numericInput("grpB", "B側 使うキャプチャ群番号", value = 1, min = 1, step = 1)
      ),
      
      hr(),
      actionButton("run", "実行", class = "btn-primary"),
      hr(),
      downloadButton("dl_txt", "TXTレポートを保存"),
      downloadButton("dl_csv", "CSVを保存")
    ),
    mainPanel(
      tags$div(style = "margin: 0.5rem 0;", textOutput("summary")),
      DTOutput("table")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  rv <- reactiveValues(results = NULL, countA = 0, countB = 0)
  
  observeEvent(input$run, {
    req(nzchar(input$dirA), nzchar(input$dirB))
    validate(need(dir_exists(input$dirA), "Aディレクトリが存在しません"))
    validate(need(dir_exists(input$dirB), "Bディレクトリが存在しません"))
    
    withProgress(message = "走査中...", value = 0.2, {
      # 走査
      dfA <- scan_files(input$dirA, recurse = isTRUE(input$recurse), case_mode = input$case)
      incProgress(0.2, detail = sprintf("A側: %dファイル（走査）", nrow(dfA)))
      dfB <- scan_files(input$dirB, recurse = isTRUE(input$recurse), case_mode = input$case)
      incProgress(0.2, detail = sprintf("B側: %dファイル（走査）", nrow(dfB)))
      
      # 拡張子フィルタ
      extsA <- parse_exts(input$extA)
      extsB <- parse_exts(input$extB)
      ext_normalize_vec <- function(v) {
        if (input$case == "sensitive" || (input$case == "auto" && !(Sys.info()[["sysname"]] %in% c("Windows")))) v else tolower(v)
      }
      if (length(extsA) > 0) {
        extsA <- ext_normalize_vec(extsA)
        dfA <- dfA %>% filter(ext_norm %in% extsA)
      }
      if (length(extsB) > 0) {
        extsB <- ext_normalize_vec(extsB)
        dfB <- dfB %>% filter(ext_norm %in% extsB)
      }
      incProgress(0.1, detail = sprintf("A側: %d件 / B側: %d件（拡張子フィルタ後）", nrow(dfA), nrow(dfB)))
      
      # 対象数（ベース名一致の初期値）
      countA <- nrow(dfA)
      countB <- nrow(dfB)
      
      # 対応付け
      if (input$pair_mode == "base") {
        pairs <- pair_by_base_name(dfA, dfB)
        
      } else if (input$pair_mode == "simple") {
        preA <- parse_tokens(input$simpleA_pre)
        sufA <- parse_tokens(input$simpleA_suf)
        preB <- parse_tokens(input$simpleB_pre)
        sufB <- parse_tokens(input$simpleB_suf)
        
        dfA$core_base_A <- remove_prefix_suffix_vector(dfA$base, preA, sufA, case_mode = input$case)
        dfB$core_base_B <- remove_prefix_suffix_vector(dfB$base, preB, sufB, case_mode = input$case)
        
        dfA$pair_key <- derive_pair_key_simple(dfA, preA, sufA, include_rel_dir = isTRUE(input$include_rel_simple), case_mode = input$case)
        dfB$pair_key <- derive_pair_key_simple(dfB, preB, sufB, include_rel_dir = isTRUE(input$include_rel_simple), case_mode = input$case)
        
        # 簡易ルールではキー抽出に成功した件数をカウント
        countA <- sum(!is.na(dfA$pair_key))
        countB <- sum(!is.na(dfB$pair_key))
        
        validate(need(countA > 0, "A側の簡易ルールでキーを抽出できるファイルがありません"))
        validate(need(countB > 0, "B側の簡易ルールでキーを抽出できるファイルがありません"))
        
        pairs <- pair_by_simple_rule(dfA, dfB)
        
      } else { # regex
        dfA$pair_key <- derive_pair_key_regex(dfA, input$patA, input$grpA, include_rel_dir = isTRUE(input$include_rel_regex), case_mode = input$case)
        dfB$pair_key <- derive_pair_key_regex(dfB, input$patB, input$grpB, include_rel_dir = isTRUE(input$include_rel_regex), case_mode = input$case)
        
        # 正規表現ではキー抽出に成功した件数をカウント
        countA <- sum(!is.na(dfA$pair_key))
        countB <- sum(!is.na(dfB$pair_key))
        
        validate(need(countA > 0, "A側の正規表現でキーを抽出できるファイルがありません"))
        validate(need(countB > 0, "B側の正規表現でキーを抽出できるファイルがありません"))
        
        pairs <- pair_by_regex(dfA, dfB)
      }
      
      # 件数を保存
      rv$countA <- countA
      rv$countB <- countB
      
      # 検証
      res <- validate_pairs(pairs, tolerance_sec = input$tol, comparator = input$cmp)
      rv$results <- res
      
      incProgress(0.1, message = "完了")
    })
  })
  
  # サマリ
  output$summary <- renderText({
    req(rv$results)
    df <- rv$results
    total  <- nrow(df)
    pass_n <- sum(df$result == "PASS", na.rm = TRUE)
    fail_n <- sum(df$result == "FAIL", na.rm = TRUE)
    sprintf("A対象: %d | B対象: %d | Total: %d | PASS: %d | FAIL: %d (条件: A %s B, tolerance=%s秒)",
            rv$countA, rv$countB, total, pass_n, fail_n, input$cmp, input$tol)
  })
  
  # 結果テーブル
  output$table <- renderDT({
    req(rv$results)
    df <- rv$results %>%
      transmute(
        pair_id, pair_key, relpath_A, ext_A, relpath_B, ext_B,
        mtime_A = format(mtime_A, "%Y-%m-%d %H:%M:%S"),
        mtime_B = format(mtime_B, "%Y-%m-%d %H:%M:%S"),
        diff_sec = round(diff_sec),
        comparator, tolerance_sec, result, fail_reason
      )
    datatable(df, options = list(pageLength = 30, autoWidth = TRUE), rownames = FALSE, filter = "top", class = "display compact")
  })
  
  # TXT出力
  output$dl_txt <- downloadHandler(
    filename = function() {
      paste0("validation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(rv$results)
      lines <- make_txt_report(rv$results, dir_A = input$dirA, dir_B = input$dirB)
      writeLines(lines, file, useBytes = TRUE)
    }
  )
  
  # CSV出力
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("validation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$results)
      out <- rv$results %>%
        mutate(
          mtime_A = format(mtime_A, "%Y-%m-%d %H:%M:%S"),
          mtime_B = format(mtime_B, "%Y-%m-%d %H:%M:%S")
        )
      readr::write_csv(out, file)
    }
  )
}

shinyApp(ui, server)
