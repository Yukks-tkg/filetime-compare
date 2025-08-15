# 上書きデプロイ（forceUpdate=TRUE）
# 環境変数はアクション側から渡されます
app_dir   <- Sys.getenv("INPUT_APPDIR", unset = ".")
app_name  <- Sys.getenv("INPUT_APPNAME")
account   <- Sys.getenv("INPUT_ACCOUNTNAME")
token     <- Sys.getenv("INPUT_ACCOUNTTOKEN")
secret    <- Sys.getenv("INPUT_ACCOUNTSECRET")
loglevel  <- Sys.getenv("INPUT_LOGLEVEL", unset = "normal")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("rsconnect package is required.")
}

# アカウント情報をセット
rsconnect::setAccountInfo(
  name   = account,
  token  = token,
  secret = secret
)

rsconnect::deployApp(
  appDir      = app_dir,
  appName     = app_name,
  account     = account,
  logLevel    = loglevel,
  forceUpdate = TRUE
)