# Project 2: PCA를 이용한 신발 이미지 검색 앱
# 발표 전에는 이미지 전처리, PCA, 거리, 예측, 시각화가 어떻게 이어지는지
# 팀원 모두가 설명할 수 있어야 한다.

# 앱에서 직접 사용하는 패키지를 한곳에서 불러온다.
# 패키지 설치는 app.R 안에서 하지 않고, 실행 전에 install.packages()로 한 번만 한다.
library(shiny)    # 웹 앱의 화면(UI)과 서버 로직을 만든다.
library(magick)   # 사진을 읽고 64 x 64 흑백 이미지로 바꾼다.
library(ggplot2)  # PCA 점수 지도와 막대그래프를 그린다.
library(dplyr)    # Data 1을 필터링하고 그룹별 통계를 계산한다.
library(irlba)    # 큰 이미지 행렬에서 앞쪽 주성분만 빠르게 계산한다.
library(scales)   # 그래프의 비율을 백분율 형식으로 표시한다.
# 업로드할 수 있는 사진의 최대 크기를 10MB로 제한한다.
options(shiny.maxRequestSize = 10 * 1024^2)

# ---- Team configuration -----------------------------------------------------
# 앱의 제목과 브라우저 창 제목에 사용할 팀 이름이다.
team_name <- "PRISM"
# AI 탭의 서명과 개인 성찰 표에 표시할 팀원 이름이다.
team_members <- c("Juwon Lee", "Jiwon Hyung", "Seungyeon Yoo", "Moona Cho", "Kamelia Daddi")

# L은 R에서 정수(integer)를 뜻한다. 모든 사진을 64 x 64로 통일하므로
# 사진 한 장은 64^2 = 4,096개의 픽셀 값으로 표현된다.
image_size <- 64L
pixel_count <- image_size^2

# 최대 100개의 주성분을 계산해 슬라이더와 비교 그래프에서 사용한다.
max_pcs <- 100L
# 기본 검색에는 앞쪽 주성분 42개를 사용하며, 슬라이더로 값을 바꿀 수 있다.
# 현재 팀 사진 12장의 K=1~100 비교에서 가장 많은 10장을 맞힌 잠정 기본값이다.
DEFAULT_K <- 42L
# 앱에서 인정하는 네 가지 정답 라벨과 득표 표의 표시 순서이다.
categories <- c("Boots", "Sandals", "Shoes", "Slippers")

# 아래 경로는 app.R이 있는 폴더를 기준으로 한 상대 경로이다.
# 카탈로그 신발 사진 폴더이다.
catalog_dir <- "shoes"
# 카탈로그 사진의 id와 종류가 적힌 CSV 파일이다.
metadata_path <- "shoes_meta.csv"
# 확인용 사진과 정답 labels.csv가 있는 폴더이다.
selfcheck_dir <- "selfcheck"
# 파일명에 이름과 종류를 적은 팀원 사진 폴더이다.
our_shoes_dir <- "our_shoes"
# 탭 4에서 분석하는 Spotify 데이터 파일이다.
data1_path <- "data1.csv"

# 핵심 데이터가 없으면 뒤에서 이해하기 어려운 오류가 나므로 시작할 때 확인한다.
if (!dir.exists(catalog_dir) || !file.exists(metadata_path)) {
  stop("shoes/ and shoes_meta.csv must be beside app.R.", call. = FALSE)
}

# ---- 1. 이미지 전처리 -------------------------------------------------------
# 카탈로그, self-check, 업로드 사진에 반드시 같은 전처리를 적용해야
# PCA 좌표와 거리를 공정하게 비교할 수 있다.
# 입력: 이미지 파일 경로 1개
# 출력: 길이 4,096의 숫자 벡터(각 값은 0=검정, 255=흰색)
read_shoe <- function(path) {
  # 1) 파일을 magick 이미지 객체로 읽는다.
  img <- magick::image_read(path)
  # 2) 원본 비율과 관계없이 정확히 64 x 64로 맞춘다. !는 강제 크기 지정이다.
  img <- magick::image_resize(img, sprintf("%dx%d!", image_size, image_size))
  # 3) 색상 정보를 제거해 모든 사진을 한 개의 밝기 채널로 통일한다.
  img <- magick::image_convert(img, colorspace = "gray", format = "png")
  # 4) 픽셀을 정수로 꺼낸다. 정상이라면 정확히 4,096개여야 한다.
  pixels <- as.integer(magick::image_data(img, channels = "gray"))
  if (length(pixels) != pixel_count) stop("Image conversion failed: ", path)
  # 5) 64 x 64 행렬의 행 방향 순서가 맞도록 전치(t)한 뒤 벡터로 편다.
  as.numeric(t(matrix(pixels, image_size, image_size)))
}

# 길이 4,096의 벡터를 다시 64 x 64 이미지로 그리는 보조 함수이다.
# signed=FALSE: 일반 사진처럼 0~255 밝기로 표시한다.
# signed=TRUE : 평균과의 차이/eigenshoe처럼 음수와 양수가 모두 있는 값을 표시한다.
plot_shoe <- function(x, title = "", signed = FALSE) {
  # read_shoe()에서 전치해 펼쳤던 벡터를 다시 전치하여 원래 방향으로 복원한다.
  # 이 t()가 없으면 이미지의 행과 열이 바뀌어 신발이 돌아가 보인다.
  z <- t(matrix(x, image_size, image_size))
  # 새 그림 영역을 준비한다.
  plot.new()
  # 그림을 놓을 가로·세로 좌표를 0~1로 정하고 한 단위의 길이를 같게 한다.
  plot.window(c(0, 1), c(0, 1), asp = 1)
  if (signed) {
    # 가장 큰 절댓값을 기준으로 -1~1 범위에 맞춘다.
    # 양수와 음수를 같은 기준으로 비교하기 위한 과정이다.
    lim <- max(abs(z), na.rm = TRUE)
    # 0이나 유효하지 않은 값으로 나누는 일을 막기 위해 lim을 1로 대체한다.
    if (!is.finite(lim) || lim == 0) lim <- 1
    # 절댓값 기준으로 나눈 뒤 각 값을 -1~1 안으로 제한한다.
    scaled <- pmax(-1, pmin(1, z / lim))
    # -1~1을 0~1로 바꾼다. 음수는 어둡게, 0은 중간 회색, 양수는 밝게 표시한다.
    gray_pixels <- grDevices::gray((scaled + 1) / 2)
    # 색 목록을 64 x 64로 배치하고 그릴 수 있는 래스터 이미지로 바꾼다.
    raster <- as.raster(matrix(gray_pixels, image_size, image_size))
  } else {
    # 실제 사진의 픽셀 범위를 0~255로 제한하고 gray()가 요구하는 0~1로 바꾼다.
    # gray()는 행렬 차원을 없애므로 matrix()로 64 x 64 차원을 다시 붙인다.
    gray_pixels <- grDevices::gray(pmax(0, pmin(255, z)) / 255)
    # 색 목록을 64 x 64로 배치하고 그릴 수 있는 래스터 이미지로 바꾼다.
    raster <- as.raster(matrix(gray_pixels, image_size, image_size))
  }
  # 좌표 영역 전체에 이미지를 그리고 픽셀 사이의 보간은 사용하지 않는다.
  rasterImage(raster, 0, 0, 1, 1, interpolate = FALSE)
  # 이미지 영역의 테두리를 그린다.
  box()
  # 이미지 위에 전달받은 제목을 표시한다.
  title(main = title)
}

# ---- 2. 카탈로그 검사와 PCA 모델 만들기 -------------------------------------
# metadata의 각 행은 신발 이미지 한 장과 대응한다.
metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
# id/category 열과 정확히 2,000개 행이 있는지 확인한다.
if (!all(c("id", "category") %in% names(metadata)) || nrow(metadata) != 2000L) {
  stop("shoes_meta.csv must have 2,000 rows and id/category columns.")
}
# id 오름차순으로 행을 정렬하고 drop=FALSE로 표 형태를 유지한다.
metadata <- metadata[order(metadata$id), , drop = FALSE]
# id=1이면 shoes/0001.png가 되도록 실제 파일 경로를 만든다.
catalog_paths <- file.path(catalog_dir, sprintf("%04d.png", metadata$id))
# 경로 목록 중 실제 파일이 하나라도 없으면 실행을 멈춘다.
if (any(!file.exists(catalog_paths))) stop("Catalogue images are missing.")

# 2,000장의 이미지를 읽어 PCA 모델을 새로 만드는 함수이다.
build_model <- function() {
  # 오래 걸릴 수 있는 작업의 진행 상황을 R 콘솔에 알린다.
  message("Reading 2,000 catalogue images...")
  # vapply 결과는 처음에 4096 x 2000이므로 t()로 전치한다.
  # X의 크기: 2,000행(사진) x 4,096열(픽셀)
  X <- t(vapply(catalog_paths, read_shoe, numeric(pixel_count)))
  # 오래 걸릴 수 있는 작업의 진행 상황을 R 콘솔에 알린다.
  message("Computing the first 100 principal components...")
  # 같은 데이터에서 항상 재현 가능한 결과를 얻기 위한 난수 시드이다.
  set.seed(39209)
  # center=TRUE: 각 픽셀에서 평균 이미지를 뺀 뒤 PCA를 수행한다.
  # scale.=FALSE: 모든 픽셀이 같은 0~255 단위이므로 표준편차로 나누지 않는다.
  # prcomp_irlba는 전체 4,096개 대신 필요한 앞 100개 PC를 효율적으로 계산한다.
  fit <- irlba::prcomp_irlba(X, n = max_pcs, center = TRUE, scale. = FALSE)

  # 각 PC가 전체 픽셀 변동 중 얼마를 설명하는지 구하기 위한 전체 분산이다.
  total_var <- sum(apply(X, 2, stats::var))
  # 평균 이미지, PC 방향, 점수, 설명 분산과 메타데이터를 한 객체에 보관한다.
  ans <- list(
    # mean: 길이 4,096의 평균 신발
    # rotation: 4,096 x 100 행렬, 각 열은 하나의 eigenshoe(PC 방향)
    # scores: 2,000 x 100 행렬, 각 카탈로그 사진의 PCA 좌표
    mean = fit$center, rotation = fit$rotation,
    # scores는 사진별 PC 점수이고 sdev는 PC 점수의 표준편차이다.
    scores = fit$x, sdev = fit$sdev,
    # 각 PC의 분산을 전체 픽셀 분산으로 나누어 설명 분산 비율을 구한다.
    explained = fit$sdev^2 / total_var, metadata = metadata
  )
  # 계산한 결과 목록을 함수의 반환값으로 돌려준다.
  ans
}

# 앱을 시작할 때마다 전체 카탈로그로 PCA 모델을 새로 계산한다.
catalog <- build_model()

# ---- 3. PCA 좌표, 복원, 최근접 검색 ----------------------------------------
# 새 사진 x를 PCA 공간의 첫 k개 좌표로 변환한다.
# 수식: z = (x - 평균) %*% rotation[, 1:k]
# 입력 x는 길이 4,096, 출력 z는 길이 k이다.
query_scores <- function(x, k = max_pcs) {
  # 사진에서 평균을 빼고 PC 방향에 투영한 뒤, 결과의 불필요한 차원을 없앤다.
  drop((x - catalog$mean) %*% catalog$rotation[, seq_len(k), drop = FALSE])
}

# 첫 k개 PCA 좌표만 사용해 사진을 픽셀 공간으로 되돌린다.
# 수식: 복원 사진 = 평균 + rotation[, 1:k] %*% z
# k가 커질수록 보통 원본의 세부 정보를 더 많이 복원한다.
reconstruct <- function(x, k) {
  z <- query_scores(x, k)
  # 선택한 PC들의 기여분을 합하고 평균 이미지를 더해 픽셀 값으로 복원한다.
  drop(catalog$mean + catalog$rotation[, seq_len(k), drop = FALSE] %*% z)
}

# 선택 사진과 가장 가까운 카탈로그 신발 5개를 찾고 카테고리를 예측한다.
# exclude_id: 카탈로그 사진으로 검색할 때 자기 자신을 결과에서 제외할 id
# true_label: 정답을 아는 경우 예측이 맞았는지 확인하기 위한 실제 카테고리
find_matches <- function(x, k, exclude_id = NA_integer_, true_label = NA_character_) {
  # 선택 사진을 길이 k의 PCA 좌표로 바꾼다.
  z <- query_scores(x, k)
  # 카탈로그 사진마다 첫 k개 PCA 점수를 한 행씩 가져온다.
  catalogue_scores <- catalog$scores[, seq_len(k), drop = FALSE]
  # 각 사진과 선택 사진의 점수 차이에 유클리드 거리 공식을 적용한다.
  # apply(..., 1, ...)에서 1은 행 단위로 함수를 실행한다는 뜻이다.
  distance <- apply(catalogue_scores, 1, function(one_shoe) {
    sqrt(sum((one_shoe - z)^2))
  })
  if (!is.na(exclude_id)) {
    # 자기 자신과의 거리는 항상 0이므로 Inf로 바꿔 Top 5에 들어가지 않게 한다.
    own_row <- match(exclude_id, catalog$metadata$id)
    if (!is.na(own_row)) distance[own_row] <- Inf
  }
  # 거리가 작은 순서대로 앞의 5개 행을 선택한다.
  rows <- order(distance)[1:5]
  top <- catalog$metadata[rows, c("id", "category"), drop = FALSE]
  # 검색 순서에 맞는 거리를 Top 5 표에 추가한다.
  top$distance <- distance[rows]
  # Top 5 사진을 그릴 때 사용할 실제 파일 경로를 추가한다.
  top$path <- catalog_paths[rows]

  # Top 5에 가장 많이 등장한 카테고리를 최종 예측으로 사용한다.
  # factor(..., levels=categories)를 사용하면 0표인 카테고리도 표에 남는다.
  counts <- table(factor(top$category, levels = categories))
  # 가장 많은 표를 받은 카테고리를 모두 찾는다. 동률이면 여러 개가 남는다.
  tied <- names(counts)[counts == max(counts)]
  # 2-2-1 동률이면 top이 이미 거리순이므로 동률 카테고리 중 가장 가까운 것을 고른다.
  prediction <- top$category[which(top$category %in% tied)[1]]
  # 정답을 모르면 NA, 알면 TRUE/FALSE를 반환한다.
  correct <- if (is.na(true_label) || !nzchar(true_label)) NA else identical(prediction, true_label)
  # 화면 여러 곳에서 재사용할 값들을 하나의 list로 묶는다.
  list(top = top, votes = counts, prediction = prediction, correct = correct, score = z)
}

# ---- 4. Self-check, 팀 사진, Project 1 데이터 준비 --------------------------
# self-check 사진 파일명과 실제 카테고리의 대응표이다.
selfcheck_labels <- read.csv(file.path(selfcheck_dir, "labels.csv"), stringsAsFactors = FALSE)

# 팀 사진 파일명 형식 Member_Category_Number.jpg에서 이름과 카테고리를 분리한다.
parse_our_shoe <- function(path) {
  # 확장자를 제거한 파일명을 밑줄로 나눠 이름·카테고리·번호를 구분한다.
  bits <- strsplit(tools::file_path_sans_ext(basename(path)), "_", fixed = TRUE)[[1]]
  data.frame(
    # 전체 경로, 파일명, 첫 번째 조각인 팀원 이름을 저장한다.
    path = path, file = basename(path), member = bits[1],
    # 두 번째 조각이 인정된 종류일 때만 정답으로 쓰고, 아니면 NA로 둔다.
    category = if (length(bits) >= 2 && bits[2] %in% categories) bits[2] else NA_character_,
    # 문자열을 factor로 자동 변환하지 않고 문자형 그대로 보관한다.
    stringsAsFactors = FALSE
  )
}
# jpg/jpeg/png 확장자를 가진 팀 사진만 찾는다.
our_paths <- if (dir.exists(our_shoes_dir)) {
  list.files(our_shoes_dir, "\\.(jpg|jpeg|png)$", full.names = TRUE, ignore.case = TRUE)
} else character()
# 각 사진의 경로, 파일명, 팀원, 실제 카테고리를 한 데이터프레임으로 합친다.
our_info <- if (length(our_paths)) do.call(rbind, lapply(our_paths, parse_our_shoe)) else {
  data.frame(path = character(), file = character(), member = character(), category = character())
}

# Project 1 데이터는 선택 사항이므로 파일이 있을 때만 읽는다.
# 읽기 오류가 나면 앱 전체를 중단하지 않고 NULL로 처리한다.
data1 <- if (file.exists(data1_path)) {
  tryCatch(read.csv(data1_path, stringsAsFactors = FALSE), error = function(e) NULL)
} else NULL
if (!is.null(data1)) {
  # 현재 필터와 국가 도달 분석에 실제로 쓰는 세 열만 검사한다.
  data1_required <- c(
    "entry_year", "active_chart_days_90", "countries_entered_90"
  )
  if (!all(data1_required %in% names(data1))) {
    stop("data1.csv does not have the expected Project 1 Spotify columns.")
  }
  # 최초 90일 동안 차트에 등장한 누적 일수를 4개 그룹으로 구분한다.
  data1$duration_group <- cut(
    data1$active_chart_days_90,
    breaks = c(0, 7, 30, 60, 90),
    labels = c("1-7 days", "8-30 days", "31-60 days", "61-90 days")
  )
  # 진입 국가 수를 3개 그룹으로 구분한다.
  data1$reach_group <- cut(
    data1$countries_entered_90,
    breaks = c(0, 1, 9, Inf),
    labels = c("1 country", "2-9 countries", "10+ countries")
  )
}
# 연도 선택 메뉴에 "All years"와 실제 데이터의 연도들을 넣는다.
data1_year_choices <- if (is.null(data1)) "All years" else {
  # 중복 없는 진입 연도를 작은 순서부터 정렬한다.
  years <- sort(unique(data1$entry_year))
  # 화면에는 All years를 보여주고 서버에는 All을 전달하며, 연도도 선택지에 추가한다.
  c("All years" = "All", setNames(as.character(years), years))
}

# ---- 5. UI: 사용자가 보는 화면 ---------------------------------------------
# UI에서 만든 id는 server의 input$id 또는 output$id와 연결된다.
# 예: sliderInput("K", ...)의 값은 server에서 input$K로 읽는다.
# navbarPage()는 탭 메뉴를 만들고 tabPanel()은 각 탭의 화면을 정의한다.
# fluidRow()는 가로 행, column()은 12칸 중 사용할 폭을 지정한다.
# h2/h3/h4는 제목, p는 문단, strong은 굵은 글씨를 만든다.
# plotOutput/tableOutput/textOutput/uiOutput은 같은 id의 서버 출력을 표시할 자리이다.
# selectInput은 단일 선택, checkboxGroupInput은 복수 선택, sliderInput은 숫자 선택이다.
ui <- navbarPage(
  team_name,
  windowTitle = paste(team_name, "- PCA Shoe Search"),
  header = tags$head(
    tags$style(HTML(".tab-content p { font-size: 16px; line-height: 1.5; }"))
  ),

  # [탭 1] 사진 출처와 K를 선택하고 예측, 비교 이미지, Top 5를 확인한다.
  tabPanel(
    "Shoe Search",
    h2("Find a similar shoe"),
    p("Choose a photo. Distances use its first K principal-component scores."),
    fluidRow(
      column(
        3,
        # src 값에 따라 catalogue/selfcheck/upload 입력 중 하나만 화면에 보여준다.
        radioButtons(
          "src", "Photo source",
          c("Catalogue" = "catalogue", "Self-check" = "selfcheck", "Upload" = "upload")
        ),
        conditionalPanel(
          "input.src == 'catalogue'",
          # 카탈로그에서 검색할 사진 번호를 1~2000 범위로 입력한다.
          numericInput("cat_id", "Catalogue id", 1, 1, 2000)
        ),
        conditionalPanel(
          "input.src == 'selfcheck'",
          selectInput("sc_file", "Self-check photo", selfcheck_labels$photo)
        ),
        conditionalPanel(
          "input.src == 'upload'",
          # 업로드 사진을 선택한다. 표시 형식 제한 외에 서버에서도 확장자를 검사한다.
          fileInput("photo", "Upload JPG or PNG", accept = c(".jpg", ".jpeg", ".png")),
          # 알고 있는 실제 종류를 선택해 정오답을 확인한다. Unknown이면 평가하지 않는다.
          selectInput("upload_label", "Actual category (optional)", c("Unknown" = "", categories))
        ),
        # 거리 계산과 복원에 사용할 PC 개수를 1~100에서 선택하며 초기값은 DEFAULT_K이다.
        sliderInput("K", "Number of PCs (K)", 1, max_pcs, DEFAULT_K)
      ),
      column(
        9,
        # textOutput, plotOutput, tableOutput의 id는 server의 output$id와 연결된다.
        h3(textOutput("prediction_card")),
        h3("Photo, average, reconstruction, and best match"),
        plotOutput("search_images", height = 280),
        h3("Top 5 shoe images"),
        plotOutput("top5_images", height = 260),
        # 아래 두 표를 좌우로 나누어 빈 공간 없이 균형 있게 배치한다.
        fluidRow(
          column(
            6,
            h3("Top 5 details"),
            tableOutput("top5")
          ),
          column(
            6,
            h3("Category votes"),
            tableOutput("category_votes")
          )
        ),
        textOutput("vote_explanation")
      )
    )
  ),

  # [탭 2] 검색에 사용한 PCA 과정을 단계별 그래프로 설명한다.
  tabPanel(
    "Explain PCA",
    h2("How PCA finds the closest shoe"),
    p("This tab uses the photo and K selected in Shoe Search."),
    fluidRow(
      column(
        6,
        h3("Step 1. Average shoe"),
        p("Pixel-wise mean of all 2,000 images."),
        plotOutput("mean_plot", height = 300)
      ),
      column(
        6,
        h3("Step 2. Photo minus average"),
        p("Dark and light shades are deviations in opposite directions."),
        plotOutput("difference_plot", height = 300)
      )
    ),
    h3("Step 3. PC1-PC2 score map"),
    p("The selected photo is the black star."),
    plotOutput("score_map", height = 520),
    fluidRow(
      column(4, h3("Step 4. Eigenshoes"),
             # 앞에서부터 보여줄 eigenshoe 개수만 조절한다. 검색에 쓰는 K와는 별개이다.
             sliderInput("n_eigenshoes", "Number to display", 2, 12, 6))
    ),
    plotOutput("eigenshoes", height = 520),
    h3("Step 5. First K scores"),
    plotOutput("score_bars", height = 360),
    h3("Step 6. Rebuild with several K values"),
    plotOutput("rebuild_plot", height = 470),
    h3("Step 7. Cumulative explained variance across all 2,000 catalogue images"),
    fluidRow(
      column(
        4,
        # Shoe Search의 K와 동기화되어 어느 탭에서든 같은 값을 조절한다.
        sliderInput("K_explain", "Number of PCs (K)", 1, max_pcs, DEFAULT_K)
      )
    ),
    textOutput("variance_summary"),
    plotOutput("cumulative_variance", height = 360)
  ),

  # [탭 3] 카탈로그 밖에서 직접 촬영한 팀 신발로 일반화 성능을 확인한다.
  tabPanel(
    "Our Shoes",
    h2("Can the model recognize our real shoes?"),
    fluidRow(
      column(
        4,
        selectInput(
          "our_file", "Choose a team photo",
          setNames(our_info$file, paste(our_info$member, our_info$category, sep = " - "))
        ),
        h3(textOutput("our_status"))
      )
    ),
    # Shoe Search와 같은 네 패널로 선택 사진부터 Best match까지 비교한다.
    h3("Photo, average, reconstruction, and best match"),
    plotOutput("our_match_plot", height = 300),
    h3("Top 5"),
    tableOutput("our_top5"),
    h3("Category prediction accuracy by K"),
    p("Uses the selected Our Shoes photo at K = 1–100. Correct = 100%; incorrect = 0%."),
    textOutput("our_k_accuracy_summary"),
    plotOutput("our_k_accuracy_plot", height = 360),
    # 전체 팀 사진 결과는 선택 사진의 상세 결과 아래, 탭 맨 밑에 표시한다.
    h3("All team shoe results"),
    # 전체 팀 사진의 정확도도 갤러리와 함께 표시한다.
    h4(textOutput("team_accuracy")),
    p("Red = Incorrect | Green = Correct."),
    plotOutput("our_gallery", height = 620)
  ),
  # [탭 4] Project 1 Spotify 데이터 분석
  tabPanel(
    "Data 1",
    h2("Chart longevity and global reach"),
    # 데이터가 바뀌어도 설명의 곡 수가 실제 행 수와 일치하도록 표시한다.
    p(if (is.null(data1)) "Project 1 Spotify chart data." else sprintf(
      "A descriptive analysis of %s songs during their first 90 days on national Spotify charts.",
      format(nrow(data1), big.mark = ",")
    )),

    # Spotify 파일을 읽지 못했으면 안내만 표시하고 분석 입력은 만들지 않는다.
    if (is.null(data1)) {
      tagList(
        h3("Project 1 data is missing"),
        p("Add data1.csv beside app.R, then restart.")
      )
    } else tagList(
      fluidRow(
        column(
          3,
          h4("Inputs"),
          # 연도를 선택하면 아래 요약·표·그래프가 모두 같은 연도로 갱신된다.
          selectInput("data_year", "Entry year", data1_year_choices),
          # 활동 기간 그룹을 여러 개 선택할 수 있게 만든다.
          checkboxGroupInput(
            "data_duration", "Chart-active days",
            # cut()으로 만든 구간 이름을 선택지로 사용한다.
            choices = levels(data1$duration_group),
            # 처음에는 모든 활동 기간 구간을 선택한다.
            selected = levels(data1$duration_group)
          )
        ),
        column(
          9,
          h3("Do longer-charting songs reach more countries?"),
          p("Question: How does international reach differ across chart-duration groups?"),
          p(strong("Why it matters: "),
            "It shows whether chart persistence and international reach tend to appear together.")

        )
      ),

      h3("Overall summary"),
      # 필터를 적용한 전체 곡 수와 평균 진입 국가 수 카드를 표시한다.
      uiOutput("data_overview"),

      h3("Country reach distribution"),
      # 국가 도달 구간별 곡 수 막대그래프를 표시한다.
      plotOutput("data_reach_plot", height = 350),

      h3("Result in numbers"),
      # 활동 기간별 곡 수·평균 및 중앙 국가 수·10개국 이상 비율을 표시한다.
      tableOutput("data_summary"),

      h3("Country reach within each duration group"),
      # 각 활동 기간 안에서 국가 도달 구간이 차지하는 비율을 표시한다.
      plotOutput("data_plot", height = 430)
    )
  ),

  # [탭 5] AI 사용 내역, 윤리 문구, 팀원별 성찰을 표시한다.
  tabPanel(
    "AI",
    h2("AI disclosure and reflection"),
    h3("Ethics statement"),
    p("We understand the rules on data and AI use for this project, including following the license and privacy rules of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be academic misconduct. We certify that we have followed all of them. Any violation is our responsibility, and we agree to receive an F in this course if we have not."),
    # 설정에 적힌 팀원 이름을 쉼표로 이어 서명란에 표시한다.
    p(strong("Signed: "), paste(team_members, collapse = ", ")),
    h3("What AI did and what we did"),
    tableOutput("ai_table"),
    h3("Reflection"),
    p("The app was developed collaboratively through continuous team feedback and revision. During the second feedback stage, we discussed ideas in real-time meetings, divided the agreed changes among team members, and reviewed the updated results together."),
    tableOutput("ai_reflection")
  )
)

# ---- 6. Server: 입력이 바뀔 때 계산하고 화면을 갱신 -------------------------
# Shiny의 핵심 흐름:
# input 변경 -> selected() -> selected_x() -> result() -> 여러 output 자동 갱신
# input은 선택값, output은 화면에 보낼 결과, session은 현재 접속 세션이다.
# renderText/renderTable/renderPlot/renderUI는 각각 글·표·그림·HTML 출력을 만든다.
# req()는 입력 준비를 기다리고 validate(need())는 조건에 맞지 않을 때 안내를 표시한다.
# par(mfrow=...)는 그림 칸 수, mar는 여백, res는 출력 이미지 해상도이다.
# ggplot의 aes()는 축·색상 매핑, geom은 그림 요소, labs는 이름, theme은 모양을 정한다.
server <- function(input, output, session) {

  # 두 탭의 K 슬라이더를 동기화해 어느 쪽에서 바꿔도 모든 결과에 같은 K를 사용한다.
  observeEvent(input$K, {
    req(input$K_explain)
    if (as.integer(input$K_explain) != as.integer(input$K)) {
      updateSliderInput(session, "K_explain", value = input$K)
    }
  })

  observeEvent(input$K_explain, {
    req(input$K)
    if (as.integer(input$K) != as.integer(input$K_explain)) {
      updateSliderInput(session, "K", value = input$K_explain)
    }
  })

  # 사용자가 고른 사진의 경로, 실제 라벨, 제외할 id, 표시 이름을 통일된 list로 만든다.
  # reactive() 안에서 읽은 input이 바뀌면 이 코드가 자동으로 다시 실행된다.
  selected <- reactive({
    if (input$src == "catalogue") {
      # 카탈로그 id가 유효한지 검사하고 metadata에서 해당 행을 찾는다.
      id <- as.integer(input$cat_id)
      # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
      validate(need(!is.na(id) && id >= 1 && id <= 2000, "Choose id 1-2000."))
      row <- match(id, metadata$id)
      list(path = catalog_paths[row], label = metadata$category[row], exclude = id,
           name = sprintf("Catalogue %04d", id))
    } else if (input$src == "selfcheck") {
      # labels.csv에서 선택한 self-check 사진의 정답 카테고리를 찾는다.
      row <- match(input$sc_file, selfcheck_labels$photo)
      path <- file.path(selfcheck_dir, input$sc_file)
      # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
      validate(need(file.exists(path), "Self-check image not found."))
      list(path = path, label = selfcheck_labels$category[row], exclude = NA_integer_, name = input$sc_file)
    } else {
      # Upload를 선택한 직후에는 아직 파일이 없으므로 NULL을 반환한다.
      # 아래 반응형 계산들은 NULL을 확인하고 사진이 준비될 때까지 조용히 기다린다.
      if (is.null(input$photo)) return(NULL)

      # 파일이 선택되면 허용된 이미지 확장자인지 검사한다.
      # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
      validate(need(tolower(tools::file_ext(input$photo$name)) %in% c("jpg", "jpeg", "png"), "Upload JPG or PNG."))
      label <- if (nzchar(input$upload_label)) input$upload_label else NA_character_
      list(path = input$photo$datapath, label = label, exclude = NA_integer_, name = input$photo$name)
    }
  })

  # 선택 정보가 있을 때만 사진을 4,096개 픽셀 벡터로 변환한다.
  selected_x <- reactive({
    info <- selected()
    req(!is.null(info))
    read_shoe(info$path)
  })

  # 현재 K로 Top 5, 예측, 정답 여부를 한 번 계산해 여러 output에서 공유한다.
  result <- reactive({
    info <- selected()
    req(!is.null(info))
    find_matches(selected_x(), input$K, info$exclude, info$label)
  })

  # textOutput("prediction_card") 자리에 사진 이름, 예측, 정답 여부를 한 줄로 표시한다.
  output$prediction_card <- renderText({
    info <- selected()
    if (is.null(info)) {
      return("Please select a JPG or PNG image.")
    }

    r <- result()
    status <- if (is.na(r$correct)) {
      "Actual category is unknown"
    } else if (r$correct) {
      "Correct prediction"
    } else {
      paste("Incorrect - actual:", info$label)
    }
    paste(info$name, "| Prediction:", r$prediction, "|", status)
  })

  # 선택 사진, 평균, K개 PC 복원 사진, 가장 가까운 사진을 한 줄에 그린다.
  output$search_images <- renderPlot({
    x <- selected_x()
    r <- result()
    old <- par(mfrow = c(1, 4), mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    plot_shoe(x, "Selected photo")
    plot_shoe(catalog$mean, "Average shoe")
    plot_shoe(reconstruct(x, input$K), sprintf("Rebuilt (K=%d)", input$K))
    plot_shoe(read_shoe(r$top$path[1]), sprintf("Best: %04d (%s)", r$top$id[1], r$top$category[1]))
  }, res = 110)

  # 가장 가까운 신발 5개를 순서대로 보여준다.
  # 각 이미지 제목에는 순위, id, 카테고리, PCA 거리를 함께 표시한다.
  output$top5_images <- renderPlot({
    top <- result()$top
    old <- par(mfrow = c(1, 5), mar = c(1, 1, 6, 1), cex.main = 1.25)
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))

    for (i in 1:5) {
      image_title <- sprintf(
        "Rank %d: %04d\n%s\nDistance: %.3f",
        i, top$id[i], top$category[i], top$distance[i]
      )
      plot_shoe(read_shoe(top$path[i]), image_title)
    }
  }, res = 110)

  # Top 5 결과에서 화면에 필요한 id/category/distance만 일반 Shiny 표로 보여준다.
  output$top5 <- renderTable({
    shown <- result()$top[, c("id", "category", "distance")]
    # 거리순으로 정렬된 Top 5에 1~5위 순위 열을 추가한다.
    shown <- cbind(Rank = 1:5, shown)
    # 표시용 거리만 소수 셋째 자리까지 반올림하며 검색 순서에는 영향을 주지 않는다.
    shown$distance <- round(shown$distance, 3)
    shown
  }, striped = TRUE, spacing = "xs", width = "100%", rownames = FALSE)

  # Top 5 안에서 각 카테고리가 받은 표의 수를 보여준다.
  # 최종 예측은 이 표에서 가장 많은 표를 받은 카테고리이다.
  output$category_votes <- renderTable({
    vote_table <- data.frame(
      Category = categories,
      Votes = as.integer(result()$votes),
      Selected = ifelse(categories == result()$prediction, "Predicted category", "")
    )
    vote_table
  }, striped = TRUE, spacing = "xs", width = "100%", rownames = FALSE)

  # 동률인 경우에만 처리 기준을 표시하고, 일반 득표 안내 문장은 생략한다.
  output$vote_explanation <- renderText({
    r <- result()
    counts <- table(r$top$category)
    # 가장 많은 표를 받은 카테고리를 모두 찾는다. 동률이면 여러 개가 남는다.
    tied <- names(counts)[counts == max(counts)]
    if (length(tied) > 1) {
      sprintf(
        "Tie: %s each received %d votes. %s was selected because its closest photo ranks highest among the tied categories.",
        paste(tied, collapse = ", "), max(counts), r$prediction
      )
    } else {
      ""
    }
  })

  # ---- Explain PCA 탭의 출력 -----------------------------------------------
  # 전체 카탈로그의 픽셀별 평균 신발이다.
  output$mean_plot <- renderPlot({
    old <- par(mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    plot_shoe(catalog$mean, "Mean of 2,000 images")
  }, res = 110)
  output$difference_plot <- renderPlot({
    # 선택 사진 - 평균 신발: 선택 사진이 평균에서 어느 방향으로 다른지 보여준다.
    old <- par(mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    plot_shoe(selected_x() - catalog$mean, "Selected - average", TRUE)
  }, res = 110)

  # 카탈로그를 PC1-PC2 평면에 그리고 선택 사진을 별표로 표시한다.
  output$score_map <- renderPlot({
    # 업로드 사진이 아직 없으면 행렬 계산을 시작하지 않고 조용히 기다린다.
    req(!is.null(selected()))
    q <- query_scores(selected_x(), 2)
    d <- data.frame(PC1 = catalog$scores[, 1], PC2 = catalog$scores[, 2], Category = catalog$metadata$category)
    p <- ggplot(d, aes(PC1, PC2, color = Category)) + geom_point(alpha = .55, size = 1.7) +
      geom_point(data = data.frame(PC1 = q[1], PC2 = q[2]), aes(PC1, PC2), inherit.aes = FALSE,
                 shape = 8, size = 6, stroke = 1.4) +
      labs(x = sprintf("PC1 (%.1f%%)", 100 * catalog$explained[1]), y = sprintf("PC2 (%.1f%%)", 100 * catalog$explained[2]))
    print(p)
  }, res = 110)

  # rotation의 각 열을 64 x 64로 바꾸어 eigenshoe로 그린다.
  # eigenshoe는 해당 PC가 이미지의 어떤 변화를 포착하는지 보여준다.
  output$eigenshoes <- renderPlot({
    n <- input$n_eigenshoes
    # 한 줄에 최대 네 개의 이미지를 배치한다.
    cols <- min(4, n)
    old <- par(mfrow = c(ceiling(n / cols), cols), mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    for (i in seq_len(n)) {
      plot_shoe(
        catalog$rotation[, i],
        sprintf("PC%d (%.2f%%)", i, 100 * catalog$explained[i]),
        TRUE
      )
    }
  }, res = 110)

  # 선택 사진의 첫 K개 PCA 점수를 막대그래프로 표시한다.
  # 이 K개 숫자가 실제 거리 계산에 사용된다.
  output$score_bars <- renderPlot({
    # 업로드 사진이 아직 없으면 PCA 점수를 계산하지 않는다.
    req(!is.null(selected()))
    z <- query_scores(selected_x(), input$K)
    d <- data.frame(PC = seq_along(z), Score = z, Direction = ifelse(z >= 0, "Positive", "Negative"))
    print(ggplot(d, aes(PC, Score, fill = Direction)) + geom_col(width = .82) +
      geom_hline(yintercept = 0) +
      labs(x = "Principal component", y = "Score"))
  }, res = 110)

  # 서로 다른 K로 복원한 사진을 비교해 K의 영향을 시각화한다.
  output$rebuild_plot <- renderPlot({
    x <- selected_x()
    # 작은 K부터 최대 K까지 넓게 비교하고, 검증한 K=42와 현재 선택값도 포함한다.
    ks <- unique(sort(c(1L, 10L, 25L, 42L, 70L, 100L, as.integer(input$K))))
    # 계산해 둔 최대 PC 개수를 넘는 비교값은 제외한다.
    ks <- ks[ks <= max_pcs]
    # 원본 이미지 한 칸과 K별 복원 이미지 칸 수를 합한다.
    panels <- length(ks) + 1
    # 한 줄에 최대 네 개의 이미지를 배치한다.
    cols <- min(4, panels)
    old <- par(mfrow = c(ceiling(panels / cols), cols), mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    plot_shoe(x, "Original")
    for (k in ks) {
      plot_shoe(reconstruct(x, k), sprintf("K=%d", k))
    }
  }, res = 110)

  # 전체 픽셀의 총분산을 기준으로 하며, 저장한 PC 100개끼리 재정규화하지 않는다.
  # 전체 분산 대비 PC별 비율을 누적하고, 선택한 K의 위치를 강조한다.
  output$variance_summary <- renderText({
    # 슬라이더의 K값이 준비된 뒤 계산한다.
    req(input$K)
    # PC1부터 각 PC까지 설명 분산을 차례대로 더한다.
    cumulative <- cumsum(catalog$explained)
    sprintf("K = %d: cumulative explained variance = %.2f%%", input$K, 100 * cumulative[input$K])
  })

  # 선택한 K와 관계없이 전체 100개 PC의 누적 곡선을 그리고 현재 위치를 강조한다.
  output$cumulative_variance <- renderPlot({
    # 슬라이더의 K값이 준비된 뒤 계산한다.
    req(input$K)
    # 가로축의 PC 개수와 세로축의 누적 설명 분산을 표로 만든다.
    d <- data.frame(K = seq_along(catalog$explained), Variance = cumsum(catalog$explained))
    k <- as.integer(input$K)
    # 현재 K까지 누적한 설명 분산 비율 하나를 선택한다.
    fraction <- d$Variance[k]
    # 선택한 K 외에 표시할 기본 가로축 눈금이다.
    regular_ticks <- c(20, 40, 60, 80)
    # 현재 K와 8 미만으로 가까운 기본 눈금을 빼서 글자가 겹치지 않게 한다.
    ticks <- sort(unique(c(1, max_pcs, k, regular_ticks[abs(regular_ticks - k) >= 8])))
    print(
      ggplot(d, aes(K, Variance)) +
        geom_line(colour = "#3478A5", linewidth = 1) +
        # 현재 선택한 K의 위치에 세로 안내선을 그린다.
        geom_vline(xintercept = k, linetype = "dashed", colour = "#B64B24") +
        # 현재 K의 누적 설명 분산 높이에 가로 안내선을 그린다.
        geom_hline(yintercept = fraction, linetype = "dotted", colour = "#B64B24") +
        # 현재 K와 누적 비율이 만나는 지점을 점으로 표시한다.
        annotate("point", x = k, y = fraction, colour = "#B64B24", size = 3) +
        # 현재 K와 백분율을 점 위에 표시하되 그래프 위쪽을 넘지 않도록 위치를 제한한다.
        annotate("label", x = k, y = min(0.96, fraction + 0.09),
                 label = sprintf("K = %d | %.2f%%", k, 100 * fraction),
                 # 큰 K에서는 라벨을 왼쪽으로 펼쳐 오른쪽 경계를 벗어나지 않게 한다.
                 hjust = if (k > max_pcs / 2) 1 else 0, colour = "#B64B24") +
        # 계산한 PC 범위 안에서 가로축 눈금을 표시한다.
        scale_x_continuous(limits = c(1, max_pcs), breaks = ticks) +
        # 전체 분산 기준인 0~100% 범위를 유지한다. PC100에서 100%가 되도록 바꾸지 않는다.
        scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
        labs(x = "Number of principal components (K)", y = "Cumulative explained variance") +
        theme_minimal(base_size = 12)
    )
  }, res = 110)

  # ---- Our Shoes 탭의 반응형 계산과 출력 ----------------------------------
  # Our Shoes에서 선택한 팀 사진의 K별 정답 여부를 계산한다.
  our_k_accuracy <- reactive({
    info <- our_selected()
    validate(need(!is.na(info$category), "Choose a photo with a known category in Our Shoes."))
    x <- read_shoe(info$path)
    predicted <- character(max_pcs)
    correct <- logical(max_pcs)
    withProgress(message = "Evaluating Our Shoes photo for K = 1–100", value = 0, {
      for (k in seq_len(max_pcs)) {
        result <- find_matches(x, k, true_label = info$category)
        predicted[k] <- result$prediction
        correct[k] <- isTRUE(result$correct)
        incProgress(1 / max_pcs)
      }
    })
    data.frame(K = seq_len(max_pcs), Photo = info$file, Actual = info$category,
               Predicted = predicted, Accuracy = as.numeric(correct))
  })

  output$our_k_accuracy_summary <- renderText({
    d <- our_k_accuracy()
    req(input$K)
    current <- d[d$K == as.integer(input$K), ]
    sprintf("Photo: %s | Actual: %s | K = %d: predicted %s — %s (%.0f%%). Correct at %d of %d K values.",
            current$Photo, current$Actual, current$K, current$Predicted,
            if (current$Accuracy == 1) "Correct" else "Incorrect", 100 * current$Accuracy,
            sum(d$Accuracy == 1), nrow(d))
  })

  output$our_k_accuracy_plot <- renderPlot({
    d <- our_k_accuracy()
    req(input$K)
    current <- d[d$K == as.integer(input$K), ]
    correct <- d[d$Accuracy == 1, ]
    print(
      ggplot(d, aes(K, Accuracy)) +
        geom_step(colour = "#3478A5", linewidth = 1) +
        geom_point(data = correct, colour = "#238443", size = 2) +
        geom_vline(xintercept = input$K, linetype = "dashed", colour = "#B64B24") +
        geom_point(data = current, colour = "#B64B24", size = 3) +
        scale_x_continuous(limits = c(1, max_pcs), breaks = c(1, 20, 40, 60, 80, 100)) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 1),
                           labels = c("0% (Incorrect)", "100% (Correct)")) +
        labs(title = d$Photo[1], x = "Number of principal components (K)",
             y = "Selected photo accuracy",
             caption = "Green: correct prediction | Orange: selected K") +
        theme_minimal(base_size = 12)
    )
  }, res = 110)

  # 선택한 팀 사진에 해당하는 our_info 한 행을 반환한다.
  our_selected <- reactive({
    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(need(nrow(our_info) > 0, "Add team photos to our_shoes/ and restart."))
    row <- match(input$our_file, our_info$file)
    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(need(!is.na(row), "Choose a team photo."))
    our_info[row, , drop = FALSE]
  })

  # 같은 사진을 여러 번 읽고 검색하지 않도록 정보, 픽셀, 결과를 한 번에 묶는다.
  our_bundle <- reactive({
    info <- our_selected()
    x <- read_shoe(info$path)
    list(info = info, x = x, result = find_matches(x, input$K, true_label = info$category))
  })

  # 카테고리가 파일명에 적힌 팀 사진을 현재 K로 하나씩 예측한다.
  # 같은 결과를 정확도 문장과 전체 결과 표에서 함께 사용한다.
  team_results <- reactive({
    # 파일명에서 올바른 정답 종류를 얻은 팀 사진만 평가한다.
    labelled <- our_info[!is.na(our_info$category), , drop = FALSE]
    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(need(nrow(labelled) > 0, "No labelled team photos yet."))

    # 각 사진에 같은 전처리와 검색을 적용해 사진별 예측 결과를 만든다.
    rows <- lapply(seq_len(nrow(labelled)), function(i) {
      x <- read_shoe(labelled$path[i])
      # 현재 K로 Top 5를 검색하고 실제 종류와 예측을 비교한다.
      match_result <- find_matches(x, input$K, true_label = labelled$category[i])

      data.frame(
        Photo = labelled$file[i],
        Actual = labelled$category[i],
        Predicted = match_result$prediction,
        # 명확히 TRUE일 때만 정답으로 기록하고 나머지는 오답으로 기록한다.
        Result = if (isTRUE(match_result$correct)) "Correct" else "Incorrect",
        # 문자열을 factor로 자동 변환하지 않고 문자형 그대로 보관한다.
        stringsAsFactors = FALSE
      )
    })

    # 사진별 한 행짜리 표들을 위아래로 이어 전체 평가표를 만든다.
    do.call(rbind, rows)
  })

  # 맞힌 사진 수, 전체 사진 수, 백분율을 화면 상단에 한 줄로 표시한다.
  output$team_accuracy <- renderText({
    results <- team_results()
    # 정답으로 표시된 사진 수를 센다.
    correct_n <- sum(results$Result == "Correct")
    # 평가에 포함된 전체 사진 수이다.
    total_n <- nrow(results)
    # 정답 사진 수를 전체 사진 수로 나눈 뒤 100을 곱해 정확도(%)를 구한다.
    accuracy <- 100 * correct_n / total_n

    sprintf(
      "Team Test Accuracy (K=%d): %d/%d correct (%.1f%%)",
      input$K, correct_n, total_n, accuracy
    )
  })

  # 오답 사진을 먼저 놓고, 실제값과 예측값을 이미지 제목에 함께 표시한다.
  # 빨간 테두리는 오답, 초록 테두리는 정답을 뜻한다.
  output$our_gallery <- renderPlot({
    results <- team_results()
    # 오답(FALSE)을 먼저, 정답(TRUE)을 나중에 놓고 같은 결과 안에서는 파일명순으로 정렬한다.
    results <- results[order(results$Result == "Correct", results$Photo), ]
    n <- nrow(results)
    # 한 줄에 최대 네 개의 이미지를 배치한다.
    cols <- min(4, n)
    old <- par(
      mfrow = c(ceiling(n / cols), cols),
      mar = c(1, 1, 5, 1),
      cex.main = 1.25
    )
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))

    for (i in seq_len(n)) {
      # 평가표의 파일명에 해당하는 원본 사진 경로를 찾는다.
      photo_row <- match(results$Photo[i], our_info$file)
      # 정답에는 초록색, 오답에는 빨간색 테두리를 사용한다.
      border_colour <- if (results$Result[i] == "Correct") "darkgreen" else "firebrick3"
      image_title <- sprintf(
        "%s\n%s -> %s",
        results$Photo[i], results$Actual[i], results$Predicted[i]
      )

      plot_shoe(
        read_shoe(our_info$path[photo_row]),
        image_title
      )
      # 그림의 기존 테두리 위에 정오답 색상의 굵은 테두리를 덧그린다.
      box(col = border_colour, lwd = 4)
    }
  }, res = 105)

  # 선택 팀 사진의 예측과 정답 여부를 일반 텍스트로 표시한다.
  output$our_status <- renderText({
    b <- our_bundle()
    r <- b$result
    status <- if (is.na(r$correct)) {
      "Filename has no recognized category."
    } else if (r$correct) {
      "Correct prediction"
    } else {
      paste("Incorrect - actual:", b$info$category)
    }
    paste("Prediction:", r$prediction, "|", status)
  })

  # Shoe Search와 같은 순서로 팀 사진, 평균, 복원, Best match를 보여준다.
  output$our_match_plot <- renderPlot({
    b <- our_bundle()
    old <- par(mfrow = c(1, 4), mar = c(1, 1, 3, 1))
    # 이 출력 계산이 끝나면 이전 그래픽 설정을 복구해 다른 그림에 영향을 주지 않는다.
    on.exit(par(old))
    plot_shoe(b$x, "Our shoe")
    plot_shoe(catalog$mean, "Average shoe")
    plot_shoe(reconstruct(b$x, input$K), sprintf("Rebuilt (K=%d)", input$K))
    plot_shoe(
      read_shoe(b$result$top$path[1]),
      sprintf("Best: %04d (%s)", b$result$top$id[1], b$result$top$category[1])
    )
  }, res = 110)

  # 선택 팀 사진과 가까운 카탈로그 Top 5를 일반 Shiny 표로 표시한다.
  output$our_top5 <- renderTable({
    shown <- our_bundle()$result$top[, c("id", "category", "distance")]
    # 거리순으로 정렬된 Top 5에 1~5위 순위 열을 추가한다.
    shown <- cbind(Rank = 1:5, shown)
    # 표시용 거리만 소수 셋째 자리까지 반올림하며 검색 순서에는 영향을 주지 않는다.
    shown$distance <- round(shown$distance, 3)
    shown
  }, striped = TRUE, spacing = "xs", rownames = FALSE)

  # ---- Data 1 탭의 필터, 요약, 그래프 ----------------------

  # 연도나 활동 기간 선택이 바뀌면 필터 결과를 다시 계산한다.
  data_filtered <- reactive({
    # 연도 입력을 기다리고, 활동 기간을 모두 해제하면 안내를 표시한다.
    req(data1, input$data_year)
    validate(need(length(input$data_duration) > 0, "Select at least one duration group."))
    d <- data1
    # All이면 전체 데이터를 쓰고, 연도를 골랐을 때만 해당 곡을 남긴다.
    if (!identical(input$data_year, "All")) {
      d <- filter(d, entry_year == as.integer(input$data_year))
    }

    # 체크한 활동 기간 그룹에 속하는 곡만 남긴다.
    filter(d, duration_group %in% input$data_duration)
  })

  # 필터 적용 후 전체 곡 수와 평균 진입 국가 수
  output$data_overview <- renderUI({
    # 현재 연도·활동 기간 필터가 적용된 곡 목록을 가져온다.
    d <- data_filtered()

    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(
      need(nrow(d) > 0, "Select at least one duration group.")
    )

    fluidRow(
      column(
        6,
        wellPanel(
          h4("Total songs"),
          # 필터 결과의 행 수를 곡 수로 표시하고 천 단위 쉼표를 넣는다.
          h3(format(nrow(d), big.mark = ","))
        )
      ),
      column(
        6,
        wellPanel(
          h4("Mean countries entered"),
          # 결측값을 제외한 평균 진입 국가 수를 소수 둘째 자리까지 표시한다.
          h3(round(mean(d$countries_entered_90,
                        na.rm = TRUE), 2))
        )
      )
    )
  })

  # 국가 진입 수별 노래 개수
  output$data_reach_plot <- renderPlot({
    # 현재 연도·활동 기간 필터가 적용된 곡 목록을 가져온다.
    d <- data_filtered()

    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(
      need(nrow(d) > 0, "Select at least one duration group.")
    )

    # 파이프(|>)로 데이터를 다음 처리에 차례로 넘겨 구간별 곡 수를 계산한다.
    counts <- d |>
      # 국가 도달 구간이 없는 행은 그래프의 집계에서 제외한다.
      filter(!is.na(reach_group)) |>
      # 각 국가 도달 구간에 속한 곡 수를 Songs 열에 저장한다.
      count(reach_group, name = "Songs")

    print(
      # 국가 도달 구간을 가로축, 곡 수를 세로축으로 두고 구간별 색상을 지정한다.
      ggplot(counts, aes(x = reach_group,
                         y = Songs,
                         fill = reach_group)) +
        # 집계한 곡 수를 막대 높이로 표현한다.
        geom_col(width = 0.65) +
        # 두 그래프와 모든 필터에서 같은 국가 구간은 같은 색으로 표시한다.
        scale_fill_manual(values = c("1 country" = "#F8766D", "2-9 countries" = "#00BA38", "10+ countries" = "#619CFF"), drop = FALSE) +
        # 각 막대 위에 천 단위 쉼표가 있는 곡 수를 표시한다.
        geom_text(aes(label = scales::comma(Songs)),
                  vjust = -0.4, size = 4) +
        scale_y_continuous(
          labels = scales::comma,
          # 막대 위 숫자가 잘리지 않도록 세로축 위쪽에 12% 여백을 둔다.
          expand = expansion(mult = c(0, 0.12))
        ) +
        labs(
          x = "Countries entered",
          y = "Number of songs"
        ) +
        theme_minimal() +
        # 가로축에 이미 구간명이 있으므로 중복되는 범례를 숨긴다.
        theme(legend.position = "none")
    )
  }, res = 110)

  # 활동 기간별 요약 통계
  data_summary <- reactive({
    # 현재 연도·활동 기간 필터가 적용된 곡 목록을 가져온다.
    d <- data_filtered()

    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(
      need(nrow(d) > 0, "Select at least one duration group.")
    )

    d |>
      # 활동 기간 그룹별로 묶어 다음 요약 계산을 각각 수행한다.
      group_by(duration_group) |>
      summarise(
        # 해당 활동 기간 그룹에 포함된 곡 수를 센다.
        Songs = n(),
        # 결측값을 제외하고 그룹의 평균 진입 국가 수를 구한다.
        `Mean countries` = mean(countries_entered_90,
                                na.rm = TRUE),
        # 국가 수를 정렬했을 때 가운데에 해당하는 값인 중앙값을 구한다.
        `Median countries` = median(countries_entered_90,
                                    na.rm = TRUE),
        # 10개국 이상이면 TRUE(1), 아니면 FALSE(0)이므로 평균이 해당 곡의 비율이다.
        `Rate reaching 10+ countries` = mean(
          countries_entered_90 >= 10,
          na.rm = TRUE
        ),
        # 요약이 끝난 결과에서 dplyr의 그룹 묶음을 해제한다.
        .groups = "drop"
      ) |>
      # 표에 보여줄 활동 기간 열 이름을 읽기 쉬운 이름으로 바꾼다.
      rename(`Duration group` = duration_group)
  })

  # 반응형 요약 통계를 일반 Shiny 표로 변환한다.
  output$data_summary <- renderTable({
    # 계산된 요약 통계를 가져와 화면에 표시할 형식만 바꾼다.
    shown <- data_summary()

    # 평균 국가 수를 소수 첫째 자리까지 반올림한다.
    shown$`Mean countries` <- round(
      shown$`Mean countries`, 1
    )

    # 중앙값을 소수 첫째 자리까지 반올림한다.
    shown$`Median countries` <- round(
      shown$`Median countries`, 1
    )

    # 0~1 비율에 100을 곱하고 % 기호를 붙여 표시한다.
    shown$`Rate reaching 10+ countries` <- paste0(
      round(100 * shown$`Rate reaching 10+ countries`, 1),
      "%"
    )

    shown
  }, striped = TRUE, spacing = "xs", rownames = FALSE)

  # 활동 기간별 국가 진입 구간 비율 (기간별 합계 100%)
  output$data_plot <- renderPlot({
    # 현재 연도·활동 기간 필터가 적용된 곡 목록을 가져온다.
    d <- data_filtered()

    # need()의 조건을 만족하지 않으면 해당 출력 계산을 멈추고 안내를 표시한다.
    validate(
      need(nrow(d) > 0, "Select at least one duration group.")
    )

    # 세 구간 전체로 기간별 비율을 계산해 1개국도 분모에 포함한다.
    all_counts <- d |>
      filter(!is.na(reach_group)) |>
      mutate(
        duration_group = droplevels(duration_group),
        reach_group = factor(
          reach_group,
          levels = c("1 country", "2-9 countries", "10+ countries")
        )
      ) |>
      count(duration_group, reach_group, name = "Songs", .drop = FALSE) |>
      group_by(duration_group) |>
      mutate(share = Songs / sum(Songs)) |>
      ungroup()

    # 그래프에는 2–9개국과 10개국 이상만 표시하고 1개국 막대는 숨긴다.
    counts <- all_counts |>
      filter(reach_group %in% c("2-9 countries", "10+ countries")) |>
      mutate(reach_group = factor(
        reach_group,
        levels = c("2-9 countries", "10+ countries")
      ))

    validate(
      need(nrow(counts) > 0, "No songs have entered 2 or more countries.")
    )

    print(
      ggplot(
        counts,
        aes(x = duration_group,
            y = share,
            fill = reach_group)
      ) +
        # 각 기간 안에서 두 구간의 비율을 별도 막대로 나란히 표시한다.
        geom_col(position = position_dodge(width = 0.85), width = 0.75) +
        # 각 막대 위에 기간별 비율을 표시한다.
        geom_text(
          aes(label = ifelse(share > 0,
                             scales::percent(share, accuracy = 1),
                             "")),
          position = position_dodge(width = 0.85),
          vjust = -0.35,
          size = 4
        ) +
        # 첫 그래프의 기본 색상 순서(1개국, 2–9개국, 10개국 이상)에 맞춘다.
        scale_fill_manual(values = c(
          "2-9 countries" = "#00BA38",
          "10+ countries" = "#619CFF"
        )) +
        # 비율 눈금의 최댓값을 60%로 두고 라벨 공간을 위해 위쪽 여백을 둔다.
        scale_y_continuous(
          labels = scales::percent,
          limits = c(0, 0.6),
          expand = expansion(mult = c(0, 0.12))
        ) +
        labs(
          x = "Chart-active days",
          y = "Share of all songs (including 1-country songs)",
          fill = "Countries entered"
        ) +
        theme_minimal()
    )
  }, res = 110)

  # ---- AI 탭의 정적 표 ------------------------------------------------------
  # 팀이 실제로 수행한 AI 활용과 검토 내용을 표로 표시한다.
  output$ai_table <- renderTable(data.frame(
    Tab = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c(
      "AI generated the code for image preprocessing, PCA-based similarity search, Top 5 matching, and category prediction.",
      "AI generated the code for the required PCA visualizations in Steps 1–6 and the additional cumulative explained variance graph.",
      "AI generated the code for image preprocessing, PCA-based similarity search, Top 5 matching, category prediction, and team test accuracy visualization.",
      "AI generated the code for the Spotify data filters, summary cards and tables, and country reach charts. It also simplified unused code and ran automated checks."
    ),
    `What we did` = c(
      "We unpacked the end-to-end PCA code using the concepts learned in class and verified each stage. To make the prediction process easier to understand, we added Top 5 images, category-vote and tie-break explanations, reorganized the results, and compared K values using our team photos before choosing K=42 as the default.",
      "We followed the instructor’s requirements for Steps 1–6. After discussing visualization options with our teammates, we chose to add a cumulative explained variance graph to help users understand how much variation is retained as K increases.",
      "We collected 12 real shoe photos and tested K values from 1 to 100. Although K=42 achieved the highest category classification accuracy (83.3%), we discovered that some Best Matches looked visually different from the input shoes. Through this observation, we identified a limitation of the model, as classification accuracy alone does not fully reflect visual similarity.",
      "We defined the question and grouping criteria and adjusted the charts to compare country reach across duration groups. We kept all songs in the denominator when displaying the shares reaching two or more countries and interpreted the results as descriptive associations rather than causation."
    ), check.names = FALSE
  ))
  # 팀원별 AI 사용 성찰을 표로 표시한다.
  output$ai_reflection <- renderTable(data.frame(
    Member = team_members,
    # 각 팀원 이름에 맞는 성찰 내용을 선택한다.
    Reflection = ifelse(
      team_members == "Juwon Lee",
      paste(
        "AI was helpful at the beginning because it produced a working version of the Shoe Search code and connected preprocessing, PCA, distance calculation, and prediction all at once. However, because many steps were written together, I did not fully understand the role of each part at first. I went through the code again using what I had learned in class and checked how each step affected the output. We also felt that a table of IDs and distances was not enough to understand the result, so I added the Top 5 images and category votes. This made it easier to see why the model produced a particular prediction.",
        "One part I questioned was the explanation of K. AI initially made it sound as though using more principal components would generally improve performance, with increased computation being the main disadvantage. This did not match my understanding of PCA, since PCA preserves overall variation but does not directly try to separate categories. When we compared different K values using our team photos, the accuracy did not continuously improve as K increased. This helped me understand that explained variance, reconstruction quality, and classification accuracy are different. We chose K=42 because it performed best on our 12 photos, although I understand that this is only a provisional choice based on a small dataset. I learned that AI can provide a useful starting point, but I still need to question its explanations and check them against both course concepts and actual results.",
        sep = "\n\n"
      ),
      ifelse(
        team_members == "Moona Cho",
        paste(
          "AI helped me follow the PCA code and explain the Explain PCA tab. However, at K = 42, Explain PCA Step 7 showed 83.44%, while Our Shoes showed 83.3%, which initially made me question whether there was a calculation error. After reviewing the code, I understood that the first value measured the proportion of total variation retained across the catalogue images, while the second measured prediction accuracy on our team’s photos.",
          "This experience helped me understand that PCA preserves differences between images, but those differences do not necessarily help distinguish shoe categories. Since variation in background or lighting can also be retained, I learned to interpret explained variance and prediction accuracy separately. I also realized the importance of checking AI explanations against the code and course concepts, rather than relying on them alone, so that I can explain the results in my own words.",
          sep = "\n\n"
        ),
        ifelse(
          team_members == "Jiwon Hyung",
          paste(
            "AI helped me turn my ideas into working graphs quickly, but I still had to review how the results were presented. In the country reach comparison chart, the largest proportion was about 26 percent while the vertical axis extended to 100 percent, so I adjusted the scale to make the differences clearer. AI also added several checks and conditions when a simple install.packages() call would have been enough for our setup. I went through these steps and removed unnecessary code, which taught me that more complicated code is not always better. While preparing the shoe photos, I also noticed that their orientation could affect predictions, although I could not align every photo consistently.",
            "Working on Data 1 helped me understand why the mean and median should be considered together. Average country reach increased across duration groups, but the median remained one country in every group, showing that a higher average does not necessarily describe the typical song. I also learned that what we display in a chart can differ from what we include in the calculation. We hid songs reaching only one country in the comparison chart but kept them in the denominator so the bars still represented shares of all songs in each group. Through this experience, I learned that AI can make implementation faster, but clear presentation and accurate interpretation still depend on my own judgment.",
            sep = "\n\n"
          ),
          ifelse(
            team_members == "Seungyeon Yoo",
          paste(
            "AI was helpful in developing our R Shiny application, especially for writing code for image preprocessing, shoe search using PCA, and data visualization. However, some of the code generated by AI had errors, so we had to fix them through testing and debugging. I also worked on resolving file path and package compatibility issues. For Data 1, we came up with our own research question, decided how to group the Spotify data, and chose graphs and statistics to make the comparisons clear and meaningful.",
            "Through this project, I learned that getting the code to run successfully does not necessarily mean the model performs well. We tested different K values from 1 to 100 using 12 shoe photos and found that K=42 correctly classified 10 of them. This showed me that classification accuracy alone cannot fully capture visual similarity. Working on Data 1 also taught me that choosing the right visualizations and interpreting the results are just as important as writing code. In the end, I learned the importance of solving problems independently, checking results from AI, and making my own analytical decisions rather than simply trusting the output.",
            sep = "\n\n"
          ),
            ifelse(
              team_members == "Kamelia Daddi",
              "Due to illness, I was unable to participate in this assignment.",
              ""
            )
          )
        )
      )
    ),
    # 공백이나 문장부호가 있는 표의 열 이름을 R이 임의로 바꾸지 않게 한다.
    check.names = FALSE
  ))
}

# 위에서 정의한 화면(ui)과 계산 로직(server)을 결합해 Shiny 앱 객체를 만든다.
shinyApp(ui, server)
  
