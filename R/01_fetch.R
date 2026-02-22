# =============================================================================
# R/01_fetch.R
# Census API 데이터 수집 함수 및 변수 사전
# =============================================================================
# 사용 패키지: tidycensus, arrow, tigris, sf, glue, dplyr
# =============================================================================

# -----------------------------------------------------------------------------
# 변수 사전 (Variable Dictionary)
# -----------------------------------------------------------------------------
# ACS 변수 코드 목록: https://api.census.gov/data/2023/acs/acs5/variables.html
# 또는 R에서: tidycensus::load_variables(2023, "acs5", cache = TRUE)

# 기본 변수셋 — 모든 분석에 공통 사용
VARS_CORE <- c(
  pop_total        = "B01003_001",  # 총 인구
  median_hh_income = "B19013_001",  # 가구 중위 소득 (달러)
  med_age          = "B01002_001",  # 중위 연령

  # 학력 — 학사 학위 소지자 수 (B15003: 25세 이상 교육 수준)
  bach_degree      = "B15003_022",  # 학사(Bachelor's) 졸업자 수

  # 25-44세 연령대 (성별 분리) — 핵심 소비층
  male_25_29       = "B01001_010",  female_25_29 = "B01001_034",
  male_30_34       = "B01001_011",  female_30_34 = "B01001_035",
  male_35_39       = "B01001_012",  female_35_39 = "B01001_036",
  male_40_44       = "B01001_013",  female_40_44 = "B01001_037"
)

# 확장 변수셋 — 유아용품 프로파일에 추가로 필요한 변수
VARS_EXTENDED <- c(
  VARS_CORE,
  # 0-4세 영유아 인구 (B01001: 성별·연령별 인구)
  pop_under5_male   = "B01001_003",   # 남아 0-4세
  pop_under5_female = "B01001_027"    # 여아 0-4세
)

# -----------------------------------------------------------------------------
# 환경 초기화 함수
# -----------------------------------------------------------------------------
setup_census <- function(
  api_key  = Sys.getenv("CENSUS_API_KEY"),
  use_cache = TRUE
) {
  # 필수 패키지 로드
  if (!require("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(
    tidycensus, dplyr, tidyr, stringr, janitor,
    ggplot2, sf, tigris, viridis, scales,
    arrow, DT, plotly, leaflet,
    glue, purrr, forcats, ggrepel, tibble
  )

  # Census API 키 등록
  if (nchar(api_key) == 0) {
    stop(
      "Census API 키가 설정되지 않았습니다.\n",
      ".Renviron 파일에 다음 줄을 추가하세요:\n",
      "CENSUS_API_KEY=<YOUR_API_KEY>\n",
      "API 키 발급: https://api.census.gov/data/key_signup.html"
    )
  }
  census_api_key(api_key, install = FALSE, overwrite = TRUE)

  # tigris 캐시 설정 (경계 shapefile 재다운로드 방지)
  options(tigris_use_cache = use_cache)

  # 데이터 저장 디렉토리 생성
  dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

  message("Census 환경 초기화 완료.")
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# ACS 카운티 데이터 수집 함수
# -----------------------------------------------------------------------------
#' @param year       분석 연도 (기본값: 2023)
#' @param vars       변수 벡터 (VARS_CORE 또는 VARS_EXTENDED)
#' @param survey     서베이 종류 ("acs5" = 5년 추정치, "acs1" = 1년)
#' @param state      주 필터: NULL = 전국, "CA" = 캘리포니아, c("CA","NY") = 복수
#' @param cache_path Parquet 캐시 파일 경로 (NULL = 캐시 미사용)
#' @return data.frame (wide 형식, {varname}E = 추정치, {varname}M = 오차범위)
fetch_acs_county <- function(
  year       = 2023,
  vars       = VARS_CORE,
  survey     = "acs5",
  state      = NULL,
  cache_path = NULL
) {
  # 캐시 우선: 이미 저장된 데이터가 있으면 재사용 (API 호출 생략)
  if (!is.null(cache_path) && file.exists(cache_path)) {
    message(glue("[캐시] {cache_path} 에서 로드합니다."))
    return(arrow::read_parquet(cache_path))
  }

  state_label <- if (is.null(state)) "전국" else paste(state, collapse = ", ")
  message(glue("[API] Census ACS {survey} ({year}년) 카운티 데이터 수집 중 [{state_label}]..."))
  message("  → 첫 실행 시 1-3분 소요될 수 있습니다.")

  df <- tidycensus::get_acs(
    geography   = "county",
    variables   = vars,
    year        = year,
    survey      = survey,
    state       = state,
    geometry    = FALSE,     # 지도 데이터는 fetch_geo_county()에서 별도 수집
    output      = "wide",    # 변수별 컬럼 분리 ({var}E, {var}M)
    cache_table = TRUE       # 변수 메타데이터 캐시 (재실행 시 속도 향상)
  )

  # 캐시 저장
  if (!is.null(cache_path)) {
    arrow::write_parquet(df, cache_path)
    message(glue("[저장] {cache_path} 에 캐시 저장 완료."))
  }

  df
}

# -----------------------------------------------------------------------------
# 지도용 geometry 데이터 수집 함수
# -----------------------------------------------------------------------------
#' @param year       지리 경계 기준 연도 (기본값: 2023)
#' @param resolution 지도 해상도: "500k" (고해상도), "5m" (중간), "20m" (저해상도, 기본)
#' @return sf 객체 (Albers Equal Area 투영, Alaska/Hawaii 본토 배치 포함)
fetch_geo_county <- function(year = 2023, resolution = "20m") {
  cache_path <- glue("data/raw/geo_county_{year}_{resolution}.rds")

  if (file.exists(cache_path)) {
    message(glue("[캐시] 지도 데이터 로드: {cache_path}"))
    return(readRDS(cache_path))
  }

  message("[API] 카운티 경계 shapefile 다운로드 중...")
  geo <- tigris::counties(
    cb         = TRUE,          # Cartographic Boundary (용량 작음)
    resolution = resolution,
    year       = year
  ) |>
    tigris::shift_geometry() |>  # Alaska, Hawaii를 본토 아래로 자동 이동
    sf::st_transform(crs = 5070) # Albers Equal Area (미국 지도 표준 투영법)

  saveRDS(geo, cache_path)
  message(glue("[저장] 지도 캐시 저장: {cache_path}"))

  geo
}
