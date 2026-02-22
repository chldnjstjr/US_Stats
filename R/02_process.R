# =============================================================================
# R/02_process.R
# ACS 원본 데이터 전처리 및 파생변수 생성
# =============================================================================
# 입력: fetch_acs_county() 반환값 (wide 형식 data.frame)
# 출력: 분석 준비 완료된 표준화 data.frame
# =============================================================================

# -----------------------------------------------------------------------------
# 핵심 전처리 함수
# -----------------------------------------------------------------------------
#' @param df_wide  fetch_acs_county() 결과 (wide 형식)
#' @param year     분석 연도 (메타 컬럼에 기록)
#' @param extended VARS_EXTENDED 사용 여부 (영유아 변수 포함 여부)
#' @return 파생변수 포함 표준화 data.frame
process_county_data <- function(df_wide, year = 2023, extended = FALSE) {
  # get_acs(output="wide") 컬럼명 패턴: {varname}E (추정치), {varname}M (오차범위)
  # janitor::clean_names()로 소문자 + 언더스코어 통일
  df <- df_wide |>
    janitor::clean_names()

  result <- df |>
    dplyr::transmute(
      # --- 식별자 ---
      geoid  = geoid,
      name   = name,

      # 주(State)와 카운티(County) 이름 분리
      # 예: "Los Angeles County, California" → state="California", county="Los Angeles County"
      state  = stringr::str_extract(name, "(?<=, ).+"),
      county = stringr::str_extract(name, ".+(?=,)"),

      # --- 핵심 인구통계 (추정치 컬럼 선택) ---
      pop_total        = pop_total_e,
      median_hh_income = median_hh_income_e,
      med_age          = med_age_e,
      bach_count       = bach_degree_e,        # 학사 학위 소지자 수 (절대값)

      # --- 25-44세 핵심 소비층 인구 합산 ---
      # 남성: 25-29, 30-34, 35-39, 40-44 / 여성: 동일 연령대
      pop_25_44 = dplyr::coalesce(male_25_29_e, 0) +
                  dplyr::coalesce(female_25_29_e, 0) +
                  dplyr::coalesce(male_30_34_e, 0) +
                  dplyr::coalesce(female_30_34_e, 0) +
                  dplyr::coalesce(male_35_39_e, 0) +
                  dplyr::coalesce(female_35_39_e, 0) +
                  dplyr::coalesce(male_40_44_e, 0) +
                  dplyr::coalesce(female_40_44_e, 0),

      # --- 파생 비율 변수 ---
      edu_ratio   = dplyr::if_else(pop_total > 0, bach_count / pop_total, NA_real_),
      share_25_44 = dplyr::if_else(pop_total > 0, pop_25_44 / pop_total,  NA_real_),

      # --- 영유아 인구 (VARS_EXTENDED 사용 시만 유효) ---
      pop_under5 = dplyr::if_else(
        extended,
        dplyr::coalesce(pop_under5_male_e, 0) + dplyr::coalesce(pop_under5_female_e, 0),
        NA_real_
      ),
      share_under5 = dplyr::if_else(
        extended && pop_total > 0,
        pop_under5 / pop_total,
        NA_real_
      ),

      # --- 메타 ---
      data_year = year,
      source    = "ACS5"
    ) |>
    dplyr::filter(
      !is.na(pop_total),
      !is.na(median_hh_income),
      pop_total > 1000          # 인구 1천 미만 극소 지역 제외 (통계 신뢰도)
    )

  message(glue::glue(
    "[전처리 완료] {nrow(result)}개 카운티 | ",
    "결측 제거: {nrow(df) - nrow(result)}개 | ",
    "총 인구 커버리지: {scales::comma(sum(result$pop_total, na.rm = TRUE))}명"
  ))

  result
}

# -----------------------------------------------------------------------------
# 정규화 함수 (점수 계산에 사용)
# -----------------------------------------------------------------------------

#' 백분위 정규화 (0~1) — 이상치에 강건 (기본 추천)
#' percent_rank(): 각 값의 백분위 순위를 0~1로 변환
normalize_percentile <- function(x) {
  dplyr::percent_rank(x)
}

#' Min-Max 정규화 (0~1) — 이상치 영향 받을 수 있음
normalize_minmax <- function(x, na.rm = TRUE) {
  rng <- range(x, na.rm = na.rm)
  if (diff(rng) == 0) return(rep(0.5, length(x)))  # 모든 값이 동일한 경우
  (x - rng[1]) / (rng[2] - rng[1])
}

# -----------------------------------------------------------------------------
# 데이터 품질 요약 함수
# -----------------------------------------------------------------------------
summarize_data_quality <- function(df) {
  cat("=== 데이터 품질 요약 ===\n")
  cat(glue::glue("총 카운티 수: {nrow(df)}\n"))
  cat(glue::glue("분석 연도: {unique(df$data_year)}\n\n"))

  cat("--- 핵심 변수 요약 ---\n")
  df |>
    dplyr::summarise(
      dplyr::across(
        c(pop_total, median_hh_income, edu_ratio, share_25_44),
        list(
          최소 = \(x) min(x, na.rm = TRUE),
          중위 = \(x) median(x, na.rm = TRUE),
          최대 = \(x) max(x, na.rm = TRUE),
          결측 = \(x) sum(is.na(x))
        )
      )
    ) |>
    tidyr::pivot_longer(
      everything(),
      names_to  = c("변수", ".value"),
      names_sep = "_(?=[^_]+$)"
    ) |>
    dplyr::mutate(
      중위 = dplyr::case_when(
        grepl("income", 변수) ~ scales::dollar(중위),
        grepl("ratio|share", 변수) ~ scales::percent(중위, accuracy = 0.1),
        TRUE ~ scales::comma(중위)
      )
    ) |>
    print()
}
