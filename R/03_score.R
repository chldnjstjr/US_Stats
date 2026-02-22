# =============================================================================
# R/03_score.R
# 산업 프로파일 기반 시장 매력도 점수 계산 엔진
# =============================================================================
# 입력: process_county_data() 결과 + INDUSTRY_PROFILES에서 선택한 프로파일
# 출력: 시장 점수(0-100) 및 등급(A-E)이 추가된 data.frame
# =============================================================================

# -----------------------------------------------------------------------------
# 시장 매력도 점수 계산 함수
# -----------------------------------------------------------------------------
#' @param df      process_county_data() 결과
#' @param profile INDUSTRY_PROFILES[[선택한_프로파일_키]]
#' @param method  정규화 방법: "percentile" (기본, 이상치 강건) | "minmax"
#' @return 점수 컬럼이 추가된 data.frame, market_score_100 기준 내림차순 정렬
compute_market_score <- function(
  df,
  profile,
  method = c("percentile", "minmax")
) {
  method <- match.arg(method)

  norm_fn <- if (method == "percentile") normalize_percentile else normalize_minmax

  # 영유아 변수가 없는 경우 기본값 0으로 처리
  has_under5 <- !all(is.na(df$share_under5))

  scored <- df |>
    dplyr::mutate(
      # -----------------------------------------------------------------------
      # 각 지표를 0~1 범위로 정규화
      # -----------------------------------------------------------------------
      s_income  = norm_fn(median_hh_income),
      s_pop     = norm_fn(log1p(pop_total)),    # 로그 변환: 인구 분포의 극단적 왜도 보정
      s_edu     = norm_fn(dplyr::coalesce(edu_ratio,   0)),
      s_young   = norm_fn(dplyr::coalesce(share_25_44, 0)),
      s_under5  = if (has_under5) norm_fn(dplyr::coalesce(share_under5, 0)) else 0,

      # -----------------------------------------------------------------------
      # 프로파일 가중치 적용 → 시장 점수 계산
      # -----------------------------------------------------------------------
      market_score = (
        profile$w_income * s_income  +
        profile$w_pop    * s_pop     +
        profile$w_edu    * s_edu     +
        profile$w_young  * s_young   +
        profile$w_under5 * s_under5
      ),

      # 0-100 스케일로 변환 (percentile 방법의 경우 이미 0~1이므로 100 곱함)
      market_score_100 = round(market_score * 100, 1),

      # 등급 부여 (A: 상위 20%, B: 21-40%, ... E: 하위 20%)
      grade = dplyr::case_when(
        market_score_100 >= 80 ~ "A",
        market_score_100 >= 60 ~ "B",
        market_score_100 >= 40 ~ "C",
        market_score_100 >= 20 ~ "D",
        TRUE                   ~ "E"
      ),

      # 분석에 사용된 프로파일 이름 기록
      profile_used = profile$name_ko
    ) |>
    dplyr::arrange(dplyr::desc(market_score_100))

  # 상위 결과 요약 출력
  top5 <- head(scored, 5)
  message(glue::glue("\n[점수 계산 완료] 프로파일: {profile$name_ko} ({profile$name_en})"))
  message(glue::glue("  → Top 5: {paste(top5$county, top5$state, sep=', ', collapse=' | ')}"))

  scored
}

# -----------------------------------------------------------------------------
# Top N 지역 필터링 함수
# -----------------------------------------------------------------------------
#' @param scored_df compute_market_score() 결과
#' @param n         반환할 상위 지역 수 (기본: 30)
#' @param state_filter 특정 주만 보려면 주 이름 벡터 전달, NULL = 전체
#' @return Top N 필터링된 data.frame
get_top_counties <- function(scored_df, n = 30, state_filter = NULL) {
  df <- scored_df

  if (!is.null(state_filter)) {
    df <- dplyr::filter(df, state %in% state_filter)
  }

  head(df, n) |>
    dplyr::mutate(rank = dplyr::row_number(), .before = geoid)
}

# -----------------------------------------------------------------------------
# 점수 구성 요소 분해 함수 (Top N 지역의 요인별 기여도)
# -----------------------------------------------------------------------------
#' @param scored_df compute_market_score() 결과
#' @param profile   분석에 사용된 프로파일
#' @param n         분석할 상위 지역 수
#' @return long 형식 data.frame (레이더/스택 차트 입력용)
decompose_score_components <- function(scored_df, profile, n = 10) {
  top_df <- head(scored_df, n)

  top_df |>
    dplyr::select(rank, county, state,
                  s_income, s_pop, s_edu, s_young, s_under5) |>
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("s_"),
      names_to  = "component",
      values_to = "normalized_score"
    ) |>
    dplyr::mutate(
      component_label = dplyr::case_match(
        component,
        "s_income" ~ glue::glue("소득 (×{profile$w_income})"),
        "s_pop"    ~ glue::glue("인구 (×{profile$w_pop})"),
        "s_edu"    ~ glue::glue("교육 (×{profile$w_edu})"),
        "s_young"  ~ glue::glue("청년층 (×{profile$w_young})"),
        "s_under5" ~ glue::glue("영유아 (×{profile$w_under5})")
      ),
      weighted_contribution = normalized_score * dplyr::case_match(
        component,
        "s_income" ~ profile$w_income,
        "s_pop"    ~ profile$w_pop,
        "s_edu"    ~ profile$w_edu,
        "s_young"  ~ profile$w_young,
        "s_under5" ~ profile$w_under5
      )
    )
}
