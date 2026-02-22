# =============================================================================
# R/04_visualize.R
# 시각화 함수 모음 (ggplot2 + plotly + leaflet + DT)
# =============================================================================
# 모든 함수는 ggplot2 정적 차트 → plotly::ggplotly()로 인터랙티브 변환
# 지도는 leaflet으로 별도 구현 (카운티 클릭 시 상세 팝업)
# =============================================================================

# -----------------------------------------------------------------------------
# [1] Top N 수평 막대 차트
# -----------------------------------------------------------------------------
#' 시장 점수 상위 N개 카운티를 수평 막대 차트로 시각화
#' @param scored_df compute_market_score() 결과
#' @param n         표시할 상위 지역 수 (기본: 20)
#' @param profile   분석에 사용된 프로파일 (제목용)
#' @param interactive TRUE = plotly 인터랙티브 차트, FALSE = ggplot2 정적
plot_top_counties <- function(scored_df, n = 20, profile, interactive = TRUE) {
  top_df <- get_top_counties(scored_df, n = n) |>
    dplyr::mutate(
      label     = glue::glue("{county}\n{state}"),
      label     = forcats::fct_reorder(label, market_score_100),
      grade_color = dplyr::case_match(
        grade,
        "A" ~ "#1a9641", "B" ~ "#a6d96a",
        "C" ~ "#fdae61", "D" ~ "#f46d43", "E" ~ "#d73027"
      )
    )

  p <- ggplot2::ggplot(
    top_df,
    ggplot2::aes(
      x    = market_score_100,
      y    = label,
      fill = grade,
      text = glue::glue(
        "<b>{county}, {state}</b><br>",
        "시장 점수: {market_score_100}점 (등급: {grade})<br>",
        "중위 소득: {scales::dollar(median_hh_income)}<br>",
        "인구: {scales::comma(pop_total)}<br>",
        "25-44세 비율: {scales::percent(share_25_44, accuracy = 0.1)}<br>",
        "대졸 비율: {scales::percent(edu_ratio, accuracy = 0.1)}"
      )
    )
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = market_score_100),
      hjust = -0.2, size = 3, color = "gray30"
    ) +
    ggplot2::scale_fill_manual(
      values = c("A" = "#1a9641", "B" = "#a6d96a",
                 "C" = "#fdae61", "D" = "#f46d43", "E" = "#d73027"),
      name   = "등급"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 105),
      labels = scales::number_format(suffix = "점")
    ) +
    ggplot2::labs(
      title    = glue::glue("【{profile$name_ko}】 시장 매력도 Top {n}"),
      subtitle = glue::glue("{profile$desc_ko}"),
      x        = "시장 매력도 점수 (0-100)",
      y        = NULL,
      caption  = "출처: US Census Bureau ACS 5-Year Estimates | 분석: Census Market Framework"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "gray50"),
      legend.position = "bottom"
    )

  if (interactive) {
    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "h", y = -0.2))
  } else {
    p
  }
}

# -----------------------------------------------------------------------------
# [2] 소득 vs. 핵심 소비층 버블 차트 (산점도)
# -----------------------------------------------------------------------------
#' @param scored_df  compute_market_score() 결과
#' @param x_var      x축 변수 (기본: "median_hh_income")
#' @param y_var      y축 변수 (기본: "share_25_44")
#' @param top_n_label 라벨을 표시할 상위 N개 수
#' @param interactive TRUE = plotly 인터랙티브
plot_scatter <- function(
  scored_df,
  x_var         = "median_hh_income",
  y_var         = "share_25_44",
  top_n_label   = 15,
  interactive   = TRUE
) {
  axis_labels <- list(
    median_hh_income = "가구 중위 소득 (달러)",
    share_25_44      = "25-44세 인구 비율",
    edu_ratio        = "대졸 이상 비율",
    share_under5     = "영유아(0-4세) 비율",
    pop_total        = "총 인구"
  )

  df_plot <- scored_df |>
    dplyr::mutate(
      label_flag = dplyr::row_number() <= top_n_label,
      short_name = glue::glue("{county}\n({state})")
    )

  # 중위값 기준선 (사분면 구분)
  x_med <- median(scored_df[[x_var]], na.rm = TRUE)
  y_med <- median(scored_df[[y_var]], na.rm = TRUE)

  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x    = .data[[x_var]],
      y    = .data[[y_var]],
      size = pop_total,
      color = market_score_100,
      text = glue::glue(
        "<b>{county}, {state}</b><br>",
        "시장 점수: {market_score_100}점 (등급: {grade})<br>",
        "중위 소득: {scales::dollar(median_hh_income)}<br>",
        "인구: {scales::comma(pop_total)}<br>",
        "25-44세: {scales::percent(share_25_44, 0.1)}<br>",
        "대졸 비율: {scales::percent(edu_ratio, 0.1)}"
      )
    )
  ) +
    ggplot2::geom_vline(xintercept = x_med, linetype = "dashed", color = "gray70", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = y_med, linetype = "dashed", color = "gray70", alpha = 0.7) +
    ggplot2::geom_point(alpha = 0.6) +
    ggrepel::geom_text_repel(
      data    = dplyr::filter(df_plot, label_flag),
      ggplot2::aes(label = county),
      size    = 2.8,
      color   = "gray20",
      max.overlaps = 20
    ) +
    ggplot2::scale_color_viridis_c(
      name   = "시장 점수",
      option = "plasma",
      guide  = ggplot2::guide_colorbar(barwidth = 8, barheight = 0.5)
    ) +
    ggplot2::scale_size_continuous(
      name   = "인구",
      range  = c(1, 10),
      labels = scales::comma
    ) +
    ggplot2::scale_x_continuous(
      labels = if (x_var == "median_hh_income") scales::dollar else scales::percent
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title    = "소득 수준 vs. 핵심 소비층 비율",
      subtitle = "버블 크기 = 총 인구 | 색상 = 시장 점수 | 점선 = 전국 중위값",
      x        = axis_labels[[x_var]],
      y        = axis_labels[[y_var]],
      caption  = "출처: US Census Bureau ACS 5-Year Estimates"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical"
    )

  if (interactive) {
    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(orientation = "h"))
  } else {
    p
  }
}

# -----------------------------------------------------------------------------
# [3] 전국 코로플레스 지도 (leaflet 인터랙티브)
# -----------------------------------------------------------------------------
#' @param scored_df compute_market_score() 결과
#' @param geo_sf    fetch_geo_county() 결과 (sf 객체)
#' @param fill_var  색상 기준 변수 (기본: "market_score_100")
#' @param profile   분석에 사용된 프로파일 (범례 제목용)
plot_map_leaflet <- function(
  scored_df,
  geo_sf,
  fill_var = "market_score_100",
  profile
) {
  # GEOID 기준으로 점수 데이터와 지리 데이터 조인
  map_data <- geo_sf |>
    dplyr::left_join(
      scored_df |> dplyr::select(geoid, county, state, market_score_100, grade,
                                  median_hh_income, pop_total, edu_ratio,
                                  share_25_44, share_under5),
      by = c("GEOID" = "geoid")
    )

  # 색상 팔레트 (viridis plasma)
  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain  = map_data$market_score_100,
    na.color = "#cccccc"
  )

  # 팝업 내용
  popup_content <- glue::glue_data(
    map_data,
    "<b>{county}, {state}</b><br>",
    "<hr style='margin:4px 0'>",
    "시장 점수: <b>{market_score_100}점</b> (등급: {grade})<br>",
    "중위 소득: {scales::dollar(median_hh_income)}<br>",
    "총 인구: {scales::comma(pop_total)}<br>",
    "25-44세 비율: {scales::percent(share_25_44, 0.1)}<br>",
    "대졸 비율: {scales::percent(edu_ratio, 0.1)}"
  )

  leaflet::leaflet(map_data) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor   = ~pal(market_score_100),
      fillOpacity = 0.75,
      color       = "white",
      weight      = 0.5,
      opacity     = 0.8,
      popup       = popup_content,
      highlight   = leaflet::highlightOptions(
        weight      = 2,
        color       = "#333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      pal      = pal,
      values   = ~market_score_100,
      position = "bottomright",
      title    = glue::glue("{profile$name_ko}<br>시장 점수"),
      labFormat = leaflet::labelFormat(suffix = "점")
    ) |>
    leaflet::addControl(
      html     = glue::glue("<b>【{profile$name_ko}】</b> 미국 카운티별 시장 매력도"),
      position = "topright"
    )
}

# -----------------------------------------------------------------------------
# [4] 인터랙티브 결과 테이블 (DT)
# -----------------------------------------------------------------------------
#' @param scored_df compute_market_score() 결과
#' @param n         표시할 상위 지역 수
plot_results_table <- function(scored_df, n = 50) {
  top_df <- get_top_counties(scored_df, n = n) |>
    dplyr::select(
      순위       = rank,
      카운티     = county,
      주         = state,
      점수       = market_score_100,
      등급       = grade,
      중위소득   = median_hh_income,
      총인구     = pop_total,
      `25-44세` = share_25_44,
      대졸비율   = edu_ratio
    ) |>
    dplyr::mutate(
      중위소득 = scales::dollar(중위소득),
      총인구   = scales::comma(총인구),
      `25-44세` = scales::percent(`25-44세`, accuracy = 0.1),
      대졸비율 = scales::percent(대졸비율, accuracy = 0.1)
    )

  DT::datatable(
    top_df,
    options = list(
      pageLength  = 20,
      scrollX     = TRUE,
      dom         = "lfrtip",
      language    = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/ko.json")
    ),
    rownames = FALSE,
    filter   = "top",
    class    = "stripe hover compact"
  ) |>
    DT::formatStyle(
      "등급",
      backgroundColor = DT::styleEqual(
        c("A", "B", "C", "D", "E"),
        c("#1a9641", "#a6d96a", "#fdae61", "#f46d43", "#d73027")
      ),
      color = DT::styleEqual(
        c("A", "B", "C", "D", "E"),
        c("white", "white", "black", "white", "white")
      )
    ) |>
    DT::formatStyle(
      "점수",
      background = DT::styleColorBar(c(0, 100), "#4e9ac7"),
      backgroundSize = "100% 80%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
}

# -----------------------------------------------------------------------------
# [5] 주별 Top 카운티 집계 차트 (State-level 드릴업)
# -----------------------------------------------------------------------------
#' @param scored_df compute_market_score() 결과
#' @param n_state   표시할 주 수 (기본: 15)
#' @param metric    집계 기준: "mean" | "max" | "count_A" (A등급 수)
plot_state_summary <- function(scored_df, n_state = 15, metric = "mean") {
  state_df <- scored_df |>
    dplyr::group_by(state) |>
    dplyr::summarise(
      mean_score = mean(market_score_100, na.rm = TRUE),
      max_score  = max(market_score_100,  na.rm = TRUE),
      count_A    = sum(grade == "A", na.rm = TRUE),
      n_counties = dplyr::n(),
      .groups    = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data[[paste0(metric, "_score")]]))

  # count_A 처리
  if (metric == "count_A") {
    state_df <- dplyr::arrange(state_df, desc(count_A))
    y_col <- "count_A"
    y_label <- "A등급 카운티 수"
  } else {
    y_col <- paste0(metric, "_score")
    y_label <- if (metric == "mean") "평균 시장 점수" else "최고 시장 점수"
  }

  top_states <- head(state_df, n_state)
  top_states$state <- forcats::fct_reorder(top_states$state, top_states[[y_col]])

  p <- ggplot2::ggplot(
    top_states,
    ggplot2::aes(
      x    = .data[[y_col]],
      y    = state,
      fill = .data[[y_col]],
      text = glue::glue(
        "<b>{state}</b><br>",
        "평균 점수: {round(mean_score, 1)}<br>",
        "최고 점수: {round(max_score, 1)}<br>",
        "A등급 카운티: {count_A}개"
      )
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c(option = "plasma", guide = "none") +
    ggplot2::labs(
      title = glue::glue("주별 시장 매력도 순위 (Top {n_state})"),
      x     = y_label,
      y     = NULL,
      caption = "출처: US Census Bureau ACS 5-Year Estimates"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  plotly::ggplotly(p, tooltip = "text")
}
