##------------------------------------------------------------------------------
## Read data from CSV
##------------------------------------------------------------------------------
audience <- read.csv("data/daily_traffic.csv")
hits <- read.csv("data/daily_hits.csv")

##------------------------------------------------------------------------------
# Change from second to HMS
##------------------------------------------------------------------------------
sec2hms <- function(x) {
  x <- round(x) |> 
    lubridate::seconds_to_period()
  
  sprintf('%02d:%02d:%02d', x@hour, lubridate::minute(x), 
          lubridate::second(x))
}

##------------------------------------------------------------------------------
## Create visualization theme
##------------------------------------------------------------------------------
create_theme <- function(family = "NanumSquare", no_axis = FALSE, 
                         no_grid = FALSE, no_grid_x = FALSE, 
                         fill = c("beige", "blue-gray", "dark blue-gray", 
                                  "light gray", "white")[1]) {
  fill = switch (
    fill, beige = "#f9f5f1", "blue-gray" = "#d5e4eb", 
    "dark blue-gray" = "#c3d6df", "light gray" = "#ebebeb", white = "#ffffff"
  )
  
  theme_custom <- theme_economist(base_family = family) +
    theme(
      text = element_text(family = family),
      rect = element_rect(fill = fill),
      plot.background = element_rect(fill = fill),
      strip.background.y = element_rect(fill = "#857670"),
      strip.text = element_text(colour = "white"),
      panel.grid.major.y = element_line(colour = "white", size = 0.6)
    )
  
  if (no_grid_x) {
    theme_custom <- theme_custom +
      theme( panel.grid.major.x = element_blank() )  
  } else {
    theme_custom <- theme_custom +
      theme( panel.grid.major.x = element_line(colour = "white", size = 0.6) )  
  }
  
  if (no_axis) {
    theme_custom <- theme_custom +
      theme(
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank()
      )
  }
  
  if (no_grid) {
    theme_custom <- theme_custom +
      theme(
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  theme_custom
}

# Font family
font_family <- "NanumSquare"

theme_mine <- create_theme(family = font_family, no_grid_x = TRUE)

##------------------------------------------------------------------------------
## Trend plot using ggplotly
##------------------------------------------------------------------------------
trend_plotly <- function(x, y, main = NULL, sub = NULL, xlim = NULL) {
  trend <- data.frame(date = as.Date(x), value = y)
  
  if (is.null(xlim)) {
    date_min <- min(trend$date)
    date_max <- max(trend$date)
  } else {
    date_min <- as.Date(xlim[1])
    date_max <- as.Date(xlim[2])    
  }
  
  datebreaks <- seq(date_min, date_max, by = 15)
  
  p <- trend |> 
    filter(date >= date_min & date <= date_max) |> 
    ggplot(aes(x = date, y = value, group = 1, text = paste(
      "Date: ", date, "\n",
      "Value: ", format(round(value), big.mark = ","), "\n", sep = ""
    ))) + 
    theme(legend.title = element_blank(), legend.position = "top") +
    geom_line(stat = "identity") + 
    stat_smooth(method = loess, formula = y ~ x) + 
    scale_x_date(breaks = datebreaks) +
    labs(x = "일자", y = NULL) + 
    ggtitle(main, subtitle = sub) + 
    theme_bw() + 
    theme_mine 
  
  ctext <- "\nSource: GA4 Log Data\nVisualization Powered by dpxReport"
  ggplotly(p, tooltip = "text", dynamicTicks = FALSE) |> 
    layout(title = list(
      text = paste0(main, '<br>', '<sup>', sub, '</sup>'))
    )
}

##------------------------------------------------------------------------------
## Get data from data.fromes - for Audience-top frow
##------------------------------------------------------------------------------
get_audience_metric <- function(metric, sdate, edate) {
  tabs <- audience |> 
    filter(date >= sdate) |>       
    filter(date <= edate) 
  
  if (metric %in% c("n_user", "n_session", "n_screen")) {
    tabs |> 
      select(all_of(paste0(metric, "_d1"))) |> 
      pull() |> 
      sum()
  } else if (metric %in% c("derived_screen_per_session")) {
    tabs |> 
      summarise(round(sum(n_screen_d1) / sum(n_session_d1), 2)) |> 
      pull()
  } else if (metric %in% c("derived_avg_duration")) {
    tabs |> 
      summarise(sum(tot_timeonsite_d1) / sum(n_session_d1)) |> 
      pull()
  } else if (metric %in% c("derived_pct_newsession")) {
    tabs |> 
      summarise(round(sum(n_newsession_d1) / sum(n_session_d1) * 100, 2)) |> 
      pull()
  }
}

##------------------------------------------------------------------------------
## Get data from data.fromes - for Audience-bottom frow
##------------------------------------------------------------------------------
get_audience_trend <- function(metric) {
  if (metric %in% c("n_user_d1", "n_session_d1", "n_screen_d1")) {
    audience |> 
      select(date, all_of(metric)) |> 
      group_by(date) |> 
      summarize_all(list(sum))
  } else if (metric %in% c("screen_per_session")) {
    audience |> 
      group_by(date) |> 
      summarise(screen_per_session = 
                  round(sum(n_screen_d1) / sum(n_session_d1), 2))
  } else if (metric %in% c("avg_duration")) {
    audience |> 
      group_by(date) |> 
      summarise(avg_duration = 
                  sum(tot_timeonsite_d1) / sum(n_session_d1))
  } else if (metric %in% c("pct_newsession")) {
    audience |> 
      group_by(date) |> 
      summarise(pct_newsession = 
                  round(sum(n_newsession_d1) / sum(n_session_d1) * 100, 2))
  }
}

##------------------------------------------------------------------------------
## Get data from data.fromes - for Behavior-top frow
##------------------------------------------------------------------------------
get_behavior_metric <- function(metric) {
  if (metric %in% c("n_screen", "n_event", "n_crash")) {
    hits |> 
      select(all_of(metric)) |> 
      pull() |> 
      sum()
  } else if (metric %in% c("screen_per_session")) {
    hits |> 
      summarise(round(sum(n_screen) / sum(n_session), 2)) |> 
      pull()
  } else if (metric %in% c("avg_duration")) {
    hits |> 
      summarise(sum(tot_duration) / sum(n_duration)) |> 
      pull()
  }
}

##------------------------------------------------------------------------------
## Get data from data.fromes - for Behavior-bottom frow
##------------------------------------------------------------------------------
get_screen_list <- function() {
  hits |> 
    group_by(screen_name) |>     
    summarize(n_screen = sum(n_screen, na.rm = TRUE),
              avg_duration = round(sum(tot_duration, na.rm = TRUE) / 
                                     sum(n_duration, na.rm = TRUE)),
              screen_per_session = round(mean(screen_per_session, na.rm = TRUE), 1),
              n_exit = sum(n_exit, na.rm = TRUE),
              .groups = "drop") |>     
    mutate(pct_screen = round(n_screen / sum(n_screen) * 100, 2)) |> 
    mutate(avg_duration = sec2hms(ifelse(is.finite(avg_duration), avg_duration, 
                                         0))) |> 
    arrange(desc(n_screen)) |> 
    select("화면 이름" = screen_name,
           "조회수" = n_screen,
           "조회 비율(%)" = pct_screen,
           "평균화면시간" = avg_duration,
           "세션당 조회수" = screen_per_session,
           "이탈수" = n_exit) |> 
    head(n = 100)
}  
