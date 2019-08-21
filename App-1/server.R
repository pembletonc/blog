# Dashboard Plots ---------------------------------------------------------
output$plot_hourly_tweet_volume <- renderPlotly({
  tweets_all() %>%
    tweets_just(created_at, is_topic) %>%
    group_by(is_topic) %>%
    tweets_volume() %>%
    mutate(topic = if_else(is_topic, "topic", "all")) %>%
    ungroup() %>%
    rename(Date = by_time) %>%
    select(-is_topic) %>%
    spread(topic, n, fill = 0) %>%
    plot_ly(x = ~ Date) %>%
    add_lines(y = ~topic, name = TOPIC$name, color = I(ADMINLTE_COLORS$teal)) %>%
    {
      if (!is.null(TOPIC$full_community)) {
        add_lines(., y = ~all, name = TOPIC$full_community, color = I(ADMINLTE_COLORS$purple))
      } else .
    }%>%
    config(displayModeBar = FALSE) %>%
    layout(
      xaxis = list(
        range = c(now(tz_global()) - days(7), now(tz_global())),
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "Today",
              step = "day",
              stepmode = "todate"),
            list(
              count = 1,
              label = "Yesterday",
              step = "day",
              stepmode = "backward"),
            list(
              count = 7,
              label = "Week",
              step = "day",
              stepmode = "backward"),
            list(step = "all", label = "All"))),
        rangeslider = list(type = "date")),
      yaxis = list(title = "Tweets"),
      legend = list(orientation = 'h', x = 0.05, y = 0.9),
      hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
    ) %>%
    config(collaborate = FALSE, cloud = FALSE, mathjax = NULL)
})

output$plot_tweets_by_hour <- renderPlotly({
  tweets() %>%
    tweets_just(created_at, is_topic) %>%
    tweets_by_time(by = "1 hour") %>%
    mutate(hour = hour(by_time)) %>%
    group_by(hour, is_topic) %>%
    count() %>%
    ungroup() %>%
    mutate(topic = if_else(is_topic, "topic", "all")) %>%
    select(-is_topic) %>%
    spread(topic, n, fill = 0) %>%
    plot_ly(x = ~hour) %>%
    add_bars(y = ~topic, name = TOPIC$name, color = I(ADMINLTE_COLORS$teal)) %>%
    config(displayModeBar = FALSE) %>%
    layout(
      yaxis = list(title = "Tweets"),
      xaxis = list(title = glue::glue("Hour of the Day ({TZ_GLOBAL})")),
      hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
    )
})


