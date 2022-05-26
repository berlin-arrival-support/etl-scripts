library(data.table)
library(ggplot2)


# Settings

DAYS <- 21
OUT_DIR <- "output/plots"

# Import data

db_data <- fread("arrival-times-db/data/cleaned_db_data.R")
db_data <- db_data[ts_planned > Sys.time() - DAYS * 24 * 60 * 60, ]

train_labels <- db_data[, .(time = min(format(ts_planned, format = "%H:%M"))), .(id, description)]
train_labels <- unique(train_labels, by = "id")
train_labels[, label := sprintf("%s (%s) %s", id, time, description)]
train_labels$label <- with(train_labels, reorder(label, time))

setorder(train_labels, time)

train_order <- train_labels$label
db_data <- db_data[train_labels[, .(id, label)], on = "id"]

db_data_summary <- db_data[,
  .(median = median(pax_info), mean = mean(pax_info)),
  by = "label"
]


plt1 <- ggplot(
    db_data
  ) +
  aes(
    x = as.Date(days),
    y = pax_info) +
  geom_smooth(
    color = "#0B3934",
    alpha = 0.4,
    linetype = "dashed",
    fill = "#ffea8b17"
  ) +
  geom_point(
    color = "#0B3934",
    alpha = 0.5
  ) +
  geom_text(
    aes(label = pax_info),
    alpha = 0.5,
    size = 3,
    check_overlap = TRUE,
    nudge_y = 40
  ) +
  geom_hline(
    data = db_data_summary,
    aes(yintercept = median),
    alpha = 0.3
  ) +
  geom_vline(xintercept = Sys.Date()) +
  geom_hline(
    data = db_data_summary,
    aes(yintercept = mean),
    linetype = "dashed",
    alpha = 0.3
  ) +
  facet_wrap(
    ~factor(label, levels = train_order),
    ncol = 3) +
  theme_linedraw() +
  coord_cartesian(ylim = c(0, max(db_data$pax_info + 20))) +
  labs(
    title = "Arrivals at Berlin Hbf | Disaggregated per Train",
    x = "Date",
    y = "Est. Number of Refugees on Board",
    caption = sprintf("Based on the BAS Infostream Data\nGenerated on: %s\nData range: %s - %s",
    Sys.time(),
    format(min(db_data$ts_planned), format = "%Y-%m-%d %H:%M"),
    format(max(db_data$ts_planned), format = "%Y-%m-%d %H:%M"))
  ) +
  theme(
    strip.background = element_rect(fill = "#0B3934"),
    strip.text = element_text(color = "#FFD513"),
    axis.text.x = element_text(angle = 45)
  )


ggsave(
  file.path(
    OUT_DIR,
    sprintf("arrival_trains_hbf_%s.jpg", format(Sys.Date(), "%m-%d"))
  ),
  plt1,
  width = 20,
  height = 25,
  units = "cm"
)
