
# Figure: multivariable vs multivariate analysis

library(ggplot2)

# a small data frame maker for boxes
make_box <- function(xmin, xmax, ymin, ymax, label, sublabel = NA,
                     group, panel) {
  data.frame(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    label = label, sublabel = sublabel,
    xmid = (xmin + xmax) / 2, ymid = (ymin + ymax) / 2,
    group = group, panel = panel,
    stringsAsFactors = FALSE
  )
}

# LEFT PANEL: multivariable analysis
# three predictors and a single outcome

mv_outcome <- make_box(0, 7, 10, 15, "Outcome", "cardiovascular disease",
                       group = "outcome", panel = "left")

mv_predictors <- rbind(
  make_box(11, 22, 15.5, 19.5, "Age",              group = "predictor", panel = "left"),
  make_box(11, 22, 10.5, 14.5, "Smoking status",    group = "predictor", panel = "left"),
  make_box(11, 22,  5.5,  9.5, "Body mass index",   group = "predictor", panel = "left")
)

mv_arrows <- data.frame(
  x    = c(11, 11, 11),
  y    = c(17.5, 12.5, 7.5),
  xend = c(7, 7, 7),
  yend = c(13.5, 12.5, 11.5)
)

# RIGHT PANEL: multivariate analysis
# one predictor, three correlated outcomes modelled jointly

x_off <- 32   # horizontal offset so the second panel sits to the right

mvt_outcomes <- rbind(
  make_box(x_off + 0, x_off + 11, 15.5, 19.5, "Systolic blood pressure",  group = "outcome2", panel = "right"),
  make_box(x_off + 0, x_off + 11, 10.5, 14.5, "Diastolic blood pressure", group = "outcome2", panel = "right"),
  make_box(x_off + 0, x_off + 11,  5.5,  9.5, "Heart rate",   group = "outcome2", panel = "right")
)

mvt_predictors <- make_box(x_off + 15, x_off + 24, 10, 15,
                           "Predictors", "age, sex, smoking status",
                           group = "predictor", panel = "right")

mvt_arrows <- data.frame(
  x    = x_off + c(15, 15, 15),
  y    = c(13.5, 12.5, 11.5),
  xend = x_off + c(11, 11, 11),
  yend = c(17.5, 12.5, 7.5)
)

# combine everything 
boxes  <- rbind(mv_predictors, mv_outcome, mvt_predictors, mvt_outcomes)
arrows <- rbind(mv_arrows, mvt_arrows)

# panel captions and titles, placed as plain text
titles <- data.frame(
  x = c(0, x_off),
  y = c(23, 23),
  label = c("Multivariable analysis", "Multivariate analysis"),
  hjust = 0
)

subtitles <- data.frame(
  x = c(0, x_off),
  y = c(21.3, 21.3),
  label = c("One outcome, several predictors", "Several outcomes, modelled jointly"),
  hjust = 0
)

captions <- data.frame(
  x = c(0, x_off),
  y = c(2.5, 2.5),
  label = c("One dependent variable, adjusted for the rest",
            "Correlation between outcomes accounted for"),
  hjust = 0
)

# outer dashed frames around each panel, for visual grouping
frames <- data.frame(
  xmin = c(-2, x_off - 2),
  xmax = c(24, x_off + 26),
  ymin = c(1, 1),
  ymax = c(24.5, 24.5)
)

# colour palette
fill_values <- c(
  predictor = "#CFE8DD",  # pale teal - left-panel predictors
  outcome   = "#D9D4F5",  # pale purple - left-panel outcome
  outcome2   = "#D9D4F5"  # pale purple - left-panel outcome
)
border_values <- c(
  predictor = "#0F6E56",
  outcome   = "#3C3489",
  outcome2   = "#3C3489"
)

# BUILD THE PLOT

p <- ggplot() +
  # dashed outer frames
  geom_rect(data = frames,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = "grey60", linetype = "dashed", linewidth = 0.4) +
  # boxes
  geom_rect(data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = group, colour = group),
            linewidth = 0.5) +
  # arrows between boxes
  geom_segment(data = arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
               colour = "grey30", linewidth = 0.5) +
  # box labels (bold title line)
  geom_text(data = boxes,
            aes(x = xmid, y = ymid + ifelse(is.na(sublabel), 0, 0.9),
                label = label),
            size = 3.4, fontface = "bold", colour = "grey15") +
  # box sublabels (smaller, muted)
  geom_text(data = boxes[!is.na(boxes$sublabel), ],
            aes(x = xmid, y = ymid - 1.1, label = sublabel),
            size = 3, colour = "grey35") +
  # panel titles, subtitles, captions
  geom_text(data = titles, aes(x = x, y = y, label = label, hjust = hjust),
            size = 4.2, fontface = "bold", colour = "grey10") +
  geom_text(data = subtitles, aes(x = x, y = y, label = label, hjust = hjust),
            size = 3.2, colour = "grey35") +
  geom_text(data = captions, aes(x = x, y = y, label = label, hjust = hjust),
            size = 2.9, colour = "grey40", fontface = "italic") +
  scale_fill_manual(values = fill_values, guide = "none") +
  scale_colour_manual(values = border_values, guide = "none") +
  coord_fixed(xlim = c(-3, x_off + 27), ylim = c(0.5, 24.5), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

# save the figure (png or pdf)
# ggsave("multivariate_vs_multivariable.png", plot = p, width = 9, height = 4.2, dpi = 300, bg = "white")
# ggsave("multivariate_vs_multivariable.pdf", plot = p, width = 9, height = 4.2)

print(p)
