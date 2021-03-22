# run setup script
source("_common.R")

df <- tibble(
  type = c("A", "B", "C"),
  value = c(3, 5, 4),
  expand = c(4, 5, 4.5)
)

p1 = ggplot(df, aes(type, value)) + 
  geom_col(fill="#56B4E9", width = 0.65, alpha=0.9) +
  scale_y_continuous(limits = c(0, 5.2), expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  coord_cartesian(clip = "off") + 
  theme_dviz_grid(12) + 
  theme(
    axis.line = element_blank(),
    plot.margin = margin(18, 12, 0, 0)
  )

p2 = ggplot(df, aes(type, value)) +
  geom_col(fill = c("#CCFF00FF", "#00FFFFFF", "#CC00FFFF"), width = 0.75) + 
  scale_y_continuous(limits = c(0, 5,2), expand = c(0, 0)) + 
  scale_x_discrete(name = NULL) +
  coord_cartesian(clip = 'off') +
  theme_dviz_grid(12) + 
  theme(
    axis.line = element_blank(),
    axis.title = element_text(family = "Comic Sans MX", size = 15),
    axis.text.x = element_text(family = "Times", size = 10),
    axis.text.y = element_text(family = "Arial", size = 15),
    panel.grid = element_line(color = "black"),
    plot.margin = margin(18, 12, 1.5, 1.5)
  )

p3 = ggplot(df, aes(type, value)) +
  geom_col(fill = "#56B4E9", width = 0.65, alpha = 0.9) + 
  geom_point(aes(y = expand), shape = NA) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  facet_wrap(~type, scales = "free") +
  coord_cartesian(clip = "off") +
  theme_dviz_grid(12) +
  theme(
    axis.line = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(18, 12, 1.5, 1.5)
  )

p3a = ggplot(df, aes(type, value)) +
  geom_col(fill = "black", fill = NA, width = .5) + 
  scale_y_continuous(limits = c(0, 5.2), expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  coord_cartesian(clip = "off") + 
  theme_dviz_grid(12) +
  background_grid(
    major = "y", minor = "none",
    colour.major = "grey30", color.minor = "black",
    size.major = 0.5,
    size.minor = 0.2
  ) +
  theme(
    axis.ticks = element_line(color = 'grey30'),
    plot.margin = margin(18, 12, 1.5, 1.5)
  ) 

p4 = ggplot(df, aes(type, value)) + 
  geom_col(fill = "#56B4E9", width = 0.65, alpha = 0.9) +
  coord_cartesian(xlim = c(0.4, 3.6), ylim = c(2, 6.2), expand = FALSE, clip = "on") +
  scale_y_continuous(breaks = 2:4, name="", labels = c("", "", "")) +
  scale_x_discrete(name = NULL) +
  theme_dviz_grid(12) + 
  theme(
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(18, 12, 1.5, 1.5)
  )


plot_grid(
  p1, NULL, stamp_ugly(p2),
  NULL, NULL, NULL,
  stamp_bad(p3), NULL, stamp_wrong(p4),
  rel_widths = c(1, .1, 1),
  rel_heights = c(1, .15, 1),
  labels = c("a", "", "b","", "", "", "c", "", "d")
)