# run setup script
source("_common.R")

library(forcats)
library(patchwork)
library(lubridate)

# x축: month, y축: temperature, color: location ============================================================

temps_long <- filter(ncdc_normals,
                     station_id %in% c(
                       "USW00014819", # Chicago, IL 60638
                       #"USC00516128", # Honolulu, HI 96813
                       #"USW00027502", # Barrow, AK 99723, coldest point in the US
                       "USC00042319", # Death Valley, CA 92328 hottest point in the US
                       "USW00093107", # San Diego, CA 92145
                       #"USC00427606"  # Salt Lake City, UT 84103
                       "USW00012918" # Houston, TX 77061
                     )) %>%
  mutate(location = fct_recode(factor(station_id),
                               "Chicago" = "USW00014819",
                               #"Honolulu, HI" = "USC00516128",
                               #"Barrow, AK" = "USW00027502",
                               "Death Valley" = "USC00042319",
                               "San Diego" = "USW00093107",
                               #"Salt Lake City, UT" = "USC00427606",
                               "Houston" = "USW00012918")) %>%
  mutate(location = factor(location, levels = c("Death Valley", "Houston", "San Diego", "Chicago")))


ggplot(temps_long, aes(x = date, y = temperature, color = location)) +
  geom_line(size = 1) +
  scale_x_date(name = "month", limits = c(ymd("0000-01-01"), ymd("0001-01-04")),
               breaks = c(ymd("0000-01-01"), ymd("0000-04-01"), ymd("0000-07-01"),
                          ymd("0000-10-01"), ymd("0001-01-01")),
               labels = c("Jan", "Apr", "Jul", "Oct", "Jan"), expand = c(1/366, 0)) +
  scale_y_continuous(limits = c(19.9, 107),
                     breaks = seq(20, 100, by = 20),
                     name = "temperature (°F)") + 
  scale_color_OkabeIto(order = c(1:3, 7), name = NULL) +
  theme_dviz_grid() +
  theme(legend.title.align = 0.5)

# x축: month, y축: location, color: temperature ============================================================

month_names <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun",
                 "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")

mean_temps = temps_long %>%
  group_by(location, month) %>% 
  summarise(mean = mean(temperature)) %>% 
  ungroup() %>%
  mutate(month = month_names[month]) %>% 
  mutate(month = factor(month, levels = unname(month_names)))



p = ggplot(mean_temps, aes(x = month, y = location, fill=mean)) + 
  geom_tile(width = .95, height = 0.95) +
  scale_fill_viridis_c(option = 'B', begin = 0.15, end = 0.98,
                       name = "temperature (°F)") + 
  scale_y_discrete(name = NULL) + 
  coord_fixed(expand = FALSE) +
  theme_dviz_open() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        # axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 12)
        )

ggdraw(align_legend(p))

# x축: displacement, y축: fuel eff, color: power, size: weight, shpae: cylindes ================

p_mtcars = ggplot(mtcars, aes(disp, mpg, fill = hp, shape = factor(cyl), size = wt)) + 
  geom_point(color = "white") +
  scale_shape_manual(values = c(23, 24, 21), name = "cylinders") +
  scale_fill_continuous_sequential(
    palette = "Emrld", name = "Power (hp)",
    breaks = c(100, 200, 300),
    rev = FALSE
  ) + 
  xlab("displacement (cu. in.") +
  ylab("fuel efficeiency (mpg)") +
  guides(
    shape = guide_legend(override.aes = list(size = 4, fill = "#329D84")),
    size = guide_legend(override.aes = list(shape = 21, fill = "#329D84"),
    title = "weight (1000 lbs)")
  ) + 
  theme_dviz_open() + background_grid() +
  theme(
    # legend.title = element_text(size = 12),
    legend.box.background = element_rect(fill = "white", color = "white"),
    legend.position = "top",
    legend.direction = 'vertical',
    legend.justification = "center",
    legend.box.margin = margin(7, 7, 7, 7)
  )

legend = get_legend(align_legend(p_mtcars))

ggdraw() +
  draw_plot(p_mtcars + theme(legend.position = "none")) +
  draw_grob(legend, x = .36, y = .7, width = .7, height = .3)

