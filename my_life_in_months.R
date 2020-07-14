library(dplyr)
library(lubridate)


library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(extrafont)
library(tibble)
library(forcats)
library(ggplot2)
library(tidyr)
loadfonts(device = "pdf", quiet = TRUE)
## Make sure you have installed these two font families
role_annotations_font_family <- "Cedarville Cursive"
initial_annotations_font_family <- "IBM Plex Mono"


## It is possible that "geom_waffle" function not found error (may need to resintall rstudio)
# devtools::install_github("https://github.com/hrbrmstr/waffle")
library("waffle")

x_end = 70.5
annotation_base_size <- 5
annotation_lineheight <- 1
initial_annotations_colour <- "#666666"
role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5
location_colour <- "#8c8c8c"


## Create the data-----
life_data <- expand_grid(
  month = month.name,
  year = 1995:2020
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == 1995 & month_number < 9))

## Add "eras" to be coloured ----
life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  mutate(era = case_when(
    year_month == "1995,9" ~ "Childhood",
    year_month == "2010,9" ~ "High School",
    year_month == "2013,9" ~ "Undergraduate",
    year_month == "2017,9" ~ "Master",
    year_month == "2019,8" ~ "PhD",
    year_month == "2065,9" ~ "70YearsOld"
  )) %>%
  fill(era) %>%
  mutate(era = fct_inorder(era))

## Waffle chart-----
life_in_months_base <- life_data %>%
  count(era) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = era, values = n)) +
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = FALSE, na.rm = FALSE) + ## make each row a year/12 months
  coord_equal() +
  scale_x_continuous(limits = c(-0.5, x_end)) +
  scale_y_continuous(limits = c(-2.5, 14.5)) +
  scale_fill_manual(values = c("#fbbcb8", "#bfdff6", "#9acbf0", "#8a99e3", "#a3e3c4", "#75d2a6", "#00a87d", "#beaef5")) + #
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    text = element_text(family = "IBM Plex Mono", face = "italic"),
    legend.position = "none",
    plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )



life_in_months_initial_annotations <- life_in_months_base +
  annotate("text", x = 0, y = 6.5, label = "1 year", angle = 90, family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 1, yend = 5), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 1, yend = 1), colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 8, yend = 12), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 12, yend = 12), colour = initial_annotations_colour) +
  annotate("text", x = 1, y = 14.5, label = "1 square = 1 month", family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4, colour = initial_annotations_colour) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 0.5, y = 0, label = "age", family = initial_annotations_font_family, fontface = "italic", hjust = 0, size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 31, y = 6.5, label = "My Life in Months", hjust = 0, family = role_annotations_font_family, fontface = "bold", lineheight = 1, size = annotation_base_size * 2.5)

life_in_months_role_annotations <- life_in_months_initial_annotations +
  annotate("text", x = 8.5, y = role_annotations_y, label = "childhood", family = role_annotations_font_family, size = roles_size, colour = "#f88f88") +
  annotate("text", x = 17, y = role_annotations_y, label = "highschool", family = role_annotations_font_family, size = roles_size, colour = "#bfdff6") +
  annotate("text", x = 19, y = role_annotations_y - 1.25, label = "undergrad", family = role_annotations_font_family, size = roles_size, colour ="#6eb4e9" ) +
  geom_curve(aes(x = 21.5, xend = 22, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour =  "#6eb4e9") +
  annotate("text", x = 24.25, y = role_annotations_y, label = "masters", family = role_annotations_font_family, size = roles_size, colour = "#8a99e3") +
  geom_curve(aes(x = 25.5, xend = 26, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour ="#a3e3c4") +
  annotate("text", x = 24.5, y = role_annotations_y - 1.5, label = "PhD", family = role_annotations_font_family, lineheight = annotation_lineheight - 0.25, size = roles_size, colour = "#a3e3c4")  +
  geom_curve(aes(x = 69.5, xend = 70, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 66.5, y = role_annotations_y - 1.5, label = "Age of 70", family = role_annotations_font_family, lineheight = annotation_lineheight - 0.25, size = roles_size, colour = initial_annotations_colour)


life_in_months_final <- life_in_months_role_annotations +
  annotate("text", x = 8.5, y = 13.1, label = "born + raised in Ningbo", family = role_annotations_font_family, size = annotation_base_size, colour = location_colour) +
  geom_segment(aes(x = 1, xend = 4, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 14, xend = 17, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 17, xend =17, y = 12.75, yend = 13.25), colour = location_colour) +
  annotate("text", x = 18, y = 14, label = "moved to HK", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, hjust = 0.75, colour = location_colour) +
  geom_curve(aes(x = 17.5, xend =18, y = 13.5, yend = 12.6), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour) +
  annotate("text", x = 22, y = 14, label = "moved to US", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, colour = location_colour) +
  geom_curve(aes(x = 23.5, xend = 24, y = 13.5, yend = 12.6), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)

ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 8, dpi = 300)

