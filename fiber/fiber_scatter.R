library(tidyverse)
library(stringr)
library(ggrepel)
library(viridis)
library(scales)
library(patchwork)
library(ggtext)
library(showtext) #this is the main package to use for ading my own fonts
library(sysfonts)
library(janitor)
theme_set(theme_minimal(base_size = 24))

#fonts were purchased and not open source
font_add(family = "Concourse", regular = "./Fonts/Concourse2Regular.ttf")
font_add(family = "ConcourseB", regular = "./Fonts/Concourse2Bold.ttf")
font_add(family = "HeliotropeI", regular = "./Fonts/Heliotrope6Italic.ttf")
showtext_auto()

#For fiber intake we will use data from Global Dietary Database (https://globaldietarydatabase.org/)
#For population size, we use data from the World Bank Group

gdd2018<- read.csv(file = "gdd2018.csv", header = TRUE)
pop_world <- read.csv(file = "Colon_cancer/pop_world_bank_group.csv", header = TRUE)%>%
  rename("iso3" = 2) %>%
  dplyr::select (iso3, X2018)

pop_world$X2018 <- format(pop_world$X2018, scientific = FALSE)
pop_world$X2018 <- as.numeric(pop_world$X2018)

gdd2018 <- inner_join(gdd2018, pop_world, by = "iso3")
gdd2018 <- gdd2018 %>%
  rename(pop_2018 = X2018)

#now we make a plot

fiber_scatter <- gdd2018 %>%
  ggplot(aes(x= 1, y = dietary_fiber)) +
  geom_point(aes(size = pop_2018, fill = continent), shape = 21, colour = "#181716", alpha = 0.6, position = position_jitter(width=0.1, seed = 3922))+
  scale_size_continuous(name = "Population size",
                        range = c(1, 20),
                        breaks = c(1e6, 1e7, 5e7, 1e8),
                        labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  guides(size = guide_legend(title = "Population size"),
         fill = guide_legend(title = "Region", override.aes = list(size=5))) +
  geom_segment(aes(x=0.5, y=25, xend=1.2, yend=25), linetype=3)+
  scale_fill_viridis(option = "H", discrete=TRUE) +
  labs(subtitle = "Daily dietary fiber intake per person",
       caption = str_wrap("Source: Global Dietary Database and World Bank Group. Data from 2018. Created by Elena Shekhova @EShekhova.<br> Subscribe to my newsletter at lumipie.com", 110),
       fill = "continent") +
  ggtitle(str_wrap(
    "Most people worldwide do not eat<br> enough fiber",
    width = 50))+
  theme_minimal() +
  theme(text = element_text(size = 24, color = "#181716", family = "ConcourseB"),
        plot.title = element_textbox_simple(size= 40, face = "bold", halign=0, margin = margin(b = 30)),
        plot.subtitle = element_textbox_simple(size= 28, face = "bold", halign=0),
        axis.title.y = element_blank(),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 20, margin = margin(b = 20)),
        legend.text = element_text(size = 16),
        legend.position = c(0.85, 0.7),
        legend.direction = "vertical",
        legend.spacing = unit(1.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(40, 40, 40, 40),
        plot.background = element_rect(fill = "#F1EFE6"),
        plot.caption = element_textbox_simple(size = 12, color = "grey", halign = 0,  margin = margin(t = 30)))+
  scale_x_continuous(limits = c(0.5, 1.5))+
  scale_y_continuous(labels = function(x) paste0(x, "g"))

# we will add arrows and text now
fiber_scatter_an <- fiber_scatter+
  annotate("text", x = 0.65, y = 34, family = "HeliotropeI", size = 7, color = "gray50", lineheight = .9,
    label = glue::glue("At least 25 g of daily\n fiber is recommended\n for optimal health")) +
  annotate("text", x = 1.35, y = 20, family = "HeliotropeI", size = 5, color = "gray50", lineheight = .9,
    label = glue::glue("Approximately 4,792 million people \n across many countries \n do not consume the recommended\n amount of dietary fiber"))

fiber_scatter_fn <- fiber_scatter_an +
  geom_curve(aes(x = 1.35, y = 16, xend = 1.25, yend = 13),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.5,
    color = "gray50", curvature = -0.5)+
  geom_curve(aes(x = 0.73, y = 30, xend = 0.77, yend = 26),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.5,
    color = "gray50", curvature = -0.5)

showtext_auto()
showtext_opts(dpi=300)
ggsave("fiber_daily_an.png", plot = fiber_scatter_fn, width = 2800, height = 3500, units = "px", dpi = 300)