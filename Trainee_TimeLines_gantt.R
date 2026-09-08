# Gantt chart of Bakker lab personnel

require("tidyverse")
require("forcats")
require("here")

gantt <- read.csv("Trainee_TimeLines.csv", header = TRUE, fileEncoding = "ISO-8859-1")
head(gantt)
gantt <- gantt %>%
  mutate(Program = factor(Program, levels = c("PI", "PhD", "MSc", 
                                              "Visiting scholar", "Technician", "Undergraduate")))

gantt <- gantt %>%
  mutate(Person = factor(Person, unique(Person)))

# Reformat into 1 measurement column only
gantt.long <- pivot_longer(data = gantt,
                           cols = c(Start, End),
                           names_to = "start_end")
head(gantt.long)
gantt.long$value <- as.Date(gantt.long$value)

plot.range <- as.Date(c("2021-09-01", "2026-08-31"))

# y = fct_reorder(Person, Item)
ggplot(data = gantt.long, 
       mapping = aes(x = value, y = Person, color = Program, group = Item)) +
  geom_line(linewidth = 4.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  scale_y_discrete(limits = rev) +
  coord_cartesian(xlim = plot.range) +
  labs(y = NULL, x = NULL, title = "Team Members (last 5 years)") +
  theme(legend.position = "bottom") +
  theme_minimal()
ggsave(here("media", "Gantt.png"), height = 20, width = 15, units = "cm")

       