library(dplyr)
library(ggplot2)
library(ggthemes)
library(usmap)

# Graph 1 -----------------------------------------------------------------

policy_by_state <-
  read.csv("data/policy_by_state.csv", header = TRUE)

colors1 <- c(
  "Bans" = "#009E73",
  "Fees or Taxes" = "#E69F00",
  "No Statute" = "gray80",
  "Preemption" = "#D55E00"
)

graph1 <- plot_usmap("states",
                     data = policy_by_state,
                     values = "policy",
                     labels = TRUE) +
  scale_fill_manual(values = alpha(colors1, 0.8)) +
  scale_color_manual(values = colors1) +
  labs(
    title = "Figure 1. Plastic Bag Policies in the United States",
    subtitle = "2021",
    caption = "Source: National Conference of State Legislatures",
    fill = "Status"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "black",
      size = 40,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "gray40",
      size = 24
    ),
    plot.caption = element_text(color = "gray 40", size = 22, face = "italic"),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 24, face = "italic"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.spacing.x = unit(0.4, "cm")
  )

graph1$layers[[2]]$aes_params$size <- 5

print(graph1)

ggsave("graph1.png", width = 16, height = 12)


# Graph 2 -----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

waste_generation <-
  read.csv("data/waste_generation.csv", header = TRUE)

waste_generation <- waste_generation %>%
  gather(key = "variable", value = "value",-year)

colors3 <- c("#E69F00", "#D55E00", "#009E73")

df <- waste_generation %>%
  subset(variable %in% c("Total.Plastic.Waste"))

graph2 <-
  ggplot(
    data = subset(
      waste_generation,
      variable %in% c("Combusted", "Landfill", "Recycled")
    ),
    mapping = aes(x = year, y = value)
  ) +
  annotate(
    geom = "line",
    x = df$year,
    y = df$value,
    size = 2,
    color = "gray80"
  ) +
  annotate(
    geom = "text",
    x = (df$year[2] - 5),
    y = (df$value[2] + 1000),
    color = "gray40",
    size = 5,
    label = "plastic\n generation"
  ) +
  geom_line(mapping = aes(group = variable,
                          color = variable),
            size = 2) +
  facet_wrap( ~ variable) +
  labs(
    x = "",
    y = "Plastic in thousands of tons",
    title = "Figure 2. Plastic Waste Disposal",
    subtitle = "1980-2018",
    caption = "Source: Environmental Protection Agency",
    fill = "Status"
  ) +
  scale_color_manual(values = colors3) +
  theme_hc() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "black",
      size = 24,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "gray40",
      size = 16
    ),
    plot.caption = element_text(color = "grey40", size = 14, face = "italic"),
    legend.position = "none",
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16)
  )

print(graph2)

ggsave("graph2.png", width = 10, height = 6)


# Graph 3 -----------------------------------------------------------------

plastic <- read.csv("data/plastic.csv")

colors3 <- c("#D55E00", "#E69F00", "#009E73")

breaks <- c(seq(1950, 2050, by = 10))

labels <- as.character(breaks)

graph3 <-
  ggplot(data = subset(
    plastic,
    Entity %in% c(
      "Emissions growth continues",
      "Emissions stagnate",
      "Emissions stop"
    )
  )) +
  geom_line(
    mapping = aes(
      x = Year,
      y = Microplastics,
      group = Entity,
      color = Entity
    ),
    size = 2,
    linetype = "dashed"
  ) +
  geom_line(
    mapping = aes(
      x = replace(Year, Year >= 2020, NA),
      y = Microplastics,
      group = Entity
    ),
    size = 2,
    color = "gray80"
  ) +
  scale_x_continuous(limits = c(1950, 2050),
                     breaks = breaks,
                     labels = labels) +
  labs(
    x = "",
    y = "Microplastics in thousands of tons",
    title = "Figure 3. Microplastics on Ocean Surface",
    subtitle = "future predictions based on 1950-2020",
    caption = "Source: Lebreton et al. (2019)",
    fill = ""
  ) +
  scale_color_manual(values = colors3) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  theme_hc() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "black",
      size = 24,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "gray40",
      size = 16
    ),
    plot.caption = element_text(color = "gray40", size = 14, face = "italic"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.spacing.x = unit(0.4, "cm")
  )

print(graph3)

ggsave("graph3.png", width = 10, height = 6)


# Graph 4 -----------------------------------------------------------------

world <- read.csv("data/world.csv")

graph4 <- ggplot(world,
                 mapping = aes(
                   x = reorder(country, waste_pc),
                   y = waste_pc,
                   fill = factor(ifelse(
                     country == "United States", "highlighted", "normal"
                   ))
                 ))

graph4 <- graph4 + geom_col() +
  scale_fill_manual(values = c("#D55E00", "grey80")) +
  labs(
    x = "",
    y = "",
    title = "Figure 4. Plastic Waste per capita by Country",
    subtitle = "measured in kilograms from 2016",
    caption = "Source: Law et al. (2020)",
    fill = "Status"
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  coord_flip() +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "black",
      size = 24,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "gray40",
      size = 16
    ),
    plot.caption = element_text(color = "gray40", size = 14, face = "italic"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  )

print(graph4)

ggsave("graph4.png", width = 10, height = 8)


# Graph 5 -----------------------------------------------------------------

reuses <- read.csv("data/reuses.csv")

shades <- c("#009E73", "#E69F00", "#D55E00")

data <- reuses %>%
  gather(key = "variable", value = "value",-times)

graph5 <- ggplot(data = data,
                 mapping = aes(x = times,
                               y = value,
                               fill = variable)) +
  geom_col(position = "dodge2") +
  labs(
    x = "",
    y = "",
    title = "Figure 5. Reuses of Different Types of Bags",
    subtitle = "survey from 2020",
    caption = "Source: U.S Packaging Preferences Survey"
  ) +
  scale_fill_manual(values = shades) +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    labels = c('0%', '10%', '20%', '30%', '40%', '50%', '60%')
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "black",
      size = 24,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "gray40",
      size = 16
    ),
    plot.caption = element_text(color = "gray40", size = 14, face = "italic"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = 'bottom',
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.spacing.x = unit(0.4, "cm")
  ) +
  coord_flip()

print(graph5)

ggsave("graph5.png", width = 10.2, height = 6)
