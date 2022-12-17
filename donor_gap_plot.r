# load packages
library(tidyverse)
library(ggsci)
library(cowplot)
library(data.table)
library(tidytext)
library(showtext)
library(lubridate)
library(ggrepel)
library(dtplyr)

# add font for plot
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 800)

# read data set
donations <- fread(
    "donations_1988_2020.csv",
    sep = ",", header = TRUE, colClasses = c("character")
)


# group by gender, candidate & year
donations <- lazy_dt(donations) |>
    mutate(
        GENDER = ifelse(is.na(GENDER), "NA", GENDER)
    ) |>
    group_by(CANDIDATE, GENDER, YEAR, PARTY) |>
    summarize(SUM = sum(as.double(TRANSACTION_AMT))) |>
    as.data.table()

# calculate female ratio
donations <- donations |>
    pivot_wider(names_from = GENDER, values_from = SUM) |>
    rename(FEMALE = F, MALE = M, UNDEFINED = `NA`) |>
    mutate(RATIO_F = FEMALE / (MALE + FEMALE)) |>
    as.data.table()

# assemble plot
out <- as_tibble(donations) |>
    ggplot(aes(x = YEAR, y = RATIO_F, group = PARTY, fill = PARTY)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal(base_family = "lmroman") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9),
        panel.background = element_rect(color = "black", fill = "#F9F6EE"),
        plot.background = element_rect(color = "white", fill = "#F9F6EE"),
        plot.margin = margin(t = 1, l = 1, b = 1, r = 1, unit = "cm"),
        axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(
        breaks = seq(0, .6, by = .1),
        labels = scales::percent_format(),
        expand = expansion(mult = c(.01, .03))
    ) +
    labs(
        x = NULL, y = NULL,
        fill = NULL, linetype = NULL,
        title = "The Female Donor Gap between Democrats and Republicans",
        subtitle = "Relative Share of Female Donors to the Nominated Presidential Candidates",
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    scale_fill_manual(
        values = c("DEM" = "#5B8FA8FF", "REP" = "#B1746FFF")
    )

ggsave("Plots/female_ratio.png", plot = out, width = 11, height = 7.35, dpi = 800)
