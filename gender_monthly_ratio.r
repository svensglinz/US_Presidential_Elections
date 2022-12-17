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


font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 800)

# read data frame
donations <- fread("donations_1988_2020.csv", sep = ",", header = TRUE)

# filter out desired year, format columns, group by month and year
donations <- donations |>
    filter(YEAR %in% seq(2008, 2020, 4)) |>
    mutate(
        YEAR = as.numeric(YEAR),
        TRANSACTION_DT = lubridate::mdy(TRANSACTION_DT),
        CANDIDATE = str_to_title(CANDIDATE),
        TRANSACTION_AMT = as.double(TRANSACTION_AMT),
        MONTH = lubridate::month(TRANSACTION_DT),
        GENDER = ifelse(is.na(GENDER), "NA", GENDER)
    ) |>
    group_by(CANDIDATE, GENDER, YEAR, MONTH, PARTY) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    pivot_wider(names_from = GENDER, values_from = SUM) |>
    rename(FEMALE = F, MALE = M, UNKNOWN = `NA`) |>
    as.data.table()

library(ggimage)
# calculate female donor ratio
donations <- dtplyr::lazy_dt(donations) |>
    mutate(RATIO_F = FEMALE / (FEMALE + MALE))

set.seed(2017 - 02 - 21)
d <- data.frame(
    x = rnorm(10),
    y = rnorm(10),
    image = sample(
        c(
            "https://www.r-project.org/logo/Rlogo.png",
            "https://jeroenooms.github.io/images/frink.png",
            "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2022/W19/authors/agatha-christie.png"
        ),
        size = 10, replace = TRUE
    )
)

ggplot(d, aes(x, y)) +
    geom_image(aes(image = image), size = .05, asp = 1.2)


# plot
as_tibble(donations) |>
    filter(MONTH != 12) |>
    ggplot(aes(
        x = MONTH, y = RATIO_F,
        linetype = PARTY,
        group = interaction(YEAR, PARTY),
        color = as.factor(YEAR),
    )) +
    geom_line() +
    geom_image(aes(x = 5, y = .4, image = c("https://www.r-project.org/logo/Rlogo.png"))) +
    geom_point(
        data = as_tibble(donations) |> filter(MONTH == 11),
        aes(x = MONTH, y = RATIO_F)
    ) +
    ggrepel::geom_text_repel(
        direction = "y",
        hjust = 0,
        show.legend = FALSE,
        data = as_tibble(donations) |> filter(MONTH == 11 & CANDIDATE != "Sanders"),
        aes(x = MONTH + .2, y = RATIO_F, label = CANDIDATE)
    ) +
    labs(
        x = NULL, y = NULL,
        color = NULL, linetype = NULL,
        title = "Monthly Female Donation Ratio to the nominated Presidential Candidate",
        subtitle = "Donations include Itemized direct Contributions as well as indirect Contributions via Joint Fundraising Commitees",
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    scale_y_continuous(
        labels = scales::label_percent(),
        breaks = seq(0.2, 0.6, 0.05)
    ) +
    scale_x_continuous(
        breaks = seq(1, 11, 1),
        expand = expansion(mult = c(.02, .1))
    ) +
    theme_minimal(base_family = "lmroman") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10, face = "italic", margin = margin(t = 0, r = 0, l = 0, b = .4, unit = "cm")),
        plot.caption = element_text(size = 9),
        panel.background = element_rect(color = "black", fill = "#F9F6EE"),
        plot.background = element_rect(color = "#5B8FA8FF", fill = "#F9F6EE"),
        plot.margin = margin(t = .5, l = 1, b = .5, r = 1, unit = "cm"),
        axis.text = element_text(size = 10)
    ) +
    ggsci::scale_color_jama()

# save output
ggsave("monthly_share.png", plot = last_plot(), dpi = 800, width = 11, height = 7.35)
