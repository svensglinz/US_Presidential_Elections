# load packages
library(tidyverse)
library(ggsci)
library(cowplot)
library(data.table)
library(tidytext)
library(showtext)

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
donations <- fread("Election_2020/donations_2020.csv", sep = ",", header = TRUE)

# group to unique donor level
donations <- donations |>
    group_by(FIRST_NAME, LAST_NAME, PARTY, OCCUPATION, ZIP_CODE) |>
    summarize(TRANSACTION_AMT = sum(as.double(TRANSACTION_AMT))) |>
    as.data.table()

# change names of professions which show up multiple times in top 20 but are identical
# group professions by party, count them
donations <- donations |>
    mutate(
        OCCUPATION = ifelse(OCCUPATION %in% c("R.N", "REGISTERED NURSE", "RN", "R.N."),
            "REGISTERED NURSE", OCCUPATION
        ),
        OCCUPATION = ifelse(OCCUPATION %in% c("TRUCKER", "TRUCK DRIVER", "TRUCKING"),
            "TRUCK DRIVER", OCCUPATION
        ),
        OCCUPATION = ifelse(OCCUPATION %in% c("ENTREPRENEUR", "ENTREPRENUER"),
            "ENTREPRENEUR", OCCUPATION
        )
    ) |>
    group_by(PARTY, OCCUPATION) |>
    summarize(DONORS_PER_OCCUP_PER_PARTY = n()) |>
    ungroup() |>
    add_count(
        OCCUPATION,
        wt = DONORS_PER_OCCUP_PER_PARTY,
        name = "DONORS_PER_OCCUP"
    ) |>
    mutate(SHARE = DONORS_PER_OCCUP_PER_PARTY / DONORS_PER_OCCUP) |>
    as.data.table()

# filter out occupations that are not clear / should not appear on chart
discard_occupation <- c(
    "INFORMATION REQUESTED PER BEST EFFORTS", "RETIRED",
    NA, "RETIRED TEACHER", "DECLINED TO STATE", "NONE",
    "NOT EMPLOYED", "INFORMATION REQUESTED",
    "SELF-EMPLOYED", "SELF EMPLOYED", "", "PRES", "C.E.O."
)

# select top 20 skewed professions for democrats
# select by: 1. larget number of donors
# Share Democratic Donors > 75%
major_dem <- donations |>
    filter(
        PARTY == "DEM", SHARE > .75,
        !OCCUPATION %in% discard_occupation
    ) |>
    slice_max(order_by = DONORS_PER_OCCUP_PER_PARTY, n = 20) |>
    mutate(LABEL = "B") |>
    as.data.frame()

# select top 20 skewed professions for republicans
# select by: 1. larget number of donors
# Share Republican Donors > 75%
major_rep <- donations |>
    filter(
        PARTY == "REP", SHARE > .75,
        !OCCUPATION %in% discard_occupation
    ) |>
    slice_max(order_by = DONORS_PER_OCCUP_PER_PARTY, n = 20) |>
    mutate(LABEL = "T") |>
    as.data.frame()

# combine data frames
plot_df <- bind_rows(major_dem, major_rep)

out <- plot_df |>
    mutate(OCCUPATION = str_to_title(OCCUPATION)) |>
    ggplot(aes(x = SHARE, y = reorder_within(OCCUPATION, SHARE, LABEL), color = LABEL)) +
    geom_point(aes(size = DONORS_PER_OCCUP_PER_PARTY)) +
    geom_segment(
        aes(
            x = .7, xend = SHARE,
            yend = reorder_within(OCCUPATION, SHARE, LABEL)
        ),
    ) +
    geom_text(data = NULL, aes(x = .5, y = 0, label = "text")) +
    labs(
        x = NULL,
        y = NULL,
        title = "Professions where Donors are heavily skewed towards one Party",
        caption = "Own Depiction | Source: Federal Election Commission",
        subtitle = "Professions selected by those occuring most often and the share per Party is > 75%",
        color = NULL,
        size = "Number of Donors"
    ) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(.7, 1)) +
    theme_minimal() +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(
            face = "italic",
            margin = margin(t = 0, b = 1.4, l = 0, r = 0, unit = "cm")
        ),
        plot.background = element_rect(fill = "#F9F6EE", color = "transparent"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(
            linetype = "dashed",
            color = "grey"
        ),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.justification = "right",
        plot.margin = margin(t = .7, r = .7, l = .7, b = .7, unit = "cm")
    ) +
    scale_color_manual(values = c("B" = "#0F425CFF", "T" = "#800000FF")) +
    scale_size_continuous(breaks = c(500, 1000, 5000, 10000, 50000)) +
    facet_wrap(~LABEL, scales = "free") +
    scale_y_reordered() +
    coord_cartesian(clip = "off") +
    guides(color = "none", size = guide_legend(title.position = "top", title.hjust = .5))

text <- paste0(
    "Many of the professions who contributed strongly to Biden are high-income whereas\n",
    "those donating excessively to Trump are rather lower income professions.\n",
    "Besides the fact that lower income donors may be less frequent, many small donations\n",
    "are not reported, this could possibly explain why we observe less donors in the\n",
    "professions strongly supporting Trump as opposed to Biden."
)

ggdraw(out) + draw_image(image = "biden.png", x = .307, y = .83, width = .08, height = .08, hjust = .5, vjust = .5) + draw_image(image = "trump.png", x = .798, y = .83, width = .08, height = .08, hjust = .5, vjust = .5) + draw_label(text,
    x = .08, y = .05, hjust = 0, vjust = 0,
    size = 9, fontfamily = "lmroman"
)

ggsave("skewed_professions.png", last_plot(), dpi = 800, width = 10.6, height = 6.83)
