# load relevant packages
library(tidyverse)

# read in and format header for donation data files
donations_header <- read_csv(
    file = "Election_2008/indiv_header_file.csv",
    col_names = FALSE
)
header <- as.character(donations_header[1, ])

# filter original data set of 2008 for donations to McCain and Obama
donations_2008 <-
    read_delim("Election_2008/itcont.txt",
        col_names = header, delim = "|"
    ) |>
    filter(CMTE_ID %in% c(
        "C00431445", "C00430470",
        "C00446104", "C00453928"
    )) |>
    mutate(
        CANDIDATE = ifelse(CMTE_ID == "C00431445", "OBAMA", "MCCAIN"),
        ELECTION_YEAR = 2008
    ) |>
    select(
        NAME, CANDIDATE, ZIP_CODE, EMPLOYER,
        OCCUPATION, TRANSACTION_DT, TRANSACTION_AMT
    )

# save filtered CSV file & remove from RAM
write_csv(file = "Election_2008/donations_2008.csv", x = donations_2008)
rm(donations_2008)

# filter original data set of 2012 for donations to Obama and Romney
donations_2012 <-
    read_delim("Election_2012/itcont.txt",
        col_names = header, delim = "|"
    ) |>
    filter(CMTE_ID %in% c(
        "C00431445", "C00431171"
    )) |>
    mutate(
        CANDIDATE = ifelse(CMTE_ID == "C00431445", "OBAMA", "ROMNEY"),
        ELECTION_YEAR = 2012
    ) |>
    select(
        NAME, CANDIDATE, ZIP_CODE, EMPLOYER,
        OCCUPATION, TRANSACTION_DT, TRANSACTION_AMT
    )

# save filtered CSV file & remove from RAM
write_csv(file = "Election_2012/donations_2012.csv", x = donations_2012)
rm(donations_2012)

# filter original data set of 2016 for donations to Trump and Clinton
donations_2016 <-
    read_delim("Election_2016/itcont.txt",
        col_names = header, delim = "|"
    ) |>
    filter(CMTE_ID %in% c(
        "C00580100", "C00575795",
        "C00431569"
    )) |>
    mutate(
        CANDIDATE = ifelse(CMTE_ID == "C00580100", "TRUMP", "CLINTON"),
        ELECTION_YEAR = 2016
    ) |>
    select(
        NAME, CANDIDATE, ZIP_CODE, EMPLOYER,
        OCCUPATION, TRANSACTION_DT, TRANSACTION_AMT
    )

# save filtered CSV file & remove from RAM
write_csv(file = "Election_2016/donations_2016.csv", x = donations_2016)
rm(donations_2016)

donations_2020 <- read_csv("Election_2020/itcont.csv") |>
    select(-LAST_NAME) |>
    mutate(ELECTION_YEAR = 2020)

write_csv(file = "Election_2020/donations_2020.csv", x = donations_2020)

test <- read_csv("Election_2016/candidate_summary_2016.csv")

