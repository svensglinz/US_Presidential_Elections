# load relevant packages
library(tidyverse)
library(rvest)
library(data.table)
library(dtplyr)

# read in and format header for donation data files
header <- read_csv(
    file = "Election_2008/indiv_header_file.csv",
    col_names = FALSE
)

# select only colnames which are imported
header <- as.character(
    unlist(header[1, c(1, 7, 8, 10, 11, 12:15)])
)

# read, clean and modify baby names for name matching
paths <- list.files("names")
baby_names <- data.table()

for (i in paths) {
    temp <- fread(paste("names/", i, sep = ""),
        header = FALSE, sep = ","
    )
    baby_names <- bind_rows(baby_names, temp)
}

colnames(baby_names) <- c("NAME", "GENDER", "COUNT")

baby_names <- lazy_dt(baby_names) |>
    mutate(NAME = toupper(NAME)) |>
    group_by(NAME, GENDER) |>
    summarize(COUNT = sum(COUNT)) |>
    pivot_wider(names_from = GENDER, values_from = COUNT) |>
    as.data.table()

# rename from F to Female as it is otherwise interpreted
# as FALSE in further dbplyr operations

baby_names[is.na(baby_names), ] <- 0
baby_names <- lazy_dt(baby_names) |>
    rename(FEMALE = F, MALE = M) |>
    mutate(
        GENDER = case_when(
            (MALE / (MALE + FEMALE)) >= .75 ~ "M",
            (FEMALE / (MALE + FEMALE)) >= .75 ~ "F"
        )
    ) |>
    as.data.table()
# read in candidate files and combine

candidates_2008 <- read_csv("Election_2008/candidate_summary_2008.csv") |>
    mutate(YEAR = 2008)
candidates_2012 <- read_csv("Election_2012/candidate_summary_2012.csv") |>
    mutate(YEAR = 2012)
candidates_2016 <- read_csv("Election_2016/candidate_summary_2016.csv") |>
    mutate(YEAR = 2016)
candidates_2020 <- read_csv("Election_2020/candidate_summary_2020.csv") |>
    mutate(YEAR = 2020)

candidates <- bind_rows(
    candidates_2008, candidates_2012,
    candidates_2016, candidates_2020
)

# select candidates of interest
candidates <- candidates |>
    filter(Cand_Office == "P" & Total_Receipt != 0) |>
    filter(
        grepl("OBAMA", Cand_Name) & YEAR %in% c(2008, 2012) |
            grepl("BIDEN", Cand_Name) & YEAR == 2020 |
            grepl("ROMNEY", Cand_Name) & YEAR == 2012 |
            grepl("CLINTON", Cand_Name) & YEAR == 2016 |
            grepl("TRUMP", Cand_Name) & YEAR %in% c(2016, 2020) |
            grepl("MCCAIN", Cand_Name) & YEAR == 2008
    ) |>
    mutate(Comittee_ID = NA) |>
    select(Link_Image, Cand_Name, Cand_Id, YEAR, Cand_Party_Affiliation) |>
    rename(CANDIDATE = Cand_Name, PARTY = Cand_Party_Affiliation)

# add data past 2008 as no other info is available!
bush_2004 <- list("https://www.fec.gov/data/candidate/P00003335/?cycle=2004", "BUSH, GEORGE W", "P00003335", 2004, "REP")
kerry_2004 <- list("https://www.fec.gov/data/candidate/P80000235/?cycle=2004", "KERRY, JOHN F", "P80000235", 2004, "DEM")
bush_2000 <- list("https://www.fec.gov/data/candidate/P00003335/?cycle=2004", "BUSH, GEORGE W", "P00003335", 2000, "REP")
gore_2000 <- list("https://www.fec.gov/data/candidate/P80000912/?cycle=2000", "GORE, AL", "P80000912", 2000, "DEM")
clinton_1996 <- list("https://www.fec.gov/data/candidate/P20000642/?cycle=1996", "CLINTON, BILL", "P20000642", 1996, "DEM")
dole_1996 <- list("https://www.fec.gov/data/candidate/P00000489/?cycle=1996", "DOLE, BOB", "P00000489", 1996, "REP")
clinton_1992 <- list("https://www.fec.gov/data/candidate/P20000642/?cycle=1992", "CLINTON, BILL", "P20000642", 1992, "DEM")
bush_1992 <- list("https://www.fec.gov/data/candidate/P00000455/?cycle=1992", "BUSH, GEORGE", "P00000455", 1992, "REP")
bush_1988 <- list("https://www.fec.gov/data/candidate/P00000455/?cycle=1988", "BUSH, GEORGE", "P00000455", 1988, "REP")
dukakis_1988 <- list("https://www.fec.gov/data/candidate/P80000789/?cycle=1988", "DUKAKIS, MICHAEL", "P80000789", 1988, "DEM")

candidates[nrow(candidates) + 1, ] <- as.list(bush_2004)
candidates[nrow(candidates) + 1, ] <- as.list(kerry_2004)
candidates[nrow(candidates) + 1, ] <- as.list(bush_2000)
candidates[nrow(candidates) + 1, ] <- as.list(gore_2000)
candidates[nrow(candidates) + 1, ] <- as.list(clinton_1996)
candidates[nrow(candidates) + 1, ] <- as.list(dole_1996)
candidates[nrow(candidates) + 1, ] <- as.list(clinton_1992)
candidates[nrow(candidates) + 1, ] <- as.list(bush_1992)
candidates[nrow(candidates) + 1, ] <- as.list(bush_1988)
candidates[nrow(candidates) + 1, ] <- as.list(dukakis_1988)

# extract comittee ID from the webpage which is needed to identify donations
for (i in seq_len(nrow(candidates))) {
    link <- candidates[i, "Link_Image"]
    webpage <- paste0(link, "&election_full=false")
    webpage <- read_html(webpage)

    # extract comittee ID(s)
    CMTE_ID <- webpage |>
        html_node(xpath = "*//table[@data-committee-id]") |>
        html_attr("data-committee-id") |>
        str_extract_all(pattern = "[:alnum:]+") |>
        unique() |>
        unlist()

    candidates$CMTE_ID[i] <- list(CMTE_ID)
}

# select only first name of candidates
candidates <- candidates |>
    select(CANDIDATE, YEAR, CMTE_ID, PARTY) |>
    unnest(everything()) |>
    mutate(
        CANDIDATE = str_split(CANDIDATE, pattern = ",", simplify = TRUE)[, 1]
    ) |>
    as.data.table()

# function to clean the donation data frames

clean_donations <- function(direct_path = NA, transfer_path = NA, year) {
    # read direct individual contributions

    if (is.na(direct_path)) {
        donation_data <- fread(transfer_path,
            sep = "|",
            header = FALSE,
            select = c(1, 7, 8, 10, 11, 12:15)
        )

        colnames(donation_data) <- header
        donation_data <- donation_data |>
            mutate(YEAR = year) |>
            filter(ENTITY_TP == "IND")
    } else if (is.na(transfer_path)) {
        donation_data <- fread(direct_path,
            sep = "|",
            header = FALSE,
            select = c(1, 7, 8, 10, 11, 12:15)
        )

        colnames(donation_data) <- header
        donation_data <- donation_data |>
            mutate(YEAR = year)
    } else {
        direct_donations <- fread(direct_path,
            sep = "|",
            header = FALSE,
            select = c(1, 7, 8, 10, 11, 12:15)
        )

        transfer_donations <- fread(transfer_path,
            sep = "|",
            header = FALSE,
            select = c(1, 7, 8, 10, 11, 12:15)
        )

        colnames(transfer_donations) <- header
        colnames(direct_donations) <- header

        donation_data <- bind_rows(direct_donations, transfer_donations) |>
            filter(ENTITY_TP == "IND") |>
            mutate(YEAR = year)
    }

    # join donations onto candidate file (to only filter out donations
    # to the candidates in the "candidates" file)
    donation_data <- candidates |>
        inner_join(donation_data,
            by = c("YEAR", "CMTE_ID")
        )

    name <- donation_data |>
        pull(NAME)

    last_name <- str_extract(name, pattern = "[^,]+")
    last_name <- gsub(pattern = "[[:punct:]]", replacement = "", last_name)

    first_name <- str_split(name, pattern = ",", simplify = TRUE)[, 2]
    first_name <- sapply(
        str_split(first_name, "[^[:alpha:]]"),
        function(x) x[which.max(nchar(x))]
    )

    donation_data <- donation_data |>
        mutate(
            FIRST_NAME = toupper(first_name),
            LAST_NAME = toupper(last_name)
        ) |>
        left_join(lazy_dt(baby_names), by = c("FIRST_NAME" = "NAME")) |>
        select(-c(FEMALE, MALE))

    return(donation_data)
}
#---------------------------------------------------------------------
# clean 2008 data set (select donations to pres. candidates only
# and identify gender of donors)
donations_1988 <- clean_donations(direct_path = "Election_1988/itcont.txt", year = 1988)
write_csv(as.data.frame(donations_1988), "Election_1988/donations_1988.csv")
rm(donations_1988)

donations_1992 <- clean_donations("Election_1992/itcont.txt", year = 1992)
write_csv(as.data.frame(donations_1992), "Election_1992/donations_1992.csv")
rm(donations_1992)

donations_1996 <- clean_donations("Election_1996/itcont.txt", year = 1996)
write_csv(as.data.frame(donations_1996), "Election_1996/donations_1996.csv")
rm(donations_1996)

donations_2000 <- clean_donations("Election_2000/itcont.txt", year = 2000)
write_csv(as.data.frame(donations_2000), "Election_2000/donations_2000.csv")
rm(donations_2000)

donations_2004 <- clean_donations("Election_2004/itcont.txt", year = 2004)
write_csv(as.data.frame(donations_2004), "Election_2004/donations_2004.csv")
rm(donations_2004)

donations_2008 <- clean_donations(
    direct_path = "Election_2008/itcont.txt",
    transfer_path = "Election_2008/itoth.txt",
    year = 2008
)
write_csv(as.data.frame(donations_2008), "Election_2008/donations_2008.csv")
rm(donations_2008)

donations_2012 <- clean_donations(
    direct_path = "Election_2012/itcont.txt",
    transfer_path = "Election_2012/itoth.txt",
    year = 2012
)
write_csv(as.data.frame(donations_2012), "Election_2012/donations_2012.csv")
rm(donations_2012)

donations_2016 <- clean_donations(
    direct_path = "Election_2016/itcont.txt",
    transfer_path = "Election_2016/itoth.txt",
    year = 2016
)
write_csv(as.data.frame(donations_2016), "Election_2016/donations_2016.csv")
rm(donations_2016)

# file is too large to read at once --> Perform cleaning in Loops!
files <- list.files("Election_2020/by_date")
donations_2020 <- data.table()
counter <- 1

for (i in files) {
    temp <- clean_donations(paste0("Election_2020/by_date/", i), year = 2020)
    donations_2020 <- bind_rows(as.data.frame(temp), donations_2020)
    print(paste(counter, "file read"))
    counter <- counter + 1
}

# read transfer donations
donations_2020_transfer <- clean_donations(transfer_path = "Election_2020/itoth.txt", year = 2020)
write_csv(as.data.frame(donations_2020_transfer), "temp.csv")
donations_2020_transfer <- read_csv("temp.csv", col_types = cols(.default = "c"))

donations_2020 <- fread("donations_2020.csv", sep = ",", header = TRUE, colClasses = c("character"))

donations_2020 <- bind_rows(as.data.frame(donations_2020_transfer), as.data.frame(donations_2020))

write_csv(as.data.frame(donations_2020), "Election_2020/donations_2020.csv")
rm(donations_2020)

lazy_dt(donation_data) |>
    group_by(CANDIDATE) |>
    summarize(sum = sum(as.double(TRANSACTION_AMT)))

lazy_dt(donations_2020_transfer) |>
    group_by(CANDIDATE, GENDER) |>
    summarize(sum = sum(as.double(TRANSACTION_AMT))) |>
    mutate(GENDER = ifelse(is.na(GENDER), "NA", GENDER)) |>
    pivot_wider(names_from = GENDER, values_from = sum) |>
    as.data.table() |>
    rename(FEMALE = F, MALE = M) |>
    mutate(RATIO_F = FEMALE / (MALE + FEMALE))
