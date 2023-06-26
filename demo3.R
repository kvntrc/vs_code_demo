require(tidyverse)
require(DBI)

# loads the PostgreSQL driver
drv <-  DBI::dbConnect(RPostgres::Postgres(), dbname="Woods_Lab_DB",  port = 5432, user = "kctracy", password="", timezone = NULL)

dbListTables(drv)

patient_pr_log <- dbGetQuery(drv, "SELECT * from patient_pr_log as a left join microlab_pr as b on a.order=b.order") %>%
    relocate(mrn)

pat_vre_isolates <- dbGetQuery(drv, "SELECT * FROM patient_vre_isolates") %>% # nolint: line_length_linter.
    as_tibble()

pat_pr_clones <- dbGetQuery(drv, "SELECT * FROM patient_pr_clones") %>% # nolint: line_length_linter.
    as_tibble()


#Convert 1-4 scale to boolean presence/absence
patient_pr_log <- patient_pr_log %>%
    select(mrn, label, cdate, order, label,
           vre_faecium_pink, vre_faecalis_blue,
           pr_swab_stored_by, comments, results) %>%
    mutate(vre_faecium_pink_density = vre_faecium_pink) %>%
    mutate(vre_faecalis_blue_density = vre_faecalis_blue) %>%
    mutate(vre_faecium_pink = case_when(
                vre_faecium_pink >= 1 ~ 1,
                TRUE ~ 0)) %>%
    mutate(vre_faecalis_blue = case_when(
                vre_faecalis_blue >= 1 ~ 1,
                TRUE ~ 0)) %>%
    mutate(test_status = case_when(
                vre_faecium_pink == 1 | vre_faecalis_blue == 1 ~ 1,
                TRUE ~ 0
    )) %>%
    relocate(test_status, .after = vre_faecalis_blue) %>%
    mutate(state = case_when(
        vre_faecium_pink == 1 & vre_faecalis_blue == 1 ~ 4,
        vre_faecium_pink == 1 & vre_faecalis_blue == 0 ~ 3,
        vre_faecium_pink == 0 & vre_faecalis_blue == 1 ~ 2,
        vre_faecium_pink == 0 & vre_faecalis_blue == 0 ~ 1)
    ) %>%
    arrange(mrn, cdate)

positive_tests <- patient_pr_log %>%
    group_by(mrn, test_status) %>%
    mutate(earliest_pos_test = case_when(
        test_status == 1 ~ min(cdate))) %>%
    ungroup() %>%
    group_by(mrn) %>%
    mutate(earliest_pos_test = case_when(
        is.na(earliest_pos_test) ~ min(earliest_pos_test, na.rm = TRUE),
        TRUE ~ earliest_pos_test)) %>%
    mutate(earliest_cocol_test = case_when(
        vre_faecium_pink == 1 & vre_faecalis_blue == 1 ~ min(earliest_pos_test, na.rm = TRUE),
        TRUE ~ earliest_pos_test))%>%
    mutate(earliest_cocol_test = case_when(
        is.na(earliest_cocol_test) ~ min(earliest_cocol_test, na.rm = TRUE),
        TRUE ~ earliest_pos_test))