
#-------- UCDP Sanity check: 
check_conflicts_raw <- function(country, year,
                                raw_path = "../raw data/ucdp_dyadic_v25_1.csv") {
  ucdp_raw <- read_csv(raw_path,
                       col_types = cols(
                         conflict_id      = col_double(),
                         type_of_conflict = col_double(),
                         year             = col_double(),
                         side_a           = col_character(),
                         side_a_2nd       = col_character(),
                         side_b           = col_character(),
                         side_b_2nd       = col_character(),
                         .default         = col_skip()
                       ))
  
  result <- ucdp_raw |>
    filter(year == !!year) |>
    filter(str_detect(side_a,     fixed(country)) |
             str_detect(side_a_2nd, fixed(country)) |
             str_detect(side_b,     fixed(country)) |
             str_detect(side_b_2nd, fixed(country))) |>
    distinct(conflict_id, .keep_all = TRUE) |>
    select(conflict_id, type_of_conflict, 
           side_a, side_b, side_a_2nd, side_b_2nd)
  
  print(result, n = Inf)
  invisible(result)
}


