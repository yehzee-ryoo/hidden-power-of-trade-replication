build_dv_table <- function(){
   #'''
   #Read in the raw UN votes data, keep only the vote information
   #on roll calls related to human rights.
   #Then add auxiliary variables related to the main and robust versions
   #of the DV (i.e., voting Yes/Abstain/No.)

   #Return the built table.
   #'''

   # load UN votes data from UN votes R package.
   un_votes<-readRDS("../raw data/un_votes.rds")
   un_roll_calls<-readRDS("../raw data/un_roll_calls.rds")
   un_roll_call_issues<-readRDS("../raw data/un_roll_call_issues.rds")
   
   # Retrieve the RCIDs of the votes that have to do with Human rights.
   # Note that a roll call may be tagged with multiple issues.
   # If ANY of these tags is HR, we include it in our analysis.
   un_roll_call_issues_hr <- un_roll_call_issues |>
     filter(issue == "Human rights")
   
   # Keep only the UN votes on issues identified as being HR-related.
   un_votes_hr <- un_votes |>
     filter(rcid %in% un_roll_call_issues_hr$rcid)
   # Join these votes back to the country-level vote information.
   # This SHOULD result in a subset of the UN votes data, where all and only
   # the votes related to HR issues are kept.
   # So if this drops a row from un_roll_calls, it should be because that vote
   # was not related to human rights.
   un_votes_hr_details <- un_votes_hr |>
     inner_join(un_roll_calls, by = "rcid")
   
   stopifnot(
     "Join to un_roll_call_issues dropped rows; all HR votes should be kept." =
     nrow(un_votes_hr_details) == nrow(un_votes_hr)
   )
   # In the end, the inner join is JUST trying to add columns
   # to the data frame, so the number of rows of un_votes_hr_details
   # should be the same as un_votes_hr.

   # Create the DVs
   voting <- un_votes_hr_details |>
		mutate(vote_numeric = ifelse(vote == "yes", 1,
									ifelse(vote == "abstain", 0.5, 0)),
			vote_binary = ifelse(vote == "yes", 1, 0),
			year = year(date))

   return( clean_country( voting ) )
}

clean_country <- function( voting ) {
    voting.fixed <- voting |> 
		mutate( fixed.name = countrycode(country,
                                  origin = "country.name.en.regex",
                                  destination = "country.name.en"),
				fixed.iso3c = countrycode(country,
                                   origin = "country.name.en.regex",
                                   destination = "iso3c") )

    # Yugoslavia dissolves and becomes Montenegro, Serbia et al.
    # The UN data accounts for Montenegro, but not Serbia.
    # Replace country name Yugoslavia as Serbia post-1995.
    voting.iso <- voting.fixed |> 
				# All other countries that "do not exist" 
				# (i.e., don't exist today) are automatically filtered 
				# by looking only at post-1995.
				filter(year >= 1995) |>  
				mutate(fixed.name = ifelse(fixed.name == "Yugoslavia", 
											"Serbia", fixed.name),
						fixed.iso3c = countrycode(fixed.name,
										origin = "country.name.en.regex",
										destination = "iso3c"))
    voting.final <- voting.iso |> 
				mutate( cown = countrycode::countrycode(fixed.name, 
											origin = "country.name.en",
											destination = "cown",
											custom_match = c("Serbia" = 345)), #see the note in build_country_list() below
											# countryname: Yugoslavia -> fixed.name: Serbia -> cown: 345 (Serbia's cown)
						region = countrycode::countrycode(cown, 
											origin = "cown", 
											destination = "region") )
    return( voting.final )
}

build_country_list <- function() {
    voting.final <- build_dv_table()
    final_countries <- data.frame(fixed.name = unique(voting.final$fixed.name),
                              cown = unique(voting.final$cown),
                              iso3c = unique(voting.final$fixed.iso3c))
    # there is no Yugoslavia in the time period we are interested in (post-1995) 
    # Yugoslavia becomes Serbia and Montenegro
    # UN data includes Montenegro and Yugoslavia; we assume, therefore, that post-1995 Yugoslavia in this data is Serbia. 
    # for the final DV, fixed.name == Serbia is replacing Yugoslavia (original votes country name); 
    # Yugoslavia does not have cown, so we put Serbia's cown (345) in for it. 
    final_countries <- final_countries |> 
		mutate(cown = ifelse(fixed.name == "Serbia", 345, cown))
    return( final_countries )
}

