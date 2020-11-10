survey_item_references <- read.csv('./data-raw/survey_item_references.csv', na.strings = c("", "NA"))

logicals <- stringr::str_starts(names(survey_item_references), 'included_v')
survey_item_references[logicals] <- lapply(survey_item_references[logicals], as.logical)

survey_item_references[!logicals] <- lapply(survey_item_references[!logicals], as.character)

usethis::use_data(survey_item_references, internal = TRUE, overwrite = TRUE)
