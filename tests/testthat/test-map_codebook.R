################################################# CODE SURVEYS #############################################
## This function takes a data frame and CourseKata release version as an input  and adds a column with
## the Likert code for the students' response and  adds another column with the variable names for
## survey items
##
## TO DO: add course version argument, and adjust code to pull variable names
## for different versions.  Right now it only works for v1.6 or higher                                                           ################
############################################################################################################
#
# code_surveys <- function(x) {
#
#   x <- x %>%
#     dplyr::mutate_at(vars(lrn_question_reference), as.character) %>%
#     dplyr::mutate(release = release)
#
#   #  I uploaded the codebook to github- but I think we'd want to make this part of the package so it's easier to reference
#   survey_items <- readr::read_csv("https://raw.githubusercontent.com/UCLATALL/CourseKataData-codebooks/master/survey-items-psych100a-v1-6.csv?token=AHLUWFXQ5EARJKZBJYKV6Z26U5LWI") %>%
#     dplyr::mutate_all(as.character)
#
#   data <- x %>%
#     #  This creates a new variable called response_likert with a numeric code based on the learnosity option
#     #  instead of the actual response text. For non-learnosity items, it replaces response_likert with the
#     #  original response text
#     dplyr::mutate(response_likert = dplyr::case_when(
#       !is.na(response) & response == lrn_option_0 ~ "1",
#       !is.na(response) & response == lrn_option_1 ~ "2",
#       !is.na(response) & response == lrn_option_2 ~ "3",
#       !is.na(response) & response == lrn_option_3 ~ "4",
#       !is.na(response) & response == lrn_option_4 ~ "5",
#       !is.na(response) & response == lrn_option_5 ~ "6",
#       !is.na(response) & response == lrn_option_6 ~ "7",
#       !is.na(response) & response == lrn_option_7 ~ "8",
#       !is.na(response) & response == lrn_option_8 ~ "9",
#       !is.na(response) & response == lrn_option_9 ~ "10",
#       !is.na(response) & response == lrn_option_10 ~ "11",
#       !is.na(response) & response == lrn_option_11 ~ "12",
#       TRUE ~ response
#     )) %>%
#     #  This joins the response data to the reference table with the variable names
#     dplyr::left_join(select(survey_items, lrn_question_reference, var_name), by = "lrn_question_reference") %>%
#     #  This replaces the value of response_likert with students' original response for the question asking about their previous
#     #  math experience. This is because students could enter multiple responses, so the above coding scheme won't work.
#     dplyr::mutate(response_likert = dplyr::case_when(
#       var_name == "prev_ma1" ~ response,
#       TRUE ~ response_likert
#     ))
#
#   return(data)
# }

test_that('a warning is thrown only if the relevant codebook cannot be found', {
  # TODO: map that should fail, use 'does-not-exist' as version
  # TODO: map that should not fail, use version 1.6
})

test_that('var_name is added mapping variable names to survey items', {
  # TODO: add available codebooks
  # TODO: simple map variable name to codebook name
})

test_that('mapping is associated with a message to inspect the codebook', {
  # TODO: this should appear after the mapping has occurred

  # Take a look at the codebook(s) for more information about the survey items
  # All of the codebooks are included as data frames in in this package and can
  # also be found on the package repository's
  # [Wiki](https://github.com/UCLATALL/CourseKataData/wiki).

  # Codebooks used for these data: (list codebooks as bullet points)
  # Type codebook_<version> to view the relevant codebook, e.g. 'codebook_1.6'
})

test_that('it only adds data to rows where the version can be found', {
  # TODO: what column? release?
})

test_that('response_likert is added mapping Likert items to integers', {
  # TODO: which variables get this?
  # TODO: are any reverse-coded?
})

# ########################################### CODE PREV MATH EXPERIENCE ######################################
# ## This  function takes the output of widen_responses() (a data frame) as an input and adds dummy variables
# ## based on students' responses to prev_ma1
# ############################################################################################################
#
#
# code_prev_ma <- function (x) {
#
#   data <- x %>%
#     dplyr::mutate_if(is.character, tolower) %>%
#     dplyr::mutate(prev_calc_ap = dplyr::case_when(
#       prev_ma1 == "ap calculus in high school" ~ 1,
#       TRUE ~ 0
#     )) %>%
#     dplyr::mutate(prev_calc_col = dplyr::case_when(
#       prev_ma1 == "calculus in college" ~ 1,
#       TRUE ~ 0
#     )) %>%
#     dplyr::mutate(prev_stat_ap = dplyr::case_when(
#       prev_ma1 == "ap statistics in high school" ~ 1,
#       TRUE ~ 0
#     )) %>%
#     dplyr::mutate(prev_stat_col = dplyr::case_when(
#       prev_ma1 == "a different statistics course (besides this one) in college" ~ 1,
#       TRUE ~ 0
#     ))
#
#   return(data)
#
# }

test_that('multi-response answers to survey items are properly broken out', {
  # TODO: prev_ma should be replaced with 4 rows indicating each part
  # TODO: other variables like prev_ma?
})
