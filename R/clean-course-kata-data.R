#
# ########################################### READ RESPONSES  ############################################
# ## This function reads and cleans response data
# ########################################################################################################
#
#
# read_responses <- function(x) {
#
#   responses <- readr::read_csv(x, na = c("", "NA"),
#                                col_names = TRUE,
#                                cols(
#                                  class_id = col_character(),
#                                  course_name = col_character(),
#                                  release = col_character(),
#                                  branch = col_character(),
#                                  student_id = col_character(),
#                                  lms_id = col_character(),
#                                  item_id = col_character(),
#                                  item_type = col_character(),
#                                  chapter = col_character(),
#                                  page = col_character(),
#                                  response = col_character(),
#                                  prompt = col_character(),
#                                  points_possible = col_integer(),
#                                  points_earned = col_integer(),
#                                  dt_submitted = col_datetime(),
#                                  attempt = col_integer(),
#                                  user_agent = col_character(),
#                                  lrn_session_id = col_character(),
#                                  lrn_response_id = col_character(),
#                                  lrn_items_api_version = col_character(),
#                                  lrn_response_api_version = col_character(),
#                                  lrn_activity_reference = col_character(),
#                                  lrn_question_reference = col_character(),
#                                  lrn_question_position = col_integer(),
#                                  lrn_type = col_character(),
#                                  lrn_dt_started = col_datetime(),
#                                  lrn_dt_saved = col_datetime(),
#                                  lrn_status = col_character(),
#                                  lrn_response_json = col_character(),
#                                  lrn_option_0 = col_character(),
#                                  lrn_option_1 = col_character(),
#                                  lrn_option_2 = col_character(),
#                                  lrn_option_3 = col_character(),
#                                  lrn_option_4 = col_character(),
#                                  lrn_option_5 = col_character(),
#                                  lrn_option_6 = col_character(),
#                                  lrn_option_7 = col_character(),
#                                  lrn_option_8 = col_character(),
#                                  lrn_option_9 = col_character(),
#                                  lrn_option_10 = col_character(),
#                                  lrn_option_11 = col_character())) %>%
#     CourseKataData::convert_types_in_responses() %>%
#     CourseKataData::map_response_options() %>%
#     dplyr::mutate_if(is.character, tolower) %>%
#     dplyr::mutate_at(vars("dt_submitted", "lrn_dt_started", "lrn_dt_saved"), ~lubridate::with_tz(., tzone = Sys.timezone()))
#
#   return(responses)
# }
#
#
# ################################################# CODE SURVEYS #############################################
# ## This function takes a data frame and CourseKata release version as an input  and adds a column with
# ## the Likert code for the students' response and  adds another column with the variable names for
# ## survey items
# ##
# ## TO DO: add course version argument, and adjust code to pull variable names
# ## for different versions.  Right now it only works for v1.6 or higher                                                           ################
# ############################################################################################################
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
#
#
#
# ########################################### WIDEN_RESPONSES  ###############################################
# ## This function makes a data frame with students as the unit of observation and questions as variables.
# ## It takes a data frame and response type as input. If response is set to "response_likert",
# ## it will return the likert scale response.
# ## It takes a responses data frame that has been run through the code_surveys function above as an input.
# ##
# ## Note: This needs some work - right now it returns the likert scale for variables that aren't likert items
# ## (gender, re1, etc)
# ############################################################################################################
#
#
# widen_responses <- function(x, response = "response") {
#
#   data <- x %>%
#     dplyr::filter(!is.na(var_name)) %>%
#     dplyr::arrange(desc(dt_submitted)) %>%
#     dplyr::distinct(student_id, lrn_question_reference, .keep_all = TRUE) %>%
#     tidyr::pivot_wider(id_cols = c("student_id", "lms_id", "class_id"), names_from = var_name, values_from = `y`)
#
#   character_vars <- c("student_id", "class_id", "course_adv", "procrast2", "attend2", "ma_bio", "con_course",
#                       "gender2", "re2", "prev_ma1", names(data)[str_detect(names(data), "sum_ch")])
#
#   numeric_vars <- names(data)[!(names(data) %in% character_vars)]
#
#   data2 <- data %>%
#     dplyr::mutate_at(vars(numeric_vars), as.numeric)
#
#   if(y == "response_likert")
#     return(data2)
#
#   return(data)
#
#
# }
#
#
#
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
#
#
#
# ########################################### SCORE_PQUIZ  ##########################################################
# ## This function calculates the points earned / points possible score for each practice
# ## quiz attempt for each student. It returns a data frame with the student_id, item_id,
# ## attempt, possible points, earned points, and score
# ###################################################################################################################
#
# score_pquiz <- function (x) {
#
#   data <- x %>%
#     dplyr::filter(stringr::str_detect(item_id, "quiz")) %>%
#     dplyr::filter(attempt == 1) %>%
#     dplyr::group_by(class_id, student_id, item_id) %>%
#     dplyr::mutate(completed = n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(attempt == 1,
#                   points_earned == 1) %>%
#     dplyr::group_by(class_id, student_id, item_id) %>%
#     dplyr::mutate(points_earned = n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::distinct(lms_id, class_id, student_id, item_id, completed, points_earned)
#
#
#   return(data)
#
# }
#
#
#
# ########################################### SCORE_R_PERFORM #######################################################
# ## This function calculates the points earned / points possible score for each chapter's
# ## R axtivites for each student. It returns a data frame with the student_id, item_id,
# ## attempt, possible points, earned points, and score
# ####################################################################################################################
#
# score_r_perform <- function (x) {
#
#   data <- x %>%
#     dplyr::filter(item_type == "datacamp",
#                   attempt == 1) %>%
#     dplyr::arrange(dt_submitted) %>%
#     dplyr::distinct(lms_id, item_id, .keep_all = TRUE) %>%
#     dplyr::group_by(lms_id, student_id, class_id, chapter) %>%
#     dplyr::summarise(num_first_attempts= n(),
#                      points_earned = sum(points_earned, na.rm = TRUE),
#                      r_score = points_earned / num_first_attempts) %>%
#     dplyr::ungroup()
#
#   return(data)
#
# }
#
#
#
#
# ########################################### SCORE_R_Z  ################################################################
# ## This function calculates the average number of attempts for R exercises students got
# ## incorrect the first time, then calculates a z-score based on other students in the class
# #######################################################################################################################
#
#
# score_r_z <- function(x) {
#
#   data <- x %>%
#     dplyr::filter(
#       item_type == "datacamp",
#       points_earned == 0,
#     ) %>%
#     dplyr::select(class_id, lms_id, student_id, chapter, item_id, points_earned) %>%
#     na.omit() %>%
#     dplyr::group_by(class_id, lms_id, student_id, item_id) %>%
#     dplyr::mutate(attempts = n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(class_id, lms_id, student_id) %>%
#     dplyr::mutate(mean_attempts = mean(attempts, na.rm = TRUE)) %>%
#     dplyr::ungroup() %>%
#     dplyr::distinct(class_id, lms_id, student_id, mean_attempts) %>%
#     dplyr::mutate(attempts_r_z = mosaic::zscore(mean_attempts)) %>%
#     dplyr::select(class_id, student_id, lms_id, mean_attempts, attempts_r_z)
#
#   return(data)
#
#
# }
#
#
#
# ########################################### COUNT PAGE VIEWS  ################################################################
# ## These functions return page view counts for different types of content. They take the page_views data frame as input.
# #######################################################################################################################
#
# count_page_views <- function(x) {
#   data <- x %>%
#     dplyr::group_by(class_id, student_id) %>%
#     dplyr::summarise(page_views = n())
# }
#
#
#
#
# count_content_views <- function(x) {
#
#   data <- x %>%
#     mutate_if(is.character, tolower) %>%
#     filter(page != "about coursekata and your data",
#            page != "r sandbox",
#            page != "r cheatsheet",
#            page != "references",
#            page != "install r studio",
#            !str_detect(page, "survey"),
#            !str_detect(page, "review questions"),
#            !str_detect(page, "practice quiz")) %>%
#     dplyr::group_by(class_id, student_id) %>%
#     dplyr::summarise(content_views = n())
#
#   return(data)
#
#
# }
#
# count_pquiz_views <- function(x) {
#
#   data <- x %>%
#     mutate_if(is.character, tolower) %>%
#     filter(str_detect(page, "review questions") | str_detect(page, "practice quiz")) %>%
#     dplyr::group_by(class_id, student_id) %>%
#     dplyr::summarise(pquiz_views = n())
#
#
#   return(data)
#
#
# }
#
# count_resource_views <- function(x) {
#
#   data <- x %>%
#     mutate_if(is.character, tolower) %>%
#     filter(page == "r cheatsheet" | page == "r sandbox") %>%
#     dplyr::group_by(class_id, student_id) %>%
#     dplyr::summarise(resource_views = n())
#
#
#
#   return(data)
#
#
# }
