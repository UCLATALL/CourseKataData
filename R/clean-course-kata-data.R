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
