context('Processing classes')

library(fs)
library(zip)

classes <- process_classes(data_file('classes.csv'))

file_copy(data_file('classes.csv'), path_temp('classes.csv'))
zip_file <- file_temp(ext = '.zip')
zipr(zip_file, path_temp('classes.csv'))

test_that('loaded class data is in a tidy table with appropriate types', {
  expect_is(classes, 'data.frame')
  expect_vectors_in_df(
    classes,
    c('class_id', 'course_name', 'release', 'teacher_id', 'lms'),
    character(), '2'
  )
  expect_vectors_in_df(classes, 'setup_yaml', list(), '2')
})

test_that('class data can be loaded from a course zip file', {
  expect_identical(process_classes(zip_file), classes)
})

test_that('class data can be loaded from a directory', {
  expect_identical(process_classes(data_dir()), classes)
})
