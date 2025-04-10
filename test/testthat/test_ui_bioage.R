library(testthat)
library(shinytest2)

test_that("UI inputs are present and functional", {
  app <- AppDriver$new(app_dir ="D:/DS/R/AWS/" , seed = 101)
  
  # Test text inputs
  app$set_inputs(Name = "Wikki")
  app$set_inputs(Surname = "Chan")
  app$set_inputs(phone_prefix = "+91")
  app$set_inputs(phone_number = "9629007580")
  
  # Test date inputs
  app$set_inputs(dob = "1999-12-12")
  app$set_inputs(bloodTestDate = as.character(Sys.Date()))
  
  # Test numeric inputs
  app$set_inputs(albumin = 4.2)
  app$set_inputs(lymph = 33.1)
  app$set_inputs(mcv = 92)
  app$set_inputs(glucose = 88)
  app$set_inputs(rdw = 13.5)
  app$set_inputs(creat = 0.9)
  app$set_inputs(crp = 1.2)
  app$set_inputs(alp = 100)
  app$set_inputs(wbc = 6200)
  
  # Submit button click
  app$click("submit")
  
  # Optionally: you can test reactive value changes or outputs here
  # app$expect_values() or app$expect_screenshot()
  
  app$stop()
})
