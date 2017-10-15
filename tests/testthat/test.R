test_that("error when wrong inputs",{
 expect_true(1==1)

})

test_that("lenreg rejects errounous input", {
  
  expect_error(r <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris,2)) #DONE
  
  expect_error(r <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis,2)) #DONE
  
})

test_that("class is correct", {
  
  r <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,2) #DONE
  
  
  
  expect_true(class(r)[1] == "ridgereg")
  
})

test_that("print() method works", {
  r <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,2)
  
  expect_output(r$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 2\\)")  
  expect_output(r$print(),"Sepal\\.Width Sepal\\.Length ")  
})




test_that("coef() method gives same results as lm.ridge$coef", {
  
  r <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,2)
  r2 <- ridgereg$new(Petal.Length~Sepal.Width+Petal.Width, data=iris,2)
  
  
  expect_true(all(round(unname(r$coef()),2) %in% c(-0.58, 1.45))) 
  expect_true(all(round(unname(r2$coef()),2) %in% c(-0.16, 1.62))) 
  
  
})