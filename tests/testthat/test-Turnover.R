context("Turnover")

test_that("Metacommunity and Turnover agree", {
	set.seed(100);
	test <- matrix(rbinom(100,1,0.5), ncol=10)
	mc.test <- Metacommunity(test, sims=100, method='swap')
  turn.test <- Turnover(test, sims=100, method='swap')
	expect_equal(mc.test$Turnover[1], turn.test[1])
  expect_that( abs(mc.test$Turnover['z'] - turn.test['z']) < 0.2)
})
