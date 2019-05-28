context("Turnover")

test_that("Metacommunity and Turnover agree", {
	set.seed(100);
	test <- matrix(rbinom(100,1,0.5), ncol=10)
	mc.test <- Metacommunity(test, sims=100, method='r1')
  turn.test <- Turnover(test, sims=100, method='EMS')
	expect_equal(mc.test$Turnover$stat[1], 
    turn.test$stat[1])
})



test_that("Turnover same as Leibold and Mikkelson 2002", {
  data(TestMatrices)
  trn.test2 <- Turnover(TestMatrices[[4]])
	expect_equal(trn.test2$stat[1], 
    191)
})

