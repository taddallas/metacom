context("Coherence")

test_that("Metacommunity and Coherence agree", {
	set.seed(100);
	test <- matrix(rbinom(100,1,0.5), ncol=10)
	mc.test <- Metacommunity(test, sims=100, method='swap')
  coh.test <- Coherence(test, sims=100, method='swap')
	expect_equal(mc.test$Coherence$stat[1], 
    coh.test$stat[1])
})



test_that("Coherence same as Leibold and Mikkelson 2002", {
  data(TestMatrices)
  coh.test2 <- Coherence(TestMatrices[[2]], sims=100, method='swap')
	expect_equal(coh.test2$stat[1], 
    22)
})


