context("Metacommunity")

test_that("Metacommunity is equal to sum of parts", {
	set.seed(100); 
  test=matrix(rbinom(100,1,0.5), ncol=10)
  mc.test=Metacommunity(test, sims=100, method='swap')
  #coh.test=Coherence(test, sims=100, method='swap')
  #tur.test=Turnover(test, sims=100, method='swap')
  bc.test=BoundaryClump(test)
  
	#expect_equal(mc.test$Coherence$EmbAbs, coh.test$EmbAbs)
	#expect_equal(mc.test$Turnover$replacements, tur.test$Turnover)
  expect_equal(mc.test$Boundary$index, bc.test$index)
})
	