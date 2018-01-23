#num_ps = makeParamSet( # algorithm parameter
#  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
#  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
#)
#ctrl = makeTuneControlRandom(maxit = 100L) # replication
#rdesc = makeResampleDesc("CV", iters = 3L) # algorithm parameter + iteration X
#res = tuneParams("classif.ksvm", task = iris.task, resampling = rdesc, par.set = num_ps, # Problem, Algorithm
#  control = ctrl, measures = list(acc, setAggregation(acc, test.sd)), show.info = FALSE)

useMlrTuningWrapper = function(res) {

}
