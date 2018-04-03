#' @title get benchmark result as use case
#'
#' @description
#' get benchmark result as use case
#'
#' @param
#'
#' @return dataframe of the benchmark result
#' @export
#' @examples
#' getBenchMarkUseCase()
getBenchMarkUseCase = function() {
  # Measures
  measures = list(mlr::mmce, mlr::ber, mlr::timetrain)
  # Algorithms
  algorithm = c("classif.randomForest", "classif.rpart", "classif.lda")
  # Tasks
  wf.task.df = mlbench::mlbench.waveform(500)
  wf.task = mlr::makeClassifTask(id = "mlbench.waveform",
                                 data = as.data.frame(wf.task.df),
                                 target = "classes")
  rg.task.df = mlbench::mlbench.ringnorm(500)
  rg.task = mlr::makeClassifTask(id = "mlbench.ringnorm",
                                 data = as.data.frame(rg.task.df),
                                 target = "classes")
  tasks = list(mlr::sonar.task, mlr::iris.task, wf.task, rg.task)
  # Resample
  rdesc = mlr::makeResampleDesc("CV", iters = 2L)

  # Learners
  learner.names = algorithm
  lrn = mlr::makeLearner("classif.randomForest", predict.type = "prob")
  learners = lapply(learner.names,  mlr::makeLearner)
  rin = mlr::makeResampleDesc("CV", iters = 2L)

  # Make and run batch task
  reg = batchtools::makeExperimentRegistry(file.dir = NA, make.default = FALSE)
  res = mlr::batchmark(
    learners = learners,
    task = tasks,
    resampling = rin,
    measures = measures,
    reg = reg
  )

  # run our tasks
  batchtools::submitJobs(reg = reg)
  success = batchtools::waitForJobs(reg = reg)
  df = data.frame()
  if (!success) {
    warning("failed to execute the jobs")
  } else {
    res = reduceBatchmarkResults(reg = reg)
    df = as.data.frame(res)
    dt = data.table::setDT(df)
    groupby = c("problem", "algorithm")
    aggcol = list("mmce", "ber", "timetrain")
    do.agg = function(fun, prefix_str, groupby, aggcol, dt) {
      newtable = aggregate(x = df[, aggcol, with = FALSE],
                           by = df[, groupby, with = FALSE],
                           FUN = eval(parse(text = fun)))
      colnames(newtable) = lapply(
        colnames(newtable),
        FUN = function(colname) {
          if (colname %in% aggcol) {
            newname = paste(prefix_str, ".", colname, "", sep  = "")
          }
          else {
            colname
          }
        }
      )
      return(newtable)
    }
    do.replication.agg = function(fun, prefix_str, groupby, aggcol, dt) {
      append.list = function (x) {
        abc = as.factor(c(x))
        return(abc)
      }
      newtable = aggregate(x = df[, aggcol, with = FALSE],
                           by = df[, groupby, with = FALSE],
                           FUN = append.list)
      colnames(newtable) = lapply(
        colnames(newtable),
        FUN = function(colname) {
          if (colname %in% aggcol) {
            newname = paste(prefix_str, ".", colname, "", sep  = "")
          }
          else {
            colname
          }
        }
      )
      return(newtable)
    }
    # Get replication data
    tmp.df = do.replication.agg(
      fun = "mean",
      prefix_str = "list.replication",
      groupby = c("task.id", "learner.id"),
      aggcol = c("mmce", "ber", "timetrain"),
      dt = data.table::setDT(df)
    )
    agg.df = do.agg(
      fun = "mean",
      prefix_str = "measure",
      groupby = c("task.id", "learner.id"),
      aggcol = c("mmce", "ber", "timetrain"),
      dt = data.table::setDT(df)
    )
    agg.df["list.replication.mmce"] = tmp.df["list.replication.mmce"]
    agg.df["list.replication.ber"] = tmp.df["list.replication.ber"]
    agg.df["list.replication.timetrain"] = tmp.df["list.replication.timetrain"]
    dt = agg.df
  }
  # Change name
  data.table::setnames(dt, old = "task.id", new = "problem")
  data.table::setnames(dt, old = "learner.id", new = "algorithm")
  # A new datatable with three columns
  new.dt = data.table::data.table(
    replication = rep(as.factor("cross-validation"), each = 12),
    algorithm.parameter = rep(list(), each = 12),
    replication.parameter = rep(list("iters" = 2), each = 12)
  )
  # Conbine them
  dt = data.table::setDT(cbind(dt, new.dt))
}
