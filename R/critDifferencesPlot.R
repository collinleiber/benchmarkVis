#' @title create a crit differences plot
#'
#' @description
#' Create critDifferences plot out of a benchmarkVis compatible data table.
#'
#' @param dt compatible data table
#' @param measure the measure to plot
#' @param group.by the column to group the markers by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a crit differences plot
#' @export
#' @examples
#' createCritDifferencesPlot(mlr.benchmark.example, "measure.mmce.test.mean")
createCritDifferencesPlot = function(dt, measure, group.by = "problem", test.str = "bd", p.value = 0.05) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(group.by)
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_true(test.str %in% list("nemenyi", "bd"))
  #first part, genarate S3Obj object
  `%>%` = magrittr::`%>%`
  df = dt %>% dplyr::group_by(dt[[group.by]]) %>% dplyr::mutate(my_ranks = order(order(eval(parse(text = sprintf("%s", measure))), decreasing = TRUE)))

  mat = matrix(unlist(df$my_ranks), ncol = 3, byrow = TRUE)
  rownames(mat) = unique(df$problem)
  colnames(mat) = unique(df$algorithm)
  rn = as.character.factor(unique(df$problem))
  cn = unique(df$algorithm)
  mat = t(mat)

  mean.rank = rowMeans(mat)
  cddf = data.frame(
    mean.rank,
    learner.id = names(mean.rank),
    rank = rank(mean.rank, ties.method = "average")
  )
  right = cddf$rank > median(cddf$rank)
  cddf$yend[!right] = rank(cddf$rank[!right], ties.method = "first") -
    0.5
  cddf$yend[right] = rank(-cddf$rank[right], ties.method = "first") -
    0.5
  cddf$xend = ifelse(!right, 0L, max(cddf$rank) + 1L)
  cddf$right = as.numeric(right)
  cddf$short.name = unique(df$algorithm)
  baseline = as.character(cddf$learner.id[which.min(cddf$rank)])

  #nem.test = friedmanPostHocTestBMR(bmr, measure, p.value)

  pfdf = data.frame(
    "task.id" = df$problem,
    "learner.id" = df$algorithm,
    "tmpname" = df[[measure]]
  )

  aggr.meas = measure

  pfdf = data.table::setnames(pfdf, "tmpname", measure)

  if (length(unique(pfdf$task.id)) < 2) {
    stop("Benchmark results for at least two tasks are required")
  }
  if (length(unique(pfdf$learner.id)) < 2) {
    stop("Benchmark results for at least two learners are required")
  }

  nem.test = stats::friedman.test(as.formula(stringi::stri_paste(aggr.meas, " ~ learner.id | task.id",
                                                 sep = "")), data = pfdf)
  #test = test.str

  f.test = nem.test
  n.learners = length(unique(pfdf$learner.id))
  n.tasks = length(unique(pfdf$task.id))
  if (!is.na(f.test$p.value)) {
    f.rejnull = f.test$p.value < p.value
    if (!f.rejnull)
      warning("Cannot reject null hypothesis of overall Friedman test,\n             returning overall Friedman test.")
  } else {
    f.rejnull = FALSE
    warning("P-value not computable. Learner performances might be exactly equal.")
  }
  q.nemenyi = qtukey(1 - p.value, n.learners, 1e+06)/sqrt(2L)
  cd.nemenyi = q.nemenyi * sqrt(n.learners * (n.learners +
                                                1L)/(6L * n.tasks))
  q.bd = qtukey(1L - (p.value/(n.learners - 1L)), 2L, 1e+06)/sqrt(2L)
  cd.bd = q.bd * sqrt(n.learners * (n.learners + 1L)/(6L *
                                                        n.tasks))
  if (f.rejnull) {
    form = as.formula(stri_paste(aggr.meas, " ~ learner.id | task.id",
                                 sep = ""))
    nem.test = PMCMR::posthoc.friedman.nemenyi.test(form,
                                                    data = df)
    nem.test$crit.difference = list(nemenyi = cd.nemenyi,
                                    bd = cd.bd)
    nem.test$f.rejnull = f.rejnull
    return(nem.test)
  } else {
    f.test$f.rejnull = f.rejnull
    f.test$crit.difference = list(nemenyi = cd.nemenyi,
                                  bd = cd.bd)
  }
  nem.test = f.test

  cd.info = list(
    test = test.str,
    cd = nem.test$crit.difference[[test.str]],
    x = cddf$mean.rank[cddf$learner.id == baseline],
    y = 0.1
  )

  if (test.str == "nemenyi") {
    sub = sort(cddf$mean.rank)
    mat = apply(
      t(outer(sub, sub, `-`)),
      c(1, 2),
      FUN = function(x)
        ifelse(x >
                 0 &&
                 x < cd.info$cd, x, 0)
    )
    xstart = round(apply(mat + sub, 1, min), 3)
    xend = round(apply(mat + sub, 1, max), 3)
    nem.df = data.table::data.table(xstart, xend, diff = xend - xstart)
    nem.df = nem.df[, data.table::.SD[which.max(data.table::.SD$diff)], by = "xend"]
    nem.df = nem.df[nem.df$xend - nem.df$xstart > 0,]
    nem.df$y = seq(from = 0.1,
                   to = 0.35,
                   length.out = dim(nem.df)[1])
    cd.info$nemenyi.data = as.data.frame(nem.df)
  }

  obj = BBmisc::makeS3Obj(
    "CritDifferencesData",
    data = cddf,
    cd.info = cd.info,
    friedman.nemenyi.test = nem.test,
    baseline = baseline,
    p.value = p.value
  )
  p = mlr::plotCritDifferences(obj)
  return(p)
}
