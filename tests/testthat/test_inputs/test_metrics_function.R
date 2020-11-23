test_metrics_function = function (results) {
  return(sum(results$all$get_attribute("abundance")))
}
