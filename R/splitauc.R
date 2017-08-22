#' Calculates the split AUC of a ROC curve
#'
#' @importFrom GENIE3 evaluate_ranking_direct
#' @importFrom pracma trapz
#' @export
get_split_auroc = function(observed, known) {
  metrics = evaluate_ranking_direct(observed, known,  sum(known), length(observed))$metrics
  meetingpoint = which(-(1-metrics$spec) + 1 < metrics$tpr)[[1]]
  specs = c((1-metrics$spec)[seq_len(meetingpoint)], 1)
  tprs = c(metrics$tpr[seq_len(meetingpoint)], 0)
  auroc_sensitivity = trapz(specs, tprs)
  auroc = trapz(1-metrics$spec, metrics$tpr)
  auroc_specificity = auroc - auroc_sensitivity
  tibble(auroc=auroc, auroc_sensitivity=auroc_sensitivity, auroc_specificity=auroc_specificity)
}
