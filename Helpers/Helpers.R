format_output = function(approxmap_obj) {
  clusters = approxmap_obj$clusters
  form_cons = approxmap_obj$formatted_results$consensus
  form_wseq = approxmap_obj$formatted_results$weighted_seq
  
  for(i in 1:length(clusters)) {
    
    cat(paste("Cluster ",i,":",sep = ""),"\n","Cluster IDs: ", clusters[[i]], "\n", "Weighted Sequence: ", form_wseq[[i]], "\n", "Consensus Pattern: ", form_cons[[i]],"\n\n")
  }
  
}