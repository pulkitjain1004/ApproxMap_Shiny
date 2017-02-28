format_output = function(approxmap_obj) {
  clusters = approxmap_obj$clusters
  form_cons = approxmap_obj$formatted_results$consensus
  form_wseq = approxmap_obj$formatted_results$weighted_seq
  
  for(i in 1:length(clusters)) {
    
    cat(paste("Cluster ",i,":",sep = ""),"\n","Sequence IDs: ", clusters[[i]], "\n", "Weighted Sequence: ", form_wseq[[i]], "\n", "Consensus Pattern: ", form_cons[[i]],"\n\n")
  }
  
}

extract_freq = function(weighted_seq) {
  weighted_seq$n = NULL
  elements = unlist(lapply(weighted_seq, function(x) x$elements))
  element_weights = unlist(lapply(weighted_seq, function(x) x$element_weights))
  return(data.frame(elements = elements, element_weights = element_weights))
}


plot_frequency = function(weighted_seq, threshhold =0.5) {
  n_thresh = threshhold * (length(weighted_seq)-1)
  fq = extract_freq(weighted_seq)
  freq_plot <- fq %>% dplyr::mutate(element_number = 1:nrow(fq)) %>%  
    ggplot2::ggplot(aes(x = element_number, y = element_weights, text = elements)) + 
      ggplot2::geom_point(size = 0.75) + 
        ggplot2::geom_path(group = 1, size=0.1) +
          ggplot2::geom_hline(yintercept = n_thresh) +
            ggplot2::theme(legend.position="none") +
              ggplot2::geom_label(aes(label = elements,size = element_weights))
  return(freq_plot)
}



# 
# inp = read.csv("./data/demo1.csv")
# inp = cvt_seq(inp,pd=1)
# weighted_seq = align_multiple_sequences(inp)
# weighted_seq$n = NULL
# 
# 
# 
# 
# (freq_plot <- ws5 %>% mutate(element_number = 1:nrow(ws5)) %>%  
#   ggplot(aes(x = element_number, y = ele_Weight, text = element)) + 
#   geom_point(size = 0.75) + 
#   geom_path(group = 1, size=0.1) +
#   geom_hline(yintercept = 60) +
#   geom_label(aes(label = element,size = ele_Weight))) 
# 
# 
