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


plot_frequency = function(weighted_seq, cons_threshhold =0.5, noise_threshold = 0, variation_threshold = 0.2) {
  #n_thresh = threshhold * (length(weighted_seq)-1)
  n_thresh = cons_threshhold * weighted_seq$n
  v_thresh = variation_threshold * weighted_seq$n
  fq = extract_freq(weighted_seq)
  freq_plot <- fq %>% dplyr::mutate(element_number = 1:nrow(fq)) %>%  filter(element_weights > noise_threshold) %>% 
    ggplot2::ggplot(aes(x = element_number, y = element_weights, text = elements)) + 
      ggplot2::geom_point(size = 0.75) + 
        ggplot2::geom_path(group = 1, size=0.1) +
          ggplot2::geom_hline(yintercept = n_thresh, linetype = 2) +
            ggplot2::geom_hline(yintercept = v_thresh, linetype = 4) +
              ggplot2::theme(legend.position="none") +
                ggplot2::geom_label(aes(label = elements,size = element_weights))
  return(freq_plot)
}



