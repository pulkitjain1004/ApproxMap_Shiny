get_consensus_pattern = function(weighted_seq, strength) {

  n = weighted_seq$n
  weighted_seq$n = NULL

  min_occurences = n * strength
  consensus_pattern = list()

  for(i in 1:length(weighted_seq)) {
    itemset = weighted_seq[[i]]
    strength_test = itemset$element_weights > min_occurences
    elements = (itemset$elements[strength_test])
    #consensus_pattern = append(consensus_pattern,elements,i-1)
    if(length(elements)>0) consensus_pattern[[length(consensus_pattern)+1]] = elements
  }

  return(consensus_pattern)
}


get_approxMap = function(seqList,k,strength, id = 1) {

  if(id==1) id = 1:length(seqList)

  cluster_info = knnCluster(seqList,k,id)
  clusters = cluster_info$Cluster
  consensus_patterns = list(list())

  for(i in 1:length(unique(clusters))) {
    current_cluster = clusters == unique(clusters)[i]
    current_id = id[current_cluster]
    current_density = cluster_info$Density[current_cluster]
    current_seqs = seqList[current_cluster]
    current_seqs = current_seqs[order(-current_density)]
    weighted_alignment = align_multiple_sequences(current_seqs)
    consensus_pattern = get_consensus_pattern(weighted_alignment,strength)
    cluster_pattern = list(ID = current_id, consensus_pattern = consensus_pattern)
    consensus_patterns[[i]] = cluster_pattern
  }

  return(noquote(consensus_patterns))
}
