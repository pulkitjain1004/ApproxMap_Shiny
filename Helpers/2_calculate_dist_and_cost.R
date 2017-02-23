calculate_sorenson_distance = function(itemset1, itemset2) {
  set1 = setdiff(itemset1,itemset2)
  set2 = setdiff(itemset2,itemset1)
  set_union = union(set1,set2)
  dist = length(set_union) / (length(itemset1) + length(itemset2))
  return(dist)
}

calculate_seq_soren_distance = function(seqItemset,WseqItemset,n) {
  v = WseqItemset$itemset_weight
  w = WseqItemset$element_weights
  x = WseqItemset$elements
  y = seqItemset
  y_no = length(seqItemset)
  eR = (sum(w) + (y_no * v) - (2 * sum(w[x %in% y]))) / (sum(w) + (y_no * v))
  repl = ((eR * v) + n - v) / n
  return(repl)
}


calculate_repl_btw_itemsets = function(itemset1, itemset2, fun) {
  return(fun(itemset1,itemset2))
}

calculate_repl_btw_itemset_Witemset = function(itemset, Witemset, fun, n) {
  return(fun(itemset, Witemset,n))
}

calculate_indel = function(itemset,fun) {
  empty = c("")
  return(calculate_repl_btw_itemsets(itemset,empty,fun))
}

calculate_indel_wseq = function(Witemset,fun) {
  empty = c("")
  return(calculate_repl_btw_itemsets(Witemset,empty,fun))
}

calculate_dist_btw_sequences = function(seq1, seq2,fun = calculate_sorenson_distance) {

  distance_matrix = matrix(nrow = length(seq1)+1,ncol = length(seq2)+1)

  distance_matrix[1,] = 0:length(seq2)
  distance_matrix[,1] = 0:length(seq1)

  for(i in 2:nrow(distance_matrix)) {
    for(j in 2:ncol(distance_matrix)) {
      repl = distance_matrix[i-1,j-1] + calculate_repl_btw_itemsets(seq1[[i-1]],seq2[[j-1]],fun)
      indel_r = distance_matrix[i,j-1] + calculate_indel(seq2[[j-1]],fun)
      indel_d = distance_matrix[i-1,j] + calculate_indel(seq1[[i-1]],fun)
      distance_matrix[i,j] = min(repl,indel_d,indel_r)
    }
  }

  results = list(distance_matrix = distance_matrix, distance = distance_matrix[nrow(distance_matrix),ncol(distance_matrix)])

  return(results)
}

calculate_dist_btw_seq_Wseq = function(seq, Wseq,fun = calculate_seq_soren_distance) {

  n = Wseq$n
  Wseq$n = NULL
  distance_matrix = matrix(nrow = length(seq)+1,ncol = length(Wseq)+1)

  distance_matrix[1,] = 0:length(Wseq)
  distance_matrix[,1] = 0:length(seq)

  for(i in 2:nrow(distance_matrix)) {
    for(j in 2:ncol(distance_matrix)) {
      xxx = seq[[i-1]]
      yyy= Wseq[[j-1]]
      repl = distance_matrix[i-1,j-1] + calculate_seq_soren_distance(seqItemset = xxx, WseqItemset = yyy, n = n)
      indel_r = distance_matrix[i,j-1] + 1 #calculate_indel(seq2[[j-1]],fun,n)
      indel_d = distance_matrix[i-1,j] + 1 #calculate_indel(seq1[[i-1]],fun,n)
      distance_matrix[i,j] = min(repl,indel_d,indel_r)
    }
  }

  results = list(distance_matrix = distance_matrix, distance = distance_matrix[nrow(distance_matrix),ncol(distance_matrix)])

  return(results)
}

