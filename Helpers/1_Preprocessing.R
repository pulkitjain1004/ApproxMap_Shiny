split_comma_unlist = function(itemsetWithSep) {
  itemset = strsplit(itemsetWithSep,split=", ")[[1]]
}

apply_on_itemsets_in_seq = function(sequence,fun) {
  sequence = lapply(sequence,fun)
}

insert_blankItemset_before = function(sequence) {
  append(sequence,"_",0)
}

insert_itemset_before = function(itemset,sequence) {
  append(sequence, itemset,0)
}

process_Input = function(sequenceVector,id, Itemset_regex) {
  #split the each sequence based on the given regex into itemsets
  SeqsWithItemsets = strsplit(sequenceVector,split = Itemset_regex, fixed = F)
  
  #The splitting results in empty strings. Remove them.
  SeqsWithItemsets = lapply(SeqsWithItemsets,function(i) i[i != ""])
  
  #Each sequence has itemsets as strings. In case an itemset has 2 or more elements, it is seperated by commas.
  #So make the string array into a list, so that we can split by the commas
  SeqsWithItemsets = lapply(SeqsWithItemsets, as.list)
  
  #split the commas in each itemset, in each sequence
  SeqsWithItemsets = lapply(SeqsWithItemsets, apply_on_itemsets_in_seq, fun = split_comma_unlist)
  
  #assign the given id to each sequence
  #if(length(id)==1 & id==1) id = 1:length(SeqsWithItemsets)
  names(SeqsWithItemsets) = id
  
  return(SeqsWithItemsets)
}

insert_itemset_before_Wseq = function(weightedItemset,alignedWSeq) {
  append(alignedWSeq, weightedItemset,0)
}

insert_blankItemset_before_Wseq = function(Wseq) {
  Witemset = list(list(elements = "_",element_weights = 0, itemset_weight = 0))
  return(append(Wseq, Witemset,0))
}









