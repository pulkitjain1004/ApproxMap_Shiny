
get_Itemset_Formatted = function(W_itemset, add_itemset_weight = T) {
  with_weights = paste(W_itemset$elements,W_itemset$element_weights,sep = " : ")
  collapsed = paste(with_weights,collapse = ", ")
  result = ifelse(add_itemset_weight,paste("( ",collapsed," ) : ",W_itemset$itemset_weight,sep=""),paste("(",collapsed,")",sep=""))
  return(result)
}

get_C_Itemset_Formatted = function(C_itemset, no_white_space = T) {

  collapsed = paste(C_itemset,collapse = " , ")
  result = paste("( ",collapsed," ) ")
  if(no_white_space) result =  gsub(" ","",result)
  return(result)
}

get_consensus_formatted = function(consensus_pattern, no_space_bw_Items = T, no_space_bw_Itemsets = T) {
  pattern_list = consensus_pattern[[2]]
  itemsets_formatted = lapply(pattern_list, get_C_Itemset_Formatted, no_space_bw_Items)
  result = character()
  if(no_space_bw_Itemsets) {
    result = paste(itemsets_formatted, collapse = "")
    result = paste("(",result,")",sep="")
  } else {
    result = paste(itemsets_formatted, collapse = " ")
    result = paste("( ",result," )",sep="")
  }
  return(result)
}


get_Wseq_Formatted = function(W_seq, add_itemset_weight = T, no_white_space=T) {
  n = W_seq$n
  W_seq$n = NULL
  formatted_itemsets = lapply(W_seq,get_Itemset_Formatted, add_itemset_weight)
  formatted_itemsets = paste(formatted_itemsets, collapse = " ")
  result = paste("< ", formatted_itemsets," > : " , n,sep = "")
  if(no_white_space) result = gsub(" ","",result)
  return(result)
}


