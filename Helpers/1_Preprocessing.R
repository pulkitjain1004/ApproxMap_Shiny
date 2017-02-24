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


#function to change date format
ord_date = function(data1, pd, st.date=1) { #st.date = start date #pd = period
  #pd = 1; st.date = 1; st.date = 2
  
  # (1) specify the format for the inputs  
  # (i) data
  data1 = data.frame(data1) #form the data as data frame format
  colnames(data1) = c("ID", "Date", "Item")
  data1 = data1[order(data1$ID, data1$Date),]
  data1$Date = as.Date(data1$Date, "%m/%d/%Y")
  #data.class(data1$Date)
  
  id.list = split(data1, data1$ID) #split data for each ID
  #data.class(id.list)
  
  for (i in 1:length(id.list)) {
    
    # (ii) period
    # period = 1: weekly
    #          2: calendar monthly 
    #          3: quaterly of a year 
    #          4: twice of a year
    #          5: annual
    
    # (iii) start date
    # for period = 1, start date: Sun=1 < Mon=2 < ... < Sat=6
    # for period = 4,             1, ..., 12
    # for period = 5,             1, ..., 12
    
    # (2) get the number for each period: dif.date
    if (pd == 1) {
      st.date = st.date - 7*(st.date > wday(min(id.list[[i]]$Date)))
      r.st.date = min(id.list[[i]]$Date) - wday(min(id.list[[i]]$Date)) + st.date #real start date
      dif.day = as.numeric(id.list[[i]]$Date - r.st.date) #day difference
      dif.date = dif.day %/% 7 + 1
    }
    else if (pd == 2) {
      fst.date = c(year(min(id.list[[i]]$Date)), month(min(id.list[[i]]$Date)))
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(fst.date, byrow=T, nrow=nrow(ym.mat), ncol=2) #year & month difference
      dif.date = dif.mat[,1]*12 + dif.mat[,2] + 1
    }
    else if (pd == 3) {
      fst.date = c(year(min(id.list[[i]]$Date)), quarter(min(id.list[[i]]$Date)))
      ym.mat = matrix(c(year(id.list[[i]]$Date), quarter(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(fst.date, byrow=T, nrow=nrow(ym.mat), ncol=2) #year & month difference
      dif.date = dif.mat[,1]*4 + dif.mat[,2] + 1
    }
    else if (pd == 4) {
      st.month = st.date - 6*(st.date > month(min(id.list[[i]]$Date)))
      r.st.date = c(year(min(id.list[[i]]$Date)), st.month) #real start date
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(r.st.date, byrow=T, nrow=nrow(ym.mat), ncol=2) 
      dif.mon = dif.mat[,1]*12 + dif.mat[,2] #month difference
      dif.date = dif.mon %/% 6 + 1
    }
    else if (pd == 5) {
      st.year = year(min(id.list[[i]]$Date)) - (st.date > month(min(id.list[[i]]$Date)))
      r.st.date = c(st.year, st.date) #real start date
      ym.mat = matrix(c(year(id.list[[i]]$Date), month(id.list[[i]]$Date)), ncol=2)
      dif.mat = ym.mat - matrix(r.st.date, byrow=T, nrow=nrow(ym.mat), ncol=2)
      dif.mon = dif.mat[,1]*12 + dif.mat[,2] #month difference
      dif.date = dif.mon %/% 12 + 1
    }
    
    # (3) output
    id.list[[i]]$Date = dif.date
  }
  
  data1 = ldply(id.list, data.frame)[,-1]
  data1 = data1[order(data1$ID, data1$Date),]
  
  
  
  return(data1)
}







