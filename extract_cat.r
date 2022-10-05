extract_cat = function(var, total = T){
  #### condicional si se quiere con total #
  if(total == T){
    
    fe = list()
    ### si no tiene etiquetas  o si
    if(is.null(labelled::val_labels(var))){
      
      fe[[1]] <- sort(unique(var))
      
    }else{
      
      fe[[1]] = labelled::val_labels(var)
    }
    
    names(fe[[1]]) = "Total"
    
    for (i in 1:length(unique(var))){
      ### condicional si no tiene etiquetas  o si
      if(!is.null(names(labelled::val_labels(var)))){
        
        labelled::val_labels(var)[labelled::val_labels(var) %in% unique(var)] -> fi
        names(fi) <- NULL
        fe[[i+1]] <- sort(fi, decreasing = T)[i] 
        names(fe[[i+1]]) <- rev(names(labelled::val_labels(var)[labelled::val_labels(var) %in% unique(var)]))[i] 
        
      }else{
        
        ####  condicional si no tiene niveles (leves)  o si
        if(is.null(levels(var))){
          
          fe[[i+1]] <-  sort(unique(var), decreasing = T)[i]
          
          names(fe[[i+1]]) <- sort(unique(var), decreasing = T)[i]
          
        }else{
          
          fe[[i+1]] <- ordered(levels(var))[i]
          
          names(fe[[i+1]]) <- ordered(levels(var))[i]
          
        }
        
      }
    }
    ##### sin total
  }else{
    fe = list()
    
    for (i in 1:length(unique(var))){
      
      if(!is.null(names(labelled::val_labels(var)))){
        
        labelled::val_labels(var)[labelled::val_labels(var) %in% unique(var)] -> fi
        names(fi) <- NULL
        
        fe[[i]] <- sort(fi, decreasing = T)[i] 
        names(fe[[i]]) <-rev(names(labelled::val_labels(var)[labelled::val_labels(var) %in% unique(var)]))[i] }else{
          
          
          if(is.null(levels(var))){
            
            fe[[i]] <-  sort(unique(var), decreasing = T)[i]
            
            names(fe[[i]]) <- sort(unique(var), decreasing = T)[i]
            
          }else{
            
            fe[[i]] <- ordered(levels(var))[i]
            
            names(fe[[i]]) <- ordered(levels(var))[i]
            
          }
          
        }
    }
  }
  
 cat_names = names(unlist(fe))[!is.na(names(unlist(fe)))]
  
  cat = unlist(fe)[!is.na(names(unlist(fe)))]
  
  names(cat) =  NULL
  
  if(total == T){
  cat[1] = length(cat)
  }
  
  fe = list(catYname = fe, cat = cat, names = cat_names)
  
  return(fe)
  
}



