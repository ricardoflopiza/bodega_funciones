grafico_red_palabras = function(base, var,varSub = NULL, subset = NULL){
  
  base[[var]] = as.character(base[[var]])
  
  txtA = base[[var]][!is.na(base[[var]])]
  
  if(!is.null(subset)){
    txtA = base[[var]][!is.na(base[[var]]) & base[[varSub]] %in% subset ]
  }
  
  txtA = as.character(txtA)
  
  toks <- quanteda::tokens(quanteda::char_tolower(txtA), remove_punct = TRUE)
  
  set.seed(232)
  
  fe = unlist(str_split(txtA,pattern = " "))
  
  fe= fe[fe != ""]
  
  fe = data.frame(table(fe)) 
  
  fe = fe[order(fe$Freq, decreasing = T),]
  
  plot = textplot_network(fcm(toks, context = "document"), edge_size = 2,omit_isolated = F,vertex_size = ((fe$Freq /10)+1))
  
  return(plot)
}
grafico_nube_palabras = function(base, var,varSub = NULL, subset = NULL){
  
  base[[var]] = as.character(base[[var]])
  
  txtA = base[[var]][!is.na(base[[var]])]
  
  if(!is.null(subset)){
    txtA = base[[var]][!is.na(base[[var]]) & base[[varSub]] %in% subset ]
  }
  
  txtA = as.character(txtA)
  
  #toks <- quanteda::tokens(char_tolower(txtA), remove_punct = TRUE)
  
  set.seed(232)
  
  #fe = unlist(str_split(txtA,pattern = " "))
  
  fe = data.frame(table(txtA)) 
  
  fe = fe[order(fe$Freq, decreasing = T),]
  
  fe$angle = (90 * sample(c(0, 1),length(fe$txtA),  
                          replace = TRUE, prob = c(40, 60)))
  
  colors = grDevices::rainbow(150, start =  0, end = 0.25, s = 1, v = 0.8)
  
  set.seed(42)
  plot = ggplot(fe, aes(label = txtA, size = Freq, angle = angle, color = factor(sample.int(150, nrow(fe), replace = TRUE)) )) +
    ggwordcloud::geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = .35, shape = "square") +
    scale_size_area(max_size = 80) +
    theme_minimal() 
  return(plot)
}

grafico_upset <- function(base, var,varSub = NULL, subset = NULL){
  
  base[[var]] = as.character(base[[var]])
  
  txtA = base[[var]][!is.na(base[[var]])]
  
  if(!is.null(subset)){
    txtA = base[[var]][!is.na(base[[var]]) & base[[varSub]] %in% subset ]
  }
  
  tibble(fe = str_split(txtA, ", ")) %>%
    #distinct(title, year, length, .keep_all=TRUE) %>%
    ggplot(aes(x=fe)) +
    geom_bar(fill = c("#ed7d31")) +
    scale_x_upset(order_by = "degree", n_sets = 5) +
    theme_elegante() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
}

recat = function(x,var,titulo = F,minuscula = F, Na_reemp = F,v.exacto = NULL,v.contenido = NULL,v.reemplazo = NULL){
  
  x = trimws(x)
  x = tolower(x)
  
  
  if(Na_reemp == T){
    x[is.na(x)] = "No Aplica"
  }
  
  if(!is.null(v.reemplazo)){
    ### reemplazo de palabras específicas
    x = as.character(x)
    
    v.reemplazo = tolower(v.reemplazo)
    
    if(is.null(v.contenido)){
      v.exacto = tolower(v.exacto)
      
      x[x %in% v.exacto] = v.reemplazo
      #print(x)
    }
    
    
    if(is.null(v.exacto)){
      
      v.contenido = tolower(v.contenido)
      x[grepl(v.contenido,x)] = v.reemplazo
      #print(x)
    }
    
    if(!is.null(v.contenido) & !is.null(v.exacto)){  
      
      v.contenido = tolower(v.contenido)
      v.exacto = tolower(v.exacto)
      
      x[x %in% v.exacto | grepl(v.contenido,x)] = v.reemplazo
      #print(x)
    }
    
    x = as.factor(x)  }
  
  if(minuscula == T){
    x = tolower(x)
  }
  
  if(titulo == T){
    x = str_to_title(x)
  }
  
  print("Valores únicos")
  print(unique(x))
  
  
  dt = data.frame(table(x))
  
  print("Cantidad valores únicos")
  print(dt[order(dt$Freq,decreasing = T),])
  
  return(x)
}

fix_text = function(x){
  
  x = trimws(x)
  x = tolower(x)  
  
  x = stringi::stri_replace_all_coll(x,pattern =  c("á","é","í","ú","ó"), replacement = c("a","e","i","u","o"), vectorize_all = F)
  return(x)
}