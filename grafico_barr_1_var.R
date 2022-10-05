

source("/home/ricardo/Documents/coding/R/bodega_funciones/tema_elegante_ggplot.R")

# var = sample(c("bajo","medio","alto","muy alto"),1000, replace = T)
# # var = des2012$desercionCAT
# size.ejes = 7
# size.text = 7
# ancho = 0.9
# horizont = T
# legend.pos = "right"
# leg.color = "black"
# nomx = ""
# nomy = ""
# et.eje.x = T
# et.eje.y = T
# eje.color = "black"
# porcentaje = T
# lab.text = T
# lab.size = 10
# lab.crece = F
# lab.contrast = F
# label.pos.text = "center"
# color = c("#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4")
# cat.filtro = F
# cat.min = 100
# exclud.NA = T
# 

grafico_barr_1_var = function(var, 
                              size.ejes = 7, 
                              size.text = 7, 
                              ancho = 0.9, 
                              horizont = T,
                              legend.pos = "right",
                              leg.color = "black",
                              nomx = "",
                              nomy = "",
                              et.eje.x = T,
                              et.eje.y = T,
                              eje.color = "black",
                              porcentaje = T,
                              lab.text = T,
                              lab.crece = F,
                              lab.contrast = F,
                              label.pos.text = "center", #'topleft', 'top', 'topright', 'right', 'bottomright', 'bottom', 'bottomleft', 'left', and 'center'/'middle' which are both synonyms for 'centre'.
                              color = c("#ed7d31"),
                              cat.filtro = F,
                              cat.min = 0,
                              exclud.NA = T
){
  if(porcentaje == F){
    
    dt2 = data.frame(table(var,exclude = F))
    dt3 = dt2 %>% 
      filter(!var %in% c("","NULL")) 
    
    if(cat.filtro == T){dt3 = dt3[dt3$Freq > cat.min,]}
    
    if(exclud.NA == T){dt3 = filter(dt3, !is.na(var))}
    
  }else{
    
    dt  = data.frame(prop.table(table(var,exclude = F)))
    dt = cbind(data.frame(table(var, exclude = F)), dt$Freq)
    names(dt) = c("var","n","Freq") 
    
    dt3 =  dt %>% 
      filter(!var %in% c("","NULL")) %>% 
      mutate(porc = round(Freq*100,1)) 
    
    if(exclud.NA == T){ dt3 = filter(dt3, !is.na(var)) }
    
    if(cat.filtro == T){dt3 = dt3[dt3$n > cat.min,]}
  } 
  
  if(porcentaje == F ){
    prop = ggplot(dt3, aes(var,Freq))
  }else{
    prop = ggplot(dt3, aes(var,Freq, label = round(Freq*100,1) ))
  }
  
  prop = prop   + {
    if(porcentaje == F)
      geom_bar(position = "dodge", stat = "identity", width = ancho, fill = color) else
        geom_bar(position = "stack", stat = "identity", width = ancho, fill = color) }+
    theme_elegante() + 
    theme(legend.position = legend.pos, legend.text = element_text(colour= leg.color), axis.text.y = element_text(color = eje.color), axis.text.x = element_text(color = eje.color)) + {
      #### condicional para texto de ejes
      if(et.eje.x == F)  theme(axis.text.x = element_blank()) }+{  
        if(et.eje.y == F)  theme(axis.text.y = element_blank())  }+{
          if(lab.text == T){
            if(porcentaje == F){
              ### condicional Horizonral o vertical, etiquetas
              if(horizont == F)  
                geom_bar_text(position = "dodge", place = label.pos.text, contrast = lab.contrast, grow = lab.crece)
            }else{
              geom_bar_text(position = "stack", place = label.pos.text, contrast = lab.contrast, grow = lab.crece) 
              #geom_text(aes(var, Freq, label = paste0(round(porc, digit = 2),"%")), size = 4, position = position_stack(vjust = .5))
            }}}+{
              if(horizont == F)  coord_flip() 
            }+{
              if(porcentaje == T) scale_y_continuous(labels = scales::percent) 
            }+
    labs(x =nomx, y = nomy)
  
  return(prop)
}

# grafico_barr_1_var(var,horizont =F, porcentaje = T,lab.contrast = T, label.pos.text = "right",color = "blue", ancho = 0.5)

#
#dt  = data.frame(prop.table(table(var,exclude = F)))
#dt = cbind(data.frame(table(var, exclude = F)), dt$Freq)
#names(dt) = c("var","n","Freq") 
#
# dt %>% 
#   filter(!var %in% c("","NULL")) %>% 
#   mutate(porc = round(Freq*100,2)) %>% 
#   ggplot( aes(var, Freq, fill = "red")) +
#    geom_bar(position = position_dodge(width = 0.01), stat = "identity", width = 0.2)
# 
# 
