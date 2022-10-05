
library(ggplot2)
library(scales)
library(extrafont)
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(forcats)
library(ggfittext)

fix_text <- function(data) {
  a = c(" ", "-", "á","é","í","ó","ú")
  b= c("_", "", "a", "e", "i", "o", "u")
    data = tolower(data)
  data =  mgsub::mgsub(data, a,b)
  return(data)
}

# gráficos 
### colores ####
colores =c("#70ad47", "#ffc000", "#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4", "#538135",
           "#bf9000", "#92d050", "#00b0f0", "#757070", "#c5e0b3") 

colores_suave = c("#e2efd9","#fff2cc", "#dae3f3", "#dae3f3", "#dae3f3", "#fbe5d6")

### conseguir los colores deseados en https://www.ginifab.com/feeds/pms/color_picker_from_image.php
scales::show_col(colores)
myCol <- viridis(n = 25, option = "A")
show_col(myCol)
show_col(colores)

rainbow(8)

scales::show_col(colores)

wesanderson::wes_palettes$Darjeeling1
show_col(wesanderson::wes_palettes[[5]][2])

wesanderson::wes_palettes$Darjeeling1[1]
wesanderson::wes_palettes$Darjeeling1[2]


### temas ####
theme_elegante <- function(base_size = 10, base_family = "Calibri Light"){
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_blank()) +
    
    # Format the grid
    theme(panel.grid = element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_blank()) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

##### G R A F I C O S ####
# Conditional ggplot2 geoms in functions (QTL plots)
#https://shiring.github.io/ggplot2/2017/02/12/qtl_plots 
# Fitting text inside a box
# https://cran.r-project.org/web/packages/ggfittext/vignettes/introduction-to-ggfittext.html 

###### gráfico de torta ####
# var =g2016$Curso
# titulo = "Curso"
# lab.size= 3
 
grafico_torta = function(var, titulo, lab.size){
  ### colores ####
  colores =c("#70ad47", "#ffc000", "#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4", "#538135",
             "#bf9000", "#92d050", "#00b0f0", "#757070", "#c5e0b3") 
  
 dt = data.frame(prop.table(table(var)))
        dt = cbind(data.frame(table(var)), dt$Freq)
 names(dt) = c("var","n","Freq") 
 
 dt = dt %>% 
   filter(!var %in% c("","NULL")) %>% 
   mutate(porcentaje = paste0(n," - ",round(Freq*100,2),"%"))
 
dt$tempvar <- titulo

dt <- dt %>% 
  mutate(end = 2 * pi * cumsum(Freq)/sum(Freq),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

ggplot(dt) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = var)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = porcentaje,
                hjust = hjust, vjust = vjust), size = lab.size) +
  coord_fixed() +
  scale_fill_manual(values= colores) + theme_elegante()  +
  theme(axis.text.x=element_blank(),
        legend.position = "right", legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.7, 1.7),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)  + facet_grid(. ~ tempvar) +
  theme(strip.background = element_rect(fill="#fbe5d6", linetype = 0, margin()),
        strip.text.x = element_text(size=15, colour="black")) 
}

#var = sample(c("bajo","medio","alto","muy alto"),1000, replace = T)
#var2 = sample(c(1,0),1000, replace = T)
#  var = des2012$Freq
#  var2 = des2012$desercion
#  size.ejes = 7
#  size.text = 7
#  ancho = 0.9
#  horizont = T
#  legend.pos = "right"
#  leg.color = "black"
#  nomx = ""
#  nomy = ""
#  et.eje.x = T
#  et.eje.y = T
#  eje.color = "black"
#  stack = T
#  lab.text = T
#  lab.size = 10
#  lab.crece = F
#  lab.contrast = F
#  label.pos.text = "center"
#  color = c("#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4")
#  cat.filtro = F
#  cat.min = 100
#  exclud.NA = T
## 
 grafico_barr_2_var = function(var, 
                                var2, 
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
                                stack = F,
                                lab.text = T,
                                lab.crece = F,
                                lab.contrast = F,
                                label.pos.text = "center", #'topleft', 'top', 'topright', 'right', 'bottomright', 'bottom', 'bottomleft', 'left', and 'center'/'middle' which are both synonyms for 'centre'.
                                color = c("#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4"),
                                cat.filtro = F,
                                cat.min = 0,
                                exclud.NA = T
                              ){
if(stack == F){
  
dt2 = data.frame(table(var, var2, exclude = F))
dt3 = dt2 %>% 
  filter(!var %in% c("","NULL")) %>% 
  mutate(Desercion = ifelse(var2 == 0, "No desertan", "Desertan"))
  
if(cat.filtro == T){dt3 = dt3[dt3$Freq > cat.min,]}

if(exclud.NA == T){dt3 = filter(dt3, !is.na(var))}

}else{

  dt  = data.frame(prop.table(table(var, var2, exclude = F),1))
  dt = cbind(data.frame(table(var, var2, exclude = F)), dt$Freq)
  names(dt) = c("var","var2","n","Freq") 

dt3 =  dt %>% 
  filter(!var %in% c("","NULL")) %>% 
  mutate(Desercion = ifelse(var2 == 0, "No desertan", "Desertan"),
         porc = round(Freq*100,2)) 

if(exclud.NA == T){ dt3 = filter(dt3, !is.na(var)) }

if(cat.filtro == T){dt3 = dt3[dt3$n > cat.min,]}
} 

   if(stack == F ){
   prop = ggplot(dt3, aes(var,Freq, fill = Desercion))
   }else{
     prop = ggplot(dt3, aes(var,Freq, fill = Desercion, label = percent(Freq)))
   }
   
prop = prop   + {
    if(stack == F)
  geom_bar(position = "dodge", stat = "identity", width = ancho) else
    geom_bar(position = "stack", stat = "identity", width = ancho) }+
  theme_elegante() + 
  theme(legend.position = legend.pos, legend.text = element_text(colour= leg.color), axis.text.y = element_text(color = eje.color), axis.text.x = element_text(color = eje.color)) + {
    #### condicional para texto de ejes
  if(et.eje.x == F)  theme(axis.text.x = element_blank()) }+{  
    if(et.eje.y == F)  theme(axis.text.y = element_blank())  }+{
    if(lab.text == T){
            if(stack == F){
                    ### condicional Horizonral o vertical, etiquetas
                   if(horizont == F)  
                     geom_bar_text(position = "dodge", place = label.pos.text, contrast = lab.contrast, grow = lab.crece)
           }else{
          geom_bar_text(position = "stack", place = label.pos.text, contrast = lab.contrast, grow = lab.crece) 
          #geom_text(aes(var, Freq, label = paste0(round(porc, digit = 2),"%")), size = 4, position = position_stack(vjust = .5))
             }}}+{
          if(horizont == F)  coord_flip() 
        }+{
          if(stack == T) scale_y_continuous(labels = scales::percent) 
    }+
  labs(x =nomx, y = nomy) +
    scale_fill_manual(values= color)

  return(prop)
 }
 
 grafico_barr_2_var(des2012$ColegioNivelSocioeconomicoFIX,des2012$desercion,horizont =F, stack = F,lab.contrast = T, label.pos.text = "right")


#                   color = c(wesanderson::wes_palettes[[5]][2],wesanderson::wes_palettes$Darjeeling1[2]),
#                   cat.filtro = T, cat.min = 150)
#
# ##### matriz de confusión ####
 
 
 modelos0 = glm(desercion ~ Genero + IdJornada + Nem1F + Edad + AlumnoAceptante + QuintilMINEDUC, data = des2012, family = binomial)
 
 
  matriz_conf =  function(mod){
 
 mod$model$predict =  mod$fitted.values
 
 mod$model$predict_bol = ifelse( mod$model$predict > 0.47,  1, 0)
 
 table(mod$model$desercion, mod$model$predict_bol) -> t0
 
 addmargins(prop.table(t0))
 
 conf_m0 = prop.table(table(mod$model$desercion, mod$model$predict_bol))
 
 #write.excel(table(mod$model$desercion, mod$model$predict_bol))
 
 verPos = t0[2,2]
 verNeg = t0[1,1]
 falPos = t0[1,2]
 falNeg = t0[2,1]
 
 total  =  addmargins(t0)[3,3]
 
 #### accuracy (exactitud) Accuracy : (TP+TN)/Total . Describes overall, how often the classifier correct. 
acc =  (verPos + verNeg) / total #  0.6054713
 
 #### Sensitivity/Recall = TP/(TP + FN). When it’s actually yes, how often does it predict yes? 
sensiti = verPos/(verPos + falNeg) # 0.4675383
 
 ### Specificity = TN/(TN + FP) .When it’s actually no, how often does it predict no?? i.e 50/(50+10)
specifi = verNeg /(verNeg + falPos) # 0.6860275
 
 ###Precision = TP/predicted yes. When it predicts yes, how often is it correct?100/(10+100)
precis =  verPos /(verPos + falPos) #0.4651473

 indicadores = data.frame(Ind = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
                          Valor = c(acc, sensiti, specifi, precis))
 
 indicadores$Valor = round(indicadores$Valor,2)
 
 conf_m0 = round(conf_m0*100,2)
 
 lista = list(indicadores = indicadores, tabla_conf = conf_m0)
 
 return(lista)
 
 }
 
 matriz_conf(modelos0)
  
 
 
 
 ##### curva de roc #####
 roc = prediction(modelos0$model$predict, modelos0$model$desercion)
 
 proc = performance(roc, "tpr", "fpr")
 
 plot(proc)
 
 plot(proc, colorize=TRUE)
 
 # 1. Open jpeg file
 jpeg("reporte2/rocplot.jpg", width = 950, height = 950)
 # 2. Create the plot
 plot(proc, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), xlab = "Tasa de falsos positivos", ylab = "Tasa de verdaderos positivos")
 abline(a = 0, b = 1 )
 # 3. Close the file
 dev.off()
  
 
 
 data.frame(conf_m0) -> df
 names(df) = c("Real", "Predicho", "Valor")
 df$Valor= as.numeric(df$Valor)
 df$Real = ifelse(df$Real == 1,"Deserta", "No deserta") 
 df$Predicho = ifelse(df$Predicho == 1,"Deserta", "No deserta") 
 df
 
 
 animals
 ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
   geom_tile() +
   geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE)
 
 
 ggplot(animals, aes(x = type, y = flies, label = animal)) +
   geom_tile(fill = "white", colour = "black") +
   geom_fit_text()
 
 ggplot(data =  df,aes(y = Real, x = Predicho, label = paste0(round(Valor*100,2),"%"))) +
   geom_tile(fill = "white", colour = "black") +
   geom_fit_text() +
   theme_elegante() + 
   scale_fill_gradientn(colours = terrain.colors(10))
 scale_x_discrete(position = "top")
 
 geom_text(aes(label = paste0(round(Valor,2),"%")), vjust = 1) +
   scale_fill_gradient(low = "blue", high = "red") +
   theme_bw() + 
   theme(legend.position = "none")
 
 
 library(caret)
 library(tidyverse)
 data("GermanCredit")
 cm <- confusionMatrix(GermanCredit$Class, sample(GermanCredit$Class))
 
 cm$table %>%
   data.frame() %>% 
   mutate(Prediction = factor(Prediction, levels = c("Good", "Bad"))) %>%
   group_by(Reference) %>% 
   mutate(
     total = sum(Freq),
     frac_fill = if_else(Prediction == Reference, Freq / total, 0),
     frac = Freq / total * frac_fill
   ) %>%
   mutate(frac_directed = if_else(Prediction == "Bad", frac_fill * -1, frac_fill)) %>%
   ggplot(aes(Prediction, Reference, fill = frac_directed)) +
   geom_tile(color = "black") +
   geom_text(aes(label = str_c(Freq, ", ", round(frac * 100), "%")), size = 8) +
   scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
   scale_x_discrete(position = "top")
 
 

#grafico_barr_porc_n()


# grafico_torta(g2016si$Curso,"Generación 2016 - Cursos")


# ##### Gráfico de barras una variables ###
#
# grafico_barr1 = function(var,nombre,size.ejes,size.text,ancho, save = F){
#   if(missing(ancho)){ancho = 0.9}
#   
#   # var = des2012$Genero
#   dt = data.frame(prop.table(table(var)))
#   
#   dt %>% 
#     filter(!var %in% c("","NULL")) %>% 
#     mutate(porcentaje = paste0(round(Freq*100,2),"%")) %>% 
#     ggplot() + geom_bar(aes(var, Freq), position = "stack", stat = "identity") 
#   
#   , position = "stack", stat = "identity", width = ancho) + scale_y_continuous(labels = scales::percent) +
#     theme_elegante() + 
#     theme(legend.position = "none", 
#           axis.text.x =  element_text(size = size.ejes), 
#           axis.text.y =  element_text(size = size.text)
#     ) +
#     coord_flip() +
#     geom_text(aes(var, Freq, label = paste0(round(porc, digit = 2),"%")), 
#               size = 4, position = position_stack(vjust = .5)) +
#     labs(y = "Porcentaje", x="") -> porc
#   
#   porc = porc + theme(plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"))
#   
#   dt2 = data.frame(table(var, des2012$desercion))
#   
#   dt2 %>% 
#     filter(!var %in% c("","NULL")) %>% 
#     mutate(Desercion = ifelse(var2 == 0, "No desertan", "Desertan")) %>% 
#     ggplot() + geom_bar(aes(var, Freq,fill = Desercion), position = position_dodge(0.9), stat = "identity", width = ancho) +
#     theme_elegante() + 
#     theme(legend.position = "right",
#           axis.text.y = element_blank(),axis.text.x =  element_text(size = size.ejes)
#     ) + 
#     # geom_text(aes( label = Freq, y = var), size= 4.5,  position = position_dodge(width = 0.9)) +
#     coord_flip() + 
#     labs(x ="", y = "Cantidad") -> prop
#   
#   prop = prop + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")); prop
#   
#   plot_grid(porc, prop,ncol = 2, rel_widths = c(1, 1),labels = nombre) -> ploot
#   
#   return(ploot)
#   
#   nom = fix_text(nombre)
#   
#   if(save == T){save_plot(filename = paste0("grafico/",nom,".png"),plot = ploot, base_height = 2.5, base_width = 6)}
#   
# }


# 
# 
# ####################### PENDIENTES #####
# 
# ### conseguir los HEX de colores deseados de una imagen en https://www.ginifab.com/feeds/pms/color_picker_from_image.php
# ### create search colors http://www.sthda.com/english/wiki/colors-in-r  
# ### idem https://stackoverflow.com/questions/51867716/plot-colors-with-hex-values-in-r
# ### graficos elegantes, crear propio tema https://pmoracho.github.io/blog/2019/03/10/sample-ggplot-plots/
# 
# 
# ### graficos  PRO ggforce https://rviews.rstudio.com/2019/09/19/intro-to-ggforce/
# 
# ### como hacer grafiscos circulares con ggforce  https://stackoverflow.com/questions/48184645/how-can-i-put-the-labels-outside-of-piechart
# ### mas bibliografia https://www.statworx.com/de/blog/coordinate-systems-in-ggplot2-easily-overlooked-and-rather-un
# 
# 
# 
# 
# graficos = data.frame("genero" = c(" Women ", " Men ", " Transgender Woman "), n = c(8,1,1))
# graficos$tempvar <- "Gender"
# 
# graficos <- graficos %>% 
#   mutate(end = 2 * pi * cumsum(n)/sum(n),
#          start = lag(end, default = 0),
#          middle = 0.5 * (start + end),
#          hjust = ifelse(middle > pi, 1, 0),
#          vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
# 
# 
# colores =c("#70ad47", "#ffc000", "#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4", "#538135",
#            "#bf9000", "#92d050", "#00b0f0", "#757070", "#c5e0b3"         ) 
# 
# colores_suave = c("#e2efd9","#fff2cc", "#dae3f3", "#dae3f3", "#dae3f3", "#fbe5d6")
# 
# 
# 
# ### conseguir los colores deseados en https://www.ginifab.com/feeds/pms/color_picker_from_image.php
# scales::show_col(colores)
# 
# ### OTRAS OPCIONES DE COLORES
# myCol <- viridis(n = 25, option = "E")
# show_col(myCol)
# 
# # Create Base Theme
# #------------------
# theme_elegante <- function(base_size = 10,
#                            base_family = "Calibri Light"
# )
# {
#   color.background = "#FFFFFF" # Chart Background
#   color.grid.major = "#D9D9D9" # Chart Gridlines
#   color.axis.text = "#666666" # 
#   color.axis.title = "#666666" # 
#   color.title = "#666666"
#   color.subtitle = "#666666"
#   strip.background.color = '#9999CC'
#   
#   ret <-
#     theme_bw(base_size=base_size) +
#     
#     # Set the entire chart region to a light gray color
#     theme(panel.background=element_rect(fill=color.background, color=color.background)) +
#     theme(plot.background=element_rect(fill=color.background, color=color.background)) +
#     theme(panel.border=element_blank()) +
#     
#     # Format the grid
#     theme(panel.grid = element_blank()) +
#     theme(axis.ticks=element_blank()) +
#     
#     # Format the legend, but hide by default
#     theme(legend.position="none") +
#     theme(legend.background = element_rect(fill=color.background)) +
#     theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
#     
#     theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
#     theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
#     #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
#     theme(strip.background = element_blank()) +
#     # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
#     # Set title and axis labels, and format these and tick marks
#     theme(plot.title=element_text(color=color.title, 
#                                   size=20, 
#                                   vjust=1.25, 
#                                   family=base_family, 
#                                   hjust = 0.5
#     )) +
#     
#     theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
#     
#     theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
#     theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
#     theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
#     
#     theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
#     theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
#     
#     # Legend  
#     theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
#     theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
#     theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
#     theme(legend.position="bottom", 
#           legend.box = "horizontal", 
#           legend.title = element_blank(),
#           legend.key.width = unit(.75, "cm"),
#           legend.key.height = unit(.75, "cm"),
#           legend.spacing.x = unit(.25, 'cm'),
#           legend.spacing.y = unit(.25, 'cm'),
#           legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
#     
#     # Plot margins
#     theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
#   
#   ret
# }
# 
# 
# 
# #lank_theme <- theme_minimal()+
# # theme(
# #   axis.title.x = element_blank(),
# #   axis.title.y = element_blank(),
# #   panel.border = element_blank(),
# #   panel.grid= element_blank(),
# #   axis.ticks = element_blank(),
# #   plot.title=element_text(size=14, face="bold")
# # )
# 
# 
# library(ggforce) # for 'geom_arc_bar'
# ###### gráfico de torta ####
# 
# ggplot(graficos) + 
#   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
#                    start = start, end = end, fill = genero)) +
#   geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = n,
#                 hjust = hjust, vjust = vjust), size = 5) +
#   coord_fixed() +
#   scale_fill_manual(values= colores) + theme_elegante()  +
#   theme(axis.text.x=element_blank(),
#         legend.position = "bottom", legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
#   labs(x = "", y = "") +
#   scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
#                      name = "", breaks = NULL, labels = NULL) +
#   scale_y_continuous(limits = c(-1.2, 1.5),      # Adjust so labels are not cut off
#                      name = "", breaks = NULL, labels = NULL)  + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#fbe5d6", linetype = 0, margin()),
#         strip.text.x = element_text(size=15, colour="black")) 
# 
# ##### graficamos nacionalidades 
# 
# grafico2 = data.frame("nacionalidad" = c("Chile", "Puerto Rico","España", "Argentina", "México", "Venezuela"), 
#                       n = c(4, 3, 1, 1, 1, 1))
# 
# grafico2$tempvar = "Nationality" 
# 
# ggplot(grafico2, aes(x = fct_reorder(nacionalidad,n), y= n, fill = nacionalidad)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_fill_manual(values= colores)  +
#   labs(x = "", y = "") +
#   geom_text(aes(x = nacionalidad, y = n, label = n), hjust = -0.4,size = 5) +
#   theme_elegante() +
#   theme(legend.position = "none", axis.text.x = element_blank()) -> nacion
# 
# 
# 
# 
# nacion + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#dae3f3", linetype = 0, margin(1,1,1,1)),
#         strip.text.x = element_text(size=15, colour="black")) +
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/nacion.jpg", width = 4.2, height = 3)
# 
# 
# grafico3 = data.frame("topic" = c("About female of all sexual orientation",	"About men and women of all sexual orientation",	"About lesbian women",	"About men of all sexual orientation",	"About transgender and transexual sexuality"),
#                       n = c(5,	2,	1,	1,	1))
# 
# levels(grafico3$topic) -> fe   
# 
# fe[c(1,3,5,4,2)] -> fe
# 
# grafico3$topic = factor(grafico3$topic, levels = fe )
# 
# grafico3$tempvar = "Content" 
# 
# ggplot(grafico3, aes(x = fct_reorder(topic,n), y= n, fill = topic)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_fill_manual(values= colores)  +
#   labs(x = "", y = "") +
#   geom_text(aes(x = topic, y = n, label = n), hjust = -0.4,size = 5) +
#   theme_elegante() +
#   theme(legend.position = "bottom", legend.direction="vertical", axis.text.x = element_blank(),
#         axis.text.y = element_blank()) -> topic; topic
# 
# 
# 
# 
# topic + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#dae3f3", linetype = 0, margin(1,1,1,1)),
#         strip.text.x = element_text(size=15, colour="black")) +
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/topic.jpg", width = 4.2, height = 3)
# 
# ### graficos sexdictionary 
# sexdic = read.csv("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/diccionario_unesco (1).csv",sep=",", header = F, encoding = "UTF-8")
# 
# names(sexdic) = c("palabra", "significado", "pais", "edad", "genero", "time")
# 
# sexdic$pais = gsub("_", " ",sexdic$pais)
# 
# ### Contributions by country
# 
# sexdic %>% 
#   group_by(pais) %>% 
#   summarise(n =n()) %>% 
#   mutate(porc = n/sum(n)*100) %>% 
#   mutate(tempvar = "Contributions by country - Sex Dictionary Webpage") %>% 
#   ggplot() +
#   geom_bar(aes(x = fct_reorder(pais,n), y= porc, fill = pais),stat = "identity", color = "grey") +
#   scale_y_continuous(limits = c(0,120)) +
#   coord_flip() +
#   scale_fill_brewer(palette = "RdBu")  +
#   labs(x = "", y = "") +
#   geom_text(aes(x = pais, y = porc, label = n), hjust = -0.4,size = 6) +
#   theme_elegante() +
#   theme(legend.position = "none", axis.text.x = element_blank(), 
#         axis.text.y = element_text(size = 20)
#   ) -> pais; pais
# 
# 
# 
# pais + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#dae3f3", linetype = 0),
#         strip.text.x = element_text(size=20, colour="black"))+
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/sexdic_pais.jpg", scale = 4)
# 
# 
# 
# 
# ### Age
# 
# 
# 
# sexdic %>% 
#   group_by(edad) %>% 
#   summarise(n =n()) -> edades 
# 
# edades$n[edades$edad %in% c(30)] = mean(edades$n[edades$edad %in% c(29,31)])
# 
# edades %>% 
#   mutate(porc = n/sum(n)*100,
#          tempvar = "Age - Sex Dictionary Webpage") %>% 
#   ggplot() +
#   geom_bar(aes(x = edad, y= n, fill = edad),stat = "identity")  +
#   # coord_flip() +
#   #scale_color_brewer(palette = "RdBu")  +
#   labs(y = "Frecuency", x = "Age") +
#   #geom_text(aes(x = edad, y = n, label = n), hjust = -0.4,size = 6) +
#   theme_elegante() +
#   scale_x_discrete(limits = c(10, 20, 30, 40, 50, 60, 70)) +
#   theme(legend.position = "none") -> edad; edad
# 
# 
# edad + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#fff2cc", linetype = 0),
#         strip.text.x = element_text(size=15, colour="black")) +
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/sexdic_edad.jpg", width = 4.2, height = 3)
# 
# 
# #### gender
# 
# sexdic$genero = gsub("_", " ",sexdic$genero)
# 
# sexdic = sexdic %>% mutate(genero = case_when(genero == "Masculino" ~ "Mens",
#                                               genero == "Femenino" ~ "Women",
#                                               genero == "No Binario" ~ "Not Binary"))
# 
# table(sexdic$genero)
# 
# sexdic %>% 
#   group_by(genero) %>% 
#   summarise(n =n()) %>%  
#   mutate(tempvar = "Gender - Sex Dictionary Webpage",
#          porc = n/sum(n)*100) %>% 
#   ggplot() +
#   geom_bar(aes(x = fct_reorder(genero,n), y= n, fill = genero),stat = "identity", color = "grey")  +
#   scale_y_continuous(limits = c(0, 1200)) +
#   coord_flip() +
#   scale_fill_brewer(palette = "RdBu")  +
#   labs(y = "Frecuency", x = "") +
#   geom_text(aes(x = genero, y = n, label = n), hjust = -0.4,size = 6) +
#   theme_elegante() +
#   theme(legend.position = "none", axis.text.x = element_blank()) -> genero; genero
# 
# genero + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#e2efd9", linetype = 0),
#         strip.text.x = element_text(size=15, colour="black")) +
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/sexdic_genero.jpg", width = 6, height = 3)
# 
# #### resultados instagram nacionalidad
# 
# graf_insta1 = data.frame("nacionalidad" = c("Chile", "Puerto Rico","España", "Argentina", "Uruguay", "Otros"), 
#                          n = c(0.18, 0.03, 0.02, 0.66, 0.04, 0.07))
# 
# #graf_insta1$tempvar = "Followers by country - Sex Dictionary Instagram"
# 
# ### followers by country
# 
# graf_insta1 %>% 
#   mutate(tempvar = "Followers country - Sex Dictionary Instagram",
#          porc = n/sum(n)*100) %>% 
#   ggplot() +
#   geom_bar(aes(x = fct_reorder(nacionalidad,n), y= n, fill = nacionalidad),stat = "identity", color = "grey") +
#   scale_y_continuous(limits = c(0,0.80)) +
#   coord_flip() +
#   scale_fill_brewer(palette = "RdBu")  +
#   labs(x = "", y = "") +
#   geom_text(aes(x = nacionalidad, y = n, label = percent(n, accuracy = 1), hjust = -0.4,size = 6)) +
#   theme_elegante() +
#   theme(legend.position = "none", axis.text.x = element_blank(), 
#         axis.text.y = element_text(size = 10)
#   ) -> nacionalidad; nacionalidad
# 
# nacionalidad + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill= colores_suave[1], linetype = 0),
#         strip.text.x = element_text(size=15, colour="black"))+
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/insta_country.jpg", width = 7, height = 5)
# 
# 
# ### followers age
# 
# graf_insta2 = data.frame("edad" = c("13-17", "18-24","25-34", "35-44", "45-54", "55+"), 
#                          n = c(0.1, 0.26, 0.55, 0.14, 0.03, 0.018))
# 
# #levels(graf_insta2$edad) = rev(levels(graf_insta2$edad))
# 
# graf_insta2 %>% 
#   mutate(tempvar = "Followers Age - Sex Dictionary Instagram",
#          porc = n/sum(n)*100) %>% 
#   ggplot() +
#   geom_bar(aes(x = edad, y= n, fill = edad),stat = "identity", color = "grey") +
#   scale_y_continuous(limits = c(0,0.80)) +
#   #  coord_flip() +
#   scale_fill_brewer(palette = "RdBu")  +
#   labs(x = "Ages by rank", y = "") +
#   geom_text(aes(x = edad, y = n, label = percent(n, accuracy = 1), vjust = -1,size = 6)) +
#   theme_elegante() +
#   theme(legend.position = "none", axis.text.y = element_blank(), 
#         axis.text.x = element_text(size = 10)
#   ) -> edad; edad
# 
# edad + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill= colores_suave[1], linetype = 0),
#         strip.text.x = element_text(size=15, colour="black"))+
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/insta_edad.jpg", width = 7, height = 5)
# 
# 
# 
# #### gender instagram
# 
# graf_insta3 = data.frame(genero = c(" Mens ", " Women "), n = c(0.19,0.81))
# 
# graf_insta3  <- graf_insta3  %>% 
#   mutate(end = 2 * pi * cumsum(n)/sum(n),
#          start = lag(end, default = 0),
#          middle = 0.5 * (start + end),
#          hjust = ifelse(middle > pi, 1, 0),
#          vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
# 
# graf_insta3$tempvar = "Followers gender - Sex Dictionary Instagram"
# 
# ggplot(graf_insta3) + 
#   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
#                    start = start, end = end, fill = genero)) +
#   geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = percent(n,accuracy = 1),
#                 hjust = hjust, vjust = vjust), size = 5) +
#   coord_fixed() +
#   scale_fill_manual(values= colores) + theme_elegante()  +
#   theme(axis.text.x=element_blank(),
#         legend.position = "bottom", legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
#   labs(x = "", y = "") +
#   scale_x_continuous(limits = c(-3, 3),  # Adjust so labels are not cut off
#                      name = "", breaks = NULL, labels = NULL) +
#   scale_y_continuous(limits = c(-1.2, 1.5),      # Adjust so labels are not cut off
#                      name = "", breaks = NULL, labels = NULL)  -> gender; gender
# 
# 
# 
# gender + facet_grid(. ~ tempvar) +
#   theme(strip.background = element_rect(fill="#fbe5d6", linetype = 0, margin()),
#         strip.text.x = element_text(size=13, colour="black")) +
#   ggsave("C:/Users/ricpi/Desktop/Escritorio/UNESCO/Entregas/Entrega 3/graficos/definitivos/gender_insta.jpg", width = 4.2, height = 3)
# 
# 
# 