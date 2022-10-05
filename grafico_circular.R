
source("/home/ricardo/Documents/coding/R/bodega_funciones/tema_elegante_ggplot.R")

###### gráfico de torta ####
# var = des2012$desercionCAT
# titulo = "Curso"
# lab.size= 7
# colores = unique(c("#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4",wesanderson::wes_palettes$Darjeeling1, wesanderson::wes_palettes$Darjeeling2,wesanderson::wes_palettes$BottleRocket1)) 
# legend.pos = "bottom"
# leg.color = "black"
# subt = ""
# captio = ""

grafico_torta = function(var, 
                         titulo = "", 
                         lab.size = 7,
                         t.size = 15,
                         t.color = "black", 
                         colores = unique(c("#ed7d31", "#5b9bd5", "#a5a5a5", "#4472c4",wesanderson::wes_palettes$Darjeeling1, wesanderson::wes_palettes$Darjeeling2,wesanderson::wes_palettes$BottleRocket1)),
                         legend.pos = "bottom",
                         leg.color = "black",
                         leg.size = 8,
                         subt = "",
                         captio = ""
                         ){

  dt = data.frame(prop.table(table(var)))
  dt = cbind(data.frame(table(var)), dt$Freq)
  names(dt) = c("var","n","Freq") 
  
  dt = dt %>% 
    filter(!var %in% c("","NULL")) %>% 
    mutate(porcentaje = paste0(round(Freq*100,2),"%"))
  
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
   # ggtitle(titulo) +
    theme(axis.text.x=element_blank(),
          legend.position = legend.pos, legend.title = element_blank(), legend.text = element_text(colour= leg.color, size = leg.size),
          plot.title = element_text(hjust = 0.5, size = t.size, colour = t.color)) +
    labs(x = "", y = "") +
    scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.7, 1.7),      # Adjust so labels are not cut off
                       name = "", breaks = NULL, labels = NULL)  + facet_grid(. ~ tempvar) +
    labs(title = titulo, subtitle = subt, caption = captio)
    
    # marco para el título, elegante
 #  theme(strip.background = element_rect(fill="#fbe5d6", linetype = 0, margin()),
 #        strip.text.x = element_text(size=15, colour="black")) 
}
