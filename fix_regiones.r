arreglar_regiones = function(x){
  
  x = as.character(x)
 
x[x == 15] = "Arica y Parinacota"
x[x == 1 ] = "Tarapacá"
x[x == 2 ] = "Antofagasta"
x[x == 3 ] = "Atacama" 
x[x == 4 ] = "Coquimbo"
x[x == 5 ] = "Valparaíso"
x[x == 13] = "Metropolitana"
x[x == 6 ] = "O'Higgins" 
x[x == 7 ] = "Maule"
x[x == 16] = "Ñuble"
x[x == 8 ] = "Biobío" 
x[x == 9 ] = "La Araucanía" 
x[x == 14] = "Los Ríos"
x[x == 10] = "Los Lagos"
x[x == 11] = "Aysén"
x[x == 12] = "Magallanes" 
  
x = factor(x, levels = c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío", "Los Ríos", "La Araucanía", "Los Lagos", "Aysén", "Magallanes") ) 

return(x)
}

