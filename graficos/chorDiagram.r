
library(chorddiag)

m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)

groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
chorddiag(m, groupColors = groupColors, groupnamePadding = 20)



# a list of connections between 20 origin nodes, and 5 destination nodes:
numbers <- sample(c(1:1000), 100, replace = T)
data <- matrix( numbers, ncol=5)
rownames(data) <- paste0("orig-", seq(1,20))
colnames(data) <- paste0("dest-", seq(1,5))

# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(data, transparency = 0.5)


# Create an edge list: a list of connections between 10 origin nodes, and 10 destination nodes:
origin <- paste0("orig ", sample(c(1:10), 20, replace = T))
destination <- paste0("dest ", sample(c(1:10), 20, replace = T))
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)

x0 = g2016si$X12...Cómo.Financias.tus.estudios.este.año....puede.seleccionar.varias.o.solo.una.

x1 = str_split(x0,", ", simplify = T)

x1 = x1[!x1 %in% "" & !is.na(x1)]

x1[x1 == "beca fundación "] = "Beca"
x1[x1 == "no sabe todavia "] = "¿Trabajas para pagar tus estudios?"
x1[x1 == "esperando los resultados de la gratuidad "] = "Ahorros"

x1[x1 == "ahorros "] = "Ahorros"

x1[x1 == "¿Trabajas para pagar tus estudios?"] = "Trabajando"

table(x1)

x0[x0 == "beca fundación "] = "Beca"
x0[x0 == "no sabe todavia "] = "¿Trabajas para pagar tus estudios?"
x0[x0 == "esperando los resultados de la gratuidad "] = "Ahorros"
x0[x0 == "ahorros "] = "Ahorros"

table(x0)

pat <- unique(x1)

pat[pat == "Trabajando"] = "Trabaj"
pat = tolower(pat)
x0 = tolower(x0)

tabla1 = matrix(nrow = length(x0),ncol = length(pat) )

for(i in seq_len(length(pat))){
assign(paste0(pat[i]), grepl(pat[i], x0, ))
        
tabla1[,i] = as.integer(get(paste0(pat[i])))

  #      print(table(get(paste0(pat[i]))))
}

adv = crossprod(tabla1)
#diag(adv) = 0

row.names(adv) = pat
colnames(adv) = pat
adv 

circos.clear()
chordDiagram(adv, transparency = 0.5, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.03, 0.15))

adv

circos.clear()
col_text <- "grey40"
circos.par("track.height"=0.8,gap.degree=5,cell.padding=c(0,0,0,0))

circos.initialize(factors=c(1,2,3,4), )


#circos.initialize(1:10, xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  sn = CELL_META$sector.index
  i_state = as.numeric(gsub("(C|R)_", "", sn))
  circos.text(CELL_META$xcenter, CELL_META$ycenter,i_state, col = "white", 
              font = 2, cex = 0.7, adj = c(0.5, 0.5), niceFacing = TRUE)
})

load("/home/ricardo/Downloads/chromatin_transition.RData")

circos.clear()

all_states = rownames(mat)
n_states = nrow(mat)

rownames(mat) = paste0("R_", seq_len(n_states))
colnames(mat) = paste0("C_", seq_len(n_states))

dimnames(meth_mat_1) = dimnames(mat)
dimnames(meth_mat_2) = dimnames(mat)


state_col = c("TssA" = "#E41A1C",    "TssAFlnk" = "#E41A1C",
              "TxFlnk" = "#E41A1C",  "Tx" = "#E41A1C",
              "TxWk" = "#E41A1C",    "EnhG" = "#E41A1C",
              "Enh" = "#E41A1C",     "ZNF/Rpts" = "#E41A1C",
              "Het" = "#377EB8",     "TssBiv" = "#377EB8",
              "BivFlnk" = "#377EB8", "EnhBiv" = "#377EB8",
              "ReprPC" = "#377EB8",  "ReprPCWk" = "#377EB8",
              "Quies" = "black")

# one for rows and one for columns
state_col2 = c(state_col, state_col)
names(state_col2) = c(rownames(mat), colnames(mat))

colmat = rep(state_col2[rownames(mat)], n_states)
colmat = rgb(t(col2rgb(colmat)), maxColorValue = 255)

qati = quantile(mat, 0.7)
colmat[mat > qati] = paste0(colmat[mat > qati], "A0")
colmat[mat <= qati] = paste0(colmat[mat <= qati], "20")
dim(colmat) = dim(mat)

circos.clear()
circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)


cdm_res = chordDiagram(mat, col = colmat, grid.col = state_col2,
                       directional = TRUE, annotationTrack = "grid", 
                       big.gap = 10, small.gap = 1,
                       preAllocateTracks = list(track.height = 0.1),
                       link.target.prop = FALSE)



circos.clear()
col_text <- "grey40"
circos.par("track.height"=0.8,gap.degree=5,cell.padding=c(0,0,0,0))
circos.initialize(factors=ref$V1,xlim=matrix(c(rep(0,3),ref$V2),ncol=2))



names(CELL_META)


circos.track(track.index = 2, panel.fun = function(x, y) {
  if(abs(CELL_META$cell.start.degree - CELL_META$cell.end.degree) > 3) {
    sn = CELL_META$sector.index
    i_state = as.numeric(gsub("(C|R)_", "", sn))
    circos.text(CELL_META$xcenter, CELL_META$ycenter, i_state, col = "white", 
                font = 2, cex = 0.7, adj = c(0.5, 0.5), niceFacing = TRUE)
    xlim = CELL_META$xlim
    breaks = seq(0, xlim[2], by = 4e5)
    circos.axis(major.at = breaks, labels = paste0(breaks/1000, "KB"), labels.cex = 0.5)
  }
}, bg.border = NA)
circos.initialize()




circos.initialize(factors = sample(letters[1:4], 20, replace = TRUE), xlim = c(0, 1))
circos.info()
circos.clear()


###############################################3
set.seed(999)
n = 1000
df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
                x = rnorm(n), y = runif(n))
  
library(circlize)
circos.par("track.height" = 0.1)
circos.initialize(df$sectors, x = df$x)

circos.track(df$sectors, y = df$y,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })

#circos.clear()

col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$sectors, df$x, df$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, c("text","fe"), sector.index = "a", track.index = 1)

