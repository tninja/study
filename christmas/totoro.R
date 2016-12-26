
library(ggplot2)

GeomChristmasTree <- ggproto("GeomChristmasTree", Geom,
     required_aes = c("x", "y"),
     default_aes = aes(shape = 19, colour = "black", 
         fill = "green4", size = 3,
         linetype = 1, alpha = 1,
         fontsize = 1),
     draw_key = draw_key_polygon,
   
     draw_panel = function(data, panel_scales, coord) {
       coords <- coord$transform(data, panel_scales)
     
       # each tree has 4*branches + 3 points
       if (length(coords$size) == 1) {
         tsize <- rep(pmax(1, round(coords$size)), length(coords$x))
         theight <- rep(pmax(0, round(coords$size)), length(coords$x))
       } else {
         tsize <- pmax(1, round(coords$size))
         theight <- pmax(0, coords$size)
       }

       # scale factors
       r01x <- diff(range(coords$x))/100
       r01y <- diff(range(coords$y))/100
     
       # coords
       longx <- unlist(lapply(seq_along(coords$x), function(i) {
         if (tsize[i] == 1) {
           dx <- -c(0.3, 0.3, 1.2, 0, -1.2, -0.3, -0.3)
         } else {
           dx <- -c(0.3, 0.3, rep(c(1.2,0.3), tsize[i]-1), 1.2, 0, -1.2, rep(c(-0.3,-1.2), tsize[i]-1), -0.3, -0.3)
         }
         r01x*dx + coords$x[i]
       }))
       longy <- unlist(lapply(seq_along(coords$y), function(i) {
         if (tsize[i] == 1) {
           dy <- c(-0.5, 0, 0, theight[i], 0, 0, -0.5)
         } else {
           dy <- c(-0.5, 0, 0, rep(1:(tsize[i]-1), each=2), theight[i], rep((tsize[i]-1):1, each=2), 0, 0, -0.5)
         }
         r01y*dy + coords$y[i]
       }))
       longid <- unlist(sapply(seq_along(coords$y), function(i) {
         rep(i, each=4*tsize[i]+3)
       }))
     
       grid::polygonGrob(
         longx, 
         longy,
         id = longid,
         gp = grid::gpar(col = coords[,"colour"],
                         fill = coords[,"fill"],
                         fontsize = 10)
       )
     }
)

geom_christmas_tree <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomChristmasTree, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

png(filename="totoro.christmas.png",height=640,width=480)
### Image processing
library(EBImage)

totoro.img <- readImage('totoro.png')
colorMode(totoro.img) = Grayscale

totoro.bw <- ifelse(totoro.img < 0.4, 1, 0)
totoro.bw.label <- bwlabel(totoro.bw)

w <- dim(totoro.bw.label)[1]; h <- dim(totoro.bw.label)[2]
totoro.img.resize <- resize(totoro.bw.label, w / 3, h / 3)

### display(totoro.img.resize)

### Convert to dataframe
library(dplyr)
totoro.df <- as.data.frame.table(totoro.img.resize) %>%
    dplyr::filter(Var3 == 'A') %>%
        mutate(x = - as.integer(Var1), y = - as.integer(Var2), z=Freq) %>%
            select(x, y, z) %>% dplyr::filter(z > 0) %>% mutate(z=as.factor(z))

summary(totoro.df)

### render
library(ggplot2)
library(ggthemes)
library(xkcd)

## theme_xkcd <- theme(panel.background = element_rect(fill="white"),
##                     axis.ticks = element_line(colour=NA),
##                     panel.grid = element_line(colour="white"),
##                     axis.text.y = element_text(colour=NA),
##                     axis.text.x = element_text(colour="black"),
##                     text = element_text(size=16, family="Humor Sans"))

g <- ggplot(totoro.df, aes(x, y, fill=z)) + geom_christmas_tree(size=1) +
    theme_solarized() + scale_colour_solarized("blue") +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                geom_text(x=(max(totoro.df$x) + min(totoro.df$x)) / 2, y=median(totoro.df$y) * 1.15, label='Merry Christmas', size=15, family = 'xkcd') +
                    ggtitle("Using ggplot2 + geom_christmas_tree() + EBImage\nCode available at https://github.com/tninja/study/blob/test/christmas/christmas.tree.org") +
                        theme(legend.position = 'none')
print(g)
dev.off()
