p_a1 <- cbind(x = c(0, 0, 0.75, 1, 0.5, 0.8, 0.69, 0),
              y = c(0, 1, 1, 0.8, 0.7, 0.6, 0, 0))
p_a2 <- cbind(x = c(0.2, 0.2, 0.4, 0.2),
              y = c(0.25, 0.45, 0.4, 0.25))
p_b1 <- cbind(x = c(0.69, 0.8, 1.1, 1.23, 0.69),
              y = c(0, 0.6, 0.63, 0.3, 0))


library(raster)
spoly <- spPolygons(list(p_a1, p_a2[rev(seq(nrow(p_a2))), ]), p_b1, attr = data.frame(num = c(1, 2), name = c("poly1", "poly2"), property = c(0.1, 3), stringsAsFactors = FALSE))
library(spbabel)
tab <- sptable(spoly)

plot(y~x, tab, asp = 1)
plot(spoly)
text(y~x, tab, lab = seq(nrow(tab)))
axis(1);axis(2)
tab$order <- seq(nrow(tab))
#gtab <- tab  %>% arrange(cump, desc(order))
library(grid)
grid.newpage()
grid.circle(0.3, 0.38, r = 0.02, gp = gpar(fill = "red"))

grid.polygon(tab$x, tab$y, id = tab$cump, gp = gpar(fill  =scales::alpha(c("grey", "NA", "brown"), c(1, 1, 1))))

str(polygonGrob(tab$x, tab$y, id = tab$cump, gp = gpar(fill  =scales::alpha(c("grey", "NA", "brown"), c(1, 1, 1)))))


library(ggplot2)
g <- ggplot(tab) + aes(x = x, y = y) +  geom_polygon(aes(fill = object, group = object))

ggplot(tab) + aes(x = x, y = y) + geom_point(data = data.frame(x = 0.3, y = 0.38) + geom_polygon(aes(fill = object, group = object))

ggplot(data.frame(x = 0.3, y = 0.38)) + aes(x = x, y = y) + geom_point()

ggplot(tab) + aes(x = x, y = y) + geom_polygon(aes(fill = part, group = object))


partord <- function(x) {
  unlist(lapply(split(x, x), function(y) seq_along(y)))
}

library(raster)
spoly <- spPolygons(list(p_a1, p_a2[rev(seq(nrow(p_a2))), ]), p_b1, attr = data.frame(num = c(1, 2), name = c("poly1", "poly2"), property = c(0.1, 3), stringsAsFactors = FALSE))

library(spbabel)
tab <- sptable(spoly)

d <- as.data.frame(geom(spoly))
library(ggplot2)
ggplot(d) + aes(x = x, y = y, fill = object, group = object) + geom_polygon()

a <- as.data.frame(head(geom(spoly[1,], sepNA = TRUE), -1))
library(grid)
grid.newpage()
grid.polygon(rev(d$x), rev(d$y), id = rev(d$part),  gp = gpar(fill = c("grey")))
?grid.polygon


polyr <- spPolygons(list(p_a1[nrow(p_a1):1, ], p_a2), p_b1, attr = data.frame(num = c(1, 2), name = c("poly1", "poly2"), property = c(0.1, 3), stringsAsFactors = FALSE))

poly1 <- spPolygons(list(p_a1, p_a2), p_b1, attr = data.frame(num = c(1, 2), name = c("poly1", "poly2"), property = c(0.1, 3), stringsAsFactors = FALSE))
poly2 <- spPolygons(list(p_a1[nrow(p_a1):1, ], p_a2[nrow(p_a2):1, ]), p_b1, attr = data.frame(num = c(1, 2), name = c("poly1", "poly2"), property = c(0.1, 3), stringsAsFactors = FALSE))

ggplot(as.data.frame(head(geom(poly1[1,], sepNA = TRUE), -1))) + aes(x = x, y = y, fill = object, group = object) + geom_polygon()


library(ggplot2)
library(maptools)
d <- as.data.frame(head(geom(poly[1,], sepNA = TRUE), -1))
plot(d$x, d$y)
polypath(d$x, d$y, col = "grey", rule = "evenodd")
ggplot(d) + aes(x = x, y = y, fill = object, group = object) + geom_polygon()

library(gris)
g <- gris(poly)


p <- p_a1
pr <- p[nrow(p):1, ]

str(Polygon(p))
str(Polygon(pr))

str(Polygons(list(Polygon(p)), "1"))
str(Polygons(list(Polygon(pr)), "1"))

str(Polygons(list(Polygon(p)), "1"))
str(Polygons(list(Polygon(pr)), "1"))

str(SpatialPolygons(list(Polygons(list(Polygon(p)), "1"))))
str(SpatialPolygons(list(Polygons(list(Polygon(pr)), "1"))))


SpatialPolygons(list(Polygons(list(Polygon(p)),  "1")))@polygons[[1]]@Polygons[[1]]@coords
SpatialPolygons(list(Polygons(list(Polygon(pr)), "1")))@polygons[[1]]@Polygons[[1]]@coords

