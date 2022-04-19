acontext("Newton's Method")
newton.method = function(
  FUN = function(x) x^2 - 4, init = 10, rg = c(-1, 10), tol = 0.001, nmax=50
) {
  i = 1
  nms = names(formals(FUN))
  grad = deriv(as.expression(body(FUN)), nms, function.arg = TRUE)
  x = c(init, init - FUN(init)/attr(grad(init), 'gradient'))
  gap = FUN(x[2])
  fx = FUN(x[1])
  
  while (abs(gap) > tol & i <= nmax & !is.na(x[i + 1])) {
    
    gap = FUN(x[i + 1])
    x = c(x, x[i + 1] - FUN(x[i + 1])/attr(grad(x[i + 1]), 'gradient'))
    fx <- c(fx, FUN(x[i + 1]))
    i = i + 1
    
    if (i > nmax) warning('Maximum number of iterations reached!')
  }
  
  x.temp = seq(rg[1], rg[2], length = abs(rg[2]-rg[1])/0.1)
  curve = data.frame(x=x.temp)
  curve$fx = as.numeric(lapply(curve$x, FUN))
  
  fx <- c(fx, FUN(x[i + 1]))
  objective <- data.frame(iteration = 1:(i+1), x = x, fx = fx)
  invisible(
    list(curve = curve, objective = objective)
  )
}

dat <- newton.method()
curve <- dat$curve
objective <- dat$objective
objective$newx <- c(objective$x[-(seq(1))], rep(tail(objective$x, n=1), 1))

library(plyr)
objective <- ldply(objective$iteration, function(i) {
  df <- subset(objective, iteration <= i)
  cbind(df, iteration2 = i)
})
objective2 <- subset(objective, iteration == iteration2)

library(animint2)
library(grid)


(curve.plot <- ggplot() + 
    geom_hline(aes(yintercept = 0), colour = "grey") +
    geom_line(data = curve, aes(x = x, y = fx), colour="black") +
#    geom_path(data = objective, aes(x = x, y = fx), 
#              showSelected = "iteration2", 
#              colour = "red", size = 1) +
    geom_segment(data = objective, aes(x = x, y = fx, xend = x, yend = 0), 
                 showSelected = "iteration2", 
                 colour = "blue") + 
    geom_segment(data = objective, aes(x = x, y = fx, xend = newx, yend = 0), 
                 showSelected = "iteration2", 
                 colour = "red") + 
    geom_point(data = objective, aes(x = x, y = fx), 
               showSelected = "iteration2", 
               colour = "red", 
               size = 3) + 
    geom_point(data = objective, aes(x = x, y = 0), 
               showSelected = "iteration2", 
               colour = "red", 
               size = 3) + 
    geom_point(data = objective, aes(x = newx, y = 0), 
               showSelected = "iteration2", 
               colour = "red", 
               size = 3) + 
    geom_text(data = objective2, aes(x = x - 0.2, y = fx + 0.5, label = fx),
              showSelected = "iteration2") +
    
    scale_x_continuous()+
    scale_y_continuous()+
    ggtitle("Root-finding by Newton-Raphson Method") + 
    theme_animint(width = 600, height = 600)
)
   

(objective.plot <- ggplot() +
    geom_hline(aes(yintercept = 0), colour = "grey") +
    geom_line(data = objective2, aes(x = iteration, y = fx), colour = "red") + 
    geom_point(data = objective2, aes(x = iteration, y = fx), colour = "red") + 
    geom_tallrect(data = objective2, aes(xmin = iteration - 1 / 2, xmax = iteration + 1 / 2), 
                  clickSelects = "iteration2", alpha = .3) + 
    geom_text(data = objective2, aes(x = iteration, y = fx + 0.5, 
                                     label = iteration), 
              showSelected = "iteration2") + 
    ggtitle("Objective Value vs. Iteration") + 
    theme_animint(width = 600, height = 600))

(viz <- animint(contour = curve.plot, objective = objective.plot, 
                time = list(variable = "iteration2", ms = 2000), 
                title = "Demonstration of Newton's Method"))

info <- animint2HTML(viz)
# Test of needed data columns in TSV files
chunk1.tsv <- file.path("animint-htmltest", "geom1_hline_contour_chunk1.tsv")
chunk1 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk2.tsv <- file.path("animint-htmltest", "geom2_line_contour_chunk1.tsv")
chunk2 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk3.tsv <- file.path("animint-htmltest", "geom3_segment_contour_chunk1.tsv")
chunk3 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

chunk4.tsv <- file.path("animint-htmltest", "geom4_segment_contour_chunk1.tsv")
chunk4 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk5.tsv <- file.path("animint-htmltest", "geom5_point_contour_chunk1.tsv")
chunk5 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk6.tsv <- file.path("animint-htmltest", "geom6_point_contour_chunk1.tsv")
chunk6 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk7.tsv <- file.path("animint-htmltest", "geom7_point_contour_chunk1.tsv")
chunk7 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk8.tsv <- file.path("animint-htmltest", "geom8_text_contour_chunk1.tsv")
chunk8 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk9.tsv <- file.path("animint-htmltest", "geom9_hline_objective_chunk1.tsv")
chunk9 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk10.tsv <- file.path("animint-htmltest", "geom10_line_objective_chunk1.tsv")
chunk10 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk11.tsv <- file.path("animint-htmltest", "geom11_point_objective_chunk1.tsv")
chunk11 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk12.tsv <- file.path("animint-htmltest", "geom12_tallrect_objective_chunk1.tsv")
chunk12 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")
chunk13.tsv <- file.path("animint-htmltest", "geom13_text_objective_chunk1.tsv")
chunk13 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

test_that("All root-finding points are same", {
  x1_nodes <- getNodeSet(info$html, "//g[@class='geom5_point_contour']/g[@class='PANEL1']/circle/@cx")
  y1_nodes <- getNodeSet(info$html, "//g[@class='geom5_point_contour']/g[@class='PANEL1']/circle/@cy")
  
  info$newHtml <- getHTML()
  
  x2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom5_point_contour']/g[@class='PANEL1']/circle/@cx")
  y2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom5_point_contour']/g[@class='PANEL1']/circle/@cy")
  
  expect_true(identical(x1_nodes, x2_nodes))
  expect_true(identical(y1_nodes, y2_nodes))
})

test_that("x and y labels are not null", {
  xlabel <- getNodeSet(info$html, "//text[@class='xtitle']")
  ylabel <- getNodeSet(info$html, "//text[@class='ytitle']")
  
  expect_equal(length(xlabel), 2)
  expect_equal(length(ylabel), 2)
})

test_that("Same Points Are Rendered on Integration Curve", {
  x1_nodes <- getNodeSet(info$html, "//g[@class='geom2_line_contour']/g[@class='PANEL1']/circle/@cx")
  y1_nodes <- getNodeSet(info$html, "//g[@class='geom2_line_contour']/g[@class='PANEL1']/circle/@cy")
  
  info$newHtml <- getHTML()
  
  x2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom2_line_contour']/g[@class='PANEL1']/circle/@cx")
  y2_nodes <- getNodeSet(info$newHtml, "//g[@class='geom2_line_contour']/g[@class='PANEL1']/circle/@cy")
  
  expect_true(identical(x1_nodes, x2_nodes))
  expect_true(identical(y1_nodes, y2_nodes))
})
