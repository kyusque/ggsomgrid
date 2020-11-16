

#' Title
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#' @importFrom ggbiplot ggbiplot
#'
#' @examples
plot.biplot.pc <- function(object, ...){
  ggbiplot(object$pc, ...)
}


#' plot.piesom.pc
#'
#' @param object
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#' @examples
plot.piesom.pc <- function(object){
  tibble(PC1 = object$pc$scores[,1],
         PC2 = object$pc$scores[,2],
         res = model.response(model.frame(object$formula, data = object$data))) %>%
    ggplot(aes(x = PC1, y = PC2, col = res)) +
    geom_point() +
    ggtitle("PCA")
}



#' plot.piesom.check
#'
#' @param object
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom viridis viridis
#' @examples
plot.piesom.check <- function(object){
  pc <- tibble(
    type = "pc",
    PC1 = object$pc$scores[,1],
    PC2 = object$pc$scores[,2],
    grid_x = 0,
    grid_y = 0,
    time = "before")
  pc_ <- tibble(
    type = "pc",
    PC1 = object$pc$scores[,1],
    PC2 = object$pc$scores[,2],
    grid_x = 0,
    grid_y = 0,
    time = "after")
  init<- tibble(
    type = "som",
    PC1 = object$init.score[,1],
    PC2 = object$init.score[,2],
    grid_x = factor(object$grid$pts[, 1]),
    grid_y = factor(object$grid$pts[, 2]),
    time = "before")
  som <- tibble(
    type = "som",
    PC1 = object$som.score[,1],
    PC2 = object$som.score[,2],
    grid_x = factor(object$grid$pts[, 1]),
    grid_y = factor(object$grid$pts[, 2]),
    time = "after")
  data <- rbind(pc, pc_,init, som) %>%
    mutate(time = factor(time, c("before", "after")))

  b <- data %>% filter(grid_x != 0) %>% mutate(grid_x_end = grid_x, grid_y_end = factor(as.integer(grid_y) + 1))
  d <- select(b, -c(grid_x, grid_y)) %>%
    left_join(select(b, -c(grid_x_end, grid_y_end)), by = c(type = "type", time = "time", grid_x_end = "grid_x", grid_y_end = "grid_y")) %>%
    mutate(grid_x = grid_x_end, grid_y = NA)

  data %>%
    ggplot(aes(x = PC1, y = PC2, col = grid_x, shape = type)) +
    geom_point() +
    scale_shape_manual(values = c(4, 16)) +
    scale_color_manual(values = c("red", viridis(object$grid$xdim, end = 0.9))) +
    facet_wrap(.~time) +
    theme(legend.position = "none") +
    ggtitle("SOM grid in PCA space") +
    geom_segment(data = d, aes(x = PC1.x, y = PC2.x, xend = PC1.y, yend = PC2.y))
}

#' plot.piesom.property
#'
#' @param object
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom ggforce geom_circle
#' @examples
#' data(iris)
#' piesom_setup(iris, Species ~ .) %>% piesom_som() %>% plot.piesom.property()
plot.piesom.property <- function(object){
  som_tidy <- as_tibble(cbind(object$grid$pts, object$som$codes[[1]])) %>%
    dplyr::mutate(., id = seq_along(x)) %>%
    pivot_longer(-c(x, y, id))
  res_sd <- som_tidy %>%
    group_by(name) %>%
    dplyr::summarise(.,sd = sd(value), .groups = "drop") %>%
    arrange(-sd)
  som_tidy %>%
    dplyr::mutate(.,name = factor(name, res_sd$name)) %>%
    ggplot(aes(x0 = x, y0 = y)) +
    geom_circle(aes(r = 0.5, fill = value)) +
    facet_wrap(.~name) +
    #scale_fill_viridis(option = "C") +
    ggtitle("Property(SOM)")
}


#' plot.piesom.map
#'
#' @param object
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom kohonen unit.distances
#' @importFrom kohonen object.distances
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggforce geom_circle
#' @importFrom ggforce geom_rect
#' @return
#' @export
#'
#' @examples
plot.piesom.map <- function(object, labels = NULL, grid.labels = NULL){
  response <- model.response(model.frame(object$formula, data = object$data))
  a <- tibble(groups = response, id = object$som$unit.classif)
  if(!is.null(labels)){
    a <- mutate(a, labels = labels)
  }
  b <- as_tibble(object$grid$pts) %>%
    mutate(id = seq_along(x))
  data <- left_join(a, b, by = "id")

  ndist <- unit.distances(object$som$grid)
  cddist <- as.matrix(object.distances(object$som, type = "codes"))
  cddist[abs(ndist - 1) > .001] <- NA
  neigh.dists <- colMeans(cddist, na.rm = TRUE)
  
  gg <- ggplot(data)
  clust <- NA
  if(!is.null(grid.labels)){
    clust <- grid.labels
  }
  data_ <- mutate(b, groups = clust, distance = neigh.dists, id = NA)
  if(!is.null(grid.labels)){
    gg <- gg + geom_rect(aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5, fill = groups), data = data_)
  }
  gg <- gg + geom_circle(aes(r = 0.5, x0 = x, y0 = y, alpha = distance), fill = "grey", data = data_)
  if(is.null(labels)){
    gg <- gg + geom_jitter(aes(x = x, y = y, col = groups), alpha = 1,size = 3, width = 0.2, height = 0.2)
  }else{
    gg <- gg + geom_text_repel(aes(x = x, y = y, col = groups, label = labels), alpha = 1)
  }
  gg <- gg + ggtitle("SOM")
  gg
}


hclust.piesom <- function(obj, data_label = NULL){
  codes <- obj$som$codes[[1]]
  id_node_with_data <- sort(unique(obj$som$unit.classif))
  codes <- codes[id_node_with_data,]
  grid <- with(obj$som$grid, glue("[SOM{1:nrow(pts)}({pts[,1]}, {pts[,2]})]"))
  label <- NULL;
  if(!is.null(data_label)){
    label <- tibble(label = data_label, classif = obj$som$unit.classif) %>% 
      group_by(classif) %>% 
      summarise(label = paste(label, collapse = " "), .groups = "drop") %>% 
      arrange(classif) %>% 
      pull(label)
    assert_that(are_equal(nrow(codes), length(label)))
  }
  if (is.null(label)) {
    label <- grid[id_node_with_data]
  }else{
    label <- paste(label, grid[id_node_with_data])
  }
  rownames(codes) <- label
  clust <- hclust(dist(codes))
  attr(clust$labels, "grid_id") <- id_node_with_data
  clust
}


#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
piesom_data <- function(object){
  res <- list()
  res$somgrid <- data.frame(object$som$grid$pts)
  res$somgrid.circle <- rename(res$somgrid, x0 = x, y0 = y, r = 1)
  res$somgrid.rect <- transmute(res$somgrid, 
                                xmin = x - 0.5, 
                                xmax = x + 0.5,
                                ymin = y - 0.5, 
                                ymax = y + 0.5)
  res$classif <- left_join(mutate(res$somgrid, id = seq_along(x)), data.frame(id = object$som$unit.classif), by = c(id = "id"))
  res$pc.score <- data.frame(
    x = object$pc$scores[,1],
    y = object$pc$scores[,2]
  )
  res$pc.score.som.pre <- data.frame(
    x = object$init.score[,1], 
    y = object$init.score[,2], 
    x.somgrid = object$grid$pts[,1], 
    y.somgrid = object$grid$pts[,2])
  
  somgrid2pc.score.som.pre.x <- with(res$pc.score.som.pre, set_names(x, paste(x.somgrid, y.somgrid)))
  somgrid2pc.score.som.pre.y <- with(res$pc.score.som.pre, set_names(y, paste(x.somgrid, y.somgrid)))
  res$segmentsbyx.som.pc.score.pre <- data.frame(
    x_ = object$som$grid$pts[, 1],
    xend_ = object$som$grid$pts[, 1],
    y_ = object$som$grid$pts[, 2],
    yend_ = object$som$grid$pts[, 2] + 1,
    x.somgrid = object$som$grid$pts[, 1]
  ) %>% 
    filter(yend_ <= max(y_)) %>% 
    transmute(
      x = somgrid2pc.score.som.pre.x[paste(x_, y_)],
      xend = somgrid2pc.score.som.pre.x[paste(xend_, yend_)],
      y = somgrid2pc.score.som.pre.y[paste(x_, y_)],
      yend = somgrid2pc.score.som.pre.y[paste(xend_, yend_)],
      x.somgrid = x.somgrid
    )
  res$pc.score.som.post <- data.frame(
    x = object$som.score[,1], 
    y = object$som.score[,2], 
    x.somgrid = object$grid$pts[,1], 
    y.somgrid = object$grid$pts[,2])
  
  somgrid2pc.score.som.post.x <- with(res$pc.score.som.post, set_names(x, paste(x.somgrid, y.somgrid)))
  somgrid2pc.score.som.post.y <- with(res$pc.score.som.post, set_names(y, paste(x.somgrid, y.somgrid)))
  res$segmentsbyx.som.pc.score.post <- data.frame(
    x_ = object$som$grid$pts[, 1],
    xend_ = object$som$grid$pts[, 1],
    y_ = object$som$grid$pts[, 2],
    yend_ = object$som$grid$pts[, 2] + 1,
    x.somgrid = object$som$grid$pts[, 1]
  ) %>% 
    filter(yend_ <= max(y_)) %>% 
    transmute(
      x = somgrid2pc.score.som.post.x[paste(x_, y_)],
      xend = somgrid2pc.score.som.post.x[paste(xend_, yend_)],
      y = somgrid2pc.score.som.post.y[paste(x_, y_)],
      yend = somgrid2pc.score.som.post.y[paste(xend_, yend_)],
      x.somgrid = x.somgrid
    )
  
  
  return(res)
}

