# Uility functions

# 0-1 scaling
scaleData_01 <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Not in function
'%!in%' <- function(x,y) !('%in%'(x,y))

# inner function for jaccard computation
jaccard <- function(a, b) {
  intersection_j <- length(intersect(a, b))
  union_j <- length(union(a, b))
  return (intersection_j/union_j)
}

#### scale free power law
plot_ScaleFreeLaw <- function(dataPlDist, rCorkpk, rCorlogklogpk){

  p1 <- ggplot(dataPlDist) +
    geom_point(aes(x=k, y=pk), color='blue', size=3) +
    stat_smooth(data=dataPlDist,
                aes(x = k, y = pk),
                fill="orange1",
                method = "lm",
                color="orange",
                size=1,
                alpha=0.1)+

    geom_line(aes(x=k, y=pk), color='purple', linetype = "dashed") +
    theme_bw()+
    ggtitle(paste0("Degree distr. (linear scale) --- R = ",
                   round(rCorkpk[["r"]][2],2))
    )+
    theme(plot.title = element_text(size = 10, face = "bold")) +
    scale_x_continuous(breaks= pretty_breaks())

  p2 <- p1 + scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    annotation_logticks()+
    ggtitle(paste0("Degree distr. (log-log scale) --- R = ",
                   round(rCorlogklogpk[["r"]][2],2))
    )+
    theme(plot.title = element_text(size = 10, face = "bold"))


  show(wrap_plots(p1, p2))
}

#### projection plot
plot_projection_plot <- function(allCmp, vectColor, method){
  myLegendTitle <- deparse(substitute(vectColor))

  if(is.null(vectColor)){
    show(
      ggplot(allCmp, aes(x = comp1, y = comp2)) +
        geom_point(size = 2, alpha = 0.5, fill = "blue",
                   color = "black", shape = 21)+
        theme_bw(base_size = 10) +
        labs(title = method, x = "Component 1", y = "Component 2",
             fill = paste0(myLegendTitle, ": "))+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "right")

    )
  }else{
    show(
      ggplot(allCmp, aes(x = comp1, y = comp2, fill = vectColor)) +
        geom_point(size = 2, alpha = 0.5, color = "black", shape = 21)+
        theme_bw(base_size = 10) +
        labs(title = method, x = "Component 1", y = "Component 2",
             fill = paste0(myLegendTitle, ": "))+
        {if (is.factor(vectColor)) {
          myCols <- c("springgreen1", "red", "yellow", "white", "cyan",
                      "black", "pink", "blue", "orange")
          scale_fill_manual(values = myCols)
        } else {
          scale_fill_gradientn(colours = rev(rainbow(5)))
        }}+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "right")

    )
  }

}

