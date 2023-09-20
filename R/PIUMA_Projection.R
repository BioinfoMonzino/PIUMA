#' @title Project Data Using a Dimensionality Reduction Method
#'
#' @description This function performs the transformation of data from a
#' high dimensional space into a low dimensional space, wrapping 6 well-knwon
#'  reduction methods; i.e., PCA, KPCA, t-SNE, UMAP, MDS, and Isomap.
#'  In the topological data analysis, the identified components are commonly
#'  used as lenses.
#'
#' @param df A data.frame representing a dataset in the classical n x m form.
#' Rows (n) and columns (m) should be, respectively, observations and features.
#' @param method Name of the dimensionality reduction method to use.
#' "PCA", "UMAP", "TSNE", "MDS", "KPCA" and "isomap" values are allowed.
#' Default is: "PCA".
#' @param nComp The number of components to be computed. Default: 2
#' @param centerPCA Whether the data should be centered before PCA. Default:TRUE
#' @param scalePCA Whether the data should be scaled before PCA. Default:TRUE
#' @param umapNNeigh The number of neighbors for UMAP. Default: 15
#' @param umapMinDist The minimum distance between points for UMAP. Default: 0.1
#' @param tsnePerpl Perplexity argument of t-SNE. Default: 30
#' @param tsneMaxIter The maximum number of iterations for t-SNE. Default: 300
#' @param kpcaKernel The type of kernel for kPCA. "rbfdot", "laplacedot",
#' "polydot", "tanhdot", "besseldot", "anovadot", "vanilladot" and "splinedot"
#'  are allowed. Default: "polydot".
#' @param kpcaSigma The 'sigma' argument for kPCA. Default: 0.1.
#' @param kpcaDegree The 'degree' argument for kPCA. Default: 1.
#' @param isomNNeigh The number of neighbors for Isomap. Default: 5.
#' @param showPlot Whether the scatter plot of the first two
#' principal components should be shown. Default: TRUE.
#' @param vectColor Vector containing the variable tocolor the scatter plot
#' Default: NULL.
#'
#' @return A data.frame containing the components of projected data
#'
#'
#' @author Laura Ballarini, Mattia Chiesa
#'
#' @examples
#' # use example data:
#' data(df_test_proj)
#' set.seed(1)
#' cmp <- dfToProjection(df_test_proj,  "PCA", nComp=2)
#'
#' @seealso
#' \code{\link{importSplitScale}},
#' \code{\link{dfToDistance}}
#'
#' @export
#'
#'
dfToProjection <- function(df,
                           method=c("PCA",
                                    "UMAP",
                                    "TSNE",
                                    "MDS",
                                    "KPCA",
                                    "ISOMAP"),
                           nComp=2,
                           centerPCA=FALSE,
                           scalePCA=FALSE,
                           umapNNeigh=15,
                           umapMinDist=0.1,
                           tsnePerpl=30,
                           tsneMaxIter=300,
                           kpcaKernel=c("rbfdot", "laplacedot", "polydot",
                                        "tanhdot", "besseldot", "anovadot",
                                        "vanilladot", "splinedot"),
                           kpcaSigma=0.1,
                           kpcaDegree=1,
                           isomNNeigh=5,
                           showPlot=TRUE,
                           vectColor=NULL) {

  ##startTime <- Sys.time()

  # checks----------------------------------------------------------------------
  # check missing arguments
  if (missing(df))
    stop("'df' argument must be provided")

  if (missing(method)) {
    method <- method[1]
  }

  if (missing(kpcaKernel)) {
    kpcaKernel <- kpcaKernel[3]
  }


  # check the type of argument
  if (!is.data.frame(df))
    stop("'df' argument must be a data.frame")

  if (!is.character(method))
    stop("'method' argument must be a string")

  if (!is.numeric(nComp))
    stop("'nComp' argument must be numeric")

  if (!is.numeric(umapNNeigh))
    stop("'umapNNeigh' argument must be numeric")

  if (!is.numeric(umapMinDist))
    stop("'umapMinDist' argument must be numeric")

  if (!is.numeric(tsnePerpl))
    stop("'tsnePerpl' argument must be numeric")

  if (!is.numeric(tsneMaxIter))
    stop("'tsneMaxIter' argument must be numeric")

  if (!is.character(kpcaKernel))
    stop("'kpcaKernel' argument must be a string")

  if (!is.numeric(kpcaSigma))
    stop("'kpcaSigma' argument must be numeric")

  if (!is.numeric(kpcaDegree))
    stop("'kpcaDegree' argument must be numeric")

  if (!is.numeric(isomNNeigh))
    stop("'isomNNeigh' argument must be numeric")

  if (!is.logical(showPlot))
    stop("'showPlot' argument must be TRUE or FALSE")

  if (!is.factor(vectColor) & !is.numeric(vectColor) & !is.integer(vectColor)
      & !is.null(vectColor))
    stop("'vectColor' argument must be: factor, numeric, integer or NULL")


  # specific checks
  if (dim(df)[1] < 10)
    stop("num. of 'df' rows must be greater than 10")

  if (dim(df)[2] < 2)
    stop("num. of 'df' columns must be greater than 2")

  if (!all(vapply(df, class, FUN.VALUE=character(1)) %in% c("numeric",
                                                            "integer")))
    stop("'df' variables must be all numeric or integer")

  if (length(method) > 1)
    stop("length(method) must be equal to 1")

  if (!(method %in% c("PCA","UMAP", "TSNE", "MDS", "KPCA",
                      "isomap")))
    stop("'method' must be one of 'PCA','UMAP', 'TSNE', 'MDS', 'KPCA',
         'isomap'")

  if (length(nComp) > 1)
    stop("length(nComp) must be equal to 1")

  if ((nComp %%1) != 0)
    stop("'nComp' must be integer")

  if (!(nComp >= 2 & nComp <= dim(df)[2]))
    stop("'nComp' must be lower than the number of 'df' columns")


  if (method %in% "UMAP"){
    if (length(umapNNeigh) > 1)
      stop("length(umapNNeigh) must be equal to 1")

    if ((umapNNeigh %% 1) != 0)
      stop("'umapNNeigh' must be integer")


    if (!(umapNNeigh >= 2 & umapNNeigh <= round(dim(df)[1])/4))
      stop("'umapNNeigh' must be in [2; N/4], with N = n. of 'df' rows")

    if (length(umapMinDist) > 1)
      stop("length(umapMinDist) must be equal to 1")

    if (!(umapMinDist >= 0 & umapMinDist < 1))
      stop("'umapMinDist' must be in [0; 1)")

  }

  if (method %in% "TSNE"){
    if (length(tsnePerpl) > 1)
      stop("length(tsnePerpl) must be equal to 1")

    if ((tsnePerpl %% 1) != 0)
      stop("'tsnePerpl' must be integer")

    if (!(tsnePerpl >= 3 & tsnePerpl <= round(dim(df)[1])/3))
      stop("'tsnePerpl' must be in [3; N/3], with N = n. of 'df' rows")

    if (length(tsneMaxIter) > 1)
      stop("length(tsneMaxIter) must be equal to 1")

    if ((tsneMaxIter %% 1) != 0)
      stop("'tsneMaxIter' must be integer")

    if (!(tsneMaxIter >= 100 & tsneMaxIter <= 3000))
      stop("'tsneMaxIter' must be in [100; 3000]")
  }


  if (method %in% "KPCA"){
    if (length(kpcaKernel) > 1)
      stop("length(kpcaKernel) must be equal to 1")

    if (!(kpcaKernel %in% c("rbfdot", "laplacedot", "polydot", "tanhdot",
                            "besseldot", "anovadot", "vanilladot", "splinedot")))
      stop("'kpcaKernel' must be one of 'rbfdot', 'laplacedot', 'polydot',
         'tanhdot', 'besseldot', 'anovadot', 'vanilladot', 'splinedot'")

    if (length(kpcaSigma) > 1)
      stop("length(kpcaSigma) must be equal to 1")

    if (!(kpcaSigma > 0 & kpcaSigma <= 5))
      stop("'kpcaSigma' must be in (0; 5]")

    if (length(kpcaDegree) > 1)
      stop("length(kpcaDegree) must be equal to 1")

    if ((kpcaDegree %% 1) != 0)
      stop("'kpcaDegree' must be integer")

    if (!(kpcaDegree >=1 & kpcaDegree <= 3))
      stop("'kpcaDegree' must be in [1; 3]")

  }

  if (method %in% "isomap"){
    if (length(isomNNeigh) > 1)
      stop("length(isomNNeigh) must be equal to 1")

    if ((isomNNeigh %% 1) != 0)
      stop("'isomNNeigh' must be integer")

    if (!(isomNNeigh >= 2 & isomNNeigh <= round( (dim(df)[1])/4) ))
      stop("'isomNNeigh' must be in [2; N/4], with N = n. of 'df' rows")
  }



  if (length(showPlot) > 1)
    stop("length(showPlot) must be equal to 1")

  if(!is.null(vectColor)){
    if(length(vectColor) != dim(df)[1]){
      stop("length(vectColor) is different from the number of df rows")
    }
  }


  # check the presence of NA or Inf
  if (any(is.na(df)))
    stop("NA values are not allowed in the 'df' data.frame")

  if (any(is.infinite(as.matrix(df))))
    stop("Inf values are not allowed in the 'df' data.frame")

  if(!is.null(vectColor)){
    if(any(is.na(vectColor)))
      warning("NA values are not allowed in 'vectColor'")
    if(any(is.infinite(vectColor)))
      warning("Inf values are not allowed in 'vectColor'")
  }

  # compute the projection (body)-----------------------------------------------
  s <- cbind(method, "is working to reduce", dim(df)[2])
  s <- cbind(s, "variables in", nComp, "components")
  cat(s, "\n")

  #set.seed(1)

  switch(method, "PCA"={

    pcaRes <- prcomp(df, center = centerPCA, scale. = scalePCA, rank.=nComp)
    allCmp<-as.data.frame(pcaRes$x)

    # percentage of explained variance
    explVar <- round(pcaRes$sdev^2/sum(pcaRes$sdev^2)*100)
    vectNumComp <- seq_len(nComp)
    s <- paste0("comp", vectNumComp, " = ", explVar[1:nComp], "%",
                collapse=", ")
    s <- strsplit(s, split=", ", fixed=TRUE)
    cat("PCA explained variance: ", unlist(s), sep="\n")

  },"UMAP"={

    # create a new settings object
    custom.settings <- umap.defaults
    custom.settings$n_neighbors <- umapNNeigh
    custom.settings$n_components <- nComp
    custom.settings$input <- "data"
    custom.settings$min_dist <- umapMinDist

    umapRes <- umap(df, config = custom.settings, preserve.seed = TRUE)
    allCmp <- as.data.frame(umapRes$layout)

  },"TSNE"={

    tsneRes <- tsne(df, k=nComp, perplexity=tsnePerpl, max_iter=tsneMaxIter)
    allCmp <- as.data.frame(tsneRes)

  },"MDS"={

    distance <- dist(df, method = "euclidean")
    mdsRes <- cmdscale(distance, k = nComp)
    allCmp<-as.data.frame(mdsRes)

  },"KPCA"={

    if (kpcaKernel %in% c("rbfdot", "laplacedot")) {
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel,
                      kpar=list(sigma=kpcaSigma), features=nComp)
    } else if (kpcaKernel %in% "polydot") {
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel,
                      kpar=list(degree=kpcaDegree, scale=1, offset=1),
                      features = nComp)
    } else if (kpcaKernel %in% "tanhdot") {
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel,
                      kpar=list(scale=1, offset=1), features=nComp)
    } else if (kpcaKernel %in% "besseldot") {
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel,
                      kpar=list(sigma=kpcaSigma, order=1, degree=kpcaDegree),
                      features=nComp)
    } else if (kpcaKernel %in% "anovadot") {
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel,
                      kpar=list(sigma=kpcaSigma, degree=kpcaDegree),
                      features=nComp)
    } else if (kpcaKernel %in% c("vanilladot", "splinedot")){
      kpcaRes <- kpca(as.matrix(df), kernel=kpcaKernel, kpar=list(),
                      features=nComp)
    } else {
      stop("'kpcaKernel' must be one of 'rbfdot', 'laplacedot', 'polydot',
            'tanhdot', 'besseldot', 'anovadot', 'vanilladot', 'splinedot'")
    }

    allCmp<-as.data.frame(kpcaRes@pcv)

  }, "isomap"={



    df <- as.data.frame(as.matrix(dist(df, method="euclidean")))
    isomapRes <- isomap(df, ndim=nComp, k=isomNNeigh)
    allCmp<-as.data.frame(isomapRes[["points"]][, 1:nComp])

  })

  # set 'allCmp' column names
  colnames(allCmp)<-rep(paste0("comp", 1:dim(allCmp)[2]))
  rownames(allCmp)<-rownames(df)

  # scatter plot----------------------------------------------------------------
  if (showPlot){
    myLegendTitle<-deparse(substitute(vectColor))

    if(is.null(vectColor)){
      print(
        ggplot(allCmp, aes(x=comp1, y=comp2)) +
          geom_point(size=2, alpha=0.5, fill="blue", color="black", shape=21)+
          theme_bw(base_size=10) +
          labs(title=method, x="Component 1", y="Component 2",
               fill=paste0(myLegendTitle, ": "))+
          theme(plot.title=element_text(hjust=0.5), legend.position="right")

      )
    }else{
      print(
        ggplot(allCmp, aes(x=comp1, y=comp2, fill=vectColor)) +
          geom_point(size=2, alpha=0.5, color="black", shape=21)+
          theme_bw(base_size=10) +
          labs(title=method, x="Component 1", y="Component 2",
               fill=paste0(myLegendTitle, ": "))+
          {if (is.factor(vectColor)) {
            myCols <- c("springgreen1", "red", "yellow", "white", "cyan",
                        "black", "pink", "blue", "orange")
            scale_fill_manual(values=myCols)
          } else {
            scale_fill_gradientn(colours = rev(rainbow(5)))
          }}+
          theme(plot.title=element_text(hjust=0.5), legend.position="right")

      )
    }
  }

  ##endTime <- Sys.time()
  ## cat("Projection completed in", difftime(endTime, startTime, units = "secs")
  ##     ,"secs\n")
  return(allCmp)
}
