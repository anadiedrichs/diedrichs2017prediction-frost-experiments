my.grid.function <- function(x, y, len = NULL, search = "grid"){
  
  scores <- c("bic-g","loglik-g","aic-g","bge")
  algorithms <- c("hc","tabu")
  
  if(search == "grid") {
    out <- expand.grid(alg = algorithms,
                       score = scores,stringsAsFactors = FALSE)
  } else { #' random search 
    out <- expand.grid(alg = "hc",
                       score= "loglik-g",stringsAsFactors = FALSE)
  }
  print(out)
  out
}

my.fit.function <- function(x, y, wts, param, lev, last, classProbs, node, ...) {
  
  # chequear que tanto x como y sean numericos los valores
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  print(param$alg)
  print(param$score)
  # armo whitelist de relacionar todo X hacia y
  # armo blacklist de y (variable futura) hacia X.
  #wl <- data.frame(from=character(),to=character(),stringsAsFactors=FALSE)
  #wl1 = data.frame(from = colnames(x), to = node)
  #wl = rbind.data.frame(wl,wl1)
  bl <- data.frame(from=character(),to=character(),stringsAsFactors=FALSE)
  bl1 = data.frame(from = node, to = colnames(x))
  bl = rbind.data.frame(bl,bl1)
  
  
  if(param$alg == "hc")
  {
    #print("entro en hc")
    out <- bnlearn::hc(dat, whitelist=NULL,blacklist = bl, score = param$score)
    
  } else if(param$alg == "tabu")
  {# case tabu
    #print("entro en tabu")
    out <- bnlearn::tabu(dat, whitelist=NULL,blacklist = bl, score = param$score)
  }
  
  fit = bnlearn::bn.fit(out, dat)
  list(network=out,fit=fit,node=node)
}
#'
#' Regresar el Markov blanket no funciona.
#' varImp requiere una lista/data.frame con variables numeradas.
my.varImpFun <- function(object, node,...){ # object is model$finalModel type
  #TODO check node is a strig
  #TODO check node is one of the predictors (nodes in BN)
  print("hola")
  out <- bnlearn::mb(object$fit, node = node, ...)
  
  out
}

### validation functions ###
#' validate parameter node
validateNodePar <- function(n, finalModel)
{
  if(is.character(n) & (n %in% finalModel$xNames)) return(TRUE)
  else return(FALSE)
} 
#' validate white or black lists
validateWBList <- function(lista, xNames)
{
  nombres <- c("from","to")
  if(is.data.frame(lista) & ncol(lista)==2 & (colnames(lista) %in% nombres)) return(TRUE)
  #TODO si alguno de los valores no es de xNames, regresar falso
  else return(FALSE)
} 

bnReg <- list(label = "Bayesian Networks for Regression",
                  library = "bnlearn",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("alg","score"), 
                                          class = c("character","character"),
                                          label = c("Structure Learning Algorithm","Score")),
                  grid = my.grid.function, 
                  fit = my.fit.function,
                  predict = function(modelFit, newdata, node=modelFit$node, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    
                    predict(modelFit$fit,node, newdata)
                  },
                  prob = NULL, #to check
                  predictors = function(x, ...) names(x),#to check
                  tags = c("Bayesian Model", "Regression", "Bayesian Network"), # to check later
                  varImp = NULL,
                  sort = function(x) x) #TODO