bnReg <- list(label = "Bayesian Networks for Regression",
                  library = "bnlearn",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("alg","score"), 
                                          class = c("character","character"),
                                          label = c("Score","Structure Learning Algorithm")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    scores <- c("bic-g","loglik-g","aic-g","bge")
                    algorithms <- c("hc","tabu")
                    
                    if(search == "grid") {
                      out <- expand.grid(alg = algorithms,
                                         score = scores,stringsAsFactors = FALSE)
                    } else { # como seria la random search 
                      out <- expand.grid(alg = "hc",
                                        score= "loglik-g",stringsAsFactors = FALSE)
                    }
                    print(out)
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, node, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    print(param$alg)
                    print(param$score)
                    if(param$alg == "hc")
                    {
                      #print("entro en hc")
                      out <- bnlearn::hc(dat, whitelist=NULL,blacklist = NULL, score = param$score)
                      
                    } else # case tabu
                    {
                      #print("entro en tabu")
                      
                      out <- bnlearn::tabu(dat, whitelist=NULL,blacklist = NULL, score = param$score)
                    }
                    
                    #net = network(x, y, wts, param, lev, last, classProbs, ...)
                    fit = bnlearn::bn.fit(out, dat)
                    list(network=out,fit=fit,node=node)
                  },
                  predict = function(modelFit, newdata, node=modelFit$node, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    
                    predict(modelFit$fit,node, newdata)
                  },
                  prob = NULL,
                  predictors = function(x, ...) names(x),#to check
                  tags = c("Bayesian Model", "Regression", "Bayesian Network"), # to check later
                  varImp = NULL,
                  sort = function(x) x) #TODO