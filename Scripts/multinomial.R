# Multinomial sensitivity using base R optim and survey Taylor score covariance.
# A separate helper is needed because no multinomial package is manuscript-listed.
multinomial <- function(design, X, outcome) {
  n <- nrow(X); p <- ncol(X); K <- max(outcome)-1
  Y <- sapply(2:(K+1),function(k)as.numeric(outcome==k))
  W <- as.numeric(weights(design)); scale <- n/sum(W); w <- W*scale
  probability <- function(beta) {
    eta <- cbind(0,X %*% matrix(beta,nrow=p,ncol=K))
    e <- exp(eta-apply(eta,1,max)); e/rowSums(e)
  }
  objective <- function(beta) -sum(w*log(pmax(probability(beta)[cbind(seq_len(n),outcome)],.Machine$double.xmin)))
  gradient <- function(beta) as.vector(crossprod(X,w*(probability(beta)[,2:(K+1),drop=FALSE]-Y)))
  model <- optim(rep(0,p*K),objective,gradient,method="BFGS",control=list(maxit=3000,reltol=1e-12))
  pr <- probability(model$par); H <- matrix(0,p*K,p*K)
  for(j in seq_len(K)) for(k in seq_len(K)) {
    a <- ((j-1)*p+1):(j*p); b <- ((k-1)*p+1):(k*p)
    H[a,b] <- crossprod(X,X*(w*pr[,j+1]*((j==k)-pr[,k+1])))
  }
  scores <- do.call(cbind,lapply(seq_len(K),function(k)X*(Y[,k]-pr[,k+1])))*scale
  colnames(scores) <- paste0("score",seq_len(ncol(scores)))
  design$variables <- cbind(design$variables,as.data.frame(scores))
  meat <- vcov(quiet(svytotal(reformulate(colnames(scores)),design)))
  bread <- solve(H); covariance <- bread %*% meat %*% bread
  list(beta=model$par,variance=covariance,converged=model$convergence==0,
    gradient=max(abs(gradient(model$par))),hessian_error=max(abs(H-optimHess(model$par,objective,gradient))),
    probability_sum_error=max(abs(rowSums(pr)-1)),parameters=p*K)
}
cat("Fitting survey multinomial sensitivity\n")
X <- model.matrix(reformulate(c(hardships,health)),d)
y <- ifelse(d$CSRVCTL2 %in% 1:2,1L,ifelse(d$CSRVCTL2==3,2L,3L))
multi <- multinomial(primary,X,y)
se <- sqrt(diag(multi$variance)); df <- degf(primary)+1-multi$parameters; critical <- qt(.975,df)
multi_rows <- data.frame(outcome=rep(c("Uncontrolled with treatment","Uncontrolled without treatment"),each=ncol(X)),
  term=rep(colnames(X),2),log_RRR=multi$beta,SE=se,RRR=exp(multi$beta),
  lower=exp(multi$beta-critical*se),upper=exp(multi$beta+critical*se),
  n=nrow(d),category_n=rep(as.numeric(table(factor(y,levels=1:3)))[2:3],each=ncol(X)),
  controlled_n=sum(y==1),parameters=multi$parameters,design_df=degf(primary),residual_df=df,converged=multi$converged)
save_csv(multi_rows,"multinomial_coefficients")
# Independent calibration: a two-category version must reproduce svyglm.
binary <- multinomial(primary,X,d$uncontrolled+1L)
diagnostics <- data.frame(check=c("Multinomial optimizer converged","Maximum absolute score gradient",
  "Analytic versus finite difference Hessian","Probability row sum error",
  "Binary coefficient agreement with svyglm","Binary covariance agreement with svyglm"),
  value=c(as.numeric(multi$converged),multi$gradient,multi$hessian_error,multi$probability_sum_error,
    max(abs(binary$beta-coef(models$Primary))),max(abs(binary$variance-vcov(models$Primary)))))
save_csv(diagnostics,"multinomial_diagnostics")
saveRDS(multi,file.path(root,"Validation","multinomial_fit.rds"))
