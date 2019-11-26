#' functions which aid cross variable linear regressions for use with data.table
#'
#' @filename data_table_quantile_regression_utils.R
#' @host github/laubblatt/phaselag/R/
#' @author Maik Renner, mrenner [at] bgc-jena.mpg.de

#' @version 1.00 2018-09-21 copies functions mlm.output.statlong() and mlm.output.statlong.call() from data.table.regression.fun.R
#' @version 1.01 2018-09-21 improve documentation
#' @version 1.10 2018-12-14 BUG fix for R1
#' @version 1.11 2018-12-14 add a function which rolls up summary objects as nested lists mrq.output.sufi()
#' @version 1.20 2019-08-26 copied three function definitions from my package phaselag to be independent from it
#' @version 1.21 2019-11-26 winter nighttime regression yielded an error in functions mlm.output.statlong.call.rq and mrq.output.statlong 
#'                          when regression failed the tau was missing in the output data.tables 

#' @import data.table
#' @import quantreg
#' @export mrq.output.statlong
#' @export mrq.output.sufi
#' @export mlm.output.statlong.call.rq
NULL

mlm.output.statlong.call.rq = function(mula, data, se = "iid", ...) {
  #' Call a linear QUANTILE regression model and reorganize its output
  #'
  #' sucessfull error handling for errors on function calls when groups are empty
  #'
  #' @param mula a formula for a lm() regression provided as character using the column names of the data
  #' @param data the data table with the column names
  #' @param ... additional arguments to call rq() in particular the quantile must be set e.g. tau = .90
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #' @return a data.table with columns statistic and value
  #' @examples
  #' DT = as.data.table(mtcars)
  #' DT[, mlm.output.statlong.call.rq(mula = "mpg ~ drat + gear", data = .SD, tau = 0.5) , by = list(am) ]
  #' # impose an error with a nonexisting column name
  #' DT[, mlm.output.statlong.call.rq(mula = "mpg ~ drat + gear + bogus", data = .SD, tau = 0.5) , by = list(am) ]

  #' @version 0.14 2018-03-05 set try to silent to avoid printout on bash
  #' @version 0.15 2018-11-20 based on mlm.output.statlong.call with little adaptions to run rq()
  if (is.null(data) |  nrow(data) < 3 |  inherits(try(ans<-rq(formula = mula, data, ...),silent = TRUE),"try-error")) {
    #     print(paste(mula, "combi no data", collapse = " "))
    ### some non null output for groups is still required !
    # otherwise j doesn't evaluate to the same number of columns for each group
    #' @update 2019-11-26 incase of polar month with Radiation being 0, we get the error in rq.fit.br(x, y, tau = tau, ...) : Singular design matrix
    #'                    for the output this requires a further column for tau, which I just set to NA
#    data.table(statistic = NA_character_, value = NA_real_)
    data.table(tau = NA_real_, statistic = NA_character_, value = NA_real_)

  } else if (any(is.na(coef(ans)))) {
    data.table(statistic = NA_character_, value = NA_real_)
  } else {
    mrq.output.statlong(ans, se)
  }
}


mrq.output.sufi = function(sufi) {
  # sufi = summary(fit)
  sufic = sufi$coefficients
  out = as.numeric(t(sufic))

  intercept = attr(sufi$terms,"intercept")

  ## we may want keep forcing through origin, hence no intercept @version 2018-11-29 uncommented
  # if (nrow(sufic)<2) {
  #  dt = data.table(variable = NA_character_, value = NA_real_)
  #  } else

  ## quantreg can only give a Named num for coefficients @version 2018-11-29
  if (! is.null(ncol(sufic) ) ) {

    if (ncol(sufic) == 4 & intercept == 1){
      # now checking if indeed the t-test is reported
      intnames = c("intercept", "intercept_sd" ,"intercept_ttest", "intercept_pvalue")
      nvars = 1 :(nrow(sufic)-1)
      # nvars = 1:2
      slopenames = c()
      for (nvar in nvars) {
        slopenames = c(slopenames,  paste0("slope",nvar),
                       paste0("slope",nvar,"_sd"),
                       paste0("slope",nvar,"_ttest"),
                       paste0("slope",nvar,"_pvalue"))
      }
    } else if (ncol(sufic) == 4 & intercept == 0){
      intnames = c()
      nvars = 1 :(nrow(sufic))
      # nvars = 1:2
      slopenames = c()
      for (nvar in nvars) {
        slopenames = c(slopenames,  paste0("slope",nvar),
                       paste0("slope",nvar,"_sd"),
                       paste0("slope",nvar,"_ttest"),
                       paste0("slope",nvar,"_pvalue"))
      }
    } else {
      # now for the case of quantreg rq which reports upper and lower bounds
      cna = colnames(sufic)
      cna = sub(pattern = " ", replacement = "", cna)
      cnaend = cna[2:ncol(sufic)]
      (intnames = c("intercept", paste0("intercept_", cnaend)))
      nvars = 1 :(nrow(sufic)-1)
      slopenames = c()
      for (nvar in nvars) {
        slopenames = c(slopenames,  paste0("slope",nvar),
                       sapply(cnaend, function(cn) paste0("slope",nvar,"_",cn)) )
      }
      # slopenames
    }
  } else { # capturing the case of coeficients as a named numeric @version 2018-11-29
    intnames = c()
    slopenames =  paste0("slope",(1:length(sufic)) )
  }
  # slopenames
  ## @20160713 error in regression
  #  names(out) = c(intnames, slopenames)
  # Error in names(out) = c(intnames, slopenames) :
  #   'names' attribute [12] must be the same length as the vector [4]

  if (inherits(try(names(out) <- c(intnames, slopenames)),"try-error")) {
    print(paste("ERROR: names of summary object do not match",out, intnames, slopenames))
    print(sufic)
    #     next
  }
  out = c(out, n = length(sufi$residuals))
}

#The pseudo-R^2 measure suggested by Koenker and Machado's 1999 JASA paper
# measures goodness of fit by comparing the sum of weighted deviations
# for the model of interest with the same sum from a model in which only the intercept appears.

#R1 = 1 - fit$rho / rq(fit$y ~ 1)$rho
#out = c(out,R1 = R1 )
#   return(out)
# }


mrq.output.statlong = function(fit, se = "iid") {
  #' Reorganize the output of a quantile regression fit into a long table format: statistic, value
  #'
  #' simple function which takes a lm model output to create a named vector intended for use with data.table and group by
  #' @param fit object returned from a linear model quantreg::rq() are supported
  #' @return a data.table with columns statistic and value
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #' @examples
  #' # output of regression model is melted to two columns
  #' library(quantreg)
  #' data(engel)
  #' fit = rq(foodexp~income,tau=0.9,data=engel)
  #' mrq.output.statlong(fit)
  #' @version 0.01 2018-12-04 based on mlm.output.statlong()
  #'                removing the R2 and adding the pseudo R1
  #' @version 0.10 2018-12-14 BUG fix for R1 -- update all previous results !
  #' @version 0.11 2018-12-14 add a function which rolls up summary objects as nested lists mrq.output.sufi()
  #' @version 0.11 2018-12-20 default of summary.rq calls se = "nid" which crashes base:backsolve, set default to se = "iid", see ?summary.rq
  #'

  if (! is.null(fit) ) {
    if (inherits(try(sufi <- summary(fit, se = se) ),"try-error")) {
      sufic = fit$coefficients
      coefnames = names(sufic)
      (out = as.numeric(t(sufic)))
      if (coefnames[1] == "(Intercept)")
        (coefnames = c("intercept", paste0("slope",1:(length(out)-1))) )
      
      #' @update 2019-11-26 winter nighttime regression yielded an error 
      #' Error in model.frame.default(formula = ord.resid ~ xt, drop.unused.levels = TRUE) : 
      #'       variable lengths differ (found for 'xt')
      out = data.table(tau = fit$tau, statistic = coefnames, value = out)

    } else {

      # check for nested list which is provided when several tau are used in rq
      if(  any(sapply(sufi, is.list)) ) {
        (sout = sapply(sufi,mrq.output.sufi) )
      } else {
        (sout = t(t(mrq.output.sufi(sufi))))
      }
      #The pseudo-R^2 measure suggested by Koenker and Machado's 1999 JASA paper
      # measures goodness of fit by comparing the sum of weighted deviations
      # for the model of interest with the same sum from a model in which only the intercept appears.

      #' @update 2018-12-14 missed the tau in the intercept regression !!!
      R1 = 1 - fit$rho / rq(fit$y ~ 1, tau = fit$tau)$rho
      sout = rbind(sout,R1 )
      dt = data.table()
      for (ii in 1 : ncol(sout)) {
        dt = rbind(dt, data.table(tau = fit$tau[ii], statistic = rownames(sout), value = sout[,ii]) )
      }
      out = dt
    }
    return(out)
  }
}

