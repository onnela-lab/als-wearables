require(lme4)

# https://stackoverflow.com/questions/72090177/how-can-i-know-whether-the-model-is-converged-or-failed-to-converge-in-lme4-with
# Reutrns: 
# - 1 if the model converged normally ie not to a singular fit, 
# - 0 if it converges to a singular fit 
# - (-1) if it fails to converge
merMod_has_converged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  retval <- NULL
  if(is.null(unlist(mm@optinfo$conv$lme4))) {
    retval = 1
  }
  else {
    if (isSingular(mm)) {
      retval = 0
    } else {
      retval = -1
    }
  }
  return(retval)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# var names and var labels


# ------------------------------------------------------------------------------
var_name_vec_actigraph_vendorprovided <- c(
  "lightactivity",
  "moderateactivity",
  "vigorousactivity",
  "mvpa",
  "sedentarybehavior",
  "nonsedentarybehavior",
  "locomotion",
  "nonlocomotion",
  "steps",
  "calories",
  "met",
  "totalvectormagnitudecounts",
  "sleepminutes"
)

var_label_vec_actigraph_vendorprovided <- c(
  "Light activity [minutes]",
  "Moderate activity [minutes]",
  "Vigorous activity [minutes]",
  "MVPA [minutes]",
  "Sedentary [minutes]",
  "Non-sedentary [minutes]",
  "Locomotion [minutes]",
  "Non-locomotion [minutes]",
  "Steps",
  "Calories",
  "METs",
  "Total activity counts" ,
  "Sleep [minutes]"
)


# ------------------------------------------------------------------------------
var_name_vec_actigraph_investigatorderived <- c(
  "tac",
  "tlac",
  "ltac",
  "astp",
  "satp",
  "time_spent_active",
  "time_spent_nonactive"
)

var_label_vec_actigraph_investigatorderived <- c(
  "Total activity counts",
  "Total log(activity counts)",
  "log(total activity counts)",
  "Active to sedentary TP",
  "Sedentary to active TP",
  "Non-sedentary [minutes]",
  "Sedentary [minutes]"
)


# ------------------------------------------------------------------------------
var_name_vec_modus_vendorprovided <- c(
  "steps_per_day",
  # "cadence_peak", # (as the same as max_1) 
  "cadence_95perc" ,
  "cadence_average",
  "cadence_median",
  "percent_time_in_low_activity",
  "percent_time_in_med_activity",
  "percent_time_in_high_activity",
  "max_60",
  "max_20",
  "max_5",
  "max_1",
  "peak_performance_index"
)

var_label_vec_modus_vendorprovided <- c(
  "Steps recorded/minute - total",
  # "Steps recorded/minute - peak",  # (as the same as max_1) 
  "Steps recorded/minute - 95th perc." ,
  "Steps recorded/minute - mean",
  "Steps recorded/minute - median",
  "Time % with 1-15 steps recorded/minute",
  "Time % with 16-40 steps recorded/minute",
  "Time % with 41+ steps recorded/minute",
  "Max 60",
  "Max 20",
  "Max 5",
  "Max 1",
  "Peak performance index" 
)


# ------------------------------------------------------------------------------

# get CI for fixed effects with the use of lmertest
get_mod_ci <- function(mod_out, mod_df){
  # lmertest CI
  out_vec <- c(
    as.numeric(contest(mod_out, L = c(1, 0), joint = FALSE)[c("lower", "upper")]),
    as.numeric(contest(mod_out, L = c(0, 1), joint = FALSE)[c("lower", "upper")])
  )
  if (max(abs(out_vec)) > 10e6){
    out_vec <- c(
      as.numeric(confint(mod_out, parm = "(Intercept)")),
      as.numeric(confint(mod_out, parm = "x_mean"))
    )
  }
  out_vec <- out_vec[c(1,3,2,4)]
  # one case seem to be hiting some numerical corner case with CIs of ~ 396580756000.22186
  names(out_vec) <- c("interc_ci_lo", "slope_ci_lo", "interc_ci_up", "slope_ci_yp")
  out_vec <- round(out_vec, 3)
  return(out_vec)
}



message("The file utils.R (with functions and variable names/labels used across >1 script) was read.")


