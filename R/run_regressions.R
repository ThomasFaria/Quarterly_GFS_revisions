library(data.table)
source("R/functions.R")

aws.s3::get_bucket("tfaria", region = "")
data <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "public/RegressionDB.parquet",
  bucket = "tfaria",
  opts = list("region" = "")
)
RegressionDB <- preprocess_regression_db(data)

##### Lists #####
Var_Revenue <- c("TOR", "DTX", "SCT", "TIN")
Var_Expenditure <- c("TOE", "THN", "PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")

Variables <- c(Var_Revenue, Var_Macro, Var_Expenditure)
unique(RegressionDB$Variable_code)

simulate_models <- function(sample, variable, criterion, model_str) {
  sample <- subset(sample, Variable_code == variable)
  tryCatch(
    expr = {
      # We estimate a model without constant
      model_summary <- dplyr::bind_rows(lapply(model_str[model_str$Variable == variable, "Model_specification"], function(model) {
        estimated_model <- lm(paste(model, "-1"), data = sample)
        summary <- broom::glance(x)
        summary$Model_specification <- model
        summary$N <- nobs(x)
        summary$Variable <- variable
        summary$RMSE <- sqrt(c(crossprod(x$residuals)) / length(x$residuals))
        summary$Group <- unique(sample$Group)
        return(summary)
      }))
      return(model_summary)
    },
    error = function(e) {
      # When there is no regressor we have to add the constant
      model <- as.character(model_str[model_str$Variable == variable, "Model_specification"])
      x <- lm(model, data = sample)
      model_summary <- dplyr::tibble(
        AIC = c(AIC(x)),
        BIC = c(BIC(x)),
        N = c(nobs(x)),
        Model_specification = c(model),
        Variable = c(variable),
        p.value = NA,
        RMSE = c(sqrt(c(crossprod(x$residuals)) / length(x$residuals))),
        Group = c(unique(sample$Group))
      )
      return(model_summary)
    }
  )
}

simulate_models <- function(data, variable, model_str) {
  sample <- subset(data, Variable_code == variable)
  tryCatch(
    expr = {
      # We estimate a model without constant
      model_summary <- dplyr::bind_rows(lapply(model_str, function(model) {
        estimated_model <- lm(paste(model, "-1"), data = sample)
        summary <- broom::glance(x)
        summary$Model_specification <- model
        summary$N <- nobs(x)
        summary$Variable <- variable
        summary$RMSE <- sqrt(c(crossprod(x$residuals)) / length(x$residuals))
        summary$Group <- unique(sample$Group)
        return(summary)
      }))
      return(model_summary)
    },
    error = function(e) {
      # When there is no regressor we have to add the constant
      x <- lm(model_str, data = sample)
      model_summary <- dplyr::tibble(
        AIC = c(AIC(x)),
        BIC = c(BIC(x)),
        N = c(nobs(x)),
        Model_specification = c(model),
        Variable = c(variable),
        p.value = NA,
        RMSE = c(sqrt(c(crossprod(x$residuals)) / length(x$residuals))),
        Group = c(unique(sample$Group))
      )
      return(model_summary)
    }
  )
}

# Faire des fonctions pour simplifier
# finir le renommage des variables
##### MODEL 8 : Macro #####
Models8 <- list()
Regressors <- c(paste0("Country_", c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA"), collapse = "+"),
               paste0("ObsQ_", 1:4, collapse = "+"),
               "ESA2010",
               "First_announcement",
               paste0("Rev_lag", 1:5)
               )
List_models <- unlist(lapply(
  1:length(Regressors),
  function(n) {
    combn(Regressors, n, FUN = function(row) {
      paste0("Final_revision ~ ", paste0(row, collapse = "+"))
    })
  }
))

for (variable in Variables) {
  sample <- subset(RegressionDB, Variable_code == variable)
  Models8[[variable]] <- dplyr::bind_rows(lapply(List_models, function(model) {
    x <- lm(paste(model, "-1"), data = sample)
    a <- broom::glance(x)
    a$Model_specification <- model
    a$N <- nobs(x)
    a$Variable <- variable
    a$RMSE <- sqrt(c(crossprod(x$residuals)) / length(x$residuals))
    a$Group <- unique(sample$Group)
    return(a)
  }))
  
  x <- lm("Final_revision ~ 0", data = sample)
  Model <- dplyr::tibble(
    AIC = c(AIC(x)),
    BIC = c(BIC(x)),
    deviance = deviance(x),
    logLik = logLik(x)[1],
    sigma = sigma(x),
    df = summary(x)$df[1],
    r.squared = summary(x)$r.squared,
    adj.r.squared = summary(x)$adj.r.squared,
    N = c(nobs(x)),
    Model_specification = c("Final_revision ~ 0"),
    Variable = c(variable),
    p.value = NA,
    RMSE = c(sqrt(c(crossprod(x$residuals)) / length(x$residuals))),
    Group = c(unique(sample$Group))
  )
  Models8[[variable]] <- Models8[[variable]] |>
    dplyr::add_row(Model)
}

TopMod8 <- list(
  "AIC" = dplyr::bind_rows(lapply(1:length(Variables), function(itemNb) {
    Models8[[itemNb]][order(Models8[[itemNb]]$AIC)[1], ]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(1:length(Variables), function(itemNb) {
    Models8[[itemNb]][order(Models8[[itemNb]]$BIC)[1], ]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)


##### MODEL 6 : First Announcement #####
Models6 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (criterion in c("AIC", "BIC")) {
  new_model <- TopMod8[[criterion]] |>
    dplyr::mutate(
      Model_specification = stringr::str_replace_all(Model_specification, c("\\+Rev_lag1" = "", "\\+Rev_lag2" = "", "\\+Rev_lag3" = "", "\\+Rev_lag4" = "", "\\+Rev_lag5" = "")),
      Model_specification = stringr::str_replace_all(Model_specification, c("Rev_lag1" = "0", "Rev_lag2" = "0", "Rev_lag3" = "0", "Rev_lag4" = "0", "Rev_lag5" = "0"))
    )

  for (variable in Variables) {
    Models6[[criterion]][[variable]] <- simulate_models(RegressionDB, variable, criterion, new_model)
  }
}

TopMod6 <- list(
  "AIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models6[["AIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models6[["BIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 5 : ESA2010 #####
Models5 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (criterion in c("AIC", "BIC")) {
  new_model <- TopMod6[[criterion]] |>
    dplyr::mutate(
      Model_specification = stringr::str_replace_all(Model_specification, c("\\+First_announcement" = "")),
      Model_specification = stringr::str_replace_all(Model_specification, c("First_announcement" = "0"))
    )

  for (variable in Variables) {
    Models5[[criterion]][[variable]] <- simulate_models(RegressionDB, variable, criterion, new_model)
  }
}

TopMod5 <- list(
  "AIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models5[["AIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models5[["BIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 4 : Quarter FIXED EFFECT #####
Models4 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (criterion in c("AIC", "BIC")) {
  new_model <- TopMod5[[criterion]] |>
    dplyr::mutate(
      Model_specification = stringr::str_replace_all(Model_specification, c("\\+ESA2010" = "")),
      Model_specification = stringr::str_replace_all(Model_specification, c("ESA2010" = "0"))
    )

  for (variable in Variables) {
    Models4[[criterion]][[variable]] <- simulate_models(RegressionDB, variable, criterion, new_model)
  }
}

TopMod4 <- list(
  "AIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models4[["AIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models4[["BIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 3 : COUNTRY FIXED EFFECT #####
Models3 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (criterion in c("AIC", "BIC")) {
  new_model <- TopMod4[[criterion]] |>
    dplyr::mutate(
      Model_specification = stringr::str_replace_all(Model_specification, c("\\+ObsQ_1" = "", "\\+ObsQ_2" = "", "\\+ObsQ_3" = "", "\\+ObsQ_4" = "")),
      Model_specification = stringr::str_replace_all(Model_specification, c("ObsQ_1" = "0"))
    )

  for (variable in Variables) {
    Models3[[criterion]][[variable]] <- simulate_models(RegressionDB, variable, criterion, new_model)
  }
}

TopMod3 <- list(
  "AIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models3[["AIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models3[["BIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 2 : NAIVE #####

Models2 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (criterion in c("AIC", "BIC")) {
  new_model <- TopMod3[[criterion]] |>
    dplyr::mutate(
      Model_specification = stringr::str_replace_all(Model_specification, c("\\+Country_DE" = "", "\\+Country_ES" = "", "\\+Country_FR" = "", "\\+Country_IT" = "", "\\+Country_NL" = "", "\\+Country_BE" = "", "\\+Country_AT" = "", "\\+Country_FI" = "", "\\+Country_PT" = "")),
      Model_specification = stringr::str_replace_all(Model_specification, c("Country_DE" = "0"))
    )

  for (variable in Variables) {
    Models2[[criterion]][[variable]] <- simulate_models(RegressionDB, variable, criterion, new_model)
  }
}

TopMod2 <- list(
  "AIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models2[["AIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Variables, function(variable) {
    Models2[["BIC"]][[variable]]
  })) |>
    dplyr::select("Variable", "Group", "Model_specification", "RMSE", "AIC", "BIC", "p.value", "N")
)

for (criterion in c("AIC", "BIC")) {
  TopMod8[[criterion]] <- TopMod8[[criterion]] |>
    dplyr::mutate(Variable = factor(Variable, levels = c(Var_Revenue, Var_Macro, Var_Expenditure))) |>
    dplyr::arrange(Variable)
}

for (criterion in c("AIC", "BIC")) {
  TopMod8[[criterion]]$RMSE3 <- TopMod3[[criterion]]$RMSE / TopMod2[[criterion]]$RMSE
  TopMod8[[criterion]]$RMSE4 <- TopMod4[[criterion]]$RMSE / TopMod2[[criterion]]$RMSE
  TopMod8[[criterion]]$RMSE5 <- TopMod5[[criterion]]$RMSE / TopMod2[[criterion]]$RMSE
  TopMod8[[criterion]]$RMSE6 <- TopMod6[[criterion]]$RMSE / TopMod2[[criterion]]$RMSE
  TopMod8[[criterion]]$RMSE8 <- TopMod8[[criterion]]$RMSE / TopMod2[[criterion]]$RMSE
}

y <- TopMod8[["AIC"]] |>
  dplyr::mutate(
    Group = factor(Group, levels = c("Revenue", "Expenditure", "Macro"))
  ) |>
  dplyr::mutate_if(is.numeric, round, digits = 2) |>
  dplyr::arrange(Group) |>
  dplyr::mutate(Group = as.character(Group)) |>
  dplyr::select("Variable", "Group", "N", "Model_specification", "p.value", "RMSE8", "RMSE6", "RMSE5", "RMSE4", "RMSE3")

x <- TopMod8[["BIC"]] |>
  dplyr::mutate(
    Group = factor(Group, levels = c("Revenue", "Expenditure", "Macro"))
  ) |>
  dplyr::mutate_if(is.numeric, round, digits = 2) |>
  dplyr::arrange(Group) |>
  dplyr::mutate(Group = as.character(Group)) |>
  dplyr::select("Variable", "Group", "N", "Model_specification", "p.value", "RMSE8", "RMSE6", "RMSE5", "RMSE4", "RMSE3")
