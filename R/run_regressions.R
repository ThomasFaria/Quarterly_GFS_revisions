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
Ctry_Agg <- c("DE", "ES", "FR", "IT", "NL", "BE", "AT", "FI", "PT", "REA")
Var_Revenue <- c("TOR", "DTX", "SCT", "TIN")
Var_Expenditure <- c("TOE", "THN", "PUR", "COE", "GIN")
Var_Macro <- c("YEN", "PCN", "ITN", "EXN", "GCN", "WGS")
ListofCountry <- paste0("Country_", Ctry_Agg, collapse = "+")
ListofQuarter <- paste0("ObsQ_", 1:4, collapse = "+")

Items <- c(Var_Revenue, Var_Macro, Var_Expenditure)

SimulModels <- function(sample, aItem, aCrit, NewModels) {
  sample <- subset(sample, Variable_code == aItem)
  tryCatch(
    expr = {
      Model <- dplyr::bind_rows(lapply(NewModels[NewModels$Item == aItem, "ModelSpe"], function(model) {
        x <- lm(paste(model, "-1"), data = sample)
        a <- broom::glance(x)
        a$ModelSpe <- model
        a$N <- nobs(x)
        a$Item <- aItem
        a$RMSE <- sqrt(c(crossprod(x$residuals)) / length(x$residuals))
        a$Group <- ifelse(aItem %in% Var_Revenue, "Revenue",
          ifelse(aItem %in% Var_Expenditure, "Expenditure",
            "Macro"
          )
        )
        return(a)
      }))
      return(Model)
    },
    error = function(e) {
      model <- as.character(NewModels[NewModels$Item == aItem, "ModelSpe"])
      x <- lm(model, data = sample)
      Model <- dplyr::tibble(
        AIC = c(AIC(x)),
        BIC = c(BIC(x)),
        N = c(nobs(x)),
        ModelSpe = c(model),
        Item = c(aItem),
        p.value = NA,
        RMSE = c(sqrt(c(crossprod(x$residuals)) / length(x$residuals))),
        Group = c(ifelse(aItem %in% Var_Revenue, "Revenue",
          ifelse(aItem %in% Var_Expenditure, "Expenditure",
            "Macro"
          )
        ))
      )
      return(Model)
    }
  )
}
##### MODEL 8 : Macro #####
Models8 <- list()
Regressor <- c(ListofCountry, ListofQuarter, "ESA2010", "First_announcement", paste0("Rev_lag", 1:5))
ListofModels8 <- unlist(lapply(
  1:length(Regressor),
  function(n) {
    combn(Regressor, n, FUN = function(row) {
      paste0("Final_revision ~ ", paste0(row, collapse = "+"))
    })
  }
))

for (aItem in Items) {
  sample <- subset(RegressionDB, Variable_code == aItem)
  Models8[[aItem]] <- dplyr::bind_rows(lapply(ListofModels8, function(model) {
    x <- lm(paste(model, "-1"), data = sample)
    a <- broom::glance(x)
    a$ModelSpe <- model
    a$N <- nobs(x)
    a$Item <- aItem
    a$RMSE <- sqrt(c(crossprod(x$residuals)) / length(x$residuals))
    a$Group <- ifelse(aItem %in% Var_Revenue, "Revenue",
      ifelse(aItem %in% Var_Expenditure, "Expenditure",
        "Macro"
      )
    )
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
    ModelSpe = c("Final_revision ~ 0"),
    Item = c(aItem),
    p.value = NA,
    RMSE = c(sqrt(c(crossprod(x$residuals)) / length(x$residuals))),
    Group = c(ifelse(aItem %in% Var_Revenue, "Revenue",
      ifelse(aItem %in% Var_Expenditure, "Expenditure",
        "Macro"
      )
    ))
  )
  Models8[[aItem]] <- Models8[[aItem]] |>
    dplyr::add_row(Model)
}

TopMod8 <- list(
  "AIC" = dplyr::bind_rows(lapply(1:length(Items), function(itemNb) {
    Models8[[itemNb]][order(Models8[[itemNb]]$AIC)[1], ]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(1:length(Items), function(itemNb) {
    Models8[[itemNb]][order(Models8[[itemNb]]$BIC)[1], ]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)


##### MODEL 6 : First Announcement #####
Models6 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (aCrit in c("AIC", "BIC")) {
  tmp <- TopMod8[[aCrit]] |>
    dplyr::mutate(
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("\\+Rev_lag1" = "", "\\+Rev_lag2" = "", "\\+Rev_lag3" = "", "\\+Rev_lag4" = "", "\\+Rev_lag5" = "")),
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("Rev_lag1" = "0", "Rev_lag2" = "0", "Rev_lag3" = "0", "Rev_lag4" = "0", "Rev_lag5" = "0"))
    )

  for (aItem in Items) {
    Models6[[aCrit]][[aItem]] <- SimulModels(RegressionDB, aItem, aCrit, tmp)
  }
}

TopMod6 <- list(
  "AIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models6[["AIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models6[["BIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 5 : ESA2010 #####
Models5 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (aCrit in c("AIC", "BIC")) {
  tmp <- TopMod6[[aCrit]] |>
    dplyr::mutate(
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("\\+First_announcement" = "")),
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("First_announcement" = "0"))
    )

  for (aItem in Items) {
    Models5[[aCrit]][[aItem]] <- SimulModels(RegressionDB, aItem, aCrit, tmp)
  }
}

TopMod5 <- list(
  "AIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models5[["AIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" = dplyr::bind_rows(lapply(Items, function(item) {
    Models5[["BIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 4 : Quarter FIXED EFFECT #####
Models4 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (aCrit in c("AIC", "BIC")) {
  tmp <- TopMod5[[aCrit]] |>
    dplyr::mutate(
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("\\+ESA2010" = "")),
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("ESA2010" = "0"))
    )

  for (aItem in Items) {
    Models4[[aCrit]][[aItem]] <- SimulModels(RegressionDB, aItem, aCrit, tmp)
  }
}

TopMod4 <- list(
  "AIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models4[["AIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models4[["BIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 3 : COUNTRY FIXED EFFECT #####
Models3 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (aCrit in c("AIC", "BIC")) {
  tmp <- TopMod4[[aCrit]] |>
    dplyr::mutate(
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("\\+ObsQ_1" = "", "\\+ObsQ_2" = "", "\\+ObsQ_3" = "", "\\+ObsQ_4" = "")),
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("ObsQ_1" = "0"))
    )

  for (aItem in Items) {
    Models3[[aCrit]][[aItem]] <- SimulModels(RegressionDB, aItem, aCrit, tmp)
  }
}

TopMod3 <- list(
  "AIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models3[["AIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models3[["BIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)

##### MODEL 2 : NAIVE #####

Models2 <- list(
  "AIC" = list(),
  "BIC" = list()
)

for (aCrit in c("AIC", "BIC")) {
  tmp <- TopMod3[[aCrit]] |>
    dplyr::mutate(
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("\\+Country_DE" = "", "\\+Country_ES" = "", "\\+Country_FR" = "", "\\+Country_IT" = "", "\\+Country_NL" = "", "\\+Country_BE" = "", "\\+Country_AT" = "", "\\+Country_FI" = "", "\\+Country_PT" = "")),
      ModelSpe =  stringr::str_replace_all(ModelSpe, c("Country_DE" = "0"))
    )

  for (aItem in Items) {
    Models2[[aCrit]][[aItem]] <- SimulModels(RegressionDB, aItem, aCrit, tmp)
  }
}

TopMod2 <- list(
  "AIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models2[["AIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N"),
  "BIC" =  dplyr::bind_rows(lapply(Items, function(item) {
    Models2[["BIC"]][[item]]
  })) |>
    dplyr::select("Item", "Group", "ModelSpe", "RMSE", "AIC", "BIC", "p.value", "N")
)

for (aCrit in c("AIC", "BIC")) {
  TopMod8[[aCrit]] <- TopMod8[[aCrit]] |>
    dplyr::mutate(Item = factor(Item, levels = c(Var_Revenue, Var_Macro, Var_Expenditure))) |>
    dplyr::arrange(Item)
}

for (aCrit in c("AIC", "BIC")) {
  TopMod8[[aCrit]]$RMSE3 <- TopMod3[[aCrit]]$RMSE / TopMod2[[aCrit]]$RMSE
  TopMod8[[aCrit]]$RMSE4 <- TopMod4[[aCrit]]$RMSE / TopMod2[[aCrit]]$RMSE
  TopMod8[[aCrit]]$RMSE5 <- TopMod5[[aCrit]]$RMSE / TopMod2[[aCrit]]$RMSE
  TopMod8[[aCrit]]$RMSE6 <- TopMod6[[aCrit]]$RMSE / TopMod2[[aCrit]]$RMSE
  TopMod8[[aCrit]]$RMSE8 <- TopMod8[[aCrit]]$RMSE / TopMod2[[aCrit]]$RMSE
}

y <- TopMod8[["AIC"]] |>
  dplyr::mutate(
    Group = factor(Group, levels = c("Revenue", "Expenditure", "Macro"))
  ) |>
  dplyr::mutate_if(is.numeric, round, digits = 2) |>
  dplyr::arrange(Group) |>
  dplyr::mutate(Group = as.character(Group)) |>
  dplyr::select("Item", "Group", "N", "ModelSpe", "p.value", "RMSE8", "RMSE6", "RMSE5", "RMSE4", "RMSE3")

x <- TopMod8[["BIC"]] |>
  dplyr::mutate(
    Group = factor(Group, levels = c("Revenue", "Expenditure", "Macro"))
  ) |>
  dplyr::mutate_if(is.numeric, round, digits = 2) |>
  dplyr::arrange(Group) |>
  dplyr::mutate(Group = as.character(Group)) |>
  dplyr::select("Item", "Group", "N", "ModelSpe", "p.value", "RMSE8", "RMSE6", "RMSE5", "RMSE4", "RMSE3")
