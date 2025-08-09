# R/line_tester.R

#' Line × Tester analysis (ANOVA + GCA/SCA)
#'
#' Runs the standard line × tester analysis using ANOVA for crosses,
#' computes GCA for lines and testers, and SCA for line:tester combinations.
#' Returns nicely formatted tables (with stars) ready for the app/UI.
#'
#' @param data A data.frame containing the columns referenced below.
#' @param line_col   Name of the column with line IDs (character).
#' @param tester_col Name of the column with tester IDs (character).
#' @param rep_col    Name of the column with replication/block (character).
#' @param trait_col  Name of the numeric trait column (character).
#' @param type_col   Name of the column indicating type: "cross" or parents
#'                   (e.g., "parent", "line", "tester"). Cross rows must use
#'                   the exact value `"cross"`.
#'
#' @return A list with:
#'   - anova_full: ANOVA table with sources incl. lines, testers, L×T
#'   - gca_lines:  GCA for lines (value, SE, t, p, stars)
#'   - gca_testers:GCA for testers (value, SE, t, p, stars)
#'   - sca:        SCA for line:tester (value, SE, t, p, stars)
#'
#' @keywords internal
#' @noRd
line_tester_manual <- function(data, line_col, tester_col, rep_col, trait_col, type_col) {

  # --- Helper: significance stars (vectorised) ---
  get_stars <- function(p_values) {
    dplyr::case_when(
      is.na(p_values)   ~ "",
      p_values < 0.001  ~ "***",
      p_values < 0.01   ~ "**",
      p_values < 0.05   ~ "*",
      p_values < 0.1    ~ ".",
      TRUE              ~ ""
    )
  }

  tryCatch({

    # ---- 1) Prepare & validate ------------------------------------------------
    df <- data.frame(
      Rep    = as.factor(data[[rep_col]]),
      Line   = as.factor(data[[line_col]]),
      Tester = as.factor(data[[tester_col]]),
      Type   = as.factor(data[[type_col]]),
      Y      = as.numeric(data[[trait_col]])
    )

    df <- stats::na.omit(df)

    if (!any(df$Type == "cross")) {
      stop("The 'Type' column must contain entries with the exact value 'cross'.")
    }

    parents <- df[df$Type != "cross", , drop = FALSE]
    crosses <- df[df$Type == "cross", , drop = FALSE]
    crosses$Line   <- droplevels(crosses$Line)
    crosses$Tester <- droplevels(crosses$Tester)

    l <- nlevels(crosses$Line)
    t <- nlevels(crosses$Tester)
    if (l < 2 || t < 1) {
      stop("Analysis requires at least two lines and one tester in the cross data.")
    }

    # ---- 2) ANOVA pieces ------------------------------------------------------
    df$TreatmentID <- interaction(df$Line, df$Tester, df$Type, drop = TRUE)

    model_overall <- stats::aov(Y ~ Rep + TreatmentID, data = df)
    anova_overall <- stats::anova(model_overall)
    MS_Error <- anova_overall["Residuals", "Mean Sq"]
    DF_Error <- anova_overall["Residuals", "Df"]

    model_crosses <- stats::aov(Y ~ Line * Tester, data = crosses)
    anova_crosses <- stats::anova(model_crosses)

    DF_Parents <- 0; SS_Parents <- 0
    if (nrow(parents) > 0 && nlevels(droplevels(parents$Line)) > 1) {
      parents$Parent <- droplevels(parents$Line)
      model_parents  <- stats::aov(Y ~ Parent, data = parents)
      anova_parents  <- stats::anova(model_parents)
      DF_Parents <- anova_parents["Parent", "Df"]
      SS_Parents <- anova_parents["Parent", "Sum Sq"]
    }

    DF_PvC <- 0; SS_PvC <- 0
    if (nrow(parents) > 0) {
      df$PvC <- factor(ifelse(df$Type == "cross", "Cross", "Parent"),
                       levels = c("Parent", "Cross"))
      model_pvc <- stats::aov(Y ~ PvC, data = df)
      anova_pvc <- stats::anova(model_pvc)
      DF_PvC <- anova_pvc["PvC", "Df"]
      SS_PvC <- anova_pvc["PvC", "Sum Sq"]
    }

    # ---- 3) Assemble ANOVA table ---------------------------------------------
    ss_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Sum Sq"])
    df_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Df"])

    source_names <- c(
      "Replications", "Treatments", "  Parents", "  Parents vs. Crosses",
      "  Crosses", "    Lines", "    Testers", "    Lines X Testers",
      "Error", "Total"
    )

    DF <- c(
      anova_overall["Rep", "Df"],
      anova_overall["TreatmentID", "Df"],
      DF_Parents, DF_PvC,
      df_crosses_total,
      anova_crosses["Line", "Df"],
      anova_crosses["Tester", "Df"],
      anova_crosses["Line:Tester", "Df"],
      DF_Error,
      sum(anova_overall[, "Df"], na.rm = TRUE)
    )

    SS <- c(
      anova_overall["Rep", "Sum Sq"],
      anova_overall["TreatmentID", "Sum Sq"],
      SS_Parents, SS_PvC,
      ss_crosses_total,
      anova_crosses["Line", "Sum Sq"],
      anova_crosses["Tester", "Sum Sq"],
      anova_crosses["Line:Tester", "Sum Sq"],
      anova_overall["Residuals", "Sum Sq"],
      sum(anova_overall[, "Sum Sq"], na.rm = TRUE)
    )

    MS <- ifelse(DF > 0, SS / DF, 0)
    F_value <- MS / MS_Error
    P_value <- stats::pf(F_value, DF, DF_Error, lower.tail = FALSE)

    anova_final <- data.frame(
      Source = source_names,
      Df = DF,
      `Sum Sq` = SS,
      `Mean Sq` = MS,
      `F value` = F_value,
      `Pr(>F)`  = P_value,
      check.names = FALSE
    )

    # Fill in correct Error MS, blank "Total" stats
    anova_final[anova_final$Source == "Error", "Mean Sq"] <- MS_Error
    anova_final[anova_final$Source == "Total", c("Mean Sq", "F value", "Pr(>F)")] <- NA
    anova_final$Stars <- get_stars(anova_final$`Pr(>F)`)

    # Pretty numeric formatting
    num_cols <- c("Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    anova_final[num_cols] <- lapply(anova_final[num_cols], function(x) sprintf("%.2f", x))
    anova_final[is.na(anova_final) | anova_final == "NA"] <- ""

    # ---- 4) GCA & SCA with t-tests -------------------------------------------
    grand_mean <- mean(crosses$Y)

    # GCA for Lines
    emm_lines <- emmeans::emmeans(model_crosses, ~ Line)
    summary_lines <- as.data.frame(summary(emm_lines))
    gca_lines_out <- data.frame(
      Line = summary_lines$Line,
      GCA  = summary_lines$emmean - grand_mean,
      SE   = summary_lines$SE
    )
    gca_lines_out$`t value`  <- gca_lines_out$GCA / gca_lines_out$SE
    gca_lines_out$`Pr(>|t|)` <- 2 * stats::pt(-abs(gca_lines_out$`t value`), df = DF_Error)
    gca_lines_out$Stars      <- get_stars(gca_lines_out$`Pr(>|t|)`)

    # GCA for Testers
    emm_testers <- emmeans::emmeans(model_crosses, ~ Tester)
    summary_testers <- as.data.frame(summary(emm_testers))
    gca_testers_out <- data.frame(
      Tester = summary_testers$Tester,
      GCA    = summary_testers$emmean - grand_mean,
      SE     = summary_testers$SE
    )
    gca_testers_out$`t value`  <- gca_testers_out$GCA / gca_testers_out$SE
    gca_testers_out$`Pr(>|t|)` <- 2 * stats::pt(-abs(gca_testers_out$`t value`), df = DF_Error)
    gca_testers_out$Stars      <- get_stars(gca_testers_out$`Pr(>|t|)`)

    # SCA for Line:Tester
    emm_sca <- emmeans::emmeans(model_crosses, ~ Line:Tester)
    summary_sca <- as.data.frame(summary(emm_sca))
    summary_sca <- merge(summary_sca, gca_lines_out[, c("Line", "GCA")], by = "Line")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Line"
    summary_sca <- merge(summary_sca, gca_testers_out[, c("Tester", "GCA")], by = "Tester")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Tester"

    summary_sca$SCA <- summary_sca$emmean - summary_sca$GCA_Line - summary_sca$GCA_Tester - grand_mean

    sca_out <- data.frame(
      Line   = summary_sca$Line,
      Tester = summary_sca$Tester,
      SCA    = summary_sca$SCA,
      SE     = summary_sca$SE
    )
    sca_out$`t value`  <- sca_out$SCA / sca_out$SE
    sca_out$`Pr(>|t|)` <- 2 * stats::pt(-abs(sca_out$`t value`), df = DF_Error)
    sca_out$Stars      <- get_stars(sca_out$`Pr(>|t|)`)

    # ---- 5) Return ------------------------------------------------------------
    list(
      anova_full  = anova_final,
      gca_lines   = gca_lines_out,
      gca_testers = gca_testers_out,
      sca         = sca_out
    )

  }, error = function(e) {
    list(error = paste("Line x Tester analysis failed:", e$message))
  })
}
