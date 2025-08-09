test_that("Line × Tester analysis runs and returns expected tables", {
  csv_path <- system.file("extdata", "Line_x_Tester_Sample.csv", package = "PBAT")
  expect_true(file.exists(csv_path))

  df <- readr::read_csv(csv_path, show_col_types = FALSE)

  # Use triple-colon if not exported yet
  res <- PBAT:::line_tester_manual(
    data       = df,
    line_col   = "Line",
    tester_col = "Tester",
    rep_col    = "Replication",
    trait_col  = "Yield",
    type_col   = "Type"
  )

  # Structure
  expect_type(res, "list")
  expect_true(all(c("anova_full", "gca_lines", "gca_testers", "sca") %in% names(res)))

  # Dimensions vs cross data
  cross_df <- df[df$Type == "cross", ]
  l <- length(unique(cross_df$Line))
  t <- length(unique(cross_df$Tester))
  expect_equal(nrow(res$gca_lines),   l)
  expect_equal(nrow(res$gca_testers), t)
  expect_equal(nrow(res$sca),         l * t)

  # Columns present
  expect_true(all(c("Line","GCA","SE")   %in% names(res$gca_lines)))
  expect_true(all(c("Tester","GCA","SE") %in% names(res$gca_testers)))
  expect_true(all(c("Line","Tester","SCA","SE") %in% names(res$sca)))
  expect_true(all(c("Source","Df","Sum Sq","Mean Sq") %in% names(res$anova_full)))

  # p-values computed (if present)
  if ("Pr(>F)" %in% names(res$anova_full)) {
    x <- res$anova_full[res$anova_full$Source != "Total", "Pr(>F)"]
    if (is.numeric(x)) expect_false(any(is.na(x)))
  }
  if ("Pr(>|t|)" %in% names(res$gca_lines))   expect_false(any(is.na(res$gca_lines$`Pr(>|t|)`)))
  if ("Pr(>|t|)" %in% names(res$gca_testers)) expect_false(any(is.na(res$gca_testers$`Pr(>|t|)`)))
  if ("Pr(>|t|)" %in% names(res$sca))         expect_false(any(is.na(res$sca$`Pr(>|t|)`)))
})
