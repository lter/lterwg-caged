## --------------------------------------------------------------- ##
# CAGED Stats Helper Functions
## --------------------------------------------------------------- ##
# Sourced by 10c_stats_max_v2.R (and future versions).
# All functions assume the parent script has already loaded:
#   tidyverse, glmmTMB, DHARMa, performance, car, MuMIn, ggeffects, patchwork

## ------------------------------------------- ##
# Color palettes ----
## ------------------------------------------- ##

# Legacy qualitative palette — kept for any direct references outside plot helpers.
CAGED_PALETTE <- c(
  "#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499",
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999",
  "#661100", "#6699CC", "#AA4466"
)

# Semantically meaningful per-variable palettes.
# Each entry is a NAMED color vector keyed to the exact factor level strings.
# get_var_pal() falls back to FALLBACK_PALETTE for variables not listed here.
VARIABLE_PALETTES <- list(
  cage.treatment_std = c(
    caged   = "#0072B2",   # dark blue  — consumers excluded
    uncaged = "#56B4E9"    # light blue — consumers present
  ),
  var_aq.or.terr = c(
    aquatic      = "#0096C7",   # ocean blue
    terrestrial  = "#40916C",   # forest green
    transitional = "#CC6677"    # muted rose — neither aquatic nor terrestrial
  ),
  var_succ.vs.late = c(
    early = "#F4A261",   # amber  — open early-successional habitats
    late  = "#264653"    # deep teal — mature late-successional habitats
  ),
  var_consumer.metabolism = c(
    ectotherm = "#2EC4B6",   # teal — cold-blooded
    endotherm = "#E63946"    # red  — warm-blooded
  )
)

# Fallback palette for variables not in VARIABLE_PALETTES (e.g., var_ecotype1).
# Avoids blues reserved for cage.treatment_std.
FALLBACK_PALETTE <- c(
  "#E69F00", "#CC79A7", "#D55E00", "#F0E442", "#882255",
  "#44AA99", "#999933", "#AA4499", "#661100", "#DDCC77",
  "#CC6677", "#117733", "#6699CC", "#AA4466", "#332288",
  "#999999", "#009E73", "#E76F51", "#2A9D8F", "#F4E285"
)

# Return a named color vector for a given predictor variable and its levels.
# Uses VARIABLE_PALETTES if all levels are matched; falls back to FALLBACK_PALETTE.
get_var_pal <- function(var_name, levels) {
  if (!is.null(var_name) && var_name %in% names(VARIABLE_PALETTES)) {
    vp      <- VARIABLE_PALETTES[[var_name]]
    matched <- vp[levels]
    if (!anyNA(matched)) return(matched)
  }
  setNames(FALLBACK_PALETTE[seq_len(length(levels))], levels)
}

## ------------------------------------------- ##
# Smithson-Verkuilen transform ----
## ------------------------------------------- ##

sv_transform <- function(y, n) { (y * (n - 1) + 0.5) / n }

## ------------------------------------------- ##
# Significance helpers ----
## ------------------------------------------- ##

# Stars label for p < 0.05; NULL (no annotation) when non-significant.
sig_label_fn <- function(p) {
  if (is.null(p) || is.na(p)) return(NULL)
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "NS"
}

# Look up the p-value for an exact term string in a car::Anova table
# (returned by summarise_model()).
lookup_p <- function(anova_tbl, term_str) {
  idx <- which(anova_tbl$term == term_str)
  if (length(idx) == 0) NA_real_ else anova_tbl[["Pr(>Chisq)"]][idx[1]]
}

## ------------------------------------------- ##
# fmt_pval() ----
## ------------------------------------------- ##
# Format p-values for printing: fixed notation for p >= 1e-4,
# scientific for p < 1e-4. Bypasses R's column-level formatting,
# which would force the entire column into scientific if any value is tiny.

fmt_pval <- function(p, digits = 4) {
  ifelse(is.na(p), NA_character_,
    ifelse(p >= 1e-4,
      formatC(p, format = "f", digits = digits),
      formatC(p, format = "e", digits = digits - 1)
    )
  )
}

## ------------------------------------------- ##
# summarise_model() ----
## ------------------------------------------- ##
# Prints and exports Type II ANOVA table; reports AIC and R².

summarise_model <- function(model, label, outdir = "results") {

  cat("\n", strrep("=", 60), "\n", label, "\n", strrep("=", 60), "\n\n", sep = "")

  conv <- tryCatch(check_convergence(model), error = function(e) NA)
  cat("Convergence:", if (isTRUE(conv)) "OK" else "CHECK WARNINGS", "\n")
  cat("AIC:", round(AIC(model), 2), "\n")

  r2 <- tryCatch({
    re_bars  <- lme4::findbars(formula(model))
    re_str   <- paste(vapply(re_bars, function(b) paste0("(", deparse(b), ")"), character(1)),
                      collapse = " + ")
    lhs      <- deparse(lme4::nobars(formula(model))[[2]])
    null_f   <- as.formula(paste(lhs, "~ 1 +", re_str))
    null_mod <- update(model, formula = null_f, data = model.frame(model))
    r.squaredLR(model, null = null_mod)
  }, error = function(e) NULL)
  if (!is.null(r2))
    cat(sprintf("R2_LR = %.4f  |  R2_LR adj = %.4f\n",
                r2[[1]], attr(r2, "adj.r.squared")))

  anova_tbl <- car::Anova(model, type = "II") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(sig = dplyr::case_when(
      `Pr(>Chisq)` < 0.001 ~ "***",
      `Pr(>Chisq)` < 0.01  ~ "**",
      `Pr(>Chisq)` < 0.05  ~ "*",
      TRUE                  ~ ""))

  # Format p-value columns for printing (fixes column-level sci notation issue)
  anova_print <- anova_tbl
  p_cols <- grep("^Pr", names(anova_print), value = TRUE)
  for (col in p_cols) anova_print[[col]] <- fmt_pval(anova_print[[col]])
  print(anova_print, digits = 4, right = FALSE)

  # Export retains original numeric p-values
  write.csv(anova_tbl, row.names = FALSE,
    file = file.path(outdir, paste0(gsub("[^A-Za-z0-9_]", "_", label), "_anova.csv")))
  invisible(anova_tbl)
}

## ------------------------------------------- ##
# validate_dharma() ----
## ------------------------------------------- ##
# DHARMa residual diagnostics. Always saves:
#   _dharma_overall.png       — QQ plot + residuals vs. predicted
#   _dharma_dispersion.png    — over/underdispersion histogram
#   _dharma_cage.treatment.png — residuals by cage treatment
# Plus one plot per variable in pred_vars.

validate_dharma <- function(model, data, prefix,
                             pred_vars = NULL,
                             outdir    = file.path("graphs", "model_validation"),
                             nsim      = 500,
                             seed      = 42) {
  set.seed(seed)
  sim <- simulateResiduals(fittedModel = model, n = nsim, plot = FALSE)

  # Subset data to only the rows used in model fitting.
  # glmmTMB silently drops rows with NAs in any model variable, so
  # data$predictor and sim residuals can have different lengths without this.
  model_rows <- tryCatch(
    as.integer(rownames(model$frame)),
    error = function(e) seq_len(nrow(data))
  )
  data_used <- data[model_rows, , drop = FALSE]

  # 1. Overall QQ + residuals vs. predicted
  png(file.path(outdir, paste0(prefix, "_dharma_overall.png")),
      width = 2400, height = 1400, res = 300)
  plot(sim, main = prefix)
  dev.off()

  # 2. Overdispersion / underdispersion test plot
  png(file.path(outdir, paste0(prefix, "_dharma_dispersion.png")),
      width = 1800, height = 1400, res = 300)
  testDispersion(sim)
  dev.off()

  # 3. Residuals vs. cage treatment (always included)
  png(file.path(outdir, paste0(prefix, "_dharma_cage.treatment.png")),
      width = 1800, height = 1400, res = 300)
  plotResiduals(sim, form = data_used$cage.treatment_std,
                main = paste(prefix, "| cage.treatment_std"))
  dev.off()

  # 4. Residuals vs. each additional predictor
  if (!is.null(pred_vars)) {
    for (v in pred_vars) {
      if (!v %in% names(data_used)) next
      v_data <- data_used[[v]]
      if (all(is.na(v_data))) next
      safe_v <- gsub("[^A-Za-z0-9]", "_", v)
      png(file.path(outdir, paste0(prefix, "_dharma_", safe_v, ".png")),
          width = 1800, height = 1400, res = 300)
      plotResiduals(sim, form = v_data, main = paste(prefix, "|", v))
      dev.off()
    }
  }

  invisible(sim)
}

## ------------------------------------------- ##
# plot_partial_with_data() ----
## ------------------------------------------- ##
# Overlays raw observed data with ggpredict() model predictions.
#
# Continuous x  → scatter (raw, semi-transparent) + ribbon + line
# Categorical x → grey points (raw) + colored pointrange (model)
# Many-group categorical (> n_facet_threshold groups on second term)
#   → facets by group variable
#
# Parameters:
#   model           — fitted glmmTMB model
#   data            — data frame used to fit the model (for raw overlay)
#   terms           — ggpredict terms vector, e.g. c("cage.treatment_std", "var_aq.or.terr")
#                     Continuous variables may include ggeffects annotations:
#                     "exclusion.duration [n=200]"
#   prefix          — output file name stem
#   outdir          — output directory
#   ylab            — y-axis label
#   x_lab           — x-axis label (defaults to variable name)
#   width, height   — figure dimensions in inches
#   pt_alpha        — transparency for raw data points
#   pt_size         — size of raw data points
#   n_facet_threshold — if n_groups (second term levels) exceeds this, use facets
#   save            — write PNG to outdir (FALSE to suppress and return plot only)

plot_partial_with_data <- function(model, data, terms, prefix,
                                    p_value           = NULL,
                                    outdir            = file.path("graphs", "model_predictions"),
                                    ylab              = "Beta dispersion (SV-transformed)",
                                    x_lab             = NULL,
                                    width             = 7,
                                    height            = 5,
                                    pt_alpha          = 0.07,
                                    pt_size           = 0.6,
                                    n_facet_threshold = 6,
                                    save              = TRUE) {

  preds   <- ggpredict(model, terms = terms)
  pred_df <- as.data.frame(preds)

  # Strip ggeffects annotations ("[n=200]", "[all]", etc.)
  term_vars <- trimws(sub("\\s*\\[.*\\]", "", terms))
  x_var    <- term_vars[1]
  grp_var  <- if (length(term_vars) >= 2) term_vars[2] else NULL

  # Named palette keyed to sorted group levels — ensures line and fill colors
  # map to the same groups regardless of factor level ordering in pred_df.
  pred_df$grp <- as.character(pred_df$group)
  grp_levels  <- sort(unique(pred_df$grp))
  pal         <- get_var_pal(grp_var, grp_levels)

  is_cont_x  <- x_var %in% names(data) && is.numeric(data[[x_var]])
  n_groups   <- length(grp_levels)
  # Facets triggered only when grouping variable has many levels
  use_facets <- !is_cont_x && !is.null(grp_var) && n_groups > n_facet_threshold

  # Rotate x-axis labels when many categories
  n_x_cats <- if (!is_cont_x && x_var %in% names(data))
    length(unique(na.omit(data[[x_var]]))) else 0
  x_angle  <- if (n_x_cats > 5) 40 else 0
  x_hjust  <- if (n_x_cats > 5) 1  else 0.5

  # Significance annotation pre-computation (used across all cases below)
  ltype   <- if (!is.null(p_value) && !is.na(p_value) && p_value < 0.05) "solid" else "dashed"
  sig_lbl <- sig_label_fn(p_value)

  base_theme <- theme_classic(base_size = 12) +
    theme(panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.7),
          axis.text       = element_text(color = "black"),
          axis.text.x     = element_text(angle = x_angle, hjust = x_hjust),
          legend.position = "right")

  ## ---------- Case 1: continuous x ----------
  if (is_cont_x) {

    sel_cols <- intersect(c(x_var, "betadisp_t", grp_var), names(data))
    raw_df   <- data[complete.cases(data[, sel_cols, drop = FALSE]), sel_cols, drop = FALSE]

    p <- ggplot()

    if (!is.null(grp_var)) {
      p <- p +
        geom_point(data  = raw_df,
                   aes(x = .data[[x_var]], y = betadisp_t, color = .data[[grp_var]]),
                   alpha = pt_alpha, size = pt_size) +
        geom_ribbon(data = pred_df,
                    aes(x = x, ymin = conf.low, ymax = conf.high, fill = grp),
                    alpha = 0.35) +
        geom_line(data   = pred_df,
                  aes(x  = x, y = predicted, color = grp),
                  linewidth = 1.1, linetype = ltype) +
        scale_color_manual(values = pal, name = grp_var) +
        scale_fill_manual( values = pal, name = grp_var)
    } else {
      p <- p +
        geom_point(data = raw_df,
                   aes(x = .data[[x_var]], y = betadisp_t),
                   color = "grey40", alpha = pt_alpha, size = pt_size) +
        geom_ribbon(data = pred_df,
                    aes(x = x, ymin = conf.low, ymax = conf.high),
                    alpha = 0.25, fill = CAGED_PALETTE[1]) +
        geom_line(data  = pred_df,
                  aes(x = x, y = predicted),
                  color = CAGED_PALETTE[1], linewidth = 1.1, linetype = ltype)
    }

    p <- p +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = if (!is.null(x_lab)) x_lab else x_var, y = ylab) +
      base_theme

  if (!is.null(sig_lbl))
    p <- p + annotate("text", x = Inf, y = Inf, label = sig_lbl,
                      hjust = 1.3, vjust = 1.8, size = 5, fontface = "bold")

  ## ---------- Case 2: many-group categorical → facets ----------
  } else if (use_facets) {

    sel_cols <- intersect(c(x_var, "betadisp_t", grp_var), names(data))
    raw_df   <- data[complete.cases(data[, sel_cols, drop = FALSE]), sel_cols, drop = FALSE]

    # Color boxplots by x_var (e.g., cage treatment) within each facet.
    # Factor both x variables to the same alphabetical level order so that
    # position_dodge and the x-axis scale use a consistent left/right assignment.
    x_levels        <- sort(unique(na.omit(as.character(raw_df[[x_var]]))))
    x_pal           <- get_var_pal(x_var, x_levels)
    raw_df[[x_var]] <- factor(raw_df[[x_var]], levels = x_levels)
    pred_df$x_chr   <- factor(as.character(pred_df$x), levels = x_levels)

    pred_df[[grp_var]] <- pred_df$grp

    n_col_facets <- min(4L, ceiling(sqrt(n_groups)))
    width  <- max(width,  3.4 * n_col_facets)
    height <- max(height, 3.5 * ceiling(n_groups / n_col_facets))

    p <- ggplot() +
      geom_boxplot(data = raw_df,
                   aes(x = .data[[x_var]], y = betadisp_t,
                       fill = .data[[x_var]]),
                   color = "grey25", width = 0.5,
                   outlier.shape = NA, linewidth = 0.4,
                   show.legend = FALSE) +
      geom_pointrange(data = pred_df,
                      aes(x = x_chr, y = predicted,
                          ymin = conf.low, ymax = conf.high),
                      color = "black", size = 0.45, linewidth = 0.85) +
      scale_fill_manual(values = adjustcolor(x_pal, alpha.f = 0.5), guide = "none") +
      facet_wrap(vars(.data[[grp_var]]), ncol = n_col_facets) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = if (!is.null(x_lab)) x_lab else x_var, y = ylab) +
      base_theme +
      theme(strip.background = element_rect(fill = "grey92", color = "black"),
            strip.text       = element_text(size = 9))

  if (!is.null(sig_lbl))
    p <- p + annotate("text", x = Inf, y = Inf, label = sig_lbl,
                      hjust = 1.3, vjust = 1.8, size = 5, fontface = "bold")

  ## ---------- Case 3: categorical x, few groups → dodge ----------
  } else {

    sel_cols <- intersect(c(x_var, "betadisp_t", grp_var), names(data))
    raw_df   <- data[complete.cases(data[, sel_cols, drop = FALSE]), sel_cols, drop = FALSE]

    # Force both raw data grouping and ggeffects predictions to the same factor
    # level order (alphabetical). Without this, position_dodge may assign opposite
    # left/right slots to the same group when ggeffects returns factor levels in a
    # different order than ggplot2's default alphabetical — causing colors to look
    # reversed on some variables (e.g., var_succ.vs.late) but not others.
    pred_df$x_chr <- factor(as.character(pred_df$x), levels = sort(unique(as.character(pred_df$x))))
    pred_df$grp   <- factor(pred_df$grp, levels = grp_levels)
    if (!is.null(grp_var))
      raw_df[[grp_var]] <- factor(raw_df[[grp_var]], levels = grp_levels)
    dodge_w <- 0.55

    p <- ggplot()

    if (!is.null(grp_var)) {
      # Boxplot fill = palette color at 50% opacity; outline fixed grey (no palette mapping
      # on color — that was causing color reversals for some variables). Pointrange on top
      # uses full-opacity palette so fills and model points clearly share the same color.
      p <- p +
        geom_boxplot(data = raw_df,
                     aes(x = .data[[x_var]], y = betadisp_t,
                         fill = .data[[grp_var]]),
                     color = "grey25",
                     position  = position_dodge(width = dodge_w),
                     width = 0.35, outlier.shape = NA, linewidth = 0.4) +
        geom_pointrange(data = pred_df,
                        aes(x = x_chr, y = predicted,
                            ymin = conf.low, ymax = conf.high,
                            color = grp),
                        position = position_dodge(width = dodge_w),
                        size = 0.65, linewidth = 1.0) +
        scale_color_manual(values = pal, name = grp_var) +
        scale_fill_manual( values = adjustcolor(pal, alpha.f = 0.5), name = grp_var)
    } else {
      # No grouping variable — color each box by its x-axis value so that, e.g.,
      # cage treatment boxes are dark/light blue even without a second term.
      x_levels_solo <- sort(unique(na.omit(as.character(raw_df[[x_var]]))))
      x_pal_solo    <- get_var_pal(x_var, x_levels_solo)
      raw_df[[x_var]] <- factor(raw_df[[x_var]], levels = x_levels_solo)
      p <- p +
        geom_boxplot(data = raw_df,
                     aes(x = .data[[x_var]], y = betadisp_t,
                         fill = .data[[x_var]]),
                     color = "grey25",
                     width = 0.5, outlier.shape = NA, linewidth = 0.4) +
        geom_pointrange(data = pred_df,
                        aes(x = x_chr, y = predicted,
                            ymin = conf.low, ymax = conf.high,
                            color = x_chr),
                        size = 0.7, linewidth = 1.1) +
        scale_fill_manual( values = adjustcolor(x_pal_solo, alpha.f = 0.5), guide = "none") +
        scale_color_manual(values = x_pal_solo, guide = "none")
    }

    p <- p +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = if (!is.null(x_lab)) x_lab else x_var, y = ylab) +
      base_theme

  if (!is.null(sig_lbl)) {
    y_b <- min(0.93, max(pred_df$conf.high, na.rm = TRUE) + 0.07)
    if (n_x_cats == 2) {
      p <- p +
        annotate("segment", x = 1,   xend = 2,   y = y_b,        yend = y_b,
                 linewidth = 0.5) +
        annotate("segment", x = 1,   xend = 1,   y = y_b,        yend = y_b - 0.02,
                 linewidth = 0.5) +
        annotate("segment", x = 2,   xend = 2,   y = y_b,        yend = y_b - 0.02,
                 linewidth = 0.5) +
        annotate("text",    x = 1.5, y = y_b + 0.02, label = sig_lbl,
                 size = 5, fontface = "bold")
    } else {
      p <- p + annotate("text", x = Inf, y = Inf, label = sig_lbl,
                        hjust = 1.3, vjust = 1.8, size = 5, fontface = "bold")
    }
  }
  }

  if (save)
    ggsave(file.path(outdir, paste0(prefix, "_partial.png")),
           plot = p, width = width, height = height, dpi = 300)
  invisible(p)
}

## ------------------------------------------- ##
# plot_covariate_effects() ----
## ------------------------------------------- ##
# Plots model-estimated partial effects of covariates overlaid on raw data.
# Saved as a multi-panel figure: <prefix>_covariates.png
#
# covariate_terms — list of named lists, each with:
#   term  : ggpredict terms string (e.g. "betadisp.sample.size [n=150]")
#   xlab  : x-axis label
#   color : line/ribbon color
# Defaults to the original two-covariate set for backward compatibility.

plot_covariate_effects <- function(model, data, prefix,
                                    outdir          = file.path("graphs", "model_predictions"),
                                    ylab            = "Beta dispersion (SV-transformed)",
                                    covariate_terms = list(
                                      list(term  = "gamma.richness_exp.name [n=150]",
                                           xlab  = "Gamma richness (exp.name)",
                                           color = "#0072B2"),
                                      list(term  = "betadisp.sample.size [n=150]",
                                           xlab  = "Betadisp sample size (# replicates)",
                                           color = "#D55E00")
                                    )) {

  cov_theme <- theme_classic(base_size = 12) +
    theme(panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.7),
          axis.text       = element_text(color = "black"),
          legend.position = "none")

  panels <- lapply(covariate_terms, function(ct) {
    raw_var <- trimws(sub("\\s*\\[.*\\]", "", ct$term))
    pd <- tryCatch(
      as.data.frame(ggpredict(model, terms = ct$term)),
      error = function(e) { warning("Could not predict ", raw_var, " for ", prefix); NULL }
    )
    if (is.null(pd)) return(NULL)

    ct_ltype   <- if (!is.null(ct$p_value) && !is.na(ct$p_value) && ct$p_value < 0.05) "solid" else "dashed"
    ct_sig_lbl <- sig_label_fn(ct$p_value)
    p <- ggplot()
    if (raw_var %in% names(data))
      p <- p + geom_point(data  = data,
                          aes(x = .data[[raw_var]], y = betadisp_t),
                          color = "grey40", alpha = 0.06, size = 0.5)
    p <- p +
      geom_ribbon(data = pd,
                  aes(x = x, ymin = conf.low, ymax = conf.high),
                  alpha = 0.25, fill = ct$color) +
      geom_line(data  = pd,
                aes(x = x, y = predicted),
                color = ct$color, linewidth = 1.1, linetype = ct_ltype) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = ct$xlab, y = ylab) +
      cov_theme
    if (!is.null(ct_sig_lbl))
      p <- p + annotate("text", x = Inf, y = Inf, label = ct_sig_lbl,
                        hjust = 1.3, vjust = 1.8, size = 5, fontface = "bold")
    p
  })

  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0) return(invisible(NULL))

  ncol_fig   <- min(length(panels), 2L)
  p_combined <- patchwork::wrap_plots(panels, ncol = ncol_fig)

  ggsave(file.path(outdir, paste0(prefix, "_covariates.png")),
         plot    = p_combined,
         width   = 5 * ncol_fig,
         height  = 5 * ceiling(length(panels) / ncol_fig),
         dpi     = 300)
  invisible(p_combined)
}

## ------------------------------------------- ##
# check_model_vif() ----
## ------------------------------------------- ##
# Fits a main-effects-only version of the model (stripping interaction terms)
# and runs check_collinearity() to detect multicollinearity.
# Prints a flagged VIF table and writes <label>_vif.csv to outdir.
#
# Parameters:
#   model   — fitted glmmTMB model
#   data    — data frame used to fit the model (must be passed explicitly
#             because aquatic-only subsets differ from the base dataset)
#   label   — string prefix for output file and console header
#   outdir  — directory for the _vif.csv output

check_model_vif <- function(model, data, label, outdir = "results") {

  f       <- formula(model)
  re_bars <- lme4::findbars(f)
  fixed_f <- lme4::nobars(f)

  te         <- attr(terms(fixed_f), "term.labels")
  main_terms <- te[!grepl(":", te)]

  re_str <- paste(
    vapply(re_bars, function(b) paste0("(", deparse(b), ")"), character(1)),
    collapse = " + "
  )
  lhs   <- deparse(fixed_f[[2]])
  new_f <- as.formula(paste(lhs, "~", paste(c(main_terms, re_str), collapse = " + ")))

  mod_vif <- tryCatch(
    update(model, formula = new_f, data = data),
    error = function(e) {
      warning("VIF refit failed for '", label, "': ", e$message)
      NULL
    }
  )
  if (is.null(mod_vif)) {
    cat("VIF check skipped (refit failed):", label, "\n")
    return(invisible(NULL))
  }

  cat("\n--- VIF (", label, "— main effects only) ---\n", sep = "")
  vif_tbl <- tryCatch(
    as.data.frame(check_collinearity(mod_vif)),
    error = function(e) { warning("check_collinearity failed: ", e$message); NULL }
  )
  if (is.null(vif_tbl)) return(invisible(NULL))

  print(vif_tbl, digits = 3)

  vif_col  <- names(vif_tbl)[grep("^VIF$",  names(vif_tbl))[1]]
  term_col <- if ("Term" %in% names(vif_tbl)) "Term" else names(vif_tbl)[1]
  vif_vals  <- vif_tbl[[vif_col]]
  vif_terms <- vif_tbl[[term_col]]

  high     <- vif_terms[!is.na(vif_vals) & vif_vals >= 10]
  moderate <- vif_terms[!is.na(vif_vals) & vif_vals >= 5 & vif_vals < 10]

  if (length(high)     > 0) cat("!! HIGH VIF (>=10):",     paste(high,     collapse = ", "), "\n")
  if (length(moderate) > 0) cat("!  MODERATE VIF (5-10):", paste(moderate, collapse = ", "), "\n")
  if (length(c(high, moderate)) == 0) cat("All VIF < 5 — no collinearity concerns.\n")

  write.csv(vif_tbl, row.names = FALSE,
    file = file.path(outdir, paste0(gsub("[^A-Za-z0-9_]", "_", label), "_vif.csv")))

  invisible(vif_tbl)
}

# End helpers ----
