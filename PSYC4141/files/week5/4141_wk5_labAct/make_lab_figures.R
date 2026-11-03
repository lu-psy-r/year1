# Figures and data for the lab questions on outliers and bivariate normality.
# Base R only. Run from this folder: source("make_lab_figures.R")
# Writes PNGs to figures/ and the simulated datasets to data/.

col_bar <- "grey80"
col_pt  <- rgb(0, 0, 0, alpha = 0.55)

three_panel <- function(file, x, y, xlab, ylab) {
  png(file, width = 2700, height = 900, res = 200)
  par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1), mgp = c(2.6, 0.7, 0))
  hist(x, breaks = 15, col = col_bar, border = "white", main = "Histogram of X",
       xlab = xlab, ylab = "Frequency", cex.main = 1.2)
  hist(y, breaks = 15, col = col_bar, border = "white", main = "Histogram of Y",
       xlab = ylab, ylab = "Frequency", cex.main = 1.2)
  plot(x, y, pch = 16, cex = 1.3, col = col_pt, main = "Scatterplot",
       xlab = xlab, ylab = ylab, cex.main = 1.2)
  dev.off()
}

# ---- Question 1: spotting outliers ----
set.seed(2026)
n1 <- 40
# Revision hours: roughly normal, truncated to 7-23 hours (no floor pile-up)
revision <- round(qnorm(runif(n1, pnorm(7, 15, 4), pnorm(23, 15, 4)), 15, 4), 1)
mark     <- round(pmin(pmax(40 + 1.5 * revision + rnorm(n1, 0, 6), 0), 100))
# Two participants who do not fit the pattern, although neither score is extreme
# on its own
revision[7]  <- 5;  mark[7]  <- 88
revision[23] <- 25; mark[23] <- 35
q1 <- data.frame(id = 1:n1, revision_hours = revision, exam_mark = mark)
write.csv(q1, "data/q1_revision_exam.csv", row.names = FALSE)
three_panel("figures/fig1_outliers.png", q1$revision_hours, q1$exam_mark,
            "Weekly revision (hours)", "Exam mark (%)")
cat("Q1 r (all):", round(cor(q1$revision_hours, q1$exam_mark), 2),
    " r (without participants 7 and 23):",
    round(cor(q1$revision_hours[-c(7, 23)], q1$exam_mark[-c(7, 23)]), 2), "\n")

# ---- Question 2: curved relationship ----
set.seed(27)   # seed chosen so that the sample r is close to zero
n2 <- 60
arousal <- round(pmin(pmax(rnorm(n2, 50, 18), 5), 100))
perf    <- round(pmin(pmax(85 - 0.025 * (arousal - 50)^2 + rnorm(n2, 0, 5), 0), 100))
q2 <- data.frame(id = 1:n2, arousal = arousal, performance = perf)
write.csv(q2, "data/q2_arousal_performance.csv", row.names = FALSE)
three_panel("figures/fig2_curved.png", q2$arousal, q2$performance,
            "Arousal score (0-100)", "Task performance (%)")
cat("Q2 r:", round(cor(q2$arousal, q2$performance), 2),
    " rho:", round(cor(q2$arousal, q2$performance, method = "spearman"), 2), "\n")

# ---- Question 3: unequal spread (fan shape) ----
set.seed(33)
n3 <- 80
# Age: roughly normal, truncated to 6-16 years (no pile-up at the limits)
age   <- round(qnorm(runif(n3, pnorm(6, 11, 2.5), pnorm(16, 11, 2.5)), 11, 2.5), 1)
# Vocabulary rises with age, and the spread grows in proportion to the mean
vocab <- round(pmax(450 * (age - 4) + rnorm(n3, 0, 60 * (age - 4)), 0))
q3 <- data.frame(id = 1:n3, age = age, vocabulary = vocab)
write.csv(q3, "data/q3_age_vocabulary.csv", row.names = FALSE)
three_panel("figures/fig3_fan.png", q3$age, q3$vocabulary,
            "Age (years)", "Vocabulary size (words)")
cat("Q3 r:", round(cor(q3$age, q3$vocabulary), 2),
    " rho:", round(cor(q3$age, q3$vocabulary, method = "spearman"), 2), "\n")

# ---- Question 4: skewed variables, and the same data on a log scale ----
set.seed(44)
n4 <- 80
rt    <- round(150 + rlnorm(n4, log(250), 0.6))
hours <- round(rlnorm(n4, log(10), 1.0), 1)
q4 <- data.frame(id = 1:n4, reaction_time = rt, social_media_hours = hours)
write.csv(q4, "data/q4_rt_socialmedia.csv", row.names = FALSE)
three_panel("figures/fig4a_skewed.png", q4$reaction_time, q4$social_media_hours,
            "Reaction time (ms)", "Social media use (hours/week)")
three_panel("figures/fig4b_logged.png", log(q4$reaction_time), log(q4$social_media_hours),
            "log(Reaction time)", "log(Social media use)")
cat("Q4 r (raw):", round(cor(q4$reaction_time, q4$social_media_hours), 2),
    " r (logged):", round(cor(log(q4$reaction_time), log(q4$social_media_hours)), 2),
    " rho:", round(cor(q4$reaction_time, q4$social_media_hours, method = "spearman"), 2), "\n")
