# Every model taught in the course, in one place.
#
# Each entry has:
#   pkgs: packages it needs (the test is skipped when one is missing)
#   tol:  relative tolerance when comparing to the reference values
#         (closed-form / IRLS fits get 1e-6; numerical ML optimisers get more slack)
#   fit:  a function that estimates the model the same way the lecture script
#         does, and returns the numbers worth guarding as a named list
#
# data-raw/make_reference.R runs these to create the reference files,
# tests/testthat/test-models.R runs them again and compares.

loglik <- function(m) as.numeric(logLik(m))  # unqualified: VGAM objects need VGAM's method

prep_contests <- function() {
  contests <- cm_data("cm_lecture5_contest")
  colnames(contests)[1] <- "id"
  contests$type <- factor(contests$type,
                          labels = c("Logo", "Website", "Design(print,product)", "Other"))
  contests$type.f <- factor(contests$type)
  contests
}

prep_flyer <- function() {
  flyer <- cm_data("cm_lecture5_flyer")
  colnames(flyer)[1] <- "Customer_ID"
  flyer$House.f <- as.factor(flyer$House)
  flyer
}

prep_churn <- function() {
  df <- cm_data("churn_data")
  for (i in 6:length(colnames(df))) df[, i] <- as.factor(df[, i])
  df
}

f_star5 <- num_star_rating_5 ~ factor(type) + num_awards + award_assured + award_amount_USD
f_creatives <- num_creatives ~ type.f + num_awards + award_assured + award_amount_USD
f_star5_pos <- num_star_rating_5 ~ type.f + num_awards + award_assured + award_amount_USD
f_hurdle <- num_star_rating_5 ~ factor(type) + num_awards + award_assured + award_amount_USD |
  factor(type) + num_awards + award_assured + award_amount_USD

cm_reference_models <- list(

  # ---- Lecture 2: logit / probit ------------------------------------------
  lecture2_logit = list(pkgs = character(), tol = 1e-6, fit = function() {
    d <- cm_data("telco_churn")
    m <- glm(churn ~ day.mins + intl.calls + voice.plan, family = binomial("logit"), data = d)
    hit <- mean((predict(m, type = "response") > 0.5) == d$churn)
    list(coef = coef(m), loglik = loglik(m), hitrate = hit)
  }),

  lecture2_probit = list(pkgs = character(), tol = 1e-6, fit = function() {
    d <- cm_data("telco_churn")
    m <- glm(churn ~ day.mins + intl.calls + voice.plan, family = binomial("probit"), data = d)
    list(coef = coef(m), loglik = loglik(m))
  }),

  lecture2_mfx = list(pkgs = "mfx", tol = 1e-6, fit = function() {
    d <- cm_data("telco_churn")
    lo <- mfx::logitmfx(churn ~ day.mins + intl.calls + voice.plan, data = d)
    pr <- mfx::probitmfx(churn ~ day.mins + intl.calls + voice.plan, data = d)
    list(logit_mfx = lo$mfxest[, "dF/dx"], probit_mfx = pr$mfxest[, "dF/dx"])
  }),

  # ---- Lecture 3: multinomial logit ---------------------------------------
  lecture3_mlogit = list(pkgs = "mlogit", tol = 1e-5, fit = function() {
    d <- cm_data("cm_lecture3")
    d$programmename <- as.factor(d$programmename)
    dm <- mlogit::mlogit.data(d, choice = "programmename", shape = "wide")
    m1 <- mlogit::mlogit(programmename ~ 0 | marketing + math + leadership + reading.writing,
                         data = dm, reflevel = "history_pr")
    m2 <- mlogit::mlogit(programmename ~ 0 | math, data = dm, reflevel = "history_pr")
    hit <- sum(max.col(m1$probabilities) == d$programme) / NROW(d)
    list(coef = coef(m1), loglik = loglik(m1), loglik_small = loglik(m2),
         mem_math = stats::effects(m1, covariate = "math"), hitrate = hit)
  }),

  # ---- Lecture 4: censoring, Tobit, two-part, Heckman ---------------------
  lecture4_ols_tobit = list(pkgs = "AER", tol = 1e-5, fit = function() {
    d <- cm_data("movie_ticket_data")
    m0 <- lm(tickets_sold ~ ticket_price + popularity + as.factor(weekday), data = d)
    m1 <- AER::tobit(tickets_sold ~ ticket_price + popularity + as.factor(weekday),
                     data = d, right = 500)
    list(ols_coef = coef(m0), tobit_coef = coef(m1), tobit_log_scale = log(m1$scale),
         tobit_loglik = loglik(m1))
  }),

  lecture4_twopart = list(pkgs = character(), tol = 1e-6, fit = function() {
    d <- cm_data("donation_data")
    d$donated <- ifelse(d$GIFT > 0, 1, 0)
    amount <- lm(log(GIFT) ~ GIFTLASTRESP + AVERAGEGIFT + MAILSPERYEAR, data = d,
                 subset = donated == 1)
    donated <- glm(donated ~ MAILSPERYEAR, data = d, family = binomial("logit"))
    list(amount_coef = coef(amount), donated_coef = coef(donated))
  }),

  lecture4_heckman = list(pkgs = "sampleSelection", tol = 1e-4, fit = function() {
    d <- cm_data("donation_data")
    d$donated <- ifelse(d$GIFT > 0, 1, 0)
    m3 <- sampleSelection::selection(
      donated ~ RESPLASTMAIL + WEEKSLASTRESP + MAILSPERYEAR + PROPRESPONSE + AVERAGEGIFT,
      GIFT ~ RESPLASTMAIL + WEEKSLASTRESP + MAILSPERYEAR + PROPRESPONSE + AVERAGEGIFT,
      data = d)
    list(coef = coef(m3), loglik = loglik(m3))
  }),

  # ---- Lecture 5: count models --------------------------------------------
  lecture5_poisson_flyer = list(pkgs = character(), tol = 1e-6, fit = function() {
    flyer <- prep_flyer()
    m1 <- glm(NumbItems ~ Flyer + House.f + Gender + Age, data = flyer, family = "poisson")
    newdat <- data.frame(House.f = "1", Age = 16, Gender = 0, Flyer = 0)
    list(coef = coef(m1), loglik = loglik(m1),
         pred_new = unname(predict(m1, newdata = newdat, type = "response")))
  }),

  lecture5_poisson_nb = list(pkgs = c("AER", "MASS"), tol = 1e-5, fit = function() {
    contests <- prep_contests()
    cm1 <- glm(f_creatives, data = contests, family = "poisson")
    disp <- AER::dispersiontest(cm1, trafo = 2)
    cm2 <- MASS::glm.nb(f_creatives, data = contests)
    list(poisson_coef = coef(cm1), dispersion_stat = unname(disp$statistic),
         nb_coef = coef(cm2), nb_theta = cm2$theta, nb_loglik = loglik(cm2))
  }),

  lecture5_vgam = list(pkgs = "VGAM", tol = 1e-4, fit = function() {
    contests <- prep_contests()
    contestspos <- subset(contests, num_star_rating_5 > 0)
    trunc <- VGAM::vglm(f_star5_pos, data = contestspos, family = VGAM::posnegbinomial())
    zip  <- VGAM::vglm(f_star5, family = VGAM::zipoisson(), data = contests)
    zinb <- VGAM::vglm(f_star5, family = VGAM::zinegbinomial(), data = contests)
    zap  <- VGAM::vglm(f_star5, family = VGAM::zapoisson(), data = contests)
    zanb <- VGAM::vglm(f_star5, family = VGAM::zanegbinomial(), data = contests)
    list(truncnb_coef = coef(trunc),
         zip_coef = coef(zip),   zip_loglik = loglik(zip),
         zinb_coef = coef(zinb), zinb_loglik = loglik(zinb),
         zap_coef = coef(zap),   zap_loglik = loglik(zap),
         zanb_coef = coef(zanb), zanb_loglik = loglik(zanb))
  }),

 
  # ---- Lecture 6: survival --------------------------------------------------
  lecture6_survival = list(pkgs = "survival", tol = 1e-6, fit = function() {
    df <- prep_churn()
    km <- survival::survfit(survival::Surv(week, churn) ~ 1, data = df)
    km_at <- summary(km, times = c(13, 26, 52), extend = TRUE)$surv
    lr <- survival::survdiff(survival::Surv(week, churn) ~ mails, data = df, rho = 0)
    cox1 <- survival::coxph(survival::Surv(week, churn) ~ mails + internet + age + educ,
                            method = "efron", data = df)
    zph <- survival::cox.zph(cox1, global = TRUE)
    agecat <- cut(df$age, breaks = c(0, 20, 25, Inf), labels = c("-20", "20-25", "25+"))
    cox3 <- survival::coxph(survival::Surv(week, churn) ~ mails + internet + educ +
                              strata(agecat), method = "efron", data = df)
    cox5 <- survival::coxph(survival::Surv(week, churn) ~ mails + internet + age + tt(age) + educ,
                            method = "efron", tt = function(x, t, ...) x * log(t), data = df)
    list(km_surv_13_26_52 = km_at, logrank_chisq = lr$chisq,
         cox1_coef = coef(cox1), cox1_loglik = loglik(cox1),
         zph_global_chisq = unname(zph$table["GLOBAL", "chisq"]),
         cox3_coef = coef(cox3), cox5_coef = coef(cox5))
  })
)
