##mab i2
mab_ft <- mab %>% 
  filter(sex == "M")


# Behavior -------------------------------------------------------------
mod_mab <- rma.mv(yi, vi,
                  random = list(~1 | outcome_id, ~1 | exp_id),
                  mods   = ~domain:hit2Grouped - 1 ,
                  data = mab_ft)


W <- diag(1/mab_ft$vi)
X <- model.matrix(mod_mab)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
i2 <- 100 * sum(mod_mab$sigma2) / (sum(mod_mab$sigma2) + (mod_mab$k-mod_mab$p)/sum(diag(P)))
between_within <- 100 * mod_mab$sigma2 / (sum(mod_mab$sigma2) + (mod_mab$k-mod_mab$p)/sum(diag(P)))
