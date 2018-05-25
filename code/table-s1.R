lib_eval("survey")

individuals <- readRDS("../data/rdata/individuals.RDS")
hh_main <- readRDS("../data/rdata/hh_main.RDS")
weights <- readRDS("../data/rdata/final_weights.RDS")

weights$sel_strata_w <- weights$barrio_w/13 # weight for each barrio

hh_main <- left_join(hh_main, weights, by = c("id", "strata")) 
hh_main$count <- 1

# create single final weight for each household
hh_main$hh_w_f <- as.numeric(hh_main$hh_w) * as.numeric(hh_main$sel_strata_w)

# add data on age and gender of indviduals
ind_main <- left_join(individuals, hh_main, by = "hh_id")

# create survey design object
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy <- svydesign(id = id.form, weights = wt.form, data = hh_main, nest = TRUE)


# create survey design object
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy2 <- svydesign(id = id.form, weights = wt.form, data = ind_main, nest = TRUE)


#table S1

#population
pop.est <- svytotal(~hh_size, dsvy, na.rm = TRUE, vartype = "se")[1]
pop.se <- SE(svytotal(~hh_size, dsvy))

saveRDS(pop.est, "../data/rdata/pop_est.RDS")

#households
hh <- unlist(svytotal(~count, dsvy, na.rm = TRUE))[1]
hh.se <- SE(svytotal(~count, dsvy, na.rm = TRUE))

#age
median.age <- unlist(svyquantile(~age, dsvy2, quantiles = 0.5, na.rm=TRUE, se = TRUE))[1]

# proportion female
prop.female <- svymean(~gender, dsvy2, na.rm = TRUE)[2]*100
fem.se <- SE(svymean(~gender, dsvy2, na.rm = TRUE))[2]*100

# hh size
mean.hh <- unlist(svymean(~hh_size, dsvy)[1])
size.se <-SE(svymean(~hh_size, dsvy))

tableS1 <- as.data.frame(cbind(c("Households", "Population", "Median Age", "Proportion Female", "Mean Household Size"),
                    c(hh, pop.est, median.age, prop.female, mean.hh),
                    c(hh.se, pop.se, NA, fem.se, size.se)))
names(tableS1) <- c("Variable", "WeightedEst", "SE")
tableS1$lower <- as.numeric(as.character(tableS1$WeightedEst)) - 1.96*as.numeric(as.character(tableS1$SE))
tableS1$upper <- as.numeric(as.character(tableS1$WeightedEst)) + 1.96*as.numeric(as.character(tableS1$SE))

saveRDS(tableS1, "../data/rdata/tableS1.RDS")


