rm(list = ls())

# load("item_clusters_heatmap/items_domain3_apar.Rdata")
load("analyse_validation/items.Rdata")

# 1. a x b: a | b = 0 | 1
# 2. a x b: a | b = 10 | 100 | 1000
# 3. a x b: a | b = 2:9

head(items$question)

# grep("(^0 x.)|(.x 0$)",                    items$question, value = T)
# grep("(^(100|1000) x .)|(.x (100|1000)$)", items$question, value = T)
# grep("[2-9] x [2-9]",                      items$question, value = T)

# Check if regular expressions are correct
grep('(\\\"(0|1) x.)|(.x (0|1)\\\")',                                     items$question, value = T) # With fractions
grep('(\\\"(10|100|1000) x .)|(.x (10|100|1000)\\\")',                    items$question, value = T) # With fractions
grep('(\\\"(0|1) x [0-9]+\\\")|(\\\"[0-9]+ x (0|1)\\\")',                 items$question, value = T) # Without fractions
grep('(\\\"(10|100|1000) x [0-9]+\\\")|(\\\"[0-9]+ x (10|100|1000)\\\")', items$question, value = T) # Without fractions
grep('\\\"[2-9] x [2-9]\\\"',                                             items$question, value = T)

# TODO(Sharon): Download all items and id's from oefenweb. Apply grep to all items.

# Assign row number to variable
zero.one     <- grep('(\\\"(0|1) x [0-9]+\\\")|(\\\"[0-9]+ x (0|1)\\\")',                 items$question) # Without fractions
powers       <- grep('(\\\"(10|100|1000) x [0-9]+\\\")|(\\\"[0-9]+ x (10|100|1000)\\\")', items$question) # Without fractions
single.digit <- grep('\\\"[2-9] x [2-9]\\\"',                                             items$question)

# Assign item_id to variable
items.zero.one     <- items[zero.one,     "id"]
items.powers       <- items[powers,       "id"]
items.single.digit <- items[single.digit, "id"]

# Load a-parameter estimations form real data
load("a-parameter_real_data/7_merge_estimations/all_estimations.Rdata")

# Create new columns for special operands and assign true or false
all.estimations$zero.one <- 0
all.estimations[all.estimations$item_id %in% items.zero.one, 'zero.one'] = 1

all.estimations$powers <- 0
all.estimations[all.estimations$item_id %in% items.powers, 'powers'] = 1

all.estimations$single.digit <- 0
all.estimations[all.estimations$item_id %in% items.single.digit, 'single.digit'] = 1

special.operands <- apply(all.estimations[, c('zero.one', 'powers', 'single.digit')], 1, sum)

all.estimations$special.operands <- ifelse(special.operands > 0, 1, 0)

# Save results
save(all.estimations, file = "analyse_validation/all_estimations_special_operands.Rdata")
