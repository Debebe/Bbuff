

keep <- grep("rslt", names(D), value = TRUE)
keep <- c("who_region","iso3", "iter", "Pop", unique(keep))

output_table <- D[, ..keep]
output_table <- melt(output_table, id = c("iter", "iso3","who_region", "Pop"))
output_table[, type := ifelse(grepl("cf", variable), "cf", "sq")]
output_table[, variable := gsub("rlst_", "", variable)]
output_table[, variable := gsub("_cf|_sq", "", variable)]
output_table[, value := value * Pop]
output_table <- dcast(output_table,
                      iter + iso3 + variable + who_region~ type,
                      value.var = "value")


## averted
output_table[, av := cf - sq]

## global TODO NaNs?
output_table <- output_table[is.finite(av), .(
  cf = sum(cf), sq = sum(sq), av = sum(av)),
by = .(iter, variable, who_region)]

## hi/lo & reshape
eps <- 0.025
output_table <- melt(output_table,
                     id = c("iter","who_region", "variable")
) # TODO better var names

output_table <- output_table[, .(
  mid = mean(value), lo = quantile(value, eps), hi = quantile(value, 1 - eps)
), by = .(who_region,variable, variable.1)]

output_table <- dcast(output_table,
                      who_region+variable ~ variable.1,
                      value.var = c("mid", "lo", "hi"))

## change units to millions for cost and health, BCG dosese
fac <- 1e6
output_table[
  variable %in% c("rslt_att_cost", "rslt_cost",
                  "rslt_bcg_doses", "rslt_health"),
  `:=`(
    mid_sq = mid_sq / fac, lo_sq = lo_sq / fac, hi_sq = hi_sq / fac,
    mid_cf = mid_cf / fac, lo_cf = lo_cf / fac, hi_cf = hi_cf / fac,
    mid_av = mid_av / fac, lo_av = lo_av / fac, hi_av = hi_av / fac
  )
]


## format numbers, add brackets
output_table[
  ,
  c("sq_txt", "cf_txt", "av_txt") := .(
    brkt(mid_sq, lo_sq, hi_sq),
    brkt(mid_cf, lo_cf, hi_cf),
    brkt(mid_av, lo_av, hi_av)
  )
]
output_table

fwrite(output_table, file = here("outputs/output_table_who.csv"))
