## Just make a copy of session info:
sink("./../session_info.txt", append = FALSE, split = FALSE)
cat(sprintf("Date/Time of Info: %s\n", Sys.time()))

cat("\n\ndevtools::session_info()\n")
devtools::session_info()

cat("\n\n\nsessionInfo()\n")
sessionInfo()
sink()
