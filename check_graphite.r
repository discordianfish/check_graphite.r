#!/usr/bin/env Rscript

FREQUENCY = 6 # per day

args = commandArgs(trailingOnly = TRUE)

graphite = args[1]
target = args[2]
months = args[3]
conditions = args[-(0:3)]

month = as.numeric(months)

csv = c(NA,NA,NA)
while (month >= 0) {
  url = sprintf("http://%s/render/?target=%s&format=csv&from=-%imonths&until=-%imonths", graphite, target, month+1, month)

  new = read.table(url, sep = ",")
  csv = rbind(csv, new)
  month = month - 1
}

exit = function(state, message)
{
  cat(paste(state, message, sep = ""))
  quit("no", switch(state, "CRITICAL" = 2, "WARNING" = 1, "OK" = 0))
}

metrics = as.numeric(csv[, 3])
metrics = metrics[is.finite(metrics)]
metrics = ts(metrics, start = 0, frequency = FREQUENCY) # in days

fit = HoltWinters(metrics)

#pahead = 30 * FREQUENCY
#X11()
#prediction = predict(fit,n.ahead=pahead)
#plot(metrics, xlim=c(0,(length(metrics)+pahead)/6), ylim=c(0,max(prediction, metrics)))
#lines(prediction, col=2)
#message("Press Return To Continue"); invisible(readLines("stdin", n=1))
# quit("no",0)

warning = FALSE
for (cp in conditions)
{
  pair = strsplit(cp,',')
  state = "CRITICAL"
  for (condition in rev(unlist(pair)))
  {
    op = substring(condition,0,1)
    rem = substring(condition,2)

    vars = strsplit(rem,"\\*")[[1]]
    threshold = as.numeric(vars[1])
    predict = vars[2]

    message = ""
    hit = FALSE

    if (is.na(predict)) {
      latest = tail(metrics,1)
      message = sprintf(": %s = %s; %s %s\n", target, latest, op, threshold)

      if (op == '>') {
        if (latest > threshold) {
          hit = TRUE
        }
      } else {
        if (latest < threshold) {
          hit = TRUE
        }
      }
    } else {
      prediction = predict(fit,n.ahead=as.numeric(predict) * FREQUENCY)
      predicted = tail(prediction,1)
      if (op == '>') {
        if (predicted > threshold) {
          hit = TRUE
        }
      } else {
        if (predicted < threshold) {
          hit = TRUE
        }
      }
      message = sprintf(": %s(%s periods) = ~%s; %s %s\n", target, predict, predicted, op, threshold)
    }

    if (hit)
    {
      if (state == "CRITICAL")
      {
        exit(state,message)
      } else
      {
        warning = message
      }
    }
    state = "WARNING"
  }
}

if (warning == FALSE) {
  exit("OK", sprintf(": %s = %s, %s %s\n", target, tail(metrics,1), op, threshold))
} else
{
  exit("WARNING", message)
}
