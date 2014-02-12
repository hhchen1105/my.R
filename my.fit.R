lm_eqn = function(x, y, logx=F, logy=F, labels=c("x", "y")){
  dx = ifelse(rep(logx, length(x)), log10(x), x)
  dy = ifelse(rep(logy, length(y)), log10(y), y)
  m = lm(dy ~ dx)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
      list(x = labels[1],
           y = labels[2],
           a = format(coef(m)[1], digits = 2),
           b = format(coef(m)[2], digits = 2),
           r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)))
}
