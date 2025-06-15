cran <- findCRANmirror("web")
avpaks <- available.packages(contrib.url(cran))
if(!("httpgd" %in% rownames(avpaks))) {
  install.packages(
    'httpgd', 
    repos = c('https://nx10.r-universe.dev', 
              cran))
}
