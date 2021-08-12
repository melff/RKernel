# RKernel

This is a re-creation from scratch of an [*R*](http://www.r-project.org) kernel
for [*Jupyter*](http://juypter.org).

It differs from the already existing [*IRKernel*](http://irkernel.github.io) by:

  - a clearer separation between the kernel protocol layer and the *R*
    evaluation layer.
  - Ordinary console output appears via the "stdout" stream, which enables
    output colorized e.g. by
    [*crayon*](https://cran.r-project.org/package=crayon).
  - Rich HTML/LaTeX output is strictly optional and will be supported by a
    dedicated `display()` function (yet to be implemented).
  - Error conditions created by `stop()` will not lead to a notebook execution
    to be stopped. (In the future, users may toggle automatic notebook
    abortion.)
  - It is planned that graphics can be "updated", so that a single cell with
    e.g. `curve(gamma(x),add=TRUE)` works as expected (this is still a TODO). 
  - It is based on the [*R6*](https://cran.r-project.org/package=R6) package
    instead of S4 reference classes.

Overall, the aim is to make the interaction as close as possible to an
"ordinary" interaction with *R* over the console or *RStudio*.  
