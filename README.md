# RKernel

This is a re-creation from scratch of an [*R*](http://www.r-project.org) kernel
for [*Jupyter*](http://juypter.org).

It differs from the already existing [*IRKernel*](http://irkernel.github.io) by:

  - Ordinary console output appears via the "stdout" stream, which enables
    output colorized e.g. by
    [*crayon*](https://cran.r-project.org/package=crayon).
  - Rich HTML/LaTeX output is strictly and is supported by a
    dedicated `display()` function (yet to be implemented).
  - Error conditions created by `stop()` will not lead necessarily to a stop 
    in the running of a Jupyter notebook.
  - Error and warning messages appear via the `stderr` stream, ordinary messages
    (created by `message()`) appear via the `stdout` stream. Consequently,
    "ordinary" messages (e.g. that appear while a package is loaded via
    `library()`) are less unobstrusive, while error messages clearly stand out. 
  - Graphics can be "updated", so that e.g. `curve(gamma(x),add=TRUE)` 
    works as expected even when the 
    plot to which the curve is added was created by code in a previous cell. 
  - It is based on the [*R6*](https://cran.r-project.org/package=R6) package
    instead of S4 reference classes.
  - Dynamic help works, including `help.start()`.
  - The polling loop allows for providing services e.g. with the "htmluv"
    package.
  - [HTML widgets](https://cran.r-project.org/?package) can be included in
    Jupyter notebooks and can interacted with even with Firefox.
  - It is possible to make use of the Widgets infrastructure provided by
    [*ipywidgets*](https://ipywidgets.readthedocs.io) to constructed interactive
    user interfaces. These can be used with jupyter notebooks, JupyterLab and
    [*voilà*](https://voila.readthedocs.io).
  - There is also a support for virtual table display (based on the ipywidget
    infrastructure).
    
TODO:

  - [ ] Add documentation
  - [ ] Add example notebooks
