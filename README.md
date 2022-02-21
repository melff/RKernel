# RKernel

This is a re-creation from scratch of an [*R*](http://www.r-project.org) kernel
for [*Jupyter*](http://juypter.org).

It differs from the already existing [*IRKernel*](http://irkernel.github.io) by:

  - Ordinary console output appears via the "stdout" stream, which enables
    output colorized e.g. by
    [*crayon*](https://cran.r-project.org/package=crayon).
  - Rich HTML/LaTeX output is strictly optional and is supported by a
    dedicated `display()` function.
  - Error conditions created by `stop()` will not lead necessarily to a stop 
    in the running of a Jupyter notebook. This can be controlled by the option 
    `rkernel_stop_on_error`.
  - Error and warning messages appear via the `stderr` stream, ordinary messages
    (created by `message()`) appear via the `stdout` stream. Consequently,
    "ordinary" messages (e.g. that appear while a package is loaded via
    `library()`) are less obtrusive, while error messages clearly stand out. 
  - Graphics can be "updated", so that e.g. `curve(gamma(x),add=TRUE)` 
    works as expected even when the 
    plot to which the curve is added was created by code in a previous cell. 
  - It is based on the [*R6*](https://cran.r-project.org/package=R6) package
    instead of S4 reference classes.
  - Dynamic help works, including `help.start() - thanks to [jupyter-server-proxy](https://pypi.org/project/jupyter-server-proxy/)
  - The polling loop allows for providing services e.g. with the "htmluv"
    package.
  - [HTML widgets](https://cran.r-project.org/?package) (partially) work. At the time of 
    writing, "rbokeh" widgets work flawless when included into a Jupyter notebook. 
    "plotly" and "d3plus" widgets only work with Firefox, but not with Google Chrome. 
    This appears to be a Javascript issue.
  - It is possible to make use of the Widgets infrastructure provided by
    [*ipywidgets*](https://ipywidgets.readthedocs.io) to constructed interactive
    user interfaces. These can be used with jupyter notebooks, JupyterLab and
    [*voilà*](https://voila.readthedocs.io).
  - There is also a support for virtual table display (based on the ipywidget
    infrastructure).
    
Some demonstration notebooks can be found [here](https://tmphub.elff.eu/user-redirect/).
    
A widget demonstration can be found [here](https://tmphub.elff.eu/user-redirect/voila/render/RKernel-demo-interact-logistic-regression.ipynb).
    
TODO:

  - [ ] Add documentation
  - [x] Make HTML help work on servers
  - [ ] Add example notebooks
  - [ ] Make HTML widgets work more generally
