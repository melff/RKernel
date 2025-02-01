#' Support for AnyWidgets
#' @export
AnyWidgetClass <- R6Class_("AnyWidget",
   inherit = DOMWidgetClass,
   public = list(
    #' @field _model_module Name of the Javascript module with the model
    `_model_module` = structure(Unicode("anywidget"),sync=TRUE),
    #' @field _model_module_version Version of the module where the model is defined
    `_model_module_version` = structure(Unicode(anywidget_version()),sync=TRUE),
    #' @field _model_name Name of the Javascript model in the frontend
    `_model_name` = structure(Unicode("AnyModel"),sync=TRUE),
    #' @field _view_name Name of the Javascript model view in the frontend
    `_view_name` = structure(Unicode("AnyView"),sync=TRUE),
    #' @field _view_module Name of the module where the view is defined
    `_view_module` = structure(Unicode("anywidget"),sync=TRUE),
    #' @field _view_module_version Version of the module where the view is defined
    `_view_module_version` = structure(Unicode(anywidget_version()),sync=TRUE),
    #' @field _anywidget_id A Unicode trait
    `_anywidget_id` = structure(Unicode(optional=TRUE),sync=TRUE),
    #' @field _css A Unicode trait with optional CSS code
    `_css` = structure(Unicode(optional=TRUE),sync=TRUE),
    #' @field _esm A Unicode trait with optional Javascript code
    `_esm` = structure(Unicode(optional=TRUE),sync=TRUE)
   )
)

#' A constructor function for a class that inherits from
#'     'AnyWidget' (or 'AnyWidgetClass') 
#' @param esm Character string, code for an ECMAscript (Javascript) moldue
#' @param css Character string with CSS code
#' @param anywidget_id Character string with a unique ID for the widget
#' @param ... Other arguments which create additional components of the widget
#' @examples
#' \dontrun{ 
#'   esm <- '
#'   function render({ model, el }) {
#'         let button = document.createElement("button");
#'         button.innerHTML = `count is ${model.get("value")}`;
#'         button.addEventListener("click", () => {
#'           model.set("value", model.get("value") + 1);
#'           model.save_changes();
#'         });
#'         model.on("change:value", () => {
#'           button.innerHTML = `count is ${model.get("value")}`;
#'         });
#'         el.classList.add("counter-widget");
#'         el.appendChild(button);
#'       }
#'       export default { render };
#'   '
#'   css <- '
#'       .counter-widget button { 
#'                               color: white; 
#'                               font-size: 1.75rem; 
#'                               background-color: #ea580c; 
#'                               padding: 0.5rem 1rem; 
#'                               border: none; 
#'                               border-radius: 0.25rem; 
#'       }
#'       .counter-widget button:hover { background-color: #9a3412; }
#'   '
#'   CountWidget <- AnyWidget(
#'               esm = esm,
#'               css = css,
#'               anywidget_id = "CountWidget",
#'               value = Integer(0)
#'              )
#'   CountWidget(value=42)
#' }
#' @export
AnyWidget <- function(esm, css, anywidget_id,...){
    args <- list(...)
    public <- list()
    for(n in names(args)){
        if(inherits(args[[n]], "TraitInstance"))
            public[[n]] <- structure(args[[n]],sync=TRUE)
        else
            public[[n]] <- args[[n]]
    }
    esm <- as.character(esm)
    public[["_esm"]] <- structure(Unicode(esm),sync=TRUE)
    anywidget_id <- as.character(anywidget_id)
    public[["_anywidget_id"]] <- structure(Unicode(anywidget_id),sync=TRUE)
    if(!missing(css))
        public[["_css"]] <- structure(Unicode(css),sync=TRUE)
    WidgetClass <- R6Class_(
        inherit = AnyWidgetClass,
        public = public
    )
    function(...) WidgetClass$new(...)
}