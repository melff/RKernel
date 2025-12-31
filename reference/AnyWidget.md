# A constructor function for a class that inherits from 'AnyWidget' (or 'AnyWidgetClass')

A constructor function for a class that inherits from 'AnyWidget' (or
'AnyWidgetClass')

## Usage

``` r
AnyWidget(esm, css, anywidget_id, ...)
```

## Arguments

- esm:

  Character string, code for an ECMAscript (Javascript) moldue

- css:

  Character string with CSS code

- anywidget_id:

  Character string with a unique ID for the widget

- ...:

  Other arguments which create additional components of the widget

## Examples

``` r
if (FALSE) { # \dontrun{ 
  esm <- '
  function render({ model, el }) {
        let button = document.createElement("button");
        button.innerHTML = `count is ${model.get("value")}`;
        button.addEventListener("click", () => {
          model.set("value", model.get("value") + 1);
          model.save_changes();
        });
        model.on("change:value", () => {
          button.innerHTML = `count is ${model.get("value")}`;
        });
        el.classList.add("counter-widget");
        el.appendChild(button);
      }
      export default { render };
  '
  css <- '
      .counter-widget button { 
                              color: white; 
                              font-size: 1.75rem; 
                              background-color: #ea580c; 
                              padding: 0.5rem 1rem; 
                              border: none; 
                              border-radius: 0.25rem; 
      }
      .counter-widget button:hover { background-color: #9a3412; }
  '
  CountWidget <- AnyWidget(
              esm = esm,
              css = css,
              anywidget_id = "CountWidget",
              value = Integer(0)
             )
  CountWidget(value=42)
} # }
```
