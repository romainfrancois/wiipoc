# Generated by jupyter.widgets.generate::generate_dom_widget("Button"): do not edit by hand

#' Button widget
#'
#' @export
jupyter.widget.Button <- R6::R6Class("jupyter.widget.Button", inherit = jupyter.widget.DOMWidget,
  public = list(

    #' @param button_style Use a predefined styling for the button.
    #' @param description Button label.
    #' @param disabled Enable or disable user changes.
    #' @param icon Font-awesome icon names, without the 'fa-' prefix.
    #' @param style Must inherit from [jupyter.widget.ButtonStyle].
    #'
    #' @param ... See [jupyter.widgets.base::DOMWidget]
    #' @param error_call see [rlang::args_error_context()]
    #'
    #' @return a new 'jupyter.widget.Button' widget
    initialize = function(
      button_style = "",
      description = "",
      disabled = FALSE,
      icon = "",
      style = ButtonStyle(),

      ...,
      error_call = caller_env()
    )
    {
      private$state_ <- update_list(private$state_,
        button_style = self$check_state('button_style', button_style),
        description = self$check_state('description', description),
        disabled = self$check_state('disabled', disabled),
        icon = self$check_state('icon', icon)
      )

      super$initialize(
        `_model_module` = "@jupyter-widgets/controls",
        `_model_name`   = "ButtonModel",
        `_view_module`  = "@jupyter-widgets/controls",
        `_view_name`    = "ButtonView",
        style = style, 
        ...,
        error_call = error_call
      )

    }
  ),

  active = list(
    
    #' @field button_style
    #' Use a predefined styling for the button.
    button_style = function(x) if(missing(x)) private$state_[['button_style']] else self$update(button_style = self$check_state('button_style', x)),
    
    #' @field description
    #' Button label.
    description = function(x) if(missing(x)) private$state_[['description']] else self$update(description = self$check_state('description', x)),
    
    #' @field disabled
    #' Enable or disable user changes.
    disabled = function(x) if(missing(x)) private$state_[['disabled']] else self$update(disabled = self$check_state('disabled', x)),
    
    #' @field icon
    #' Font-awesome icon names, without the 'fa-' prefix.
    icon = function(x) if(missing(x)) private$state_[['icon']] else self$update(icon = self$check_state('icon', x))
  )
)

#' Button widget
#'
#' @param button_style Use a predefined styling for the button.
#' @param description Button label.
#' @param disabled Enable or disable user changes.
#' @param icon Font-awesome icon names, without the 'fa-' prefix.
#' 
#' @param style Must inherit from [jupyter.widget.ButtonStyle].
#' 
#' @param ... forwarded to [jupyter.widgets.base::jupyter.widget.DOMWidget] constructor
#' @inheritParams rlang::args_error_context
#'
#' @export
Button <- function(
  button_style = "",
  description = "",
  disabled = FALSE,
  icon = "",
  style = ButtonStyle(),
  ...,
  error_call = current_env()
){
  jupyter.widget.Button$new(
    button_style = button_style,
    description = description,
    disabled = disabled,
    icon = icon,
    style = style,
    ...,
    error_call = error_call
  )
}
