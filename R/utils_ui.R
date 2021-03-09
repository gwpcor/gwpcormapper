# Title     : gwpcorMapper UI Helper Functions
# Objective : Holds ui functions to be used by gwpcorMapper.
# Created by: Joseph Percival
# Created on: 2021/02/26


custom_loader <- function(idButton, labelButton, idText, labelText){
  tagList(
    div(style="display:inline-block",
        fluidRow(
          column(
            width = 4,
            actionButton(
              inputId = idButton,
              label = labelButton
            )
          ),
          column(
            width = 8,
            shinyjs::disabled(
              textInput(
                inputId = idText,
                label = labelText,
                placeholder = "   No file selected"
              )
            )
          )
        )
    ),
    conditionalPanel(
      condition = "input.loadedData != ''",
      div(
        class='progress-bar', 'Upload Complete',
        style="margin-left: 15px; margin-top: -20px; margin-bottom: 20px; padding: 0; width: 202px;"
      )
    ),
  )
}