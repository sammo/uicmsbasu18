# For this simple example we are not using a model.  But to show the need for it
#  to be global, we will load it into this environment.
MODEL = load('/work/api/models/model.rds')

# =============================================================================#
#                                 Predict Functions
# =============================================================================#
ConfirmRawDataIsValid = function(args) {
  # This function validates that the values given in the client's json (args)
  #  are what we expect.
  #
  # Args:
  #   args (list): list of json values POSTed by client
  #
  # Returns:
  #   output (list): list with success logical, error message, and our data.

  # Set up the output list.  It is nice to use a list with success and error so
  #  you can track this throughout your POST call and return all relevant info
  #  at once.
  output = list(
    success = TRUE,
    error   = NULL,
    data    = list()
  )

  # For this API, these are the values we expect.
  expectedJsonValues = c(
    'start_longitude',
    'start_latitude',
    'end_longitude',
    'end_latitude'
  )

  # Plumber automatically adds req and res, so we need to account for this.
  requiredJsonValues = append(expectedJsonValues, c('req', 'res'))
  givenJsonValues    = names(args)

  # This function ensures that the values given are EXACTLY what we expect, extra
  #  or missing values will be reported back to the client with an error.
  output$error = GetJsonValueError(requiredJsonValues, givenJsonValues)
  # Check if we have an error and return if so
  if (!is.null(output$error)) {
    output$success = FALSE
    return(output)
  }

  # Look over the values and do some endpoint specific validation
  for (value in expectedJsonValues) {
    # First, let's assert each value in a number (int or numeric)
    tempError = GetIsNumericError(value, args[[value]])
    if (!is.null(tempError)) {
      output$error = append(output$error, tempError)
      next
    }

    # Now, do specific checks on longitude and latitude
    if (grepl('longitude', value)) {
      tempError = GetIsValidLongitudeError(value, args[[value]])
      if (!is.null(tempError)) {
        output$error = append(output$error, tempError)
        next
      }
      output$data[[value]] = args[[value]]
    }
    if (grepl('latitude', value)) {
      tempError = GetIsValidLatitudeError(value, args[[value]])
      if (!is.null(tempError)) {
        output$error = append(output$error, tempError)
        next
      }
      output$data[[value]] = args[[value]]
    }
  }

  # Return the error if we failed
  if (!is.null(output$error)) {
    output$success = FALSE
    return(output)
  }

  # If we made it to the end, return the output (why list is nice)
  return(output)
}


PreprocessDF = function(data) {
  # This function does our preprocessing.  Remember this should be IDENTICAL to
  #  the preprocessing we did before we trained our model.
  #
  # Args:
  #   data (list): list of four long/lat values.
  #
  # Returns:
  #   output (list): list with success logical, error message, and our data.

  output = list(
    success = TRUE,
    error   = NULL,
    data    = list()
  )

  # Let's keep this simple, let's just round the data that was given.
  for (name in names(data)) {
    output$data[[name]] = round(data[[name]], 3)
  }

  # Return the output
  return(output)
}


MakePredictions = function(data) {
  # This is where we would take the preprocessed data and make predictions.  For
  #  simplicity of this example, we will simple just return a prediction.
  #
  # Args:
  #   data (list): list of four rounded long/lat values.
  #
  # Returns:
  #   output (list): list with success logical, error message, and our pred

  output = list(
    success = TRUE,
    error   = NULL,
    pred    = NULL
  )

  # Let's add up all of our values and base the pred on that sum
  sum = 0
  for (value in data) {
    sum = sum + value
  }

  # Make the prediciton
  if (sum > 250) {
    output$pred = '$10.00'
  } else {
    output$pred = '$8.88'
  }

  # Return
  return(output)
}


PackageOutput = function(pred) {
  # This is where we take our prediction(s) and format that the way we have
  #  previously specified to the user.
  #
  # Args:
  #   pred (string): our single prediction string.
  #
  # Returns:
  #   output (list): list with success logical, error message, and our json list

  # This nested list structure of json will easily convert to json when plumber
  #  returns it.
  output = list(
    success = TRUE,
    error   = NULL,
    json    = list(
      date='today',
      service='UBER API',
      fare=pred
    )
  )

  # Return
  return(output)
}

# =============================================================================#
#                                 Utility Functions
# =============================================================================#
GetJsonValueError = function(requiredJsonValues, givenJsonValues) {
  # This function takes in a list of the json values provided and list of the
  #  json values expected.  If these are not identical, we return a meaningful
  #  error message that states which features are extra and/or missing.
  #
  # Args:
  #  requiredJsonValues (character): list of json values required by the API
  #  givenJsonValues    (character): list of json values given by the client
  #
  # Returns:
  #  error (string): the error string (NULL if there is not one)

  # Setup a NULL message.  This is nice because we can append to it and also
  #  no error would be is.null(error)
  error = NULL

  # If these lists are not equal, check why
  if (!setequal(requiredJsonValues, givenJsonValues)) {
      # Check if there is a missing json value(s) and give appropriate error
      missing = setdiff(requiredJsonValues, givenJsonValues)
      if (length(missing) > 0) {
          for (value in missing) {
              error = append(error,
                             paste0('JSON Error: Required value "',
                                    value,
                                    '" is missing.'))
          }
      }
      # Check if there is an extra json value(s) and give appropriate error
      extra = setdiff(givenJsonValues, requiredJsonValues)
      if (length(extra) > 0) {
          for (value in extra) {
              error = append(error,
                             paste0('JSON Error: Extra value "',
                                    value,
                                    '" was given.'))
          }
      }
  }

  # Return
  return(error)
}


GetIsNumericError = function(name, value) {
  # Check if the value with name name is numeric (or int)
  #
  # Args:
  #   name   (string): json value name
  #   value (unknown): json value
  #
  # Returns:
  #  error (string): the error string (NULL if there is not one)

  # Setup a NULL message.  This is nice because we can append to it and also
  #  no error would be is.null(error)
  error = NULL

  # If not int or numeric, create an error
  if (!(class(value) %in% c('integer', 'numeric'))) {
    error = paste(name, 'is not numeric!')
  }

  # Return
  return(error)
}


GetIsValidLongitudeError = function(name, longitude) {
  # Check if the longitude is valid
  #
  # Args:
  #   name       (string): json value name
  #   longitude (numeric): longitude value
  #
  # Returns:
  #  error (string): the error string (NULL if there is not one)

  # Setup a NULL message.  This is nice because we can append to it and also
  #  no error would be is.null(error)
  error = NULL

  # Assert value is within appropriate range
  if (longitude < -180 | longitude > 180) {
    error = paste(name, 'is not in range [-180, 180]!')
  }

  # Return
  return(error)
}


GetIsValidLatitudeError = function(name, latitude) {
  # Check if the latitude is valid
  #
  # Args:
  #   name      (string): json value name
  #   latitude (numeric): latitude value
  #
  # Returns:
  #  error (string): the error string (NULL if there is not one)

  # Setup a NULL message.  This is nice because we can append to it and also
  #  no error would be is.null(error)
  error = NULL

  # Assert value is within appropriate range
  if (latitude < -90 | latitude > 90) {
    error = paste(name, 'is not in range [-90, 90]!')
  }

  # Return
  return(error)
}
