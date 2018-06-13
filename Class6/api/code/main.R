# Main API file.  Set's up app and routing.

# Load required packages
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(plumber))
suppressPackageStartupMessages(library(readr))

# Source the config file.  This is important for a few reasons, mainly so we can
#  abstract away certain parameters from the main.R file so they can be easily
#  swapped in and out by managing another script.  For security, we could also
#  not check config.R into our repository incase it uses sensitive information
#  such as passwords or tokens.
source('utils/config.R')

# We are going to create an environment for each endpoint.  In this simple
#  example we are going to have two endpoints (/help and /predict).  /help
#  for this simple API will not require an env as it has no functions or vars.
Predict = new.env()
sys.source(file='utils/predict.R', envir=Predict)

# =============================================================================#
#                               Make Router
# =============================================================================#

# Instantiate router.
router = plumber$new()

# Create hooks
#  These hooks allows us to do things such as logging before and after a
#  request has made it through our API.  For this example we will keep it simple
router$registerHooks(list(
    'preroute' = function(data, req, res) {
      print('lets log this request on the way in')
    },
    'postroute' = function(data, req, res, value) {
      print('lets log this request on the way out')
      return(value)
    }
))

# Help endpoint
#  This is the '/help' endpoint.  We are setting this as a 'GET' endpoint
#  because we are not expecting the user to give us any data.  This simple
#  example returns a message to the user.
router$handle('GET', '/help', function(req, res) {
  res$status = 200
  return(list(message='no one can help you'))
})

# Predict endpoint
#  This is the '/predict' endpoint.  This is a 'POST' because we are expecting
#  data (longitude and latitude coordinates) to be given to the API.  We are
#  also expecting an authorization token to make sure we know the user.  This
#  endpoint ensures the posted values are valid, manipulates them, makes a fare
#  prediction, and returns the output formatted as the user expects.
router$handle('POST', '/predict', function(req, res) {
  # First lets make sure the header of the request (req$HTTP_ has all headers)
  #  has the authtoken we are expecting (in the config)
  if (is.null(req$HTTP_AUTHTOKEN) || req$HTTP_AUTHTOKEN != hostStaticToken) {
    res$status = 403
    return(list(error='Invalid Authentication Token!'))
  }

  # Let's send the request (req$args has the values that came in on the json) to
  #  our validation function to ensure the data is valid.
  DataValidationResults = Predict$ConfirmRawDataIsValid(req$args)
  # If this step failed, we return 400 (client error)
  if (DataValidationResults$success == FALSE) {
    res$status = 400
    return(list(error=DataValidationResults$error))
  }

  # If successful, we pass the data into our preprocessing function.
  PreprocessDFResults = Predict$PreprocessDF(DataValidationResults$data)
  # For this example, failing here would be an error on our part, so we return
  #  500 (server error).  Depending on the preprocessing requirements this could
  #  also be a 400, but let's keep this simple.
  if (PreprocessDFResults$success == FALSE) {
    res$status = 500
    return(list(error=DataValidationResults$error))
  }

  # If successful, let's send the data to our prediciton function.
  MakePredictionsResults = Predict$MakePredictions(PreprocessDFResults$data)
  # If this fails, this must be a 500 (server error)
  if (MakePredictionsResults$success == FALSE) {
    res$status = 500
    return(list(error=MakePredictionsResults$error))
  }

  # Now that we have our fare prediction, let's format the json the way the user
  #  is expecting.
  PackageOutputResults = Predict$PackageOutput(MakePredictionsResults$pred)
  # If this fails, this must be a 500 (server error)
  if (PackageOutputResults$success == FALSE) {
    res$status = 500
    return(list(error=PackageOutputResults$error))
  }

  # If we made it this far, we can confirm success (200) and return our json
  #  (which plumber with format for us!)
  res$status = 200
  return(PackageOutputResults$json)
})

# =============================================================================#
#                                 Run Router
# =============================================================================#
# Host and port come from our config file.  See above comments on why.
router$run(host=host, port=port)
