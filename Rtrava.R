#Version 0.0.1
strava_oauth <- function(app_name, app_client_id, app_secret, app_scope = NULL) {
      
      strava_app <- oauth_app(app_name, app_client_id, app_secret)  
      
      oauth2.0_token(oauth_endpoint(
                request = "https://www.strava.com/oauth/authorize?",
                authorize = "https://www.strava.com/oauth/authorize",
                access = "https://www.strava.com/oauth/token"),
                strava_app, scope = app_scope)
}

