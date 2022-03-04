# Install R packages for helping with Git authentication and setup
install.packages(c("usethis", "gitcreds", "installr"))

# Download and install Git for Windows
installr::install.git()

# Create authentication token for using RStudio with GitHub:
usethis::create_github_token()

# Register generated token with R:
gitcreds::gitcreds_set()

# Install additional for code styling and linting
install.packages(c("lintr", "styler"))
