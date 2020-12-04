
### 07/01/2019: How to re-install all your R packages after updating the software without having to install.package() each of them manually.
### Made by © Fabio Benedetti when updationg R from 3.4.3 to 3.5.2. Inspired from: 
#   https://community.rstudio.com/t/reinstalling-packages-on-new-version-of-r/7670/4

# First, find the location of the packages installed in the directory of the older R version (in our case: 3.4).
lib <- "/Library/Frameworks/R.framework/Versions/3.4/Resources/library"
# Make vector of packages name to install
to_install <- unname(installed.packages(lib.loc = lib)[, "Package"])
# to_install
install.packages(pkgs = to_install)

############################################################################################################################################

