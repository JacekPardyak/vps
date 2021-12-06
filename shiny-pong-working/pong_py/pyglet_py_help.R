library(reticulate)
Sys.setenv(RETICULATE_MINICONDA_PATH = 'C:/Users/Public/r-miniconda')
Sys.getenv("RETICULATE_MINICONDA_PATH")

# list environments
conda_list()
use_condaenv("r-miniconda")
# install SciPy
conda_install(packages = "setuptools")
conda_install(packages = "pyglet")
conda_version()

py_available()

Sys.which("python")

use_python("C:\\Users\\Public\\r-miniconda\\")
