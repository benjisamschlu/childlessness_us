## About

Template repository having the required folder structure and files to create a new research project. 

To use this template repository, just click on the green "Use this template" button on [the main repo page](https://github.com/benjisamschlu/new_project/) or go to [https://github.com/benjisamschlu/new_project/generate](https://github.com/benjisamschlu/new_project/generate) and create a new repository.

## Project structure

- **`codes/`**: This folder should contain all your codes. Most files should begin with a number and all files should be ordered so they can be run sequentially (e.g., 01_clean_data.R, 02_fit_models.R, 03_analyze_results.R, etc.). 
- **`codes/functions/`**: This folder ontains own-defined functions used across multiple code files and *theme_plot.R*  a plot theme used across multiple plotting scripts.
- **`inputs/data_public/`**: In this folder are raw data that can be shared or have a size uploadable to Github. 
- **`inputs/data_private/`**: In this folder are raw data that can not be shared or have a size not uploadable to Github. Data files here will be gitignored and not shared.
- **`literature/`**`: This folder contains key papers related to this project.
- **`outputs/data_public/`**: In this folder are tidy data/estimates that can be shared or have a size uploadable to Github. 
- **`outputs/data_private/`**: In this folder are tidy data/estimates that can not be shared or have a size not uploadable to Github. Data files here will be gitignored and not shared.


## Important files
- **`/new_project.Rproj`**: Rename this file according to new RStudio project file.
- **`/.gitignore`**: Gitignore items to things that don't have to be shared.
- **`codes/secrets.R`**: Store information that is not super sensitive but that don't has to be shared publicly (i.e credentials for HMD).
- **`outputs/abstracts`**, **`outputs/paper`**, and **`outputs/reports`** contain Rmarkdown file, Latex template, BibTex file and Demographic Research template of bibliography.
- **`DELETEME.md`**: Delete this files before you get started. They are just here to retain the folder structure (Github ignore empty folders).

### Acknowledgments

This functionality was discovered thanks to Mathew Kiang (Stanford University) and based on [the project template of his lab](https://github.com/kianglab/new_project)