# uasimg 1.6.9 (2021-05-05)

* `uas_dirs_find()`: fixed a case where images in tb folders were not excluded 

# uasimg 1.6.8 (2021-05-05)

* `uas_dirs_find()`: path column in the tibble is now wrapped in `normalizePath()` (primarily for readability with network paths)
* `uas_report()`: returned HTML file names are now wrapped in `normalizePath()`; `show_gps_coord` deprecated
* `uas_toc()`: fixed an error where the libs subfolder was not being gathered
* `uas_toc.Rmd`: added error trapping code if metadata fields are missing
* `uas_report.Rmd`: modified the balloon text to show GPS coords

# uasimg 1.6.7 (2021-05-04)

* `uas_thumbnails_make()`: modified to automatically switch to the magick package functions (with a message) if thumbnails from TIFs are being created *and* `imager:::has.magick()` is FALSE.

# uasimg 1.6.6 (2021-05-03)

* `uas_thumbnails_make()`: enabled `use_magick` argument

# uasimg 1.6.5 (2021-05-02)

* `uas_report()`: added checks for directory existence and access
* `uas_metadata_make()`: argument `md_template` can now be a URL
* added Flight Metadata Vignette

# uasimg 1.6.4 (2021-05-01)

* `uas_dirs_find()`: new function to find all sub-directories with images
* `uas_thumbnails_make()`: added `stats` argument to display the amount of time taken per thumbnail
* `uas_report()`: added `use_tmpdir` argument to render the flight summary report in temp folder (can prevent Pandoc errors when images are on a network drive)

# uasimg 1.6.3 (2021-04-30)

* `uas_cropctr()`: removed a leftover debugging statement

# uasimg 1.6.2 (2021-04-28)

* `uas_metadata_make()`: added `use_system_editor` argument
* `uas_report()`: added `header_html` and `footer_html` arguments

# uasimg 1.6.1 (2021-04-26)

* `uas_metadata_make()`: appended `file:` to metadata files on network paths to get it to open in notepad; added more comments in template

# uasimg 1.6.0 (2021-04-24)

* `uas_convert()`: this new function converts images between file formats (e.g., DNG to JPG), while preserving EXIF data  
* `magick` and `exiftoolr` added as dependent packages (as imports)
* `uas_info()`: added argument for `exiftoolr_use`

# uasimg 1.5.2 (2020-12-02)

* `uas_getoemflds()`: added 'uav' to the list of default flight metadata fields
* updated camera database:  (added senseFly Aeria X and DJI M100 X3Z)
* `uas_report()`, `uas_exp_kml()`, and `uas_exp_shp()`: added checks after dir.create()

# uasimg 1.5.1 (2020-11-14)

* `uas_metadata_make()`: additional comments inserted when creating a brand new new metadata file 
* `uas_getflds()`: exported
* `uas_thumbnails_make()`: increased the number of bytes read to create a unique filename from 500 to 2000
* `uas_info()`: updated to always rename yaw field even when fp = FALSE
* `uas_clearcache()`: created

# uasimg 1.5.0 (2020-11-01)

* `uas_report()` and `uas_report.Rmd`: arguments `report_title` and `kml_ctr` added; trapped an error if metadata exists but without an image collection name; thumbnails disabled for grouped images (until a better visualization is implemented); additional <span id="..."> tags added to facilitate web scraping
* `uas_info()`: deafult value of `fp` changed to `FALSE`; default value of `cache` changed to `TRUE`; a flight id value saved in return value (used primarily to name files if metadata$name_short is missing)
* `uas_flds_oem()`: `name_short` and `notes` added to default fields; `collection_name` removed
* `uas_toc()` and `uas_toc.Rmd`: check that html files exist added; all code migrated from XML to xml2
* `uas_exp()` renamed (back to) `uas_exp_shp()`;
* `uas_exp_kml()` created
* `uas_thumbnails_make()` created
* *xml2*, *tibble* and *imager* added to Imports (required packages); *XML* and *magick* package removed

# uasimg 1.4.0 (2020-10-01)

* `uas_toc()` and `uas_toc.Rmd` updated - new `summary_map` argument displays an interactive mission footprint map
* `uas_report.Rmd` updated - flight MCP converted to base64 and encoded in a meta tag
* `uas_report.R` updated - argument `local_dir` renamed `show_local_dir`; `gps_coord` renamed `show_gps_coord`
* `uas_exp()` renamed `uas_exp_shp()`

# uasimg 1.3.8 (2020-09-28)

* added `kml_mcp` to `uas_report()`

# uasimg 1.3.7 (2020-09-27)

* improved help page for `uas_info()` and `uas_cameras()`
* added `name_only` argument to `uas_cameras()`
* renamed argument `csv` to `exif_csv` in `uas_info()`
* added argument `gps_coord` to `uas_report()` & modified `uas_report.Rmd` to show the GPS coordinates in the pop-up windows

# uasimg 1.3.6 (2020-09-26)

* Added DJI ZenMuse X7 to camera database
* `uas_info()` edited to i) check the number of images in the directory before computing forward overlap, ii) add bullet characters to feedback messages

# uasimg 1.3.5 (2020-09-24)

* Added a `NEWS.md` file to track changes to the package.  
* `geo2utm()` modified to return a EPSG number (to comply with recent changes to rgdal and proj4). After installing this update, users are advised to delete their old cache (see `uas_getcache()`) to prevent warning messages about unknown datums in a proj4string.
* `uas_cropctr()` added - crops around the center of each image for a better photo mosaic

# Changes in version 1.3.4 (2020-08-25)

* `uas_report()` modified to return the file name of HTML files that already exist and overwrite = FALSE; corrected the extension for tif thumbnail files.  
* Added `uas_metadata_make()`.  
* Modified `uas_toc()` and `uas_toc.Rmd` to support `gather = '.'`, handle duplicate HTML names, a description, and default footer.  
* Corrected a bug in `uas_info()` when no external metadata was found or passed as an argument. 
* Other minor fixes and enhancements.  

# Changes in version 1.3.3 (2020-08-20)

* Added an option in `uas_report()` to create thumbnails previews that appear in the popup windows of the image collection catalog.  
* Added `combine_dirs` argument to `uas_grp_flt()` to support multi-directory flight parsing.  
* Added custom print functions for uas_info and uas_grp objects.  
* Numerous small enhacements and bug fixes.  

# Changes in version 1.3.2 (2020-08-08)

* `uas_info(()` modified to support file names metadata.txt expanded to metadata*.txt.  

# Changes in version 1.3.1 (2020-08-07)

* Renamed `uas_getcache()` and `uas_setcache()`.  
* Added `uas_grp_flt()` and `uas_grps2col()`.  
* Added `uas_getflds()` and `uas_setflds()`.  
* Added Mavic 2 Pro to camera database.  
* renamed `meta_extra` argument to `metadata` in `uas_info()`.  

# Changes in version 1.3 (2020-07-04)

* Added `uas_getcachedir()` and `uas_setcachedir()` to manage cache directory for EXIF data.  
* Renamed cache arguments on `uas_info()` to `cache` and `update_cache`.  
* Added `uas_toc()`.  

# Changes in version 1.2.7 (2020-06-30)

* Modified `uas_report()` and uas_report.Rmd, metadata now encoded in HTML.  
* Argument `toc_csv` deprecated.  
* `summary.uas_info()` replaced with `print.uas_info()` (generic print method).  

# Changes in version 1.2.6 (2020-06-20)

* Added 'description' element to `meta_extra` argument to `uas_info()`.  
* Added `cache_dir` and `cache_update` to `uas_info()`.    
* Split `overwrite` argument in `uas_report()` into `overwrite_html` and `overwrite_png`.    

# Changes in version 1.2.5 (2020-06-14)

* Added `meta_extra` argument to `uas_info()`.    
* Added `toc_csv` argument to `uas_report()`.    

# Changes in version 1.2.4 (2020-04-03)

* Initial release of `uasimg`  
* Key differences from older `uavimg`:  
    * All prefixes changed from `uavimg_` to `uas_`
    * Converted all functions to the `sf` package
    * Added `overwrite` argument to `uas_exp()` and `uas_report()`
    * Added support for paths with spaces: `uas_info()`
    * Added `sf`, `tidyr` and `crayon` packages as dependencies
    * Fixed a bug in `findonpath()` when path values had trailing slashes
