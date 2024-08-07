% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uas_rename.R
\name{uas_rename}
\alias{uas_rename}
\title{Rename UAS images}
\usage{
uas_rename(
  x,
  flt = NULL,
  name_template = "img{i}_{alt_agl}m_{camera_abbrev}_{Y}_{m}_{d}_{H}_{M}_{S}",
  all_lower = TRUE,
  preview = TRUE,
  confirm = TRUE,
  flt_idx = deprecated()
)
}
\arguments{
\item{x}{A list of class 'uas_info'}

\item{flt}{Flight(s) in x to process (character or numeric vector, default is all)}

\item{name_template}{A template for generating the file names (see Details)}

\item{all_lower}{Make file names all lowercase, Logical}

\item{preview}{Preview the new names only}

\item{confirm}{Confirm continue before changing file names}

\item{flt_idx}{`r lifecycle::badge("deprecated")` Use `flt` instead}
}
\value{
A tibble showing the old and new names for each image.
}
\description{
Rename UAS images
}
\details{
This function will rename image files on disk based on a naming template which can include
placeholders (tokens) from image and/or flight metadata. This can be useful when you want
to rename your image files based on some formula of date-time parts, AGL altitude, flight metadata
fields, camera type, etc. Renaming image files can be helpful when you're doing analysis of individual images, but
is generally not needed when you're doing photogrammetry whereby you're more likely to be using directory
names to help you identify groups of images for stitching.

\strong{Caution is advised} when using this function, because it will actually \strong{rename your files!} Use \code{preview = TRUE}
to test your naming template. When \code{preview = TRUE}, the function will return a tibble with the 'old' and 'new' names, but not actually change
any file names.

\code{flt} allows you to specify a subset of image folders in \code{x} to process. You can pass a vector of flight names (use names(x)
to see what those are) or integers.

When you're ready, set \code{preview = FALSE}. Aafter renaming files, you'll need to rerun
\code{\link{uas_info}} on the directory(s) to update the info.
}
\section{File Name Template}{


\code{name_template} should be a pattern containing placeholders (or 'tokens') in curly brackets. When you run the function, the tokens will be swapped out
for actual values.

For example, if a filename is \emph{DJI_0213.JPG} and the name template was
\code{"img_{Y}-{m}-{d}_{H}-{M}-{S}"}, the file would be renamed something like
\emph{img_2018-06-17_13-04-46.jpg}, where the numbers for date and time are extracted from the
image EXIF data. Supported tokens you can use include:

\itemize{
  \item \code{\{i\}} an integer starting from 1 and going up, padded with enough leading zeros to accommodate all values
  \item \code{\{Y\}} year (4-digit)
  \item \code{\{y\}} year (2-digit)
  \item \code{\{m\}} month (2-digit)
  \item \code{\{d\}} day (2-digit)
  \item \code{\{j\}} Julian day
  \item \code{\{H\}} hour (2-digits)
  \item \code{\{M\}} minutes (2-digits)
  \item \code{\{S\}} seconds (2-digits)
  \item \code{\{camera_abbrev\}} an abbreviated name of the camera
  \item \code{\{alt_agl\}} altitude above the launch point (usually in meters). You can indicate rounding by
adding a comma and the number of decimal places. For example \code{\{alt_agl,0\}} would round the AGL value to the nearest whole number.
This option is only available for images where the relative altitude is saved in the EXIF data (which excludes most multi-spectral images).
}

In addition, name templates can include any flight metadata field that has been defined. For example if the flight metadata information
includes fields for \code{proj} (project abbreviation) and \code{loc} (location), the name template could include  \code{\{proj\}} and  \code{\{loc\}}.

When creating your name template, remember:

1) All file names must come out unique. An easy way to ensure this is to include \code{\{i\}} in your name template, which will be replaced with a sequence of integers.

2) File names should not include characters that aren't allowed for file names. These include \code{< > : `"` / \\ | ? *}. If any of these characters are found in name_template, it will be rejected.

3) \code{name_template} should not contain an extension
}

\seealso{
\code{\link{uas_info}}, \code{\link{uas_metadata_make}}
}
