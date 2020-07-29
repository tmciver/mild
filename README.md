# MILD

MILD is an application that extracts metadata from images and exports it as
Linked Data, specifically RDF.

## Building

It's simple with stack:

    $ stack build

## Usage

Run the executable with stack and pass it the path to an image file:

    $ stack exec mild-exe <path/to/image/file.ext>

The exported metadata is output through standart out.
