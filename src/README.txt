To compile with gfortran, run twice on the command line:
========================

gfortran constants.f90 control.f90 encoding.f90 messageIO.f90 ppmIO.f90 
promptUI.f90

or alternatively, type twice:

gfortran *.f90

The first run will produce the mod-files and the second an executable a.out.
The help menu etc. will assume that the name of the program is a.out. If you
choose some other name for it, adjust accordingly.


To use the program:
===================

1. Help
-------

Type
	./a.out help
to print the help.

2. Hiding data
--------------

2.1. Type
 
	./a.out image.ppm writeHere.ppm text="Type your message here"

to encode a message to the image.ppm.

NOTE: Image.ppm won't be altered, the hidden message will be in writeHere.ppm.
      Write no spaces between 'text=' and the message.
      Only ~250 characters of the text will be written. For longer messages, 
      use the next method.


2.2 Type 

	./a.out image.ppm writeHere.ppm data.xxx

to hide file data.xxx into the image.ppm.

NOTE: Image.ppm won't be altered, the hidden message will be in writeHere.ppm.
      The file type of data.xxx doesn't matter, but too large files won't be
      hidden.


3. Retrieveing hidden data
--------------------------

3.1 Type

	./a.out image.ppm

to print on screen a text message hidden in image.ppm


3.2 Type

	./a.out image.ppm hiddenData.xxx

to extract hidden data from image.ppm to the file hiddenData.xxx.

NOTE: This programs doesn't tell you what kind of data is hidden in
      the image. You'll have to choose the file extensions by yourself.

