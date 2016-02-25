! This module handles all the input and output of .ppm-images

module ppmIO
	use constants
	implicit none
	

contains

	! Reads metadata from the image fname into the rest of the variables.
	! valid = is the file a valid .ppm-picture (i.e. does it contain the "P6").
	! width, height = dimensions of the pictures
	! colors = maximum value of a single color (e.g. 225 shades of red)
	subroutine read_metadata(fname, valid, width, height, colors)
		character(len=*),intent(in) :: fname
		logical,intent(inout) :: valid
		integer,intent(inout) :: width, height, colors
		character(len=2) :: magic_number
		integer :: ios
		
		! Open the file and check if it's ok:

		open(unit=1, file=fname, iostat=ios, status="old")
		if (ios /= 0) then
			valid = .FALSE.
			return
		end if

		! Read the metadata:

		read(1,*) magic_number, width, height, colors
		valid = (magic_number == "P6")		
		close(1, status="keep")
	end subroutine read_metadata


	! Finds the postition where the actual pixel data of the picture
	! starts from.
	! fname = the .ppm file name.
	! This doesn't work with .ppm images that have a comment line.
	integer function start_pos(fname)
		implicit none
		character(len=*), intent(in) :: fname
		integer(kind=byte) :: read_byte = 0
		integer :: cur_pos, ios, sets = 4

		open(unit=1, file=fname,status="old", iostat=ios, access="stream",& 
				form="unformatted") 		
		start_pos=0
		
		! sets indicated how many continuous lumps of non-whitespace data
		! there is in the file before the pixel data.
		find_pos: do while (sets >0)
			! Go through the non-ws bytes:
			cycle_text: do while ( .NOT. is_ws(read_byte) )
				read(1,iostat=ios) read_byte		
				start_pos = start_pos +1	
			end do cycle_text

			! Go through the ws-bytes:
			cycle_whitespace:	do while ( is_ws(read_byte) )
				read(1,iostat=ios) read_byte			
				start_pos = start_pos +1	
			end do cycle_whitespace
	
			sets = sets - 1

		end do find_pos
	
		close(1)
	end function start_pos 


	! Tells if the argument is an ascii number of a white space,
	! i.e. tab, newline, carriage return or space
	logical function is_ws(char)
		implicit none
		integer(kind=byte), intent(in):: char

		is_ws = ( any([9,10,13,32] == char) )
	end function is_ws


	! Reads the pixel data from the image fname into the variable array
	! Array's elements should be one byte integers.
	! Doesn't check the array size.
	subroutine read_data(fname, array)
		implicit none
		character(len=*) :: fname
		integer(kind=byte), intent(inout) :: array(:)
		integer :: start, ios

		! Open and check the file:
		start = start_pos(fname)
		open(unit=1, file=fname,status="old", iostat=ios, access="stream",& 
				form="unformatted")
		if (ios /= 0) then
			write(6,*) "File " // fname // " couldn't be read. Exiting."
			call exit(ios)
		end if
		
		! Read the data into the array:
		read(1,pos=start) array
		close(1) 			
	end subroutine read_data


	! Writes a .ppm image.
	! fname = the name of the ppm-file to be written.
	! width, height = dimensions of the picture	
	! colors = number of shades of one color (e.g. red 255)
	! arr = the pixel data as a onedimensional array
	subroutine write_data(fname, arr, width, height, colors)
		implicit none
		character(len=*), intent(in) :: fname
		integer, intent(in) :: width, height, colors
		integer(kind=byte), intent(in) :: arr(:)
		integer :: ios 

		! Write the metadata:
		open(unit=1, file=fname, iostat=ios, status="new")
		write(1,'(A)') "P6"
		write(1,*) width, height
		write(1,*) colors
		close(1)

		! Write the pixel data:
		open(unit=1, file=fname, position = "append", status="old", iostat=ios, &
				access="stream", form="unformatted")
		write(1) arr
		close(1)
	end subroutine write_data

end module ppmIO
