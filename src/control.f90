! This module contains all the complete tasks the program wil run,
! i.e. hides data to an image, extracts data from an image, prints
! the help on screen.

module control
	use constants
	use ppmio
	use encoding
	use messageio
	implicit none

contains


! Hides the data in an image
! img = the image to which data will be hidden, must be a .ppm-file.
!	      won't change.
! obj_img = the name of the file to which the image with hidden data 
!	      will be written to.
! message, optional = message that will be hidden in ascii-text.
! sourcef, optional = the file that will be hidden in the image.
! NOTICE: one and only one of the optional arguments must be present.

subroutine hide(img, obj_img, message, sourcef)
	implicit none
	character(len=*), intent(in) :: img, obj_img
	character(len=*), intent(in), optional :: sourcef, message 
	integer(kind=byte), allocatable :: pic_data(:), hide_data(:)
	integer :: width, height, colors, psize, s
	logical :: valid

	! 1. Read the image data:
	call read_metadata(img, valid, width, height, colors)
	psize = pixel_size(colors)		
	s = 3 * psize * width * height

	allocate(pic_data(s))
	call read_data(img, pic_data)

	! 2. Hide the data in the image
	if (present(message)) then
		allocate(hide_data(len(message)))
		call text_to_ascii(message, hide_data)
	else if (present(sourcef)) then
		inquire(file = sourcef, size = s)
		allocate(hide_data(s))
		call read_raw_data(sourcef, hide_data)
	end if		

	! 3. Check if the data fits into the image:
	if ( size(hide_data) > size(pic_data) / (8 * psize) ) then
		write(6,*) "Too small picture for the message"
		write(6,*) "No data hidden."
		call exit(0)
	end if
	
	! 4. Write the image with the hidden image:
	call encode(pic_data, hide_data, psize)
	call write_data(obj_img, pic_data, width, height, colors)
end subroutine hide


! Extracts hidden data from an image.
! fname = the name of the .ppm-image from which the data will be extracted.
! to_file, optional = the file to which the hidden data will be written.
! NOTICE: If the argument to_file is not present, the data will be written
! in the std output.
subroutine extract(fname, to_file)
	character(len=*), intent(in) :: fname
	character(len=*), intent(in), optional :: to_file
	character(len=:), allocatable :: msg
	integer(kind=byte), allocatable :: arr(:), dec(:)
	integer :: width, height, colors, psize, msg_size
	logical :: valid

	! Read the metadata of the image, and do the checks:
	call read_metadata(fname, valid, width, height, colors)

	if (.not. valid) then
		write(6,*) "Not a valid -ppm file"
		call exit(0)
	end if

	! Read the image data:
	psize = pixel_size(colors)
	
	allocate(arr(3*width*height*psize))
	call read_data(fname, arr)

	! Extract the hidden data to dec:
	msg_size = message_size(arr)	
	allocate(dec(msg_size))
	call decode(arr, dec, psize)

	! Output the hidden data to the file or in std output:
	if (present(to_file)) then
		call write_raw_data(to_file, dec)
	else
		allocate(character(len=msg_size) :: msg  )
		call ascii_to_text(dec, msg)
		write(6,*) msg
	end if
end subroutine extract


! Auxiliary function that tells how many bytes are needed to present
! [colors] shades of one rgb-color.
integer function pixel_size(colors)
	implicit none
	integer, intent(in) :: colors

	if (colors > 255) then
		pixel_size = 2
	else
		pixel_size = 1
	end if	
end function pixel_size


! Print quick help on the screen:
subroutine quick_help()
	write(6,*) 'Type "./a.out help" for help.'
end subroutine quick_help


! Print help on the screen: 
subroutine help()
	write(6,*) ""
	write(6,*) "1. Encoding data into a picture."
	write(6,*) ""
	write(6,*) "To encode a file file.xxx to a picture pic.ppm"
	write(6,*) "so that the file will be hidden in obj.ppm, type:"
	write(6,*) "    ./a.out	pic.ppm obj.ppm file.xxx"
	write(6,*) ""
	write(6,*) "2. Decoding data from a picture."
	write(6,*) ""
	write(6,*) "To decode a file from picture pic.ppm into a file"
	write(6,*) "file.xxx, type"
	write(6,*) "    ./a.out pic.ppm file.xxx "
	write(6,*) ""
	write(6,*) "2. Quick en/decoding"
	write(6,*) ""
	write(6,*) "If an image pic.ppm contains ascii-text, you can "
	write(6,*) "decode it on the screen by typing"
	write(6,*) "     ./a.out pic.ppm"
	write(6,*) ""
	write(6,*) "To encode ascii-text no more than 160 characters long to an image"
	write(6,*) "pic.ppm so that the message will be hidden in the image obj.ppm, type"
	write(6,*) '     ./a.out pic.ppm obj.ppm text="The message"'
	write(6,*) ""
end subroutine help
end module control
