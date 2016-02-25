! This module contains the subroutines used to reaing and writing the
! hidden message, for example: writing unformated data and converting
! text to ascii and back again.

module messageIO
	use constants
	implicit none

	contains


	! reads data from a file to an array of bytes.
	! fname = the name of the file to be read.
	! arr = the array of bytes to which the data will be written
	! size(arr) bytes of data will be read.
	subroutine read_raw_data(fname, arr)
		implicit none
		character(len=*), intent(in) :: fname
		integer(kind=byte), intent(inout) :: arr(:)
		integer :: ios, s

		open(unit = 1, file = fname, status = "old", access = "stream", &
				form = "unformatted", iostat = ios)
			
		! Checks:
		if (ios /= 0) then
			write(6,*) "Read error in file " // fname
		end if
		inquire(file=fname, size=s)
		if ( s < size(arr)) then
			write(6,*) "Tried to read too many bytes from file " // fname
			write(6,*) "Exiting."
			call exit(ios)
		end if

		! Reading:
		read(1) arr
		close(1) 
	end subroutine read_raw_data


	! Writes a byte array arr to the file fname as unformatted data.
	subroutine write_raw_data(fname, arr)
		character(len=*), intent(in) :: fname
		integer(kind=byte), intent(in) :: arr(:)
		integer :: ios

		open(unit = 1, file = fname, status = "new", access="stream", &
				form="unformatted")
		if (ios /= 0) then
			write(6,*) "Something went wrong while writing to file " // fname
			write(6,*) "Exiting."
			call exit(ios)
		end if
		write(1) arr
		close(1)
	end subroutine write_raw_data


	! converts a string to the corresponding ascii numbers.
	! ch = the string to be converted
	! bytes = the array of bytes to which the ascii numbers will be written.
	subroutine text_to_ascii(ch, bytes)
		implicit none
		character(len=*), intent(in) :: ch
		integer(kind=byte), intent(inout) :: bytes(:)
		integer :: i

		! length check:
		if (len(ch) > size(bytes)) then
			write(6,*) "Tried to read too many characters into a byte array. Exiting."
			call exit(1)
		end if

		! read the chars:
		do i = 1,len(ch)
			bytes(i) = iachar(ch(i:i+1))
		end do
	end	subroutine text_to_ascii


	! converts an array of ascii numbers to text
	! a = the byte array to be converted
	! ch = the string to which the text will be written.
	subroutine ascii_to_text(a,ch)
		implicit none
		integer(kind=byte), intent(in) :: a(:)
		character(len=*), intent(inout) :: ch
		integer :: i

		! Check the length:
		if (len(ch) < size(a)) then
			write(6,*) "Tried to read too many bytes into a string. Exiting."
			call exit(1)
		end if

		! Write the text:
		do i=1,size(a)
			ch(i:i+1) = achar(a(i)) 
		end do		
	end subroutine ascii_to_text

end module messageIO
