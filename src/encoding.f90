! This module conaints the subroutines of coding hidden messages into the
! pixel data of an image. 

module encoding
	use constants
	implicit none
	
	contains

	! Hides the array d into the array arr.
	! d will be written to the least significant bits of the arr.
	! arr = an array of bytes to which the d will be hidden to.
	! d = an array of bytes that will be hidden to arr.
	! psize = "pixel size", tells how many bytes are used to represent
	!          one shade of one rgb-color, usually 1 or 2 (255 or 65535 colors).
	subroutine encode(arr, d, psize)
		implicit none
		integer(kind=byte), intent(inout) :: arr(:)
		integer(kind=byte), intent(in) :: d(:)
		integer, intent(in) :: psize
		integer :: i, j, wi 
		integer(kind=byte) :: bit
		integer :: length

		length = size(d)

		! Code first the size of the hidden data:

		l_write_byte: do i = 0, sizeof(length)
			l_write_bit: do j= 0, 7				
				bit = iand(rshift(length,8*i+j), byte_mask)
				wi = psize * (8*i + j + 1)    ! wi = "write index"
				call change_bit(arr(wi),bit,0)
			end do l_write_bit
		end do l_write_byte

		! Then the actual data:

		length = size(d)
		write_byte: do i = 1, length
			write_bit: do j = 0, 7 
				bit = iand(byte_mask,rshift(d(i),j))
				wi = 32 + psize*(8*(i-1) + j + 1) 
				call change_bit(arr(wi), bit, 0)
			end do write_bit
		end do write_byte				

	end subroutine encode


	! Returns the size of the hidden message in arr in bytes.
	! The message is assumed to be written as an integer in
	! the first bytes of arr.
	integer function message_size(arr)
		integer(kind=byte), intent(in) :: arr(:)
		integer :: i

		message_size = 0
		do i=0, 4*sizeof(message_size) -1
			message_size = message_size + lshift( iand(arr(i+1), int_mask), i)
		end do
	end function message_size


	! Decodes hidden data from unc to dec.
	! unc and dec should be 1D byte arrays.
	! psize tells the stride of the encoding. E.g. if pzise=2, only the 
	! even byets in unc contain hidden data.
	subroutine decode(unc, dec, psize)
		integer(kind=byte), intent(in) :: unc(:)
		integer(kind=byte), intent(inout) :: dec(:)
		integer, intent(in) :: psize
		integer :: i, j, ri
		integer(kind=byte) :: b

		read_byte: do i = 1,  size(dec)
			read_bit: do j = 1, 8
				ri = 8 * (i+sizeof(psize) -1) +j  ! ri = read index 
				b = iand(unc(ri), byte_mask)
				call change_bit(dec(i),b,j-1)
			end do read_bit
		end do read_byte
		
	end subroutine decode


	! Auxiliary routine that changes one bit in the byte b.
	! i = 0,1,...,7. The bit that will be changed, 0 is the least signifant bit.
	! newbit = the bit that will appear in the i:th place of b.
	subroutine change_bit(b,newbit,i)
		implicit none
		integer(kind=byte), intent(inout) :: b
		integer(kind=byte), intent(in) :: newbit
		integer, intent(in) :: i
		
			if (newbit == 0_byte) then
				b = ibclr(b,i)
			else
				b = ibset(b,i)
			end if			
	end subroutine change_bit

end module encoding
