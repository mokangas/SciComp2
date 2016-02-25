! The user interface for the command line.
!
! This is the main program.
!
! Decides on action first by the number of command line arguments 
! and in some cases by the content of them and then asks the control
! module to execute them.
!
! (See help() in control.f90 for details).
!
program promptUI
	use control
	implicit none
	integer :: args

	args = command_argument_count()

	! Choose action:
	select case (args)
		case (1)
			call one_arg()			
		case (2)
			call two_arg()		
		case (3)
			call three_arg()
		case default
			write(6,*) "Wrong amount of arguments."
			call quick_help()
	end select


contains 

! Will be launched if there's one command line argument arg1.
! Prints the help (if arg1="help") 
! or decodes the message from .ppm image [arg1] on screen.
subroutine one_arg()
	character(len=255) :: arg

	call get_command_argument(1,arg)
	if (trim(arg) == "help") then
		call help()
	else
		call extract(trim(arg))
	end if
end subroutine one_arg


! Will be launched if there's two command line arguments (arg1, arg2).
! Extrats the hidden data from the .ppm-file [arg1] into the file [arg2].
subroutine two_arg()
	character(len=255) :: arg1, arg2

	call get_command_argument(1,arg1)
	call get_command_argument(2,arg2)

	call overwrite_check(arg2)

	call extract(trim(arg1),trim(arg2))
end subroutine two_arg


! Will be launched if there's three command line arguments,
! (arg1, arg2, arg3). 
! Codes data from file [arg3] to the .ppm-image [arg1] and
! writes it into the file [arg2].
! If arg3 begins with "text=", the rest of the arg3 will be coded
! into the image as ascii numbers.
subroutine three_arg()
	character(len=255) :: arg1, arg2, arg3
	integer :: l
	
	call get_command_argument(1, arg1)
	call get_command_argument(2, arg2)
	call get_command_argument(3, arg3)

	call overwrite_check(arg2)		

	if ( arg3(1:5) == "text=" ) then
		l = len(trim(arg3))
		call hide(arg1, arg2, message = arg3(6:l))
	else 
		call hide(arg1, arg2, sourcef=arg3)
	end if
end subroutine three_arg


! An auxiliary routine that checks whether the file [fname]
! already exists, and if the user wants to overwrite it.
subroutine overwrite_check(fname)
	character(len=*), intent(in) :: fname
	character :: ch
	logical :: ex

	! Check if the file exists:
	inquire(file = trim(fname), exist = ex)
	if (.not. ex) return

	! Ask if the user wants to overwrite it:
	write(6,*)	"The file " // trim(fname) // " exists already. Overwrite?"		
	write(6,*) "(y/n): "
	read(5,*) ch
	if (ch =="n") call exit(0)

	! Overwrite it:
	open(unit = 1, file = trim(fname))
	close(unit = 1, status = "delete")
end subroutine overwrite_check


end program promptUI


