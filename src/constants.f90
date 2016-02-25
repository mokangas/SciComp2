! Contais the constants that are used in more than one modules.

module constants
	implicit none
	integer, parameter :: byte = selected_int_kind(1)
	integer(kind=byte), parameter :: byte_mask = 1
	integer, parameter :: int_mask = 1
end module constants


