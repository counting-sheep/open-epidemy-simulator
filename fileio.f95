module fileio
	implicit none
	
contains

	subroutine write_step(agents,n,time,fname)
		implicit none
		integer, intent(in) :: n, time
		integer, intent(in) :: agents(n,2)
		character(len=40) :: fname
		
		integer :: i
		
		open(1, file=fname, position="APPEND", action="WRITE")
		
		write(1,*), n
		write(1,*), "step=",time,"status mortal_rate contage_rate x y"
		do i=1,n
			write(1,*) agents(i,:)
		end do
		
		close(1)

	end subroutine write_step
	
end module fileio
