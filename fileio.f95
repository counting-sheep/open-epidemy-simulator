module fileio
	implicit none
	
contains

	subroutine write_step(agents , n, agent_status, time,fname)
		implicit none
		integer, intent(in) :: n, time
		integer, intent(in) :: agents(n,2), agent_status(n)
		character(len=40) :: fname
		
		integer :: i
		
		open(1, file=fname, position="APPEND", action="WRITE")
		
		write(1,*), n
		write(1,*), "step=",time, "boxsize 10.0 10.0 1.0 status x y"
		do i=1,n
			write(1,*) min(agent_status(i),4), agents(i,:)-5
		end do
		
		close(1)

	end subroutine write_step
	
end module fileio
