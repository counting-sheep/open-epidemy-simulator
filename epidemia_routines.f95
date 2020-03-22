module epidemia_routines
	use mt19937_64
	implicit none

contains

	subroutine move_agents(agents, n_agents, agents_prev_dir, xmax, ymax)
		implicit none
		integer, intent(in) :: n_agents, xmax, ymax
		integer, intent(inout) :: agents(n_agents, 2), agents_prev_dir(n_agents)
		integer :: prev_dir, j
		real(8) :: u
		
		!Drunken sailor
		do j = 1, n_agents
			u = genrand64_real2()
			prev_dir = agents_prev_dir(j)
			if(u>0.75 .and. prev_dir /= 2) then
				agents(j,1) = modulo(agents(j,1) + 1, xmax)
				agents_prev_dir(j) = 1 
			else if(u>0.5 .and. prev_dir /= 1) then
				agents(j,1) = modulo(agents(j,1) - 1, xmax)
				agents_prev_dir(j) = 2 
			else if(u>0.25 .and. prev_dir /= 4) then
				agents(j,2) = modulo(agents(j,2) + 1, ymax) 
				agents_prev_dir(j) = 3
			else if(prev_dir /= 3) then
				agents(j,2) = modulo(agents(j,2) - 1, ymax)
				agents_prev_dir(j) = 4
			end if		
		end do 
	
	end subroutine move_agents

	!subroutine contage_agents()
	!	implicit none
	!
	!end subroutine contage_agents

end module epidemia_routines
