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

	subroutine contage_agents(agents, n_agents, agent_status, contage_rate)
		!agent_status: 0 = dead, 1 = immune, 2 = healthy, 3 = sick, contageous, >= 4 sick, not contageous
		implicit none
		integer, intent(in) :: n_agents, agents(n_agents,2)
		integer, intent(inout) :: agent_status(n_agents)
		real(8), intent(in) :: contage_rate
		integer :: i, j
		real(8) :: u
		
		do i = 1, n_agents - 1
			do j = i + 1, n_agents
				if(agents(i,1) == agents(j,1) .and. agents(i,2) == agents(j,2)) then
					if(agent_status(i) == 2 .and. agent_status(j) == 3) then
						u = genrand64_real2()
						if(u < contage_rate) then
							agent_status(i) = 4
						end if
					else if(agent_status(i) == 3 .and. agent_status(j) == 2) then
						u = genrand64_real2()
						if(u < contage_rate) then
							agent_status(j) = 4
						end if
					end if
				end if
			end do
		end do
	
	end subroutine contage_agents
	
	subroutine advance_disease(agent_status, n_agents, death_rate, cure_rate, start_time)
		implicit none
		integer, intent(in) :: n_agents, start_time
		integer, intent(inout) :: agent_status(n_agents)
		real(8), intent(in) :: death_rate, cure_rate
		integer :: i
		real(8) :: u, u2
		
		do i = 1, n_agents
			select case(agent_status(i))
				case (3)
					u = genrand64_real2()
					if(u < cure_rate) then
						agent_status(i) = 1 !Healed, immune
					else
						u2 = genrand64_real2()
						if(u2 < death_rate) then
							agent_status(i) = 0 !Died to disease
						end if
					end if
				case (4:)
					if(agent_status(i) >= 4 + start_time) then
						agent_status(i) = 3 ! Disease goes contageous, symptoms start
					else
						agent_status(i) = agent_status(i) + 1 !No symptoms yet
					end if
			end select
		end do
		
	end subroutine advance_disease

end module epidemia_routines
