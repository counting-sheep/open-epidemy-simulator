!Agents: status mortal_rate contage_rate x y
program oes
	use fileio
	use mt19937_64
	use epidemia_routines
	implicit none
	integer:: n_agents, timesteps = 1000
	integer :: time, i, xmax, ymax, start_time, n_ini_contaged
	integer(8) :: seed = 21345216
	integer, allocatable :: agents(:,:), agents_prev_dir(:), agent_status(:)
	character(len=40) :: fname_out = "output.xyz"
	real(8) ::  contage_rate, heal_rate, death_rate
	
	time = 0
	xmax = 10
	ymax = 10
	n_agents = 100
	n_ini_contaged = 10
	contage_rate = 1.0
	heal_rate = 0.01
	death_rate = 0.0
	start_time = 10
	
	!Coordinates from 0 to xmax - 1
	
	call init_genrand64(seed)
	allocate(agents(n_agents,2))
	allocate(agents_prev_dir(n_agents))
	allocate(agent_status(n_agents))

	open(1,file=fname_out,status="REPLACE")
	close(1)

	agents = 0
	agents_prev_dir = 0 !no previous direction
	
	!agent_status: 0 = dead, 1 = immune, 2 = healthy, 3 = sick, contageous, >= 4 sick, not contageous
	agent_status = 2
	agent_status(1:n_ini_contaged) = 3
	
	do i = 1, n_agents
		agents(i,1) = genrand64_real2()*xmax
		agents(i,2) = genrand64_real2()*ymax
	end do 
	call write_step(agents,n_agents, agent_status, 0,fname_out)

	do i = 1, timesteps
		call move_agents(agents,n_agents, agents_prev_dir, xmax, ymax)
		call contage_agents(agents, n_agents, agent_status, contage_rate)
		call advance_disease(agent_status, n_agents, death_rate, heal_rate, start_time)
		call write_step(agents, n_agents, agent_status, i, fname_out)
	end do

end program oes
