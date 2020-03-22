!Agents: status mortal_rate contage_rate x y
program oes
	use fileio
	use mt19937_64
	use epidemia_routines
	implicit none
	integer:: n_agents, timesteps = 1000
	integer :: time, i, xmax, ymax
	integer(8) :: seed = 21345216
	integer, allocatable :: agents(:,:), agents_prev_dir(:)
	character(len=40) :: fname_out = "output.xyz"
	
	time = 0
	xmax = 10
	ymax = 10
	n_agents = 5
	
	!Coordinates from 0 to xmax - 1
	
	call init_genrand64(seed)
	allocate(agents(n_agents,2))
	allocate(agents_prev_dir(n_agents))

	open(1,file=fname_out,status="REPLACE")
	close(1)

	agents = 0
	agents_prev_dir = 0 !no previous direction
	
	do i = 1, n_agents
		agents(i,1) = genrand64_real2()*xmax
		agents(i,2) = genrand64_real2()*ymax
	end do 
	call write_step(agents,n_agents,0,fname_out)

	do i = 1, timesteps
		call move_agents(agents,n_agents, agents_prev_dir, xmax, ymax)
		call write_step(agents,n_agents,i,fname_out)
	end do

end program oes
