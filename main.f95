!Agents: status mortal_rate contage_rate x y
program oes
	use fileio
	use mt19937_64
	implicit none
	integer:: n_agents = 100, timesteps = 100
	integer :: time, i, j
	integer(8) :: seed = 21345216
	integer, allocatable :: agents(:,:)
	character(len=40) :: fname_out = "output.xyz"
	real(8) :: u
	
	time = 0
	
	call init_genrand64(seed)
	allocate(agents(n_agents,2))

	open(1,file=fname_out,status="REPLACE")
	close(1)

	agents = 0
	
	do i = 1, n_agents
		agents(i,1) = genrand64_real2()*100
		agents(i,2) = genrand64_real2()*100
	end do 
	call write_step(agents,n_agents,time,fname_out)
	do i = 1, timesteps
		do j = 1, n_agents
			!Drunken sailor
			u = genrand64_real2()
			if(u>0.75) then
				agents(j,1) = agents(j,1) + 1 
			else if(u>0.5) then
				agents(j,1) = agents(j,1) - 1 
			else if(u>0.25) then
				agents(j,2) = agents(j,2) + 1 
			else 
				agents(j,2) = agents(j,2) - 1
			end if		
		end do 
		
		call write_step(agents,n_agents,time,fname_out)
	end do

end program oes
