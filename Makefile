# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.f95)
OBJS=$(patsubst %.f95,%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.f95)
MOD_OBJS=$(patsubst %.f95,%.o,$(MODS))

# Compiler/Linker settings
FC = gfortran
FLFLAGS = -g
FCFLAGS = -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
PROGRAM = oes
PRG_OBJ = $(PROGRAM).o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.f95
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

# If something doesn't work right, have a 'make debug' to 
# show what each variable contains.
debug:
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"
	@echo "PRG_OBJ = $(PRG_OBJ)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# Blocks and allocations depends on shared
mod_blocks.o mod_allocations.o : mod_shared.o
