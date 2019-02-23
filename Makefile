# happy-birthday Makefile

FC = gfortran
FCFLAGS =
OBJS = mod_datetime.o

all: countdown

.SUFFIXES: .f90 .o

# general rule
.f90.o:
	$(FC) $(FCFLAGS) -c $<

%.o: %.mod

# programs
countdown: countdown.f90 $(OBJS)
	$(FC) $(FCFLAGS) $< $(OBJS) -o $@

# modules
mod_datetime.o: mod_datetime.f90

.PHONY:
clean:
	$(RM) *.o *.mod countdown
