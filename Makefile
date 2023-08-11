## Makefile (See https://fortran-lang.org/en/learn/building_programs/project_make/ to improve)

FC = gfortran #Compiler
FFLAGS = -O3  #Optimization Flags (maybe tinker with these?)
FSRC = main.f90 
FOBJECTS = $(FSRC:.f90=.o) #Objets

#===============================================
# General Parameters.  
#===============================================

OBJECTS =	$(FOBJECTS) $(COBJECTS)
EXEC =  Executable

#===============================================
# Compilation rules
#===============================================
default: $(EXEC)

clean:	
	rm -f *~ $(OBJECTS) *.pro *.ascii *.nat *.mod *.Gh 

spotless:	
	rm -f *~ $(OBJECTS) $(EXEC)

$(EXEC): $(OBJECTS)
	$(FC) $(FFLAGS) $(FLFLAGS) -o $(EXEC) \
		 $(OBJECTS) -lm

%.o:  %.f90
	$(FC) $(FFLAGS) -c $< -o $@
