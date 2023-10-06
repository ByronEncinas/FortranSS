program FSS

    use iso_fortran_env, only: real32
    use Particles
    use Calculus
    !use MShape   ! Assuming the module containing Shape type and procedures is named "ShapeModule"
    
    implicit none

    ! Type and variable cannot be named the same
!   type(Particle):: electron
    type(Integrate):: Integration
    real(kind=real32), dimension(2):: interval
    real(kind=real32):: delta = 1.0e-6_real32

! Test for Calculus Class

    interval = [0.0_real32,1.0_real32]

    call Integration%Euler(func, Interval, delta)

    print*, Integration%Integral

! Test for Particle Class

!  call electron%init("Electron  ", 9.1E-31_real32, 1.601E-19_real32, [0.0_real32,0.0_real32,0.0_real32], &
!                                                                       [0.0_real32,0.0_real32,0.0_real32])
!   print*, electron%name
!   print*, electron%mass
!   print*, electron%charge!    
!   print*, electron%position
!   print*, electron%velocity!
!   gfortran -c main.f90
!   gfortran -c Particles.f90
!   gfortran -o exe Particles.o main.o    
!   ./exe!!!    
!   call electron%displacemen![0.7_real32,0.6_real32,0.5_real32], electron%position)!
!   print*, electron%position!


contains

real function func(x) result(res) ! output
    real(kind=real32), intent (in) :: x ! input

    res = x**2 
end function

end program FSS