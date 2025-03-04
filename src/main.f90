program main

    use iso_fortran_env, only: real32,real64,real128

    use Particles
    use Calculus
    use LinAlg

    implicit none

    ! Type and variable cannot be named the same
    ! type(Particle):: electron
    type(Integrate):: Integration
    type(Derivative):: Flux
    real(kind=real32), dimension(2):: interval
    real(kind=real32):: delta = 1.0e-3_real32

! Test for Calculus Class

    print*, real32,real64,real128
    print*, 4,8,16

    interval = [1.0_real32,2.0_real32]
    
    call Integration%Euler(func, interval, delta)
    write(*,*)
    call Integration%Simpson(func, interval, "1/3", delta)
    write(*,*)
    call Integration%Simpson(func, interval, "3/8", delta)
    write(*,*)
    call Flux%Diff(func, 0.0_real32, delta)
    write(*,*)
    call Flux%SecDiff(func, 0.0_real32, delta)


contains

real function func(x) result(res) ! output
    real(kind=real32), intent (in) :: x ! input

    res = 2.*x**3. +2.*x -x**2.+1.

end function

end program main
