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
    real(kind=real32):: delta = 1.0e-6_real32
    type(Matrix) :: A
    real(kind=real32) :: det
    integer :: i, j, dim

    dim = 4  ! Size of the matrix

    ! Initialize matrix A
    call A%init(dim)

    ! Print the matrix
    print *, "Matrix A:"
    do i = 1, dim
        print *, (A%M(i, j), j = 1, dim)
    end do

    ! Compute determinant
    call A%determinant(det)

    ! Print result
    print *, "Determinant of A:", det
    

    ! Test for Calculus Class

    interval = [0.0_real32,1.0_real32]
    
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

    res = x**2.

end function

end program main
