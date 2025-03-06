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
    real(kind=real32):: delta = 1.0e-8_real32
    type(Matrix) :: A
    real(kind=real32) :: det
    integer :: i, j, dim

    dim = 4  ! Size of the matrix

    ! Initialize matrix A
    call A%init(dim, 0)

    A%M(1,1) = 1.0
    A%M(1,2) = 0.0
    A%M(1,3) = 0.0
    A%M(2,1) = 0.0
    A%M(2,2) = 1.0
    A%M(2,3) = 0.0
    A%M(3,1) = 0.0
    A%M(3,2) = 0.0
    A%M(3,3) = 1.0

    ! Print the matrix
    print *, "Matrix A:"
    do i = 1, dim
        print *, (A%M(i, j), j = 1, dim)
    end do

    ! Compute determinant
    call A%determinant(det)

    ! Print result
    print *, "Determinant of A:", det
    
end program main
