program main

    use iso_fortran_env, only: real32,real64,real128

    use Particles
    use Calculus
    use LinAlg

    implicit none

    real(kind=real32):: delta = 1.0e-8_real32
    type(Matrix) :: A
    real(kind=real32) :: det
    integer :: i, j, dim
    real(kind=real32) :: xi, xj, max_tolerance
    real(kind=real32), parameter :: tolerance_threshold = 1.0e-6_real32

    xi = 1.0_real32  ! Starting guess
    max_tolerance = tolerance_threshold  

    ! Call the fixed_point subroutine
    call Fixed_Point_Method(func, xi, xj, max_tolerance)


    ! Output the result
    print *, "Fixed point found: ", xj
    print *, "Initial guess: ", xi
    print *, "Tolerance: ", max_tolerance

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

    print *, "Determinant of A:", det

contains

real function func(x)
    real(kind=real32), intent(in) :: x
    func =  cos(x)*sin(x)-x
end function func

end program main
