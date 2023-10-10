Module Calculus
    
    use iso_fortran_env, only: real32, real64, real128

    implicit none

    private

    type, public :: Integrate
    !private
        real(kind=real32) :: Integral
    contains
        procedure :: Euler   => Euler_Method
        procedure :: Simpson => Simpson_Method
        procedure :: RungeKutta => RK4_Method
    end type Integrate

contains

Subroutine Euler_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real32):: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32):: xi

    write(*,*) "Here is Euler Method"

    !if( .not. present(delta) ) delta = 1.0e-6_real32 ! default value

    write(*,*) delta

    self%Integral = 0
    n = floor((ab(2) - ab(1))/delta) 
    
    write(*,*) n

    Do i = 1, n, 1 ! for midpoints

        xi = ab(1) + i*delta/2
        write(*,*) self%Integral
        self%Integral = self%Integral + func(xi)*delta
    
    End do

    self%Integral =  (self%Integral)*delta

End Subroutine Euler_Method

Subroutine Simpson_Method(self, func, delta, ab, Integral, type)
    
    class(Integrate), intent(in out) :: self
    integer:: i, n
    real, optional, intent(inout) :: delta
    character(len=5), optional, intent(inout) :: type ! options: 1/3, 1/8, 3/8
    real, external :: func
    real, intent(out) :: Integral
    real, intent(in), dimension(2):: ab
    real:: xi

    if( .not. present(delta) ) delta = 1e-5_real32 ! default value

    if( .not. present(type) ) type = '1/3' ! default value

    if (type == '1/3') Then

        write(*,*) "Here is Simpsons Method 1/3"

        Integral = 0
        n = floor((ab(2) - ab(1))/delta/2) 

        Do i = 1, n, 1 ! i follow odd numbers  

            xi = ab(1) + delta*i
            Integral = Integral + func( 2*xi - 2 ) + 4*func( 2*xi - 1 ) + 4*func( 2*xi )

        End do

        self%Integral =  (Integral + func(ab(1)) + func(ab(2)))*delta/3.0

    else if (type == '1/8') Then

        write(*,*) "Here is Simpsons Method 1/8"

    else if (type == '3/8') Then

        write(*,*) "Here is Simpsons Method 3/8"

    end if

End Subroutine Simpson_Method


Subroutine RK4_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real32):: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32):: xi

    write(*,*) "Here is Euler Method"

    !if( .not. present(delta) ) delta = 1.0e-6_real32 ! default value

    write(*,*) delta

    self%Integral = 0
    n = floor((ab(2) - ab(1))/delta) 
    
    write(*,*) n

    Do i = 1, n, 1 ! for midpoints

        xi = ab(1) + i*delta/2
        write(*,*) self%Integral
        self%Integral = self%Integral + func(xi)*delta
    
    End do

    self%Integral =  (self%Integral)*delta

End Subroutine RK4_Method




Subroutine Derivative( func, x, delta, diff )
    
    real, optional,intent(inout) :: delta
    real, external :: func
    real, intent(out) :: diff, x

    if( .not. present(delta) ) delta = 1e-6 ! default value

        diff = (func(x + delta) - func(x))/delta

End Subroutine Derivative

Subroutine SecondDerivative(func, x, delta, diff)
    
    real, optional,intent(inout) :: delta
    real, external :: func
    real, intent(out) :: diff, x

    if( .not. present(delta) ) delta = 1e-6 ! default value

        diff = (func(x + delta) + func(x - delta)  - 2*func(x))/(delta**2)

End Subroutine SecondDerivative

End Module calculus
