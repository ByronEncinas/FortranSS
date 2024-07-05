Module Calculus
    
    use iso_fortran_env, only: real32, real64, real128

    implicit none

    private

    type, public :: Derivative
    !private
        real(kind=real32) :: Differential
    contains
        procedure :: Diff   => Fluxion
        procedure :: SecDiff => SecondFluxion
    end type Derivative

    type, public :: Integrate
    !private
        real(kind=real32) :: Integral
    contains
        procedure :: Euler   => Euler_Method
        procedure :: Simpson => Simpson_Method
        procedure :: RK => RK4_Method
    end type Integrate

contains

Subroutine Fluxion(self,func, x, delta)

    class(Derivative), intent(in out) :: self
    real(kind=real32), intent(in) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in) :: x

    self%Differential = (func(x + delta) - func(x))/delta
    
    write(*,'(A,F20.10)') "Value of f at x",func(x)
    write(*,'(A,F20.10)') "Value of Derivative of f at x", self%Differential
    

End Subroutine Fluxion

Subroutine SecondFluxion(self,func, x, delta) 
    
    class(Derivative), intent(in out) :: self
    real(kind=real32), intent(in) :: delta
    real(kind=real32), intent(in) :: x
    real(kind=real32), external :: func

    self%Differential =  (func(x + delta) + func(x - delta)  - 2*func(x))/(delta**2)

    write(*,'(A,F20.10)') "Value of Second Derivative of f at x ",self%Differential

End Subroutine SecondFluxion

Subroutine Euler_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real32):: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32):: xi

    write(*,*) ""
    write(*,'(A)') "Here is Euler Method"

    if (delta <= 0) Then
        delta = 1.0e-4_real32
    endif

    self%Integral = 0
    n = floor((ab(2) - ab(1))/delta) 
    
    write(*,'(A, I0)') "Iteration: ",n

    Do i = 1, n, 1 ! for midpoints

        xi = ab(1) + i*delta
        !write(*,*) self%Integral
        self%Integral = self%Integral + func(xi)*delta
    
    End do

    write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

End Subroutine Euler_Method

Subroutine Simpson_Method(self, func, ab, type, delta)
    
    class(Integrate), intent(in out) :: self
    integer:: i,j,k, n
    real, optional, intent(inout) :: delta
    character(len=3), intent(in) :: type ! options: 1/3, 1/8, 3/8
    real, external :: func
    real, intent(in), dimension(2):: ab
    real:: xi, xj, xk

    i = 0
    j = 0
    k = 0
    if (type == '1/3') Then

        write(*,'(A, F20.10)') "Here is Simpsons Method 1/3"

        self%Integral = func(ab(1))*delta/3.0
        n = floor((ab(2) - ab(1))/delta/2) 
        write(*,'(I0)') n

        Do i = 1, n, 1 ! i follow odd numbers  
            j = 2*i-1
            k = 2*i

            xi = ab(1) + delta*i
            xj = ab(1) + delta*j ! odds
            xk = ab(1) + delta*k ! even

            self%Integral = self%Integral + 4*func( xj )*delta/3.0 + 2*func( xk )*delta/3.0

        End do

        self%Integral =  self%Integral + func(ab(2))*delta/3.0

        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

    else if (type == '3/8') Then

        write(*,'(A, F20.10)') "Here is Extended Simpsons Method 3/8"


        self%Integral = (17.0*func(ab(1)) +17.0*func(ab(2)) + &
        & 59*func(ab(1)+delta) +59*func(ab(2)-delta) + &
        & 43*func(ab(1)+2*delta) + 43*func(ab(2)-2*delta) + &
        & 49*func(ab(1)+3*delta) + 49*func(ab(2)-3*delta) )*delta/48.0
        
        n = floor((ab(2) - ab(1))/delta) 

        xi = ab(1)

        Do i = 4, n-4, 1 ! i follow odd numbers  

            xi = ab(1) + delta*i

            self%Integral = self%Integral + func(xi)*delta

        End do

        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

    end if

End Subroutine Simpson_Method


Subroutine RK4_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real32):: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32):: xi

    write(*,*) ""
    write(*,'(A)') "Here is Euler Method"

    if (delta <= 0) Then
        delta = 1.0e-4_real32
    endif

    self%Integral = 0
    n = floor((ab(2) - ab(1))/delta) 
    
    write(*,'(A, I0)') "Iteration: ",n

    Do i = 1, n, 1 ! for midpoints

        xi = ab(1) + i*delta
        !write(*,*) self%Integral
        self%Integral = self%Integral + func(xi)*delta
    
    End do

    write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral
    
End Subroutine RK4_Method

End Module calculus
