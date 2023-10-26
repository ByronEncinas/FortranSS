module LinAlg

    use iso_fortran_env, only: real32

    implicit none

    private

    type, public :: Matrix
    !private
        integer(kind=real32) :: dim
        real(kind=real32), allocatable:: M(:,:)
        character(len=20):: data = "Square Matrices ONLY"
    contains
        procedure :: init   => init
    end type Matrix

    type, public :: Vector
    !private
        real(kind=real32):: vector(3)
        real(kind=real32):: norm
        character(len=20):: data = "Vector in Space"
    contains
        procedure :: set   => set
        procedure :: mag   => mag
    end type Vector

contains

    subroutine set(self)

        integer:: i
        class(Vector), intent(in out) :: self

        read*, (self%vector(i), i = 1, 3)

    end subroutine set   

    subroutine mag(self)

        class(Vector), intent(in out) :: self

        ! calculate magnitude of vector
        self%norm = sqrt(self%vector(1)*self%vector(1) + self%vector(2)*self%vector(2) + self%vector(3)*self%vector(3))

    end subroutine mag

    subroutine init(self, dim)

        integer:: i, j
        integer, intent(in) :: dim
        class(Matrix), intent(in out) :: self

        self%dim = dim
        
        allocate(self%M(dim,dim))

        ! read the matrix here
        do j = 1, dim
            read*, (self%M(i,j), i = 1, dim)
        end do

    end subroutine init   

    subroutine levi_civita(Eijk)

        integer, intent(in out), dimension(3,3,3):: Eijk

        ! set up all zero
        Eijk(:,:,:) = 0

        ! even permutations
        Eijk(1,2,3) = 1
        Eijk(3,1,2) = 1
        Eijk(2,3,1) = 1

        ! odd permutations
        Eijk(1,3,2) = -1
        Eijk(3,2,1) = -1
        Eijk(2,1,3) = -1

    end subroutine levi_civita
    
end module LinAlg