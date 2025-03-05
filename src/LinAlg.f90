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
        procedure :: determinant   => determinant
!        procedure :: trace   => trace
!        procedure :: eigen   => eigenmethods
!        procedure :: lu_decom   => lu_decomposition
!        procedure :: qr_decom   => qr_decomposition
!        procedure :: inverse   => inverse
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

    subroutine determinant(self, deter)
    ! recursive implementation

        integer:: i,j,k
        real(kind=real32), intent(out) :: deter
        real(kind=real32), intent(in out) :: minor_deter
        class(Matrix), intent(in out) :: self
	class(Matrix), intent(in out) :: cof(:,:)

        if (self%dim == 1) then
            deter = self%M(1,1)
            return
        endif
        
        if (self%dim == 2) then
            deter = self%M(1,1)*self%M(2,2)-self%M(1,2)*self%M(2,1)
            return
        endif

        allocate(cof(self%dim - 1, self%dim - 1))
        deter = 0.0_real32

        do j = 1, self%dim
            do i = 1, self%dim
                if (i == 1) cycle
                do k = 1, self%dim
                    if (k == j) cycle
                    if (i > 1 .and. k > j) then
                        cof%M(i-1, k-1) = self%M(i, k)
                    elseif (i > 1) then
                        cof%M(i-1, k) = self%M(i, k)
                    endif
                end do
            end do

            call determinant(cof, minor_deter)
            deter  =  deter + (-1)**(i+j) * self%M(i, j)*minor_deter
        end do

    end subroutine determinant

    ! Subroutine to calculate the trace of a matrix
    ! subroutine trace(self, trace)

    !     integer:: j   ! Loop index for summing diagonal elements
    !     real(kind=real32), intent(out) :: trace  ! Output variable to store the trace value
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     trace = 0.0_real32  ! Initialize trace to zero

    !     ! Loop over the diagonal elements of the matrix and sum them
    !     do j = 1, self%dim  ! Loop over each diagonal element (1 to dim)
    !         trace = trace + self%M(j,j)  ! Add the j-th diagonal element to the trace
    !     end do

    ! end subroutine trace  ! End of trace subroutine


    ! Subroutine for Gaussian elimination (partial implementation)
    ! subroutine gaussian_elimination(self)

    !     integer:: j   ! Loop index for rows of the matrix
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)
    !     real(kind=real32), dimension(self%dim):: row  ! Array to temporarily store a row of the matrix
    !     real(kind=real32):: elem  ! Variable to store individual matrix elements
        
    !     ! Loop over the rows of the matrix for Gaussian elimination
    !     do j = 1, self%dim  ! Loop from 1 to the number of rows (dim)
    !         vec  = self%M(j,:)  ! Extract the entire j-th row from the matrix and assign it to vec
    !     end do

    ! end subroutine gaussian_elimination  ! End of gaussian_elimination subroutine


    ! Subroutine for QR decomposition (partial implementation)
    ! subroutine qr_decomposition(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for the matrix dimension
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here, typically used in QR)

    ! end subroutine qr_decomp  ! End of qr_decomposition subroutine


    ! Subroutine for LU decomposition (partial implementation)
    ! subroutine lu_decomposition(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for matrix operations
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here, typically used in LU)

    ! end subroutine lu_decomp  ! End of lu_decomposition subroutine


    ! Subroutine for computing eigenvalues (partial implementation)
    ! subroutine eigenmethods(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for matrix operations
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here)

    ! end subroutine eigenmethods  ! End of eigenmethods subroutine


end module LinAlg
