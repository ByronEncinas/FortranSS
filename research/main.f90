program Main
    use MShape   ! Assuming the module containing Shape type and procedures is named "ShapeModule"
    implicit none

    ! Type and variable cannot be named the same
    type(Shape) :: myShape

    ! Set the radius using the shape_set_radius procedure
    call myShape%set(5)

    ! Print the shape's information using the shape_print procedure
    call myShape%print()

end program Main
