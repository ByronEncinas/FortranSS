Module stat

    implicit none


    Contains

    ! Standard Deviation 
    ! sample: array | data_num: integer
    Real Function std_dev(sample, data_num) result()

    End Function

    ! given a floatin point operation, this will return the uncertainty deviation from the mean
    Real Function plus_minus(xi, yi, operator) result(res_interval)
        Integer:: i
        Real(2)::res_interval, aux
        Real(2), intent(in):: xi, yi
        Character(len=2), intent(in):: operator 
        
        ! possible operators + - * /
        if      (operator == '+')then
            res_interval = [min(xi(1) + yi(1), xi(2) + yi(2)) , max(xi(1) + yi(1),xi(2) + yi(2))]
        else if (operator == '-')then
            res_interval = [min(xi(1) - yi(1), xi(2) - yi(2)) , max(xi(1) - yi(1),xi(2) - yi(2))]
        else if (operator == '*')then
            ! find minimum in maximum across all this
            xi(1)*yi(1)
            xi(1)*yi(2)
            xi(2)*yi(1),xi(2)*yi(1)

            if (min( xi(1)*yi(1), xi(1)*yi(2))  < min( xi(2)*yi(1),xi(2)*yi(1) ) ) then
                minimum = min(xi(1)*yi(1), xi(1)*yi(2))
            else
                minimum = min( xi(2)*yi(1),xi(2)*yi(1) )
            endif

            if (max( xi(1)*yi(1), xi(1)*yi(2)) > max( xi(2)*yi(1),xi(2)*yi(1) ) ) then
                maximum = max(xi(1)*yi(1), xi(1)*yi(2))
            else
                maximum = max( xi(2)*yi(1),xi(2)*yi(1) )
            endif

            res_interval = [minimum, maximum]
            
        else if (operator == '/')then
            ! for special case [x]/[y] = [x1, x2]*[1/y1, 1/y2]

            if (yi(1) /= 0 .and. yi(2) /= 0) then

                aux = [1/yi(1), 1/yi(2)]
                res_interval = plus_minus(xi, aux, '*')
            else
                ERROR STOP 'second body interval cannot contain zero'

            endif
    
        else
            ERROR STOP 'Operator input in plus_minus must be elemental operation + - * /'
        end if

    End Function

End Module stat