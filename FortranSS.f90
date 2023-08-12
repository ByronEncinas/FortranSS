program main
    
    implicit none
    
    integer,parameter :: sp = selected_real_kind(p=6,r=37)
    integer,parameter :: dp = selected_real_kind(p=15,r=307)

    call system("echo 'Hola mundo'")

    Print*, Precision(sp)
    Print*, Precision(dp)

end program main