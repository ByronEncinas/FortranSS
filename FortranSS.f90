program main
    
    implicit none
    
    real,parameter :: sp = selected_real_kind(p=6,r=37)
    real,parameter :: dp = selected_real_kind(p=15,r=307)

    call system("echo 'Hola mundo'")

    Print*, Precision(sp)
    Print*, Precision(dp)

end program main
