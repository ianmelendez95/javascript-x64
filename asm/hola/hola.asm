          global      main
          extern      puts

          section     .text 
main:                                       ; This is called by the C library startup code
          mov         rdi, message          ; first integer (or pointer) argument in rdi
          call        puts                  ; puts(message)
          ret                               ; return from main back into C library wrapper
message:  
          db          "Hola, mundo", 0      ; note strings must be terminated with 0 in C