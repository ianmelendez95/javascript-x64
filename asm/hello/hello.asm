global _start  ; must be declared for linker (ld)

          section   .text
_start:   mov       rax, 1                ; system call for write
          mov       rdi, 1                ; file handle 1 is stdout
          mov       rsi, message          ; address of string to output
          add       rsi, 0
          mov       rdx, 13               ; number of bytes
          syscall                         ; invoke operating system to do the write
          mov       rax, 60               ; system call for exit
          xor       rdi, rdi              ; exit code 0
          syscall                         ; invoke operating system to exit

          section   .data 
message:  db        "Hello, World!", 10   ; string to be printed
