[BITS 64]

global ffs

SECTION .text
  
ffs:
    ENTER 0, 0
    XOR rcx, rcx

loop:
    CMP rcx, 1
    JZ skip
    INC rcx

skip:
    JMP loop

end:
    MOV rax, rcx
    LEAVE
    RET
