[BITS 64]

global strlen
global my_strlen

SECTION .text
  
strlen:
my_strlen:
    ENTER 0, 0
    XOR rcx, rcx

loop:
    CMP BYTE [rdi + rcx], 0
    JZ end
    INC rcx
    JMP loop

end:
    MOV rax, rcx
    LEAVE
    RET
