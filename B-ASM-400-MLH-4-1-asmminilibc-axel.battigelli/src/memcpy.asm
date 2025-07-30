[BITS 64]

global memcpy
global my_memcpy

SECTION .text

memcpy:
my_memcpy:
    ENTER 0, 0
    MOV rcx, rdx
    MOV rdx, rdi
    CMP rcx, 0
    JE end
    REP MOVSB

end:
    MOV rax, rdx
    LEAVE
    RET
