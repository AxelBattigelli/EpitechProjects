[BITS 64]

global memset
global my_memset

SECTION .text

my_memset:
    ENTER 0, 0
    XOR rcx, rcx
    TEST rdx, rdx
    JZ end

loop:
    MOV [rdi + rcx], sil
    INC rcx
    DEC rdx
    JNZ loop

end:
    MOV rax, rdi
    LEAVE
    RET
